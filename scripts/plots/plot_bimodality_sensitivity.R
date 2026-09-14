# =============================================================================
#  ГРАФИК: градиент alpha / H / a_mean по границе усечения выборки
# =============================================================================
#  Результат шага 2 — не значение на какой-то одной границе, а сам градиент:
#  естественного порога нет, поэтому «alpha на вовлечённой подвыборке» есть
#  артефакт выбранной границы. Рисунок показывает именно это. Монотонность по
#  метрикам не заявляется подписью, а читается из cut_monotonicity.csv.
# =============================================================================

# Бутстрап: рабочая директория — корень проекта, затем общие функции графиков
local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/plots/_plot_utils.R")

library(tidyverse)
library(ggplot2)

# КОНФИГУРАЦИЯ
width  <- W_2COL   # дюймы
height <- 3.4      # дюймы
dpi    <- PLOT_DPI

data_file <- "output/Bimodality/cut_sensitivity.csv"
mono_file <- "output/Bimodality/cut_monotonicity.csv"
require_input(c(data_file, mono_file), "scripts/2_bimodality.R (шаг 2)")

cuts <- read_csv(data_file)
mono <- read_csv(mono_file)

SWEEP_LAB <- c(lower     = "Lower cut moves (upper fixed)",
               upper     = "Upper cut moves (lower fixed)",
               symmetric = "Symmetric trim (both ends)")
METRIC_LAB <- c(alpha = "alpha", H = "Loevinger H", a_mean = "mean 2PL a")
# METRIC_LAB — литерал поверх набора метрик, который задаёт шаг 2, поэтому проверка
# ПОЛНАЯ, а не по пустому пересечению как в need_levels(): здесь набор фиксирован
# производителем, и метрика без подписи ушла бы в подзаголовок строкой NA, то есть
# молчаливой деградацией вместо отказа.
MONO_COLS <- c("Metric", "Monotone", "N_assessed", "N_points")
absent <- setdiff(MONO_COLS, names(mono))
if (length(absent))
  stop(sprintf(paste0("plot_bimodality_sensitivity: в cut_monotonicity.csv нет колонок %s. ",
                      "Артефакт старше mono_one() в scripts/2_bimodality.R -- нужен полный ",
                      "прогон, иначе подпись потеряла бы оговорку об охвате молча."),
               paste(absent, collapse = ", ")), call. = FALSE)

unlabelled <- setdiff(mono$Metric, names(METRIC_LAB))
if (length(unlabelled))
  stop(sprintf(paste0("plot_bimodality_sensitivity: у метрик %s нет подписи в METRIC_LAB. ",
                      "Литерал разошёлся с mono_tbl в scripts/2_bimodality.R."),
               paste(unlabelled, collapse = ", ")), call. = FALSE)

# omega_h в рисунок не идёт намеренно: на этих объёмах он скачет без
# закономерности (шаг 2, раздел 5), и линия читалась бы как результат.
plot_df <- cuts %>%
  select(Sweep, Cut, n, alpha, H, a_mean) %>%
  pivot_longer(c(alpha, H, a_mean), names_to = "Metric", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  mutate(Sweep  = factor(SWEEP_LAB[Sweep], levels = unname(SWEEP_LAB)),
         Metric = factor(METRIC_LAB[Metric], levels = unname(METRIC_LAB)))

# Подзаголовок собирается по измерению шага 2, а не литералом: заявление
# «монотонно во всех трёх развёртках» отрицало бы линии под собой, если по одной из
# метрик монотонность нарушена (на текущих данных так и есть у a_mean, который на
# крайнем усечении вырождается в отрицательное значение). Ветка трёхзначна:
# Monotone = NA означает метрику, не оценившуюся ни в одной развёртке, и %in%
# намеренно не приравнивает её ни к TRUE, ни к FALSE. Охват (N_assessed / N_points)
# называется рядом с метрикой, когда он неполон: точка, снятая гейтом сходимости шага 2,
# в вердикт не входит, и без этой оговорки подпись выдала бы вердикт по подмножеству
# точек за вердикт по всей развёртке.
is_mono  <- as.logical(mono$Monotone)
partial  <- mono$N_assessed < mono$N_points
mono_lab <- function(sel)
  paste(sprintf("%s%s", unname(METRIC_LAB[mono$Metric[sel]]),
                ifelse(partial[sel],
                       sprintf(" (%d of %d points)", mono$N_assessed[sel], mono$N_points[sel]),
                       "")),
        collapse = ", ")
sub_txt <- if (nrow(mono) && all(is_mono %in% TRUE) && !any(partial)) {
  "Monotone in all three sweeps: no natural threshold, so any single cut is arbitrary"
} else {
  paste0(
    "No natural threshold, so any single cut is arbitrary",
    if (any(is_mono %in% TRUE))  sprintf("; monotone: %s", mono_lab(is_mono %in% TRUE)) else "",
    if (any(is_mono %in% FALSE)) sprintf("; non-monotone: %s", mono_lab(is_mono %in% FALSE)) else "",
    if (anyNA(is_mono))          sprintf("; not assessed: %s", mono_lab(is.na(is_mono))) else "")
}

# Цвет И форма маркера на метрику: на трёх сплошных чёрных линиях кривые
# различались только маркером, и в местах пересечения принадлежность точки
# кривой читалась по соседям, а не по самой точке.
METRIC_COL <- setNames(OK_PAL[c(1, 3, 4)], unname(METRIC_LAB))

p <- ggplot(plot_df, aes(x = Cut, y = Value, group = Metric,
                         shape = Metric, colour = Metric)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.5) +
  facet_wrap(~ Sweep, scales = "free_x") +
  scale_shape_manual(values = c(16, 17, 15), name = NULL) +
  scale_colour_manual(values = METRIC_COL, name = NULL) +
  scale_x_continuous(breaks = function(lim) seq(ceiling(lim[1]), floor(lim[2]), by = 1)) +
  labs(
    title = "Reliability and scalability against the truncation cut",
    subtitle = wrap_lab(sub_txt),
    x = "Cut point on Score_Total",
    y = "Value"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# Сохранение
OUT <- "output/Bimodality/plots"
output_path <- file.path(OUT, paste0("cut_sensitivity.", PLOT_FORMAT))
save_ggplot(p, output_path, width, height, dpi = dpi)
