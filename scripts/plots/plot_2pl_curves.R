# =============================================================================
#  ГРАФИКИ: набор визуализаций 2PL IRT
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
library(ggrepel)
library(mirt)   # штатные кривые: plot(mod_2pl, type = "trace")

# КОНФИГУРАЦИЯ
dpi <- PLOT_DPI

# Размеры графиков (ширина, высота в дюймах). Ширина — полоса набора: рисунок,
# нарисованный шире, в вёрстке сжимается вместе со всеми подписями.
dim_icc_all        <- c(W_2COL, 5.4)   # панель на пункт: штатная сетка mirt
dim_test_info      <- c(W_2COL, 3.4)
dim_ab_bubble      <- c(W_2COL, 4.6)
dim_wright_map     <- c(W_2COL * 0.72, 6.2)

OUT <- "output/2PL"
params_path <- file.path(OUT, "2pl_params.csv")
theta_path  <- file.path(OUT, "2pl_theta.csv")
model_path  <- file.path(OUT, "2pl_model.RData")

# Все три артефакта пишет шаг 6 безусловно (2PL сошлась -> есть и a/b, и EAP-теты,
# и сама модель), поэтому карта Райта и штатные кривые mirt — не «по возможности»,
# а обязательная часть набора.
require_input(c(params_path, theta_path, model_path), "scripts/6_2pl.R (шаг 6)")

loaded_objs <- load(model_path) # mod_1pl, mod_2pl
# Артефакт старше рисунка роняет его, а не подставляет кривые другой модели.
if (!"mod_2pl" %in% loaded_objs)
  stop(sprintf(paste0("УСТАРЕВШИЙ ВХОД для кривых 2PL: в %s нет mod_2pl ",
                      "(есть: %s) — сначала перезапустите 6_2pl.R."),
               model_path, paste(loaded_objs, collapse = ", ")), call. = FALSE)

params_full <- read_csv(params_path)
source("scripts/config.R")
validate_items(params_full$Item, "2PL curves (2pl_params.csv)")

# Сохранение графика в OUT/plots/<base_name>.<format> через общий выбор устройства
save_plot <- function(plot_obj, base_name, dims) {
  output_path <- file.path(OUT, "plots", paste0(base_name, ".", PLOT_FORMAT))
  save_ggplot(plot_obj, output_path, dims[1], dims[2], dpi = dpi)
}

theta_seq <- seq(-4, 4, by = 0.05)
# Собственное имя, а не ITEMS: этот вектор — состав пунктов ПРОЧИТАННОГО артефакта, а
# ITEMS из config.R — состав, объявленный input/items.csv, и сверяет их validate_items()
# выше. Присваивание в ITEMS сделало бы любую последующую сверку проверкой артефакта
# самим собой, потому что validate_items() берёт ITEMS из окружения скрипта.
plot_items <- params_full$Item

# ── 1. ICC ВСЕХ ПУНКТОВ ─────────────────────────────────────────────────────
# Цвет — по СУБШКАЛЕ, не по пункту: легенда из 26 цветов неразличима по
# построению, а вопрос рисунка — разброс кривых внутри субшкалы. Параметры
# отдельного пункта несут 2pl_ab_bubble и 2pl_params.csv.
# Кривые рисует ШТАТНЫЙ mirt: plot(mod_2pl, type = "trace") отдаёт объект trellis
# с панелью на пункт — одна страница, поэтому в артефакт попадают все 26 (в
# отличие от TAM, который на pnp/tiff нумеровать страницы не может; см.
# plot_rasch_icc.R). Вход — объект модели, сохранённый шагом 6, а не a/b из CSV:
# рисунок остаётся транскрипцией артефакта и ничего не пересчитывает.
# Кегли — из cex_pub(), как у theme_pub() и у остальных рисунков пакетов: lattice
# считает cex от собственного fontsize (12), поэтому подобранные на глаз значения
# давали здесь другой заголовок, чем rel(1.15) на соседнем рисунке.
#
# xlab/ylab НЕ передаются: подписи осей mirt ставит сам (theta и P(theta)), и в
# сигнатуре его метода plot их нет — переданные через ... они дошли бы до
# lattice::xyplot вторым набором и уронили бы вызов на «argument matched by
# multiple actual arguments».
p_icc <- mirt::plot(
  mod_2pl, type = "trace", facet_items = TRUE,
  main = "Item characteristic curves (2PL)",
  par.strip.text = list(cex = cex_pub(0.75)),
  par.settings = list(
    axis.text        = list(cex = cex_pub(0.7)),
    par.xlab.text    = list(cex = cex_pub(1.0)),
    par.ylab.text    = list(cex = cex_pub(1.0)),
    par.main.text    = list(cex = cex_pub(1.15), font = 2, just = "left",
                            x = grid::unit(5, "mm")),
    superpose.line   = list(lwd = 0.7)
  )
)

# print(), а не save_plot(): trellis печатается на устройство так же, как ggplot,
# но объект другого класса, и вызов через save_base_plot() это не скрывает.
save_base_plot(function() print(p_icc),
               file.path(OUT, "plots", paste0("2pl_icc_all.", PLOT_FORMAT)),
               dim_icc_all[1], dim_icc_all[2], dpi = dpi)


# ── 2. ИНФОРМАЦИОННАЯ ФУНКЦИЯ ТЕСТА ─────────────────────────────────────────
info_data <- lapply(seq_along(plot_items), function(j) {
  item_row <- params_full[j, ]
  a_j <- item_row$a
  b_j <- item_row$b
  p_j <- 1 / (1 + exp(-a_j * (theta_seq - b_j)))
  tibble(theta = theta_seq, Info = a_j^2 * p_j * (1 - p_j), Item = item_row$Item)
}) %>% bind_rows()

test_info <- info_data %>%
  group_by(theta) %>%
  summarise(TI = sum(Info), SEM = 1 / sqrt(TI), .groups = "drop")

# SEM живёт на своей оси в СВОИХ единицах (стандартная ошибка в логитах), а не
# в долях максимума информации. Прежний рисунок множил SEM на max(TI) и подписывал
# вторую ось как «SEM (relative)»: величина с оси не снималась, а сравнить её с
# принятыми порогами (SEM = 0.5 -> надёжность около 0.75) было нельзя.
SEM_SCALE <- max(test_info$TI) / max(test_info$SEM)

p_info <- ggplot(test_info, aes(x = theta)) +
  geom_area(aes(y = TI), fill = OK_PAL[6], alpha = 0.35) +
  geom_line(aes(y = TI), colour = OK_PAL[1], linewidth = 0.7) +
  geom_line(aes(y = SEM * SEM_SCALE), colour = OK_PAL[4], linewidth = 0.6, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-4, 4, 1)) +
  scale_y_continuous(
    name = "Test information",
    sec.axis = sec_axis(~ . / SEM_SCALE, name = "SEM (logits)")
  ) +
  labs(
    title = "Test information function (2PL)",
    subtitle = wrap_lab("Solid blue with shading: test information. Dashed orange: standard error of measurement on the right-hand axis."),
    x = expression(theta ~ "(logits)")
  ) +
  theme_pub() +
  theme(axis.title.y.left  = element_text(colour = OK_PAL[1]),
        axis.title.y.right = element_text(colour = OK_PAL[4]))

save_plot(p_info, "2pl_test_info", dim_test_info)


# ── 3. ПУЗЫРЬКОВАЯ ДИАГРАММА A VS B ─────────────────────────────────────────
# Расхождение 2PL с ограничением Раша a = 1 читается ЗДЕСЬ, по расстоянию точки
# от пунктирной линии, и сразу по всем 26 пунктам. Отдельного рисунка с парами
# кривых 1PL/2PL для шести крайних пунктов (2pl_icc_comparison) нет: он показывал
# то же самое расхождение по подмножеству пунктов и в виде, из которого величина
# a не снимается, а полный набор кривых 2PL уже даёт рисунок 1.

# Категории дискриминации по Baker & Kim (2004) для раскраски
DISC_LEV <- c("negligible", "low", "moderate", "high", "very high")
params_bubble <- params_full %>%
  mutate(
    disc_class = factor(case_when(
      a < 0.35 ~ "negligible",
      a < 0.65 ~ "low",
      a < 1.35 ~ "moderate",
      a < 1.70 ~ "high",
      TRUE     ~ "very high"
    ), levels = DISC_LEV)
  )

# Подписи пунктов разводит ggrepel. Прежний geom_text(vjust = -0.9) ставил метку
# в фиксированное место над точкой, поэтому в сгущениях метки ложились на
# соседние пузыри и друг на друга, а цвет метки совпадал с цветом пузыря.
p_bubble <- ggplot(params_bubble, aes(x = b, y = a)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70", linewidth = 0.3) +
  geom_hline(yintercept = 1.0, linetype = "dotted", colour = "grey70", linewidth = 0.3) +
  geom_point(aes(size = p_value, colour = disc_class), alpha = 0.7) +
  geom_text_repel(aes(label = Item), size = 2.2, colour = "grey15",
                  point.padding = 0.15, box.padding = 0.25, segment.size = 0.2,
                  segment.colour = "grey65", min.segment.length = 0.2,
                  max.overlaps = Inf, seed = 1) +
  scale_size_continuous(range = c(1.2, 5), name = "p-value") +
  # drop = TRUE: класс без единого пункта иначе стоит в легенде строкой без
  # плашки, и пустая строка читается как сбой отрисовки, а не как «таких пунктов
  # нет». Полная шкала классов названа в подписи.
  scale_colour_manual(values = setNames(OK_PAL[c(8, 5, 6, 1, 3)], DISC_LEV),
                      drop = TRUE, name = "Discrimination") +
  labs(
    title = "2PL item parameters: discrimination against difficulty",
    subtitle = wrap_lab("Bubble area is the classical p-value (share correct). Dotted line: the Rasch constraint a = 1. Dashed line: b = 0."),
    x = "Difficulty b (logits)",
    y = "Discrimination a",
    caption = wrap_lab("Discrimination classes follow Baker & Kim (2004): negligible < 0.35, low < 0.65, moderate < 1.35, high < 1.70, very high otherwise. Only the classes present in the data appear in the legend.")
  ) +
  theme_pub() +
  theme(legend.position = "right", legend.box = "vertical")

save_plot(p_bubble, "2pl_ab_bubble", dim_ab_bubble)


# ── 4. КАРТА РАЙТА ───────────────────────────────────────────────────────────
# Раскладку строит wright_map() из _plot_utils.R — та же, что у карты Раша:
# карты двух моделей читаются рядом только пока у них совпадают оси и подписи.
b_vals <- params_full %>%
  transmute(Item, Difficulty = b) %>%
  arrange(Difficulty)

# theta_path проверен вверху через require_input, поэтому ветки без тет здесь нет:
# рисунок с ДРУГИМ содержанием под тем же именем файла — подмена, заметная только
# по заголовку.
th <- read_csv(theta_path)$Theta
BINW <- 0.25   # логиты

draw_wright <- function() {
  wright_map(
    theta    = th,
    items    = b_vals,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    binwidth = BINW,
    width_in = dim_wright_map[1]
  )
}

# Карта Райта — base graphics (WrightMap), поэтому не через save_plot(), который
# печатает ggplot-объект, а напрямую через save_base_plot().
save_base_plot(draw_wright,
               file.path(OUT, "plots", paste0("2pl_wright_map.", PLOT_FORMAT)),
               dim_wright_map[1], dim_wright_map[2], dpi = dpi)
