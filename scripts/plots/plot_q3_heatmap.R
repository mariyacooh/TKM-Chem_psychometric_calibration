# =============================================================================
#  ГРАФИК: остаточные корреляции Yen's Q3 (локальная независимость)
# =============================================================================
#  Шаг 6 считает полную матрицу Q3 по 325 парам, но до этого рисунка её видел
#  только тот, кто открывал 2pl_q3_matrix.csv: у проверки локальной независимости
#  не было ни одной иллюстрации, хотя это допущение обеих IRT-моделей (шаги 4 и 6).
#
#  Показана нижняя треугольная часть матрицы: Q3 симметрична, и вторая половина
#  повторяла бы первую, отнимая у плиток половину поля. Пары выше относительного
#  порога Christensen (M + 0.2) обведены рамкой, порог берётся из артефакта шага 6.
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
width  <- W_2COL * 0.82   # дюймы
height <- 5.6             # дюймы
dpi    <- PLOT_DPI

pairs_file   <- "output/2PL/2pl_q3_pairs.csv"
summary_file <- "output/2PL/2pl_q3_summary.csv"

# УСЛОВНЫЙ вход: шаг 6 пишет Q3-артефакты только при успешном residuals(type =
# "Q3"); при ошибке оценки остатков он сообщает об этом и файлов не создаёт
# (ветка q3_mat в 6_2pl.R). Отсутствие — честный результат, а не пропущенный шаг.
if (!optional_input(c(pairs_file, summary_file), "scripts/6_2pl.R (шаг 6)",
                    "остаточные корреляции Q3 не рассчитаны")) {
  quit(save = "no", status = 0)
}

q3_pairs <- read_csv(pairs_file)
q3_sum   <- read_csv(summary_file)

source("scripts/config.R")
validate_items(unique(c(q3_pairs$Item_A, q3_pairs$Item_B)),
               "Q3 heatmap (2pl_q3_pairs.csv)")

# Порог и сводные числа читаются из артефакта шага 6, а не пересчитываются здесь:
# правило отбора пар живёт в одном месте (q3_crit в 6_2pl.R), иначе рамка на
# рисунке и колонка Flag в таблице разошлись бы при смене критерия.
get_metric <- function(name) {
  v <- q3_sum$Value[q3_sum$Metric == name]
  if (length(v) != 1L || is.na(v))
    stop(sprintf(paste0("plot_q3_heatmap: в %s нет строки %s — артефакт старше ",
                        "сводки Q3 в scripts/6_2pl.R, нужен полный прогон."),
                 summary_file, name), call. = FALSE)
  v
}
q3_crit <- get_metric("Crit_rel_M+0.2")
q3_mean <- get_metric("Mean_Q3")
n_flag  <- get_metric("N_flagged")
n_pairs <- get_metric("N_pairs")

# Порядок пунктов — по субшкалам (ITEMS из config.R), а не по алфавиту: так
# сгущение высоких Q3 внутри одной субшкалы видно блоком по диагонали.
ORD <- ITEMS

# Каждая пара кладётся в ОДНУ ячейку нижнего треугольника: ориентация выбирается
# по позиции в ORD, поэтому от порядка колонок Item_A/Item_B в артефакте рисунок
# не зависит.
tile_df <- q3_pairs %>%
  mutate(
    ia = match(Item_A, ORD), ib = match(Item_B, ORD),
    Row = ORD[pmax(ia, ib)],
    Col = ORD[pmin(ia, ib)]
  ) %>%
  transmute(
    Row = factor(Row, levels = rev(ORD)),
    Col = factor(Col, levels = ORD),
    Q3, Flag
  )

lim <- max(0.3, ceiling(max(abs(tile_df$Q3), na.rm = TRUE) * 10) / 10)

p_q3 <- ggplot(tile_df, aes(x = Col, y = Row, fill = Q3)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  geom_tile(data = filter(tile_df, Flag), colour = "grey10", linewidth = 0.5, fill = NA) +
  scale_fill_gradient2(low = OK_PAL[1], mid = "#f7f7f7", high = OK_PAL[4],
                       midpoint = 0, limits = c(-lim, lim), name = "Q3") +
  scale_x_discrete(drop = FALSE, expand = expansion(0)) +
  scale_y_discrete(drop = FALSE, expand = expansion(0)) +
  coord_fixed() +
  labs(
    title = "Local independence: Yen's Q3 residual correlations (2PL)",
    subtitle = wrap_lab(sprintf("%.0f of %.0f item pairs exceed the relative criterion Q3 > M + 0.2 = %.3f and are outlined; M(Q3) = %.3f",
                                n_flag, n_pairs, q3_crit, q3_mean),
                        width_in = width),
    x = "Item (column of the pair)", y = "Item (row of the pair)",
    caption = wrap_lab("Items are ordered by subscale, so a block of high residual correlations along the diagonal means the dependence sits inside one subscale. The relative criterion follows Christensen et al. (2017).",
                       width_in = width)
  ) +
  theme_pub(grid = "none") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(0.8)),
    axis.text.y = element_text(size = rel(0.8)),
    legend.key.width = unit(0.28, "cm")
  )

output_path <- file.path("output/2PL/plots", paste0("2pl_q3_heatmap.", PLOT_FORMAT))
save_ggplot(p_q3, output_path, width, height, dpi = dpi)
