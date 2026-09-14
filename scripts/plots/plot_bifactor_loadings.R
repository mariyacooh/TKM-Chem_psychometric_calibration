# =============================================================================
#  ГРАФИК: бифакторная 2PL (IRT) — нагрузки G vs S + панель индексов
# =============================================================================
#  Публикационный рисунок бифакторной 2PL-модели (mirt::bfactor, шаг 6).
#  Верхняя панель: плашки omega_h / omega_total / ECV / PUC.
#  Основная панель: dumbbell-профиль — строка на пункт, стандартизованные
#  нагрузки общего (G) и специфического (S) факторов, соединённые отрезком.
#
#  ВХОД (УСЛОВНЫЙ — пишется шагом 6 ТОЛЬКО при сходимости бифакторной 2PL):
#    output/2PL/2pl_bifactor_loadings.csv   (Factor, Item, Beta)  — без него [SKIP]
#    output/2PL/2pl_bifactor_indices.csv    (Index, Value)        — без него без панели индексов
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
library(patchwork)

# КОНФИГУРАЦИЯ
width  <- W_2COL * 0.78   # дюймы
height <- 7.0             # дюймы
dpi    <- PLOT_DPI

color_g <- OK_PAL[1]  # общий фактор — синий Okabe-Ito
color_s <- OK_PAL[4]  # специфические факторы — киноварь Okabe-Ito

loads_file   <- "output/2PL/2pl_bifactor_loadings.csv"
indices_file <- "output/2PL/2pl_bifactor_indices.csv"
# Условный вход: шаг 6 пишет файл ТОЛЬКО при сходимости bfactor(). Отсутствие —
# не ошибка запуска, а честный сигнал «модель не сошлась», поэтому optional_input
# (видимый [SKIP] в логе), а не require_input. Отчёт отражает отсутствие
# результата как есть, без подмены правды. Второй такой вход среди графиков —
# CFA/cfa_fits.RData у plot_cfa_path.R / plot_cfa_loadings.R.
if (!optional_input(loads_file, "scripts/6_2pl.R (шаг 6)",
                    "бифакторная 2PL не сошлась")) {
  quit(save = "no", status = 0)
}

loads <- read_csv(loads_file)

source("scripts/config.R")
validate_items(unique(loads$Item), "график нагрузок бифакторной 2PL (2pl_bifactor_loadings.csv)")

# ── Основная панель: dumbbell-профиль ────────────────────────────────────────
gen  <- loads %>% filter(Factor == "G")  %>% select(Item, G = Beta)
spec <- loads %>% filter(Factor != "G") %>% select(Item, Specific = Beta, SpecFactor = Factor)

plot_df <- gen %>%
  inner_join(spec, by = "Item") %>%
  mutate(
    # Подпись специфического фактора — sub_label() из _plot_utils.R; порядок фасетов
    # следует SUBSCALES, а не алфавиту.
    Subscale = factor(sub_label(SpecFactor), levels = sub_label(names(SUBSCALES))),
    Item = fct_reorder(Item, G)
  )

long_df <- plot_df %>%
  pivot_longer(c(G, Specific), names_to = "Loading_on", values_to = "Loading") %>%
  mutate(Loading_on = recode(Loading_on, G = "General factor (G)", Specific = "Specific factor (S)"))

p_bi <- ggplot(plot_df, aes(y = Item)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = 0.3, colour = "grey65", linetype = "dashed", linewidth = 0.35) +
  geom_segment(aes(x = Specific, xend = G, yend = Item), colour = "grey65", linewidth = 0.4) +
  geom_point(data = long_df, aes(x = Loading, colour = Loading_on, shape = Loading_on), size = 1.6) +
  scale_colour_manual(values = c("General factor (G)" = color_g, "Specific factor (S)" = color_s), name = NULL) +
  scale_shape_manual(values = c("General factor (G)" = 16, "Specific factor (S)" = 17), name = NULL) +
  scale_x_continuous(
    limits = c(min(-0.4, floor(min(long_df$Loading, na.rm = TRUE) * 10) / 10),
               max(1,    ceiling(max(long_df$Loading, na.rm = TRUE) * 10) / 10)),
    breaks = seq(-1, 1, 0.2), expand = expansion(mult = 0.01)
  ) +
  facet_grid(Subscale ~ ., scales = "free_y", space = "free_y") +
  labs(
    x = expression("Standardized loading " * lambda), y = "Item",
    caption = wrap_lab("Dashed line: salience threshold 0.30. Model: bifactor 2PL (mirt::bfactor). Each segment joins the two loadings of one item.",
                       width_in = width)
  ) +
  theme_pub(grid = "x") +
  theme(
    strip.text.y = element_text(angle = 0),
    panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey93"),
    legend.position = "bottom"
  )

# ── Панель индексов: плашки omega_h / omega_total / ECV / PUC ───────────────
title_text <- "Bifactor 2PL (IRT): Standardized Factor Loadings"
has_indices <- optional_input(indices_file, "scripts/6_2pl.R (шаг 6)",
                              "индексы не рассчитаны — рисунок без панели плашек")

if (has_indices) {
  idx <- read_csv(indices_file)
  tile_df <- tibble(
    key   = c("omega_h", "omega_total", "ECV", "PUC"),
    label = c("ωH (hierarchical)", "ω Total", "ECV", "PUC")
  ) %>%
    inner_join(idx, by = c("key" = "Index")) %>%
    mutate(x = row_number())

  p_tiles <- ggplot(tile_df, aes(x = x)) +
    geom_text(aes(y = 0.68, label = sprintf("%.3f", Value)),
              size = 3.6, fontface = "bold", colour = "grey10") +
    geom_text(aes(y = 0.22, label = label), size = 2.3, colour = "grey35") +
    scale_x_continuous(limits = c(0.5, nrow(tile_df) + 0.5)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(plot.margin = margin(2, 6, 0, 6))
}

# ── Компоновка и сохранение ──────────────────────────────────────────────────
if (has_indices) {
  p_final <- p_tiles / p_bi +
    plot_layout(heights = c(1, 13)) +
    plot_annotation(
      title = title_text,
      theme = theme_pub() + theme(plot.title = element_text(size = 10, face = "bold"))
    )
} else {
  # Индексов ещё нет (шаг 6 их не записал) -> нагрузки без панели плашек.
  p_final <- p_bi + labs(title = title_text)
}

out_dir <- "output/2PL/plots"
output_path <- file.path(out_dir, paste0("2pl_bifactor_loadings.", PLOT_FORMAT))
save_ggplot(p_final, output_path, width, height, dpi = dpi)
