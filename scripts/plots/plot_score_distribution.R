# =============================================================================
#  ГРАФИК: распределение Score_Total по всей выборке с полосами пола и потолка
# =============================================================================
#  Кривые плотности по курсам рисует plot_descriptive_density.R; там распределение
#  разбито по группам, поэтому объединённая форма — два горба с почти пустой
#  серединой — на нём не видна. Здесь показана именно она.
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
width <- W_2COL # дюймы
height <- 3.7 # дюймы
dpi <- PLOT_DPI

freq_file <- "output/Bimodality/score_total_frequency.csv"
flag_file <- "output/Bimodality/extreme_patterns.csv"
require_input(c(freq_file, flag_file), "scripts/2_bimodality.R (шаг 2)")

freq <- read_csv(freq_file)
flag <- read_csv(flag_file)

# Границы полос берутся из артефакта, а не повторяются литералами: у шага 2 они
# выведены из состава теста (потолок) и из уровня угадывания (пол).
floor_cut <- flag$Floor_cut[1]
ceil_cut <- flag$Ceiling_cut[1]
n_total <- flag$N[1]

# Пустой нижний хвост в артефакте присутствует как нули (сетка 0..k); ось
# кадрируется по наблюдённым баллам, чтобы полосы не тонули в пустом поле.
x_lo <- min(freq$Score[freq$N > 0])
x_hi <- max(freq$Score[freq$N > 0])
y_hi <- max(freq$N)

band_lab <- tibble(
  x = c((x_lo + floor_cut) / 2, (floor_cut + ceil_cut) / 2, (ceil_cut + x_hi) / 2),
  label = c(
    sprintf("lower cluster: %.1f%%", flag$Pct_floor[1]),
    sprintf("middle: %.1f%%", 100 - flag$Pct_floor[1] - flag$Pct_ceiling[1]),
    sprintf("upper cluster: %.1f%%", flag$Pct_ceiling[1])
  )
)

p <- ggplot(freq, aes(x = Score, y = N)) +
  # 1. Пунктирные границы кластеров
  geom_vline(
    xintercept = c(floor_cut + 0.5, ceil_cut - 0.5),
    linetype = "dashed", colour = "grey55", linewidth = 0.35
  ) +
  # 2. Частоты по каждому баллу
  geom_col(fill = FILL_NEUTRAL, width = 0.85, colour = LINE_NEUTRAL, linewidth = 0.25) +
  # 3. Доли зон подписаны над столбцами
  geom_text(
    data = band_lab, aes(x = x, y = y_hi * 1.22, label = label),
    size = 2.6, fontface = "bold", colour = "grey15",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = seq(0, x_hi, by = 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  coord_cartesian(xlim = c(x_lo - 0.6, x_hi + 0.6), ylim = c(0, y_hi * 1.32)) +
  labs(
    title = "Total score distribution, pooled sample",
    subtitle = sprintf(
      "n = %d; bimodality coefficient BC = %.3f against the uniform reference %.3f",
      n_total, flag$BC[1], flag$BC_ref[1]
    ),
    x = "Total score",
    y = "Respondents"
  ) +
  theme_pub(grid = "y")

# Сохранение
OUT <- "output/Bimodality/plots"
output_path <- file.path(OUT, paste0("score_total_distribution.", PLOT_FORMAT))
save_ggplot(p, output_path, width, height, dpi = dpi)
