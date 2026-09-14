# =============================================================================
#  ГРАФИК: условная доля верных по пунктам внутри floor-страты
# =============================================================================
#  Один рисунок на один вопрос: лежит ли доля верных на полу шкалы на уровне
#  чистого угадывания одинаково по всем пунктам, или она сосредоточена на
#  hub-кластере. Цвет кодирует группу пунктов, а не украшает ряд.
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
height <- 4.6 # дюймы
dpi <- PLOT_DPI

item_file  <- "output/FloorStratum/floor_item_proportions.csv"
group_file <- "output/FloorStratum/floor_group_tests.csv"
require_input(c(item_file, group_file), "scripts/10_floor_stratum.R (шаг 10)")
flag_file <- "output/Bimodality/extreme_patterns.csv"
require_input(flag_file, "scripts/2_bimodality.R (шаг 2)")

items  <- read_csv(item_file)
groups <- read_csv(group_file)
flag   <- read_csv(flag_file)
validate_items(items$Item, "plot_floor_conditional_p")

# Граница пола — из того же артефакта, что и полосы на score_total_distribution:
# она выведена шагом 2 из уровня угадывания и литералом не повторяется.
floor_cut <- flag$Floor_cut[1]
# Уровень чистого угадывания при четырёх вариантах ответа; тот же уровень задаёт
# нижнюю границу пола в 2_bimodality.R и нулевую гипотезу шага 10.
p_guess <- 0.25
n_floor <- items$N[1]

grp_lab <- need_levels(items$Group, c("hub", "rest"), "группы пунктов floor-страты")
GRP_NAME <- c(hub = "Hub cluster (6 items)", rest = "Remaining 20 items")
GRP_COL  <- c(hub = OK_PAL[4], rest = OK_PAL[1])

plot_df <- items %>%
  mutate(
    Group_lab = factor(GRP_NAME[Group], levels = unname(GRP_NAME[grp_lab])),
    Item = fct_reorder(Item, P_correct)
  )

# Средние по группам — на уровне респондента (колонка Mean_person_prop), поэтому
# линия описывает тот же тест, который вынес вердикт шага.
grp_mean <- groups %>%
  filter(Group %in% grp_lab) %>%
  mutate(Group_lab = factor(GRP_NAME[Group], levels = unname(GRP_NAME[grp_lab])))

hub_m  <- grp_mean$Mean_person_prop[grp_mean$Group == "hub"]
rest_m <- grp_mean$Mean_person_prop[grp_mean$Group == "rest"]

p <- ggplot(plot_df, aes(x = P_correct, y = Item, colour = Group_lab)) +
  # 1. Уровень чистого угадывания
  geom_vline(xintercept = p_guess, linetype = "dashed",
             colour = "grey35", linewidth = 0.4) +
  # 2. Средние по группам
  geom_vline(data = grp_mean, aes(xintercept = Mean_person_prop, colour = Group_lab),
             linetype = "dotted", linewidth = 0.4, show.legend = FALSE) +
  # 3. Точечная оценка с точным ДИ Клоппера-Пирсона
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), orientation = "y",
                width = 0, linewidth = 0.4) +
  geom_point(size = 1.7) +
  scale_colour_manual(values = unname(GRP_COL[grp_lab]), name = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = function(x) sprintf("%.1f", x)) +
  coord_cartesian(xlim = c(0, max(plot_df$CI_high) + 0.03)) +
  labs(
    title = "Conditional proportion correct inside the floor stratum",
    subtitle = sprintf(
      "n = %d (total score <= %d); dashed line = chance level %.2f; dotted lines = group means %.3f (hub) and %.3f (rest)",
      n_floor, floor_cut, p_guess, hub_m, rest_m),
    x = "Proportion correct (95% Clopper-Pearson CI)",
    y = NULL,
    caption = wrap_lab(paste0(
      "Selection into the stratum is by total score, so the mean proportion across all 26 items is fixed at M(score)/k ",
      "and cannot exceed the cut/k: the overall level is a property of the selection, not a measurement. What the ",
      "stratum does measure is how that fixed budget is distributed across items."))
  ) +
  theme_pub(grid = "x") +
  theme(legend.position = "top")

# Сохранение
OUT <- "output/FloorStratum/plots"
output_path <- file.path(OUT, paste0("floor_conditional_p.", PLOT_FORMAT))
save_ggplot(p, output_path, width, height, dpi = dpi)
