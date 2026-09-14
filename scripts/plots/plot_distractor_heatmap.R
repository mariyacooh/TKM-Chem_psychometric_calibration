# =============================================================================
#  ГРАФИК: тепловая карта point-biserial по дистракторам
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
height <- 4.4      # дюймы (панель на каждую языковую форму)
dpi    <- PLOT_DPI

color_palette <- "RdBu" # дивергентная палитра (варианты: RdBu, Spectral, RdYlBu)

data_file <- "output/Distractor/distractor_pb_table.csv"
require_input(data_file, "scripts/9_distractor.R (шаг 9)")

pb_tbl <- read_csv(data_file)

# Строки без валидных вариантов или с пропущенным pbis отбрасываются. Пропуск pbis
# реален: 9_distractor.R не считает корреляцию (порог n_opt) для варианта,
# выбранного менее чем двумя (или всеми кроме одного) респондентами формы. Без этого
# фильтра label_text собирался бы через sprintf("%.2f", NA) и плитка получала бы
# подпись строкой "NA" — geom_text(na.rm = TRUE) её не убирает, подпись не является NA.
pb_tbl <- pb_tbl %>%
  filter(!is.na(Option) & Option != "X" & !is.na(pbis)) %>%
  mutate(
    label_text = ifelse(IsCorrect, paste0(sprintf("%.2f", pbis), "*"), sprintf("%.2f", pbis)),
    # обратный порядок уровней Option, чтобы A был сверху
    Option = factor(Option, levels = rev(sort(unique(Option), method = "radix")))
  )

# Буквы вариантов раздаются внутри своей языковой формы (9_distractor.R), поэтому
# B в KZ и B в RU — разные варианты: своя панель на каждую форму, иначе две
# несопоставимые ячейки молча накладывались бы друг на друга.
p_heatmap <- ggplot(pb_tbl, aes(x = Item, y = Option, fill = pbis)) +
  geom_tile(color = "white", linewidth = 0.25) +
  geom_text(aes(label = label_text), size = 1.7, na.rm = TRUE) +
  facet_wrap(~ Language, ncol = 1) +
  scale_fill_distiller(palette = color_palette, limits = c(-1, 1), name = "pbis", na.value = "grey90") +
  scale_x_discrete(expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0)) +
  labs(
    title = "Distractor point-biserial correlations",
    subtitle = wrap_lab("Item-rest correlation of each answer option within its language form; an asterisk marks the correct option"),
    x = "Item",
    y = "Option"
  ) +
  theme_pub(grid = "none") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.key.width = unit(0.28, "cm")
  )

output_path <- file.path("output/Distractor/plots", paste0("distractor_heatmap.", PLOT_FORMAT))
save_ggplot(p_heatmap, output_path, width, height, dpi = dpi)
