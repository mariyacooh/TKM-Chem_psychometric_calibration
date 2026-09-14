# =============================================================================
#  ГРАФИК: тепловая карта нагрузок ESEM (geomin)
# =============================================================================
#  Параллель к plot_efa_heatmap.R, но по нагрузкам ESEM (output/ESEM/).
#
#  ВХОД: output/ESEM/esem_loadings.csv — пишет шаг 3 (БЛОК 4 в scripts/3_efa_cfa.R)
#  ВСЕГДА; при несходимости ESEM файл пишется с 0 строк. Поэтому ОТСУТСТВИЕ файла =
#  шаг 3 не выполнялся (stop), а ПУСТОЙ файл = модель не сошлась (легальный [SKIP]).
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

width  <- W_2COL * 0.58   # дюймы
height <- 7.4             # дюймы
dpi    <- PLOT_DPI

color_low  <- "#0072B2"
color_mid  <- "#f7f7f7"
color_high <- "#D55E00"

data_file <- "output/ESEM/esem_loadings.csv"
require_input(data_file, "scripts/3_efa_cfa.R (шаг 3, БЛОК 4)")
loadings <- read_csv(data_file)
if (nrow(loadings) == 0) {
  message("[SKIP] ", data_file, " пуст (0 строк): ESEM не сошёлся в шаге 3 — рисунок пропущен.")
  quit(save = "no")
}

source("scripts/config.R")
validate_items(loadings$Item, "ESEM heatmap (esem_loadings.csv)")

# Колонки факторов в CSV уже упорядочены под субшкалы и озаглавлены
# "f<k> (~S<j>)". Ось факторов берёт сами заголовки.
fac_cols <- setdiff(names(loadings), "Item")

load_df <- loadings %>%
  pivot_longer(all_of(fac_cols), names_to = "Factor", values_to = "Loading") %>%
  mutate(
    # Субшкала пункта — из ITEM_SUB (config.R), подпись — sub_label() из
    # _plot_utils.R; порядок фасетов следует SUBSCALES, а не алфавиту.
    Subscale = factor(sub_label(ITEM_SUB[Item], short = TRUE),
                      levels = sub_label(names(SUBSCALES), short = TRUE)),
    Item = factor(Item, levels = rev(unlist(SUBSCALES))),
    Factor = factor(Factor, levels = fac_cols),
    Label = sprintf("%.2f", Loading),
    Significant = abs(Loading) >= 0.30
  )

p_heatmap <- ggplot(load_df, aes(x = Factor, y = Item, fill = Loading)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = Label, colour = Significant), size = 2.1, fontface = "bold") +
  scale_fill_gradient2(
    low = color_low, mid = color_mid, high = color_high,
    midpoint = 0, limits = c(-1, 1), name = expression(lambda)
  ) +
  scale_colour_manual(values = c("FALSE" = "grey60", "TRUE" = "grey5"), guide = "none") +
  scale_x_discrete(expand = expansion(0)) +
  scale_y_discrete(expand = expansion(0)) +
  facet_grid(Subscale ~ ., scales = "free_y", space = "free_y") +
  labs(title = "ESEM loadings (geomin)",
       subtitle = wrap_lab("Bold values are salient loadings, |lambda| >= 0.30; columns are aligned to the intended subscales",
                           width_in = width),
       x = "Factor", y = "Item") +
  theme_pub(grid = "none") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    strip.text.y = element_text(angle = 0),
    panel.spacing = unit(0.15, "cm"),
    legend.key.width = unit(0.28, "cm")
  )

output_path <- file.path("output/ESEM/plots", paste0("esem_loadings_heatmap.", PLOT_FORMAT))
save_ggplot(p_heatmap, output_path, width, height, dpi = dpi)
