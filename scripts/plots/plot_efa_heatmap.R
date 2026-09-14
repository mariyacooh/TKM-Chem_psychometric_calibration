# =============================================================================
#  ГРАФИК: тепловая карта нагрузок EFA
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

# КОНФИГУРАЦИЯ. Высота — под 26 строк пунктов, но в пределах полосы набора
# (H_MAX): прежние 13 дюймов в полосу не входили, и вёрстка ужимала карту вместе
# со всеми подписями.
width  <- W_2COL * 0.52   # дюймы — тепловая карта
height <- 7.4             # дюймы
dpi <- PLOT_DPI

# Дивергентная шкала, безопасная при нарушениях цветовосприятия
# (синий = отрицательные ↔ оранжевый = положительные); без красно-зелёной пары
color_low  <- "#0072B2"
color_mid  <- "#f7f7f7"
color_high <- "#D55E00"

data_file <- "output/EFA/efa_loadings.csv"
require_input(data_file, "scripts/3_efa_cfa.R (шаг 3, БЛОК 1)")

loadings <- read_csv(data_file)

source("scripts/config.R")
validate_items(loadings$Item, "EFA heatmap (efa_loadings.csv)")

# Номер фактора — ПОЗИЦИЯ колонки, а не цифра в имени ML.
#
# psych::fa отдаёт `$loadings` уже упорядоченными по объяснённой дисперсии, и
# 3_efa_cfa.R пишет колонки в этом же порядке, но ИМЕНА ML1/ML2/ML3 нумеруют
# факторы по внутреннему порядку извлечения, а не по вкладу. На действующем
# прогоне порядок колонок ML2, ML3, ML1 при SS loadings 7.403, 2.436, 0.909
# (`output/EFA/efa_results.txt`). Перекодировка по имени (ML1 -> "Factor 1")
# поэтому вешала ярлык «Factor 1» на САМЫЙ СЛАБЫЙ фактор с 3.5% дисперсии, а
# доминирующий с 28.5% показывала как «Factor 2» — рисунок противоречил и
# собственным значениям на scree, и нагрузкам ESEM.
#
# Имя ML остаётся в подписи ряда: `efa_results.txt` и таблица нагрузок в отчёте
# печатают ML-имена, и без них рисунок с ними не сводится.
fac_cols  <- setdiff(names(loadings), "Item")
FAC_LAB   <- setNames(sprintf("Factor %d\n(%s)", seq_along(fac_cols), fac_cols), fac_cols)

load_df <- loadings %>%
  pivot_longer(-Item, names_to = "Factor", values_to = "Loading") %>%
  mutate(
    # Субшкала пункта — из ITEM_SUB (config.R), подпись — sub_label() из
    # _plot_utils.R; порядок фасетов следует SUBSCALES, а не алфавиту.
    Subscale = factor(sub_label(ITEM_SUB[Item], short = TRUE),
                      levels = sub_label(names(SUBSCALES), short = TRUE)),
    Item = factor(Item, levels = rev(unlist(SUBSCALES))),
    Factor = factor(FAC_LAB[Factor], levels = unname(FAC_LAB)),
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
  labs(title = "EFA loadings (oblimin)",
       subtitle = wrap_lab("Bold: |lambda| >= 0.30. Factors are numbered by variance explained; the psych label is in brackets.",
                           width_in = width),
       x = "Factor", y = "Item") +
  theme_pub(grid = "none") +
  theme(strip.text.y = element_text(angle = 0),
        panel.spacing = unit(0.15, "cm"),
        legend.key.width = unit(0.28, "cm"))

output_path <- file.path("output/EFA/plots", paste0("efa_loadings_heatmap.", PLOT_FORMAT))
save_ggplot(p_heatmap, output_path, width, height, dpi = dpi)

# Столбчатой диаграммы тех же нагрузок (`efa_factor_bars.*`) здесь больше нет: она
# рисовалась из ЭТОГО ЖЕ load_df и несла ровно те числа, что уже подписаны на
# плитках карты, — то есть второй рисунок на один вопрос. Порог |lambda| = 0.30
# карта отмечает начертанием подписи, структуру «пункт -> ведущий фактор» —
# scripts/plots/plot_efa_diagram.R.
