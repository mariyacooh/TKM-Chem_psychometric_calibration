# =============================================================================
#  ГРАФИК: плотность распределения общего балла по курсам
# =============================================================================
#  Четыре кривые в ОДНИХ осях: вопрос рисунка — совпадают ли формы распределения
#  между курсами, а он читается наложением, а не сравнением четырёх панелей.
#
#  Различаются кривые цветом И типом линии сразу (не одним чёрным цветом, как
#  раньше), а n и медиана каждого курса стоят в ЛЕГЕНДЕ. Поэтому внутри поля нет
#  ни одной подписи: прежние повёрнутые подписи медиан у верхнего края сливались
#  между собой, когда медианы курсов оказывались рядом (17.5 и 18).
#
#  Ось X кончается на length(ITEMS) — максимально достижимом балле. Ядерная
#  оценка плотности сама по себе уходит за диапазон данных (ядро размазывает
#  крайние наблюдения), и без явных границы `from`/`to` рисунок показывал бы
#  баллы выше максимума теста, то есть больше пунктов, чем в тесте есть.
#
#  Объединённую по выборке форму рисует plot_score_distribution.R.
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
height <- 4.2      # дюймы
dpi    <- PLOT_DPI

data_file <- "output/cleaned_responses.csv"
require_input(data_file, "scripts/0_preprocess.R (шаг 0)")

df <- read_csv(data_file)

if (!"Score_Total" %in% names(df)) {
  stop("Колонка 'Score_Total' отсутствует в данных.")
}

# Верхняя граница шкалы — число пунктов анализа из input/items.csv, а не максимум
# наблюдённого балла: рисунок показывает шкалу теста, и она не сжимается от того,
# что никто не набрал максимум.
source("scripts/config.R")
X_MAX <- length(ITEMS)

df_plot <- df %>%
  filter(!is.na(Course)) %>%
  mutate(Course_num = as.integer(str_extract(as.character(Course), "\\d+"))) %>%
  filter(!is.na(Course_num), Course_num %in% 1:4)

if (max(df_plot$Score_Total, na.rm = TRUE) > X_MAX)
  stop(sprintf(paste0("plot_descriptive_density: Score_Total достигает %g при %d ",
                      "пунктах анализа — состав пунктов разошёлся с данными."),
               max(df_plot$Score_Total, na.rm = TRUE), X_MAX), call. = FALSE)

# n и медиана уходят в ПОДПИСЬ РЯДА: в легенде им место есть всегда, а внутри
# поля четыре подписи конкурируют за одно и то же свободное место у верхнего края.
grp <- df_plot %>%
  group_by(Course_num) %>%
  summarise(n = n(), Median = median(Score_Total), .groups = "drop") %>%
  arrange(Course_num) %>%
  mutate(Series = sprintf("Year %d (n = %d, Mdn = %g)", Course_num, n, Median))

SER_LEV <- grp$Series
df_plot <- df_plot %>% left_join(grp, by = "Course_num") %>%
  mutate(Series = factor(Series, levels = SER_LEV))
grp <- grp %>% mutate(Series = factor(Series, levels = SER_LEV))

SER_COL <- setNames(OK_PAL[c(1, 2, 3, 4)][seq_along(SER_LEV)], SER_LEV)
SER_LTY <- setNames(c("solid", "22", "42", "1343")[seq_along(SER_LEV)], SER_LEV)

# Плотность считается заранее по каждому курсу (устойчиво к версиям ggplot2 и к
# передаче from/to через stat_density) и обрезана границами шкалы теста.
dens_df <- df_plot %>%
  group_by(Series) %>%
  group_modify(~ {
    d <- stats::density(.x$Score_Total, n = 512, from = 0, to = X_MAX)
    tibble(x = d$x, y = d$y)
  }) %>%
  ungroup()

# Медиана курса — засечка на нулевой линии в цвете своего ряда: вертикальная
# линия во всю высоту при четырёх курсах перечёркивает чужие кривые.
med_df <- grp %>% mutate(y = 0)

p_density <- ggplot() +
  geom_line(data = dens_df, aes(x = x, y = y, colour = Series, linetype = Series),
            linewidth = 0.65) +
  geom_point(data = med_df, aes(x = Median, y = y, colour = Series, shape = Series),
             size = 2, show.legend = FALSE) +
  scale_colour_manual(values = SER_COL, name = NULL) +
  scale_linetype_manual(values = SER_LTY, name = NULL) +
  scale_shape_manual(values = c(17, 15, 18, 16)[seq_along(SER_LEV)]) +
  scale_x_continuous(breaks = seq(0, X_MAX, by = 2), limits = c(0, X_MAX),
                     expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Total score density by course year",
    subtitle = wrap_lab(sprintf("Kernel density of Score_Total, bounded by the test scale 0-%d (all %d analysed items)",
                                X_MAX, X_MAX)),
    x = sprintf("Total score (maximum %d)", X_MAX),
    y = "Density",
    caption = wrap_lab("Markers on the baseline give the group median. The two-peaked shape holds within every cohort, so it is not an artefact of pooling them.")
  ) +
  # Легенда в ДВЕ строки: четыре подписи с n и медианой в одну строку не
  # укладываются в полосу и обрезаются с правого края.
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pub(grid = "both") +
  theme(legend.position = "bottom", legend.key.width = unit(0.85, "cm"))

OUT <- "output/Descriptive/plots"
output_path <- file.path(OUT, paste0("score_density_by_course.", PLOT_FORMAT))
save_ggplot(p_density, output_path, width, height, dpi = dpi)
