# =============================================================================
#  ГРАФИКИ: валидность по известным группам (группы ANOVA и t-тестов)
# =============================================================================
#  ДВА рисунка на весь шаг 8:
#    anova_validity_groups  — общий балл по всем четырём группирующим переменным,
#                             панель на переменную;
#    anova_validity_profile — субшкалы по курсам, средние с 95% ДИ.
#
#  Прежде рисунков было пять: по одному на группирующую переменную (курс, GPA,
#  конференции, язык), каждый с четырьмя панелями баллов, плюс профиль. Четыре из
#  них различались ТОЛЬКО осью X, и вопрос шага — «расходятся ли группы по общему
#  баллу» — читался по четырём файлам вместо одного ряда панелей. Баллы субшкал по
#  каждой группе остаются в таблицах шага 8 (`anova_results.csv`, `ttest_*.csv`):
#  на рисунке они давали 16 панелей, из которых работала одна строка.
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
dpi <- PLOT_DPI

# Размеры графиков (ширина, высота в дюймах); ширина — полоса набора. Панели идут
# В РЯД (facet nrow = 1), поэтому высота — под одну строку панелей, а не под сетку
# 2x2: квадратная раскладка на полосе набора давала половину холста под пустое
# поле над скрипками.
dim_groups  <- c(W_2COL, 3.4)
dim_profile <- c(W_2COL, 3.1)

OUT <- "output/ANOVA"
data_prepared_path <- file.path(OUT, "anova_prepared_data.csv")
data_gpa_path <- file.path(OUT, "gpa_valid_data.csv")

require_input(c(data_prepared_path, data_gpa_path), "scripts/8_anova_known_groups.R (шаг 8)")

df <- read_csv(data_prepared_path)
gpa_valid <- read_csv(data_gpa_path)

source("scripts/config.R")   # SUBSCALES (состав и порядок субшкал)

# Колонки баллов и их подписи выводятся из SUBSCALES, а не перечисляются: субшкала,
# добавленная в input/items.csv, даёт колонку Score_S<k> в шаге 0, и при перечислении
# она молча не попала бы ни в один фасет.
SCORE_COLS <- c("Score_Total", paste0("Score_", names(SUBSCALES)))
SCORE_LABS <- c("Total Score", sub_label(names(SUBSCALES)))

# Приведение уровней факторов
# Литералы уровней объявлены здесь заново поверх тех, что породил шаг 8, поэтому
# каждый набор проходит через need_levels(): переименование метки в шаге 8 иначе
# дало бы сплошные NA, filter(!is.na(...)) -- пустую панель, а рисунок сохранился
# бы с кодом возврата 0. Пропуск обязан быть видимым.
df <- df %>%
  mutate(
    # levels соответствуют значениям в данных; отображаемые метки — английские (публикационный рисунок)
    Course = factor(Course, levels = need_levels(Course, paste0(1:4, " курс"), "курс"),
                    labels = paste0("Year ", 1:4)),
    Language = factor(Language,
                      levels = need_levels(Language, c("Қазақ", "Русский"), "язык"),
                      labels = c("Kazakh", "Russian")),
    Conference = factor(Conference,
                        levels = need_levels(Conference, c("Нет", "Да"), "конференция"),
                        labels = c("No", "Yes"))
  )

gpa_valid <- gpa_valid %>%
  mutate(
    GPA_group = factor(GPA_group,
      levels = need_levels(GPA_group,
                           c("Низкий (≤2)", "Средний (3)", "Выше среднего (4)", "Высокий (5)"),
                           "группа GPA"),
      labels = c("Low (≤2)", "Medium (3)", "Above avg (4)", "High (5)"))
  )

# Сохранение графика в OUT/plots/<base_name>.<format> через общий выбор устройства
save_plot <- function(plot_obj, base_name, dims) {
  output_path <- file.path(OUT, "plots", paste0(base_name, ".", PLOT_FORMAT))
  save_ggplot(plot_obj, output_path, dims[1], dims[2], dpi = dpi)
}

# Длинный формат для фасетирования в ggplot
df_long <- df %>%
  select(Course, Conference, Language, all_of(SCORE_COLS)) %>%
  pivot_longer(cols = all_of(SCORE_COLS), names_to = "Score_Type", values_to = "Score") %>%
  mutate(Score_Type = factor(Score_Type, levels = SCORE_COLS, labels = SCORE_LABS))

# ── 1. ОБЩИЙ БАЛЛ ПО ВСЕМ ГРУППИРУЮЩИМ ПЕРЕМЕННЫМ ────────────────────────────
# Панель на переменную, общая ось Y: сравниваются не переменные между собой, а
# группы внутри каждой, и разброс общего балла у них один и тот же. Свободная ось
# на панель растянула бы каждую под свой размах и показала бы расхождение групп
# там, где его нет.
#
# GPA приходит из СВОЕЙ таблицы (gpa_valid_data.csv — шаг 8 отбирает записи с
# валидной самооценкой), поэтому набор собирается из двух источников, а не одним
# pivot_longer по df.
#
# Заголовок панели держится КОРОТКИМ: полоса фасета длинный текст не переносит и
# не ужимает, она его ОБРЕЗАЕТ, и на четверти полосы набора «Self-reported GPA
# group» не помещается. Расшифровка — в подписи под рисунком.
GROUP_LAB <- c(Course     = "Course year",
               GPA_group  = "GPA (self-rated)",
               Conference = "Conference",
               Language   = "Test language")

groups_df <- bind_rows(
  df %>% filter(!is.na(Course)) %>%
    transmute(Grouping = "Course", Level = as.character(Course), Score = Score_Total),
  gpa_valid %>% filter(!is.na(GPA_group)) %>%
    transmute(Grouping = "GPA_group", Level = as.character(GPA_group), Score = Score_Total),
  df %>% filter(!is.na(Conference)) %>%
    transmute(Grouping = "Conference", Level = as.character(Conference), Score = Score_Total),
  df %>% filter(!is.na(Language)) %>%
    transmute(Grouping = "Language", Level = as.character(Language), Score = Score_Total)
) %>%
  mutate(Panel = factor(GROUP_LAB[Grouping], levels = unname(GROUP_LAB)))

# Уровни собираются с префиксом переменной и снимаются обратно на оси: курс даёт
# подписи "1".."4", а самооценка GPA — "Low (<=2)".."High (5)", и общий вектор
# уровней перемешал бы порядок панелей между собой. Тот же приём, что в
# plot_sample_composition.R.
LEV_ORDER <- c(paste("Course",     levels(df$Course),          sep = "\u001f"),
               paste("GPA_group",  levels(gpa_valid$GPA_group), sep = "\u001f"),
               paste("Conference", levels(df$Conference),      sep = "\u001f"),
               paste("Language",   levels(df$Language),        sep = "\u001f"))
groups_df <- groups_df %>%
  mutate(Key = factor(paste(Grouping, Level, sep = "\u001f"), levels = LEV_ORDER))

# Заливка НЕ кодирует ничего: категория уже названа подписью на оси, а цвет на
# ней различал бы ряды, которых нет. Смысл несут ящик, скрипка и ромб средней —
# они одинаковы во всех панелях.
p_groups <- ggplot(groups_df, aes(x = Key, y = Score)) +
  geom_violin(fill = FILL_NEUTRAL, alpha = 0.55, trim = FALSE,
              linewidth = 0.25, colour = LINE_NEUTRAL) +
  geom_boxplot(width = 0.16, fill = "white", outlier.shape = NA,
               linewidth = 0.3, colour = "grey20") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 1.4,
               fill = "black", colour = "white", stroke = 0.3) +
  facet_wrap(~ Panel, scales = "free_x", nrow = 1) +
  scale_x_discrete(labels = function(k) sub("^[^\u001f]*\u001f", "", k)) +
  labs(
    title = "Known-groups validity: total score by group",
    subtitle = wrap_lab("One panel per grouping variable of step 8, all on the same score axis. Subscale scores by group stay in the step 8 tables."),
    x = "Group", y = sprintf("Total score (maximum %d)", length(ITEMS)),
    caption = wrap_lab("Violin: kernel density of the score. Box: median and interquartile range, outliers not drawn. Black diamond: group mean. GPA (self-rated): the respondent's own judgement of their marks on the national 1-5 scale; that panel covers only the respondents with a valid self-rating, so its n is smaller. Conference: took part in a subject conference or olympiad.")
  ) +
  theme_pub(grid = "y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot(p_groups, "anova_validity_groups", dim_groups)


# ── 2. ПРОФИЛЬНЫЕ ЛИНИИ ПО КУРСАМ ────────────────────────────────────────────
profile_data <- df_long %>%
  filter(!is.na(Course)) %>%
  group_by(Course, Score_Type) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    SE = sd(Score, na.rm = TRUE) / sqrt(n()),
    CI_lower = Mean - 1.96 * SE,
    CI_upper = Mean + 1.96 * SE,
    .groups = "drop"
  )

# Цвет ряда снят: он кодировал Score_Type, который уже назван заголовком панели,
# а легенда при этом была выключена — то есть ряд красился признаком, который с
# рисунка и так читается.
p_profile <- ggplot(profile_data, aes(x = Course, y = Mean, group = Score_Type)) +
  geom_line(linewidth = 0.5, colour = LINE_NEUTRAL) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.12, linewidth = 0.4,
                colour = LINE_NEUTRAL) +
  geom_point(size = 1.6, colour = "grey15") +
  facet_wrap(~ Score_Type, scales = "free_y", nrow = 1) +
  labs(
    title = "Known-groups profile: subscale means with 95% confidence intervals",
    subtitle = wrap_lab("Each panel has its own y-axis range, so the panels show the shape of the progression across course years, not the relative level of the scores."),
    x = "Course year", y = "Mean score"
  ) +
  theme_pub(grid = "y")

save_plot(p_profile, "anova_validity_profile", dim_profile)
