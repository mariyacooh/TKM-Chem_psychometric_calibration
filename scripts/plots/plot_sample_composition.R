# =============================================================================
#  ГРАФИК: состав выборки (описательная статистика, шаг 1)
# =============================================================================
#  У шага 1 не было ни одного рисунка: частоты по языку анкеты, курсу,
#  самооценке успеваемости и участию в конференциях жили только в таблицах
#  descriptive_stats_tables.csv и в тексте отчёта. Между тем это ровно те группы,
#  по которым шаг 8 проверяет валидность, и раздел «Участники» статьи опирается
#  на них же.
#
#  Две РАЗНЫЕ величины про успеваемость показаны раздельно, и это не дубль:
#    GPA_numeric  — сообщённый ДИАПАЗОН GPA, свёрнутый в число (закрытый бин ->
#                   середина, открытый -> граница; map_gpa_range в 0_preprocess.R);
#    GPA_ordinal  — самооценка «выше/ниже среднего» по шкале 1-5
#                   (map_gpa_text_to_num там же).
#  Первая — заявленный балл, вторая — суждение о себе; шаг 8 группирует по второй,
#  поэтому обе стоят рядом.
#
#  Региональный состав в рисунок НЕ входит: у 16 областей и городов
#  Казахстана нет английских названий ни в input/region_map.csv, ни где-либо ещё
#  в репозитории, а придумывать их здесь означало бы завести второй источник
#  правды для того, что пайплайн нигде не хранит. Разбивка по регионам остаётся
#  таблицей отчёта; подпись рисунка на неё ссылается.
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
height <- 3.3      # дюймы
dpi    <- PLOT_DPI

data_file <- "output/Descriptive/descriptive_stats_tables.csv"
# GPA_numeric — непрерывная величина, поэтому в частотную таблицу шага 1 она не
# попадает (там только категориальные переменные): частоты по её значениям
# считаются здесь из центрального артефакта шага 0. Это подсчёт значений готовой
# колонки, а не новая статистика.
resp_file <- "output/cleaned_responses.csv"
require_input(data_file, "scripts/1_descriptive_stats.R (шаг 1)")
require_input(resp_file, "scripts/0_preprocess.R (шаг 0)")

tbl  <- read_csv(data_file)
resp <- read_csv(resp_file)

# Английские подписи категорий объявлены здесь поверх значений, которые пишет
# шаг 1, поэтому каждое значение обязано найтись в карте: незамапленная категория
# иначе стала бы столбцом NA — молчаливой деградацией вместо отказа. Тот же
# принцип, что need_levels() в scripts/_artifacts.R, но карта здесь ещё и
# переименовывает, а не только упорядочивает.
VALUE_LAB <- list(
  Language    = c(kz = "Kazakh", ru = "Russian"),
  # Голая цифра курса, а не "Year 1": смысл несёт заголовок панели, а четыре
  # подписи по шесть знаков в полосе 1.4" смыкаются друг с другом.
  Course      = c(`1` = "1", `2` = "2", `3` = "3", `4` = "4"),
  GPA_ordinal = c(`1` = "1", `2` = "2", `3` = "3", `4` = "4", `5` = "5"),
  Conference  = c(`0` = "No", `1` = "Yes")
)
# Заголовок панели держится КОРОТКИМ: полоса фасета не переносит текст и не
# ужимает шрифт, она его обрезает, поэтому длинное название уходит за края
# панели молча. Расшифровка — в подписи под рисунком.
# Предел длины — ширина панели: 26 пунктов подписи в полосе шириной 1.4" при
# 8.5 pt полужирным не помещаются, и «Questionnaire language» обрезалось.
PANEL_LAB <- c(Language    = "Language",
               Course      = "Course year",
               GPA_range   = "GPA band",
               GPA_ordinal = "Self-rating (1-5)",
               Conference  = "Conference")

# Диапазон GPA приходит отдельной строкой набора: значения — свёрнутые бины, и
# подпись у них своя собственная, а не из VALUE_LAB.
gpa_rng <- resp %>%
  filter(!is.na(GPA_numeric)) %>%
  count(GPA_numeric, name = "N") %>%
  arrange(GPA_numeric) %>%
  transmute(Variable = "GPA_range",
            Value = sprintf("%.1f", GPA_numeric),
            N,
            `%` = round(N / sum(N) * 100, 1))

comp <- tbl %>% filter(Variable %in% names(VALUE_LAB))

unmapped <- comp %>%
  mutate(known = map2_lgl(Variable, Value, ~ as.character(.y) %in% names(VALUE_LAB[[.x]]))) %>%
  filter(!known)
if (nrow(unmapped))
  stop(sprintf(paste0("plot_sample_composition: у категорий %s нет английской ",
                      "подписи в VALUE_LAB. Литералы разошлись с шагом 1."),
               paste(sprintf("%s = %s", unmapped$Variable, unmapped$Value), collapse = ", ")),
       call. = FALSE)

missing_panels <- setdiff(names(VALUE_LAB), unique(comp$Variable))
if (length(missing_panels))
  stop(sprintf(paste0("plot_sample_composition: в %s нет строк для %s — ",
                      "артефакт шага 1 не содержит объявленных здесь переменных."),
               data_file, paste(missing_panels, collapse = ", ")), call. = FALSE)

n_total <- tbl %>% filter(Variable == "Language") %>% summarise(n = sum(N)) %>% pull(n)

plot_df <- comp %>%
  mutate(Label = map2_chr(Variable, Value, ~ unname(VALUE_LAB[[.x]][as.character(.y)]))) %>%
  bind_rows(gpa_rng %>% mutate(Label = Value)) %>%
  mutate(
    Panel = factor(PANEL_LAB[Variable], levels = unname(PANEL_LAB)),
    # Порядок категорий — как в VALUE_LAB (курс по возрастанию, оценка по
    # возрастанию) и по возрастанию балла у диапазонов GPA, а не по частоте:
    # панели читаются как шкалы, и сортировка по частоте перемешала бы
    # порядковые уровни.
    # Уровни собираются с префиксом переменной и снимаются обратно: курс и
    # самооценка дают одинаковые подписи "1".."4", и общий вектор уровней
    # схлопнул бы их в одну категорию на обеих панелях.
    Key = paste(Variable, Label, sep = "\u001f")
  ) %>%
  mutate(
    Key = factor(Key, levels = c(
      unlist(lapply(names(VALUE_LAB), function(v) paste(v, VALUE_LAB[[v]], sep = "\u001f"))),
      paste("GPA_range", gpa_rng$Value, sep = "\u001f")))
  )

p_comp <- ggplot(plot_df, aes(x = Key, y = N)) +
  scale_x_discrete(labels = function(k) sub("^[^\u001f]*\u001f", "", k)) +
  # Заливка НЕ кодирует ничего: серия одна, а категория названа подписью на оси.
  geom_col(width = 0.68, fill = FILL_NEUTRAL, colour = LINE_NEUTRAL, linewidth = 0.2) +
  geom_text(aes(label = sprintf("%d\n%.1f%%", N, `%`)), vjust = -0.25,
            size = 1.9, colour = "grey15", lineheight = 0.95) +
  facet_wrap(~ Panel, scales = "free_x", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "Sample composition",
    subtitle = wrap_lab(sprintf("n = %d respondents; bar labels give the count and the share of the sample", n_total)),
    x = "Category", y = "Respondents",
    caption = wrap_lab("Reported GPA band: the GPA range each respondent selected, collapsed to a number (closed band to its midpoint, open band to its boundary). Self-rated standing: the respondent's own judgement of their marks in core subjects on the national 1-5 scale. Conference: took part in a subject conference or olympiad. The regional breakdown stays in the descriptive statistics table, since the region names have no English form in the pipeline inputs.")
  ) +
  theme_pub(grid = "y")

OUT <- "output/Descriptive/plots"
output_path <- file.path(OUT, paste0("sample_composition.", PLOT_FORMAT))
save_ggplot(p_comp, output_path, width, height, dpi = dpi)
