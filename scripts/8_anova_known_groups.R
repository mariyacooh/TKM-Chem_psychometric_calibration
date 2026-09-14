# =============================================================================
#  ШАГ 8 — ВАЛИДНОСТЬ ПО ИЗВЕСТНЫМ ГРУППАМ (ANOVA / t-тесты)
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv     (шаг 0; баллы + демография)
#  ВЫХОД: output/ANOVA/anova_prepared_data.csv     -> plot_anova_validity
#         output/ANOVA/gpa_valid_data.csv          -> plot_anova_validity
#         output/ANOVA/group_tests_significance.csv -> report.Rmd (флаг оговорок)
#         output/ANOVA/anova_results.txt + xlsx    -> report.Rmd
#
#  Зависимые переменные — только сырые баллы: Score_Total и субшкалы S1-S3.
#  WLE-тета шага 4 зависимой переменной НЕ является ни в ранговых семействах, ни в
#  параметрических; обоснование и цена решения — DECISIONS.md D9. Поэтому шаг читает
#  только артефакт шага 0, и ребра «шаг 4 -> шаг 8» в графе нет.
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------------

# ── СЕКЦИЯ 0: ПАКЕТЫ И КОНФИГ ────────────────────────────────────────────────
# Бутстрап: рабочая директория — корень проекта, затем общие функции настройки
local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/_setup.R")
reset_sink()
set.seed(42)   # паритет с прочими шагами; ГПСЧ общий на процесс в run_all.R

load_pkgs(c("tidyverse", "rstatix", "effectsize", "car", "writexl"))

select <- dplyr::select
filter <- dplyr::filter
recode <- dplyr::recode

DATA_PATH <- "output/cleaned_responses.csv"
OUT_DIR <- "output/ANOVA"
ensure_dir(OUT_DIR)

SCORES <- c("Score_Total", "Score_S1", "Score_S2", "Score_S3")
LABELS <- c(
  Score_Total = "Общий балл",
  Score_S1 = "S1: Интерпретация",
  Score_S2 = "S2: Анализ",
  Score_S3 = "S3: Оценка"
)

sig_stars <- function(p) {
  ifelse(is.na(p), "-",
    ifelse(p < .001, "***",
      ifelse(p < .01, "**",
        ifelse(p < .05, "*", "ns")
      )
    )
  )
}

cohens_d_safe <- function(df, score, group) {
  g <- split(df[[score]], df[[group]])
  nms <- names(g)
  n1 <- length(g[[1]])
  n2 <- length(g[[2]])
  sd_pool <- sqrt(((n1 - 1) * var(g[[1]]) + (n2 - 1) * var(g[[2]])) / (n1 + n2 - 2))
  # направление group1 - group2: тот же знак, что у rstatix::t_test, с которым d
  # печатается в одной строке (pub_conf) и в соседних листах xlsx
  d <- (mean(g[[1]]) - mean(g[[2]])) / sd_pool
  tibble(group1 = nms[1], group2 = nms[2], d = round(d, 3))
}

SEP <- paste(rep("=", 68), collapse = "")
sep2 <- paste(rep("-", 68), collapse = "")

# ── СЕКЦИЯ 2: ЗАГРУЗКА И ПОДГОТОВКА ─────────────────────────────────────────
require_input(DATA_PATH, "scripts/0_preprocess.R (шаг 0)")

df_raw <- read_csv(DATA_PATH)

df <- df_raw %>%
  mutate(
    Course = factor(
      as.integer(str_extract(as.character(Course), "\\d+")),
      levels = 1:4, labels = paste0(1:4, " курс")
    ),
    Language = factor(Language, levels = c("kz", "ru"), labels = c("Қазақ", "Русский")),
    Conference = factor(as.integer(Conference), levels = 0:1, labels = c("Нет", "Да")),
    # GPA_ordinal маппится один раз в 0_preprocess.R; здесь только приведение типа
    GPA_ordinal = as.integer(GPA_ordinal),
    GPA_group = cut(GPA_ordinal,
      breaks = c(0, 2, 3, 4, 5),
      labels = c("Низкий (≤2)", "Средний (3)", "Выше среднего (4)", "Высокий (5)"),
      include.lowest = TRUE
    )
    # Score_S1..S3 приходят готовыми из cleaned_responses.csv (считаются в шаге 0)
  )

# Тета зависимой переменной не является НИ В ОДНОМ семействе, поэтому наборы DV
# ранговых и параметрических тестов совпадают. Данные полные (шаг 0 не оставляет
# пропусков по пунктам), а шаг 4 калибрует ровно тот же набор пунктов, по которому
# считается Score_Total, поэтому сырой балл — достаточная статистика модели Раша, а
# WLE-тета есть строго возрастающая функция Score_Total с той же структурой связок:
#   - ранговые тесты инвариантны к монотонному преобразованию, значит Краскел-Уоллис,
#     epsilon2, Данн и Спирмен по тете дают ПОРАЗРЯДНО те же числа, что по Score_Total;
#   - параметрические к нему не инвариантны, но тета — детерминированная функция того
#     же балла с 24 различными значениями на 253 респондента и точечной массой в
#     45 человек на потолке, поэтому нормальность там нарушена структурно
#     (ISSUES.md 1.5), а независимой информации о различиях групп тета не несёт.
# Обоснование и цена решения — DECISIONS.md D9. Отдельного набора RANK_SCORES для
# ранговых тестов нет: наборы совпадают, и второе имя для того же вектора вводило бы
# различие, которого в анализе не существует.

gpa_valid <- df %>%
  filter(!is.na(GPA_group)) %>%
  group_by(GPA_group) %>%
  filter(n() >= 5) %>%
  ungroup()

# Сохранение данных ANOVA для построения графиков
write_csv_excel(df, file.path(OUT_DIR, "anova_prepared_data.csv"))
write_csv_excel(gpa_valid, file.path(OUT_DIR, "gpa_valid_data.csv"))

# ── СЕКЦИЯ 3: ВСЕ АНАЛИЗЫ ───────────────────────────────────────────────────
desc_course <- df %>%
  filter(!is.na(Course)) %>%
  group_by(Course) %>%
  summarise(n = n(), across(all_of(SCORES), list(M = \(x)round(mean(x, na.rm = T), 2), SD = \(x)round(sd(x, na.rm = T), 2))), .groups = "drop")

desc_msd <- df %>%
  filter(!is.na(Course)) %>%
  group_by(Course) %>%
  summarise(n = n(), across(all_of(SCORES), \(x) sprintf("%.2f (%.2f)", mean(x, na.rm = T), sd(x, na.rm = T))), .groups = "drop")

desc_conf <- df %>%
  filter(!is.na(Conference)) %>%
  group_by(Conference) %>%
  summarise(n = n(), across(all_of(SCORES), list(M = \(x)round(mean(x, na.rm = T), 2), SD = \(x)round(sd(x, na.rm = T), 2))), .groups = "drop")

desc_lang <- df %>%
  filter(!is.na(Language)) %>%
  group_by(Language) %>%
  summarise(n = n(), across(all_of(SCORES), list(M = \(x)round(mean(x, na.rm = T), 2), SD = \(x)round(sd(x, na.rm = T), 2))), .groups = "drop")

levene_tbl <- lapply(SCORES, function(s) {
  levene_test(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course"))) %>%
    mutate(Score = s, sig = sig_stars(p)) %>% select(Score, F = statistic, df1, df2, p, sig)
}) %>% bind_rows()

anova_course <- lapply(SCORES, function(s) {
  welch_anova_test(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course"))) %>%
    mutate(Score = s) %>% select(Score, F = statistic, DFn, DFd, p) %>% mutate(sig = sig_stars(p))
}) %>% bind_rows()

# Размеры эффекта считаются от классической aov(), тогда как F и p выше — от ANOVA
# Уэлча: это разные модели (гомо- против гетероскедастичной) с разным разложением
# суммы квадратов и разными степенями свободы, поэтому eta2 и omega2 нельзя читать
# как размер эффекта для напечатанного рядом F. Источник назван в заголовках
# колонок pub_course и в сноске раздела 3 anova_results.txt.
es_course <- lapply(SCORES, function(s) {
  mod <- aov(as.formula(paste(s, "~ Course")), data = df %>% filter(!is.na(Course)))
  # alternative по умолчанию = "greater": верхняя граница жёстко фиксируется на 1 и
  # колонка ДИ вырождается в [0; 1], поэтому интервал запрашивается двусторонним
  e2 <- eta_squared(mod, partial = FALSE, ci = 0.95, alternative = "two.sided")
  o2 <- omega_squared(mod, partial = FALSE)
  tibble(Score = s, eta2 = round(e2$Eta2, 3), CI_low = round(e2$CI_low, 3), CI_high = round(e2$CI_high, 3), omega2 = round(o2$Omega2, 3))
}) %>% bind_rows()

# Омнибусный p может быть не определён (в группе меньше двух наблюдений), и тогда
# пост-хок не считается — как и при p >= 0.05. Охрана обязательна: if (NA) — ошибка
# «missing value where TRUE/FALSE needed» с обрывом шага, а прогон прерывается на
# первом падении (DECISIONS.md D8), то есть шаг 9 и графики не выполнились бы.
# Тот же предикат у anova_gpa ниже.
gh_course <- lapply(SCORES, function(s) {
  p_om <- anova_course %>% filter(Score == s) %>% pull(p)
  if (!is.na(p_om) && p_om < .05) {
    games_howell_test(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course"))) %>%
      mutate(Score = s) %>% select(Score, group1, group2, estimate, conf.low, conf.high, p.adj, p.adj.signif)
  }
}) %>% bind_rows()

kw_course <- lapply(SCORES, function(s) {
  kruskal_test(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course"))) %>%
    mutate(Score = s) %>% select(Score, H = statistic, df, p) %>% mutate(sig = sig_stars(p))
}) %>% bind_rows()

eps2 <- lapply(SCORES, function(s) {
  kruskal_effsize(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course"))) %>%
    mutate(Score = s) %>% select(Score, epsilon2 = effsize, magnitude)
}) %>% bind_rows()

dunn_course <- lapply(SCORES, function(s) {
  p_om <- kw_course %>% filter(Score == s) %>% pull(p)
  if (!is.na(p_om) && p_om < .05) {
    dunn_test(df %>% filter(!is.na(Course)), as.formula(paste(s, "~ Course")), p.adjust.method = "holm") %>%
      mutate(Score = s) %>% select(Score, group1, group2, statistic, p.adj, p.adj.signif)
  }
}) %>% bind_rows()

# MANOVA
mdf <- df %>% filter(!is.na(Course), !is.na(Score_S1), !is.na(Score_S2), !is.na(Score_S3))
man <- manova(cbind(Score_S1, Score_S2, Score_S3) ~ Course, data = mdf)
man_sum <- summary(man, test = "Pillai")

# Conference
t_conf <- lapply(SCORES, function(s) {
  t_test(df %>% filter(!is.na(Conference)), as.formula(paste(s, "~ Conference")), var.equal = FALSE) %>%
    mutate(Score = s, sig = sig_stars(p)) %>% select(Score, group1, group2, statistic, df, p, sig)
}) %>% bind_rows()

d_conf <- lapply(SCORES, function(s) {
  sub <- df %>% filter(!is.na(Conference), !is.na(.data[[s]]))
  cohens_d_safe(sub, s, "Conference") %>% mutate(Score = s)
}) %>% bind_rows()

# Язык
t_lang <- lapply(SCORES, function(s) {
  t_test(df %>% filter(!is.na(Language)), as.formula(paste(s, "~ Language")), var.equal = FALSE) %>%
    mutate(Score = s, sig = sig_stars(p)) %>% select(Score, group1, group2, statistic, df, p, sig)
}) %>% bind_rows()

d_lang <- lapply(SCORES, function(s) {
  sub <- df %>% filter(!is.na(Language), !is.na(.data[[s]]))
  cohens_d_safe(sub, s, "Language") %>% mutate(Score = s)
}) %>% bind_rows()

# GPA
anova_gpa <- lapply(SCORES, function(s) {
  tryCatch(
    welch_anova_test(gpa_valid, as.formula(paste(s, "~ GPA_group"))) %>%
      mutate(Score = s) %>% select(Score, F = statistic, DFn, DFd, p) %>% mutate(sig = sig_stars(p)),
    error = function(e) tibble(Score = s, F = NA, DFn = NA, DFd = NA, p = NA, sig = "error")
  )
}) %>% bind_rows()

gh_gpa <- lapply(SCORES, function(s) {
  gpa_p <- anova_gpa %>% filter(Score == s) %>% pull(p)
  if (!is.na(gpa_p) && gpa_p < .05) {
    tryCatch(
      games_howell_test(gpa_valid, as.formula(paste(s, "~ GPA_group"))) %>%
        mutate(Score = s) %>% select(Score, group1, group2, estimate, p.adj, p.adj.signif),
      error = function(e) tibble(Score = s, note = "ошибка post-hoc")
    )
  }
}) %>% bind_rows()

spearman_gpa <- lapply(SCORES, function(s) {
  sub <- df %>% filter(!is.na(GPA_ordinal), !is.na(.data[[s]]))
  r <- cor.test(sub$GPA_ordinal, sub[[s]], method = "spearman", exact = FALSE)
  tibble(Score = s, rho = round(r$estimate, 3), p = round(r$p.value, 4), sig = sig_stars(r$p.value))
}) %>% bind_rows()

# Пирсон по числовому GPA — по сырым баллам (тета исключена, DECISIONS.md D9)
pearson_gpa <- lapply(SCORES, function(s) {
  sub <- df %>% filter(!is.na(GPA_numeric), !is.na(.data[[s]]))
  cr <- cor.test(sub$GPA_numeric, sub[[s]], method = "pearson")
  tibble(Score = s, n = nrow(sub), r = round(unname(cr$estimate), 3), p = round(cr$p.value, 4), sig = sig_stars(cr$p.value))
}) %>% bind_rows()

# Регион: классическая ANOVA — среди регионов есть малые группы, по которым
# Welch-аппроксимация не считается
df_region <- df %>% filter(!is.na(Region))
has_region <- nrow(df_region) > 0 && length(unique(df_region$Region)) > 1
if (has_region) {
  desc_region <- df_region %>%
    group_by(Region) %>%
    summarise(n = n(), across(all_of(SCORES), list(M = \(x)round(mean(x, na.rm = T), 2), SD = \(x)round(sd(x, na.rm = T), 2))), .groups = "drop") %>%
    arrange(desc(n))
  # Размер эффекта считается и для региона (eta2, как у курса): без него F при
  # малых неравных группах нечем взвесить. Welch для региона не применяется
  # намеренно (комментарий выше), поэтому гомогенность дисперсий здесь НЕ
  # проверяется — это оговаривается в выводе.
  anova_region <- lapply(SCORES, function(s) {
    mod <- aov(as.formula(paste(s, "~ Region")), data = df_region)
    sm  <- summary(mod)[[1]]
    # ДИ двусторонний: при alternative = "greater" (умолчание effectsize) верхняя
    # граница фиксируется на 1 и интервал вырождается в [0; 1]
    e2  <- eta_squared(mod, partial = FALSE, ci = 0.95, alternative = "two.sided")
    tibble(Score = s, F = round(sm$`F value`[1], 3), df1 = sm$Df[1], df2 = sm$Df[2],
           p = round(sm$`Pr(>F)`[1], 4),
           eta2 = round(e2$Eta2, 3), CI_low = round(e2$CI_low, 3),
           CI_high = round(e2$CI_high, 3)) %>% mutate(sig = sig_stars(p))
  }) %>% bind_rows()
} else {
  desc_region <- tibble(note = "нет данных по региону")
  anova_region <- tibble(note = "нет данных по региону")
}

# Машиночитаемая сводка значимости ОМНИБУСНЫХ групповых сравнений (без корреляций
# с GPA — это не сравнение групп). report.Rmd ветвит по ней оговорку "ни одно
# групповое сравнение не достигло значимости", а не держит её в прозе: набор
# пунктов и подвыборки меняются, а текст оговорки менялся бы вручную.
group_tests <- bind_rows(
  anova_course %>% transmute(Family = "Курс (Welch ANOVA)",     Score, p = as.numeric(p)),
  kw_course    %>% transmute(Family = "Курс (Kruskal-Wallis)",  Score, p = as.numeric(p)),
  tibble(Family = "Курс (MANOVA, Pillai)", Score = "Score_S1-S3",
         p = as.numeric(man_sum$stats["Course", "Pr(>F)"])),
  t_conf %>% transmute(Family = "Конференции (t Уэлча)", Score, p = as.numeric(p)),
  t_lang %>% transmute(Family = "Язык (t Уэлча)",        Score, p = as.numeric(p)),
  anova_gpa %>% transmute(Family = "GPA (Welch ANOVA)",  Score, p = as.numeric(p))
)
if (has_region) {
  group_tests <- bind_rows(
    group_tests,
    anova_region %>% transmute(Family = "Регион (ANOVA)", Score, p = as.numeric(p))
  )
}
group_tests <- group_tests %>% mutate(sig = sig_stars(p))
write_csv_excel(group_tests, file.path(OUT_DIR, "group_tests_significance.csv"))

# Таблицы для публикации.
# В строке курса F/p приходят от ANOVA Уэлча, а eta2/omega2/ДИ — от классической
# aov(): заголовки колонок называют модель, иначе пара «F и eta2» читается как одна
# оценка. Регион (anova_region) от этого свободен — там обе величины из одной aov().
# Имена колонок публикационной таблицы повторяют литералы уровней, заданные выше в
# этом же файле, поэтому набор сверяется до сборки: при рассинхроне msd[["1 курс"]]
# вернул бы NULL, а tibble() снял бы колонку молча -- лист 0_PubTable_Course вышел
# бы без курса и без ошибки.
need_levels(desc_msd$Course, paste0(1:4, " курс"), "курс в pub_course")
pub_course <- lapply(SCORES, function(s) {
  msd <- desc_msd %>% select(Course, val = all_of(s)) %>% pivot_wider(names_from = Course, values_from = val)
  f_r <- anova_course %>% filter(Score == s)
  es_r <- es_course %>% filter(Score == s)
  tibble(
    Subscale = LABELS[s],
    `1 курс` = msd[["1 курс"]], `2 курс` = msd[["2 курс"]], `3 курс` = msd[["3 курс"]], `4 курс` = msd[["4 курс"]],
    `F (Уэлч)` = sprintf("%.2f", f_r$F), `p (Уэлч)` = ifelse(f_r$p < .001, "<.001", sprintf("%.3f", f_r$p)), sig = f_r$sig,
    `eta2 (ANOVA)` = sprintf("%.3f", es_r$eta2), `omega2 (ANOVA)` = sprintf("%.3f", es_r$omega2),
    `CI eta2 (ANOVA)` = sprintf("[%.3f; %.3f]", es_r$CI_low, es_r$CI_high)
  )
}) %>% bind_rows()

# Тот же дубль литералов, что и в pub_course: filter(Conference == "Нет") при
# рассинхроне даёт ноль строк, pull() -- character(0), и колонка уходит пустой.
need_levels(desc_conf$Conference, c("Нет", "Да"), "конференция в pub_conf")
pub_conf <- lapply(SCORES, function(s) {
  t_r <- t_conf %>% filter(Score == s)
  d_r <- d_conf %>% filter(Score == s)
  m_no <- desc_conf %>% filter(Conference == "Нет") %>% transmute(val = sprintf("%.2f (%.2f)", .data[[paste0(s, "_M")]], .data[[paste0(s, "_SD")]])) %>% pull()
  m_da <- desc_conf %>% filter(Conference == "Да") %>% transmute(val = sprintf("%.2f (%.2f)", .data[[paste0(s, "_M")]], .data[[paste0(s, "_SD")]])) %>% pull()
  tibble(
    Subscale = LABELS[s], `Нет` = m_no, `Да` = m_da,
    t = sprintf("%.2f", t_r$statistic), df = sprintf("%.1f", t_r$df),
    p = ifelse(t_r$p < .001, "<.001", sprintf("%.3f", t_r$p)), sig = t_r$sig, d = sprintf("%.3f", d_r$d)
  )
}) %>% bind_rows()

# Текстовый вывод без словесных оценок соответствия/несоответствия.
# БЕЗ split = TRUE: с ним весь лог дублируется в консоль, то есть все таблицы
# ANOVA уходят в stdout. Консоль отдана диагностике, числа живут в артефакте
# (README.ru.md, «Вывод прогона — два лога; числа — артефакты»).
txt_path <- file.path(OUT_DIR, "anova_results.txt")
sink(txt_path, append = FALSE)

w <- function(...) cat(..., "\n")
ww <- function(x) { print(x); cat("\n") }

w(SEP)
w("  ANOVA — ВАЛИДНОСТЬ ПО ИЗВЕСТНЫМ ГРУППАМ | ТКМ-Хим")
w(sprintf("  n = %d | Дата: %s", nrow(df), Sys.Date()))
w(SEP)

w()
w("  1. ОПИСАТЕЛЬНЫЕ СТАТИСТИКИ")
w(SEP)
w("  1.1 По курсам")
ww(desc_course)
w("  1.2 По конференциям")
ww(desc_conf)
w("  1.3 По языку")
ww(desc_lang)
w("  1.4 По регионам")
ww(desc_region)

w(SEP)
w("  2. ТЕСТ ЛЕВЕНА (курс)")
w(SEP)
ww(levene_tbl)

w(SEP)
w("  3. WELCH ANOVA — КУРС")
w(SEP)
w("  F и p — ANOVA Уэлча (равенство дисперсий не предполагается). eta2, omega2 и ДИ")
w("  ниже — из классической aov() на тех же данных: другая модель, другое разложение")
w("  суммы квадратов и другие степени свободы, поэтому размер эффекта относится не к")
w("  напечатанному выше F. В листе 0_PubTable_Course источник назван в заголовке")
w("  каждой колонки. ДИ для eta2 — двусторонний 95 %.")
w("  3.1 F-статистики")
ww(anova_course)
w("  3.2 Размеры эффекта (eta2, omega2) — классическая ANOVA")
ww(es_course)
w("  3.3 Пост-хок Геймса-Хауэлла")
if (nrow(gh_course) > 0) ww(gh_course) else w("  Все p>=0.05\n")

w(SEP)
w("  4. KRUSKAL-WALLIS")
w(SEP)
w("  Зависимые переменные — только сырые баллы. WLE-тета шага 4 не включена ни здесь,")
w("  ни в параметрических семействах: при полных данных она — строго возрастающая")
w("  функция Score_Total, поэтому ранговые тесты по ней дают те же H, epsilon2 и p, а")
w("  параметрические опирались бы на нормальность при 24 значениях теты на 253")
w("  респондента и связке в 45 человек на потолке (ISSUES.md 1.5, DECISIONS.md D9).")
ww(kw_course)
w("  Epsilon2")
ww(eps2)
if (nrow(dunn_course) > 0) {
  w("  Пост-хок Данна")
  ww(dunn_course)
}

w(SEP)
w("  5. MANOVA (Pillai)")
w(SEP)
print(man_sum)
cat("\n")
if (man_sum$stats["Course", "Pr(>F)"] < .05) {
  print(summary.aov(man))
  cat("\n")
}

w(SEP)
w("  6. КОНФЕРЕНЦИИ / ОЛИМПИАДЫ — t-тест + d Коэна")
w(SEP)
w("  t-тест (Уэлча)")
ww(t_conf)
w("  d Коэна")
ww(d_conf)

w(SEP)
w("  7. ЯЗЫК — t-тест + d Коэна")
w(SEP)
ww(t_lang)
w("  d Коэна")
ww(d_lang)

w(SEP)
w("  8. GPA — ANOVA + корреляция Спирмена")
w(SEP)
w("  ANOVA Уэлча (GPA)")
ww(anova_gpa)
if (nrow(gh_gpa) > 0) {
  w("  Геймс-Хауэлл (GPA)")
  ww(gh_gpa)
}
w("  Спирмен: GPA_ordinal x баллы")
ww(spearman_gpa)
w("  Пирсон: GPA_numeric x баллы")
ww(pearson_gpa)

w(SEP)
w("  9. РЕГИОН — ANOVA (классическая: в регионах есть малые группы)")
w(SEP)
w("  Гомогенность дисперсий по региону НЕ проверялась (тест Левена не применялся),")
w("  Welch-аппроксимация не считается из-за малых групп. F интерпретируется вместе")
w("  с eta2 и его двусторонним 95 % ДИ в той же таблице: обе величины из одной aov().")
ww(anova_region)

w(SEP)
w("  10. СВОДКА ЗНАЧИМОСТИ ОМНИБУСНЫХ ГРУППОВЫХ СРАВНЕНИЙ")
w(SEP)
ww(as.data.frame(group_tests))
w(sprintf("  Сравнений с p < 0.05: %d из %d",
          sum(!is.na(group_tests$p) & group_tests$p < .05), nrow(group_tests)))
sink()

# Экспорт в Excel
write_xlsx(list(
  "0_PubTable_Course"   = pub_course,
  "1_PubTable_Conf"     = pub_conf,
  "2_Desc_Course"       = desc_course,
  "3_Desc_Conference"   = desc_conf,
  "4_Desc_Language"     = desc_lang,
  "5_ANOVA_Course"      = anova_course,
  "6_EffectSize_Course" = es_course,
  "7_GH_Course"         = if (nrow(gh_course) > 0) gh_course else tibble(note = "ns"),
  "8_KW_Course"         = kw_course,
  "9_Epsilon2"          = eps2,
  "10_ttest_Conf"       = t_conf,
  "11_CohenD_Conf"      = d_conf,
  "12_ttest_Lang"       = t_lang,
  "13_CohenD_Lang"      = d_lang,
  "14_ANOVA_GPA"        = anova_gpa,
  "15_GH_GPA"           = if (nrow(gh_gpa) > 0) gh_gpa else tibble(note = "ns"),
  "16_Spearman_GPA"     = spearman_gpa,
  "17_Levene"           = levene_tbl,
  "18_Pearson_GPAnum"   = pearson_gpa,
  "19_Desc_Region"      = desc_region,
  "20_ANOVA_Region"     = anova_region,
  "21_GroupTests_Sig"   = group_tests
), file.path(OUT_DIR, "anova_results.xlsx"))
