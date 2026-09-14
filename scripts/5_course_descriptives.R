#!/usr/bin/env Rscript
# =============================================================================
#  ШАГ 5 — ОПИСАТЕЛЬНАЯ СТАТИСТИКА ПО КУРСАМ
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv     (шаг 0; Score_Total, Score_S1..S3, Course)
#         output/Rasch/person_theta.csv    (ШАГ 4; строки Theta таблицы)
#  ВЫХОД: output/Descriptive/descriptive_stats_by_course.csv (utf-8-sig) -> report.Rmd
#
#  Идёт ПОСЛЕ шага 4, потому что Theta (WLE) в cleaned_responses.csv не попадает
#  и приходит отдельным артефактом Раша. Оба входа ОБЯЗАТЕЛЬНЫ: без Theta таблица
#  вышла бы тихо неполной — без строк Theta, но с кодом возврата 0.
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------------

local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
# config.R здесь не нужен: субшкальные баллы приходят готовыми из шага 0
source("scripts/_setup.R")   # csv_field / write_csv_excel (CSV utf-8-sig)

INPUT_FILE <- "output/cleaned_responses.csv"
THETA_FILE <- "output/Rasch/person_theta.csv"
OUT_DIR    <- "output/Descriptive"
OUT_CSV    <- file.path(OUT_DIR, "descriptive_stats_by_course.csv")

fmt_num <- function(x) vapply(x, function(v) {
  if (is.na(v)) return("")
  if (v == round(v)) return(sprintf("%.1f", v))
  as.character(v)
}, character(1))
skew_biased <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); mean((x - m)^3) / m2^1.5 }
kurt_excess <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); mean((x - m)^4) / m2^2 - 3 }

main <- function() {
  require_input(INPUT_FILE, "scripts/0_preprocess.R (шаг 0)")
  require_input(THETA_FILE, "scripts/4_rasch.R (шаг 4)")
  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  df <- read.csv(INPUT_FILE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)

  # Course приходит из cleaned_responses.csv как "1".."4" (или "3.0"/"" при наличии
  # NA после render_int_col), поэтому ведущее число извлекается ОДИН раз и переиспользуется.
  # sub сохраняет длину вектора (NA на несовпадениях) — далее строки с NA отсекаются.
  df$Course_num <- as.numeric(sub(".*?([0-9]+).*", "\\1", as.character(df$Course)))
  df <- df[!is.na(df$Course_num), , drop = FALSE]
  df$Course_num <- as.integer(df$Course_num)

  # Тета (WLE, модель Раша) живёт в отдельном артефакте шага 4 и в
  # cleaned_responses.csv не попадает — присоединение по ID, как это делает шаг 8.
  # Вход обязателен (проверен выше), поэтому ветки «без Theta» здесь нет.
  th <- read.csv(THETA_FILE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)
  df <- merge(df, th[, c("ID", "Theta")], by = "ID", all.x = TRUE)

  # Score_S1..S3 приходят готовыми из cleaned_responses.csv (считаются в шаге 0)
  scores <- c("Score_Total", "Score_S1", "Score_S2", "Score_S3", "Theta")
  labels <- c(Score_Total = "Общий балл", Score_S1 = "S1: Интерпретация",
              Score_S2 = "S2: Анализ", Score_S3 = "S3: Оценка", Theta = "Theta (Раш)")

  rows <- list()
  for (score in scores) {
    for (course in sort(unique(df$Course_num))) {
      sub <- df[[score]][df$Course_num == course]
      sub <- sub[!is.na(sub)]
      if (length(sub) == 0) next
      rows[[length(rows)+1]] <- data.frame(
        `Показатель` = labels[[score]],
        `Курс` = sprintf("%d курс", course),
        N = length(sub),
        `Mean (M)` = round(mean(sub), 2),
        SD = round(sd(sub), 2),
        `Median (Mdn)` = round(median(sub), 2),
        Min = round(min(sub), 2),
        Max = round(max(sub), 2),
        Skewness = round(skew_biased(sub), 2),
        Kurtosis = round(kurt_excess(sub), 2),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  res <- do.call(rbind, rows)

  out <- data.frame(
    `Показатель` = res$`Показатель`, `Курс` = res$`Курс`,
    N = as.character(as.integer(res$N)),
    `Mean (M)` = fmt_num(res$`Mean (M)`), SD = fmt_num(res$SD),
    `Median (Mdn)` = fmt_num(res$`Median (Mdn)`), Min = fmt_num(res$Min), Max = fmt_num(res$Max),
    Skewness = fmt_num(res$Skewness), Kurtosis = fmt_num(res$Kurtosis),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  write_csv_excel(out, OUT_CSV)
}

main()
