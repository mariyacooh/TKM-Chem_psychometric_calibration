#!/usr/bin/env Rscript
# =============================================================================
#  ШАГ 1 — ОПИСАТЕЛЬНАЯ СТАТИСТИКА (по всей выборке)
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/Descriptive/descriptive_stats_detailed.csv (utf-8-sig) -> report.Rmd
#         output/Descriptive/descriptive_stats_tables.csv   (utf-8-sig) -> report.Rmd
#         output/Descriptive/descriptive_stats.txt          (utf-8, без BOM)
#
#  Разбивку по курсам считает отдельный шаг 5 (нужна Theta из шага 4).
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------------
#
#  Моменты (skew/kurtosis) — смещённые оценки: g1 и g2 (excess).
#  Shapiro-Wilk — shapiro.test. Частоты категорий: по убыванию, стабильно
#  (при равенстве — в порядке первого появления значения).
# =============================================================================

local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/_setup.R")   # csv_field / write_csv_excel (CSV utf-8-sig)
source("scripts/config.R")   # SUBSCALES, ITEMS

INPUT_FILE <- "output/cleaned_responses.csv"
OUT_DIR    <- "output/Descriptive"

# ── Форматирование чисел для отчёта: целочисленные -> "N.0", иначе как есть ───
fmt_num <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (v == round(v)) return(sprintf("%.1f", v))
    as.character(v)
  }, character(1))
}
# Выравнивание по ЧИСЛУ СИМВОЛОВ, не по байтам.
ljust <- function(s, w) { n <- nchar(s, type = "chars"); ifelse(n < w, paste0(s, strrep(" ", w - n)), s) }
rjust <- function(s, w) { n <- nchar(s, type = "chars"); ifelse(n < w, paste0(strrep(" ", w - n), s), s) }

# Моменты (смещённые оценки): g1 (skewness) и g2 (kurtosis, excess).
skew_biased <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m3 / m2^1.5 }
kurt_excess <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); m4 <- mean((x - m)^4); m4 / m2^2 - 3 }

# Частоты значений (включая NA): по убыванию, стабильно (первое появление).
freq_counts <- function(x) {
  u <- unique(x)                                  # порядок первого появления
  cnt <- vapply(u, function(v) if (is.na(v)) sum(is.na(x)) else sum(!is.na(x) & x == v), integer(1))
  ord <- order(-cnt)                              # radix -> стабильно
  data.frame(value = u[ord], n = cnt[ord], stringsAsFactors = FALSE)
}

cont_stats <- function(series, label) {
  s <- series[!is.na(series)]
  if (length(s) == 0) return(NULL)
  n <- length(s)
  # Shapiro-Wilk осмыслен только для непрерывных величин. На дихотомическом пункте
  # (0/1) он ненормальность не измеряет: при n = 253 отвергается всегда по
  # построению, независимо от качества пункта, — а в таблицу отчёта уходил бы
  # содержательным столбцом. На постоянном векторе shapiro.test() ещё и падает с
  # ошибкой, так что пункт нулевой дисперсии уронил бы весь шаг (шаги 4/6/7 такие
  # пункты отсекают перед оценкой). Для Score_S1..S3 и Score_Total тест сохранён.
  # Границы n — область определения самого shapiro.test().
  sw <- if (length(unique(s)) > 2 && n >= 3L && n <= 5000L) shapiro.test(s)
        else list(statistic = NA_real_, p.value = NA_real_)
  list(
    Variable = label, N = n, Missing = sum(is.na(series)),
    Mean = round(mean(s), 3),
    SD = if (n > 1) round(sd(s), 3) else 0.0,
    SE = if (n > 1) round(sd(s) / sqrt(n), 3) else 0.0,
    Min = round(min(s), 3),
    P25 = round(unname(quantile(s, .25, type = 7)), 3),
    Mdn = round(median(s), 3),
    P75 = round(unname(quantile(s, .75, type = 7)), 3),
    Max = round(max(s), 3),
    IQR = round(unname(quantile(s, .75, type = 7) - quantile(s, .25, type = 7)), 3),
    Skewness = round(skew_biased(s), 3),
    Kurtosis = round(kurt_excess(s), 3),
    Shapiro_W = round(unname(sw$statistic), 4),
    Shapiro_p = round(sw$p.value, 4)
  )
}

# CSV (utf-8-sig, LF, минимальное квотирование): csv_field / write_csv_excel —
# общие в scripts/_setup.R.

# ── Печать частот/непрерывных в текстовый отчёт ──────────────────────────────
freq_lines <- function(series) {
  vc <- freq_counts(series)
  total <- length(series)
  out <- c(
    paste0("   ", ljust("Категория", 35), "  ", rjust("N", 5), "  ", rjust("%", 6)),
    paste0("   ", strrep("-", 35), "  ", strrep("-", 5), "  ", strrep("-", 6))
  )
  for (i in seq_len(nrow(vc))) {
    val <- vc$value[i]; cnt <- vc$n[i]
    val_str <- if (is.na(val)) "(пусто)" else as.character(val)
    pct <- cnt / total * 100
    out <- c(out, paste0("   ", ljust(val_str, 35), "  ", rjust(as.character(cnt), 5), "  ", rjust(sprintf("%.1f", pct), 5), "%"))
  }
  c(out, paste0("   ", ljust("ИТОГО", 35), "  ", rjust(as.character(total), 5), "  ", "100.0%"))
}
cont_lines <- function(d) {
  if (is.null(d)) return("   (нет данных)")
  out <- c(
    sprintf("   N = %d  (пропущено: %d)", d$N, d$Missing),
    sprintf("   M = %s  SD = %s  SE = %s", fmt_num(d$Mean), fmt_num(d$SD), fmt_num(d$SE)),
    sprintf("   Min = %s  P25 = %s  Mdn = %s  P75 = %s  Max = %s", fmt_num(d$Min), fmt_num(d$P25), fmt_num(d$Mdn), fmt_num(d$P75), fmt_num(d$Max)),
    sprintf("   IQR = %s", fmt_num(d$IQR)),
    sprintf("   Skewness = %s", fmt_num(d$Skewness)),
    sprintf("   Kurtosis (excess) = %s", fmt_num(d$Kurtosis))
  )
  if (!is.na(d$Shapiro_W)) out <- c(out, sprintf("   Shapiro-Wilk: W = %s, p = %s", fmt_num(d$Shapiro_W), fmt_num(d$Shapiro_p)))
  out
}
freq_table <- function(series, label) {
  vc <- freq_counts(series)
  total <- sum(vc$n)
  data.frame(Variable = label, Value = as.character(vc$value),
             N = vc$n, `%` = round(vc$n / total * 100, 1),
             check.names = FALSE, stringsAsFactors = FALSE)
}

main <- function() {
  require_input(INPUT_FILE, "scripts/0_preprocess.R (шаг 0)")
  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
  df <- read.csv(INPUT_FILE, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE, check.names = FALSE)

  # Score_S1..S3 приходят готовыми из cleaned_responses.csv (считаются в шаге 0)

  # detailed: пункты (в порядке ITEMS) + субшкалы + общий балл
  q_cols <- ITEMS[ITEMS %in% names(df)]
  detail_vars <- c(q_cols, "Score_S1", "Score_S2", "Score_S3", "Score_Total")
  rows <- lapply(detail_vars, function(v) if (v %in% names(df)) cont_stats(df[[v]], v))
  rows <- Filter(Negate(is.null), rows)
  cols <- c("Variable","N","Missing","Mean","SD","SE","Min","P25","Mdn","P75","Max","IQR","Skewness","Kurtosis","Shapiro_W","Shapiro_p")
  detailed <- data.frame(lapply(cols, function(cn) {
    vals <- vapply(rows, function(r) r[[cn]], if (cn == "Variable") character(1) else numeric(1))
    if (cn %in% c("Variable")) vals
    else if (cn %in% c("N","Missing")) as.character(as.integer(vals))
    else fmt_num(vals)
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(detailed) <- cols
  write_csv_excel(detailed, file.path(OUT_DIR, "descriptive_stats_detailed.csv"))

  # ── Текстовый отчёт ──
  SEP <- strrep("=", 68); sep2 <- strrep("-", 68)
  L <- c(SEP, "  ОПИСАТЕЛЬНАЯ СТАТИСТИКА", sprintf("  N = %d участников", nrow(df)), SEP)
  all_tables <- list()

  L <- c(L, "", sep2, "  1. ЯЗЫК АНКЕТЫ", sep2, freq_lines(df$Language))
  all_tables[[length(all_tables)+1]] <- freq_table(df$Language, "Language")

  L <- c(L, "", sep2, "  2. КУРС ОБУЧЕНИЯ", sep2, freq_lines(df$Course), "", cont_lines(cont_stats(df$Course, "Course")))
  all_tables[[length(all_tables)+1]] <- freq_table(as.character(df$Course), "Course")

  L <- c(L, "", sep2, "  3. РЕГИОН", sep2)
  if ("Region" %in% names(df) && any(!is.na(df$Region))) {
    L <- c(L, freq_lines(df$Region)); all_tables[[length(all_tables)+1]] <- freq_table(df$Region, "Region")
  } else L <- c(L, "   (нет данных)")

  L <- c(L, "", sep2, "  4. САМООЦЕНКА УСПЕВАЕМОСТИ ПО ПРОФИЛЬНЫМ ПРЕДМЕТАМ (GPA_ordinal)", sep2)
  if ("GPA_ordinal" %in% names(df) && any(!is.na(df$GPA_ordinal))) {
    L <- c(L, freq_lines(as.character(df$GPA_ordinal)), "", cont_lines(cont_stats(df$GPA_ordinal, "GPA_ordinal")))
    all_tables[[length(all_tables)+1]] <- freq_table(as.character(df$GPA_ordinal), "GPA_ordinal")
  }

  L <- c(L, "", sep2, "  5. GPA ЧИСЛОВОЙ", sep2)
  if ("GPA_numeric" %in% names(df) && any(!is.na(df$GPA_numeric))) L <- c(L, cont_lines(cont_stats(df$GPA_numeric, "GPA_numeric")))

  L <- c(L, "", sep2, "  6. УЧАСТИЕ В КОНФЕРЕНЦИЯХ / ОЛИМПИАДАХ", sep2)
  if ("Conference" %in% names(df) && any(!is.na(df$Conference))) {
    L <- c(L, freq_lines(as.character(df$Conference)))
    all_tables[[length(all_tables)+1]] <- freq_table(as.character(df$Conference), "Conference")
  }

  L <- c(L, "", sep2, "  7. СУММАРНЫЙ БАЛЛ (Score_Total)", sep2, cont_lines(cont_stats(df$Score_Total, "Score_Total")))

  con <- file(file.path(OUT_DIR, "descriptive_stats.txt"), open = "wb")   # utf-8, без BOM
  writeBin(charToRaw(enc2utf8(paste0(paste(L, collapse = "\n"), "\n"))), con)
  close(con)

  if (length(all_tables)) {
    tbl <- do.call(rbind, all_tables)
    tbl$`%` <- fmt_num(tbl$`%`)
    tbl$N <- as.character(as.integer(tbl$N))
    write_csv_excel(tbl, file.path(OUT_DIR, "descriptive_stats_tables.csv"))
  }
}

main()
