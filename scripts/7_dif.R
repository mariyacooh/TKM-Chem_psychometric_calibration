# =======================================================================
#  ШАГ 7 — АНАЛИЗ DIF (ru vs. kz): Mantel-Haenszel + ETS delta
# -----------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0; ответы + Language)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/DIF/dif_mh_table.csv        -> plot_dif_mh, report.Rmd
#         output/DIF/dif_excluded_items.csv  -> write_report_text.R (отсев нулевой
#                                               дисперсии; пишется ВСЕГДА, пустой =
#                                               ничего не снято)
#         output/DIF/dif_language_results.txt -> report.Rmd
#
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------

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
set.seed(42)

load_pkgs(c("tidyverse", "difR"))

OUT <- "output/DIF"
ensure_dir(OUT)

source("scripts/config.R")   # ITEMS, ITEM_SUB (единый источник)

require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read_csv("output/cleaned_responses.csv") %>%
  mutate(across(starts_with("Q"), as.integer),
         lang_bin = as.integer(Language == "kz"))
item_data <- df %>% select(all_of(ITEMS)) %>% as.matrix()
lang_vec  <- df$lang_bin
n_ru <- sum(lang_vec == 0); n_kz <- sum(lang_vec == 1)

# Пункт с нулевой дисперсией не даёт таблицы 2xK и валит difMH; zero_var_cols в
# 4_rasch.R отсекает такие пункты перед tam.mml — здесь тот же фильтр.
item_var <- apply(item_data, 2, var, na.rm = TRUE)
zero_var <- names(item_var)[is.na(item_var) | item_var == 0]
DIF_ITEMS <- setdiff(colnames(item_data), zero_var)
# Отсев пишется артефактом всегда, в том числе пустым (0 строк): «ничего не удалено»
# тоже результат. Консоль дублирует его диагностикой в stderr.
write_csv_excel(data.frame(
  Item = zero_var, Variance = unname(item_var[zero_var]),
  stringsAsFactors = FALSE, check.names = FALSE
), file.path(OUT, "dif_excluded_items.csv"))
if (length(zero_var)) {
  message("[7] Пункты нулевой дисперсии исключены из DIF: ", paste(zero_var, collapse = ", "))
  item_data <- item_data[, DIF_ITEMS, drop = FALSE]
}

dif_mh <- tryCatch(
  difMH(Data = item_data, group = lang_vec, focal.name = 1, alpha = 0.05,
        purify = TRUE, p.adjust.method = "BH"),
  error = function(e) stop(sprintf("7_dif: difMH() упал: %s", conditionMessage(e)),
                           call. = FALSE))
mh_ln_or <- if (is.numeric(dif_mh$alphaMH) && length(dif_mh$alphaMH) == length(DIF_ITEMS)) log(dif_mh$alphaMH) else rep(NA, length(DIF_ITEMS))
# Величина эффекта DIF по шкале ETS Delta (Holland & Thayer, 1988):
#   MH D-DIF = -2.35 * ln(alphaMH). Колонка ETS_delta и ЕСТЬ MH D-DIF; второго
#   имени для той же величины в таблице нет (имя закреплено DECISIONS.md D4).
# ПРЕЖНЯЯ колонка ETS — только по модулю |Delta| (грубый тир величины,
#   оставлена для совместимости); полная классификация — в ETS_2 ниже.
ets_delta <- -2.35 * mh_ln_or
ets_class <- as.character(cut(abs(ets_delta), breaks = c(-Inf, 1.0, 1.5, Inf),
                              labels = c("A", "B", "C"), right = FALSE))

# --- Полная двойная классификация ETS (Zwick, 2012) ------------------------
# difMH возвращает дисперсию ln(alphaMH) (оценка Robins-Breslow-Greenland) в
# $varLambda, поэтому SE и ДИ MH D-DIF берутся напрямую из difR, без переизобретения:
#   SE(MH D-DIF) = 2.35 * sqrt(varLambda);  95% ДИ = D-DIF +/- 1.96 * SE.
# Значимость (!= 0) — по BH-скорректированному p (тот же p_adj, alpha = .05).
#   C — |D-DIF| >= 1.5 И значимо > 1.0. Гипотеза H1: |D-DIF| > 1 ОДНОсторонняя,
#       поэтому граница односторонняя: 1.645, не 1.96 (двусторонний 1.96 систематически
#       строже ETS и уводил бы пункты из C в B);
#   B — значимо != 0 И (|D-DIF| >= 1.0 ИЛИ не прошёл порог C);
#   A — незначимо ЛИБО |D-DIF| < 1.0.
mh_var_lambda <- if (is.numeric(dif_mh$varLambda) && length(dif_mh$varLambda) == length(DIF_ITEMS)) dif_mh$varLambda else rep(NA_real_, length(DIF_ITEMS))
mh_se   <- 2.35 * sqrt(mh_var_lambda)          # SE MH D-DIF на шкале ETS Delta
ci_low  <- ets_delta - 1.96 * mh_se
ci_high <- ets_delta + 1.96 * mh_se
abs_d   <- abs(ets_delta)
p_adj_v <- dif_mh$adjusted.p
sig      <- !is.na(p_adj_v) & p_adj_v < 0.05                       # значимо != 0
c_bound  <- !is.na(mh_se) & (abs_d - 1.645 * mh_se) > 1.0          # односторонняя нижняя граница > 1.0
ets_2 <- ifelse(is.na(ets_delta) | is.na(p_adj_v), NA_character_,
          ifelse(!sig | abs_d < 1.0, "A",
          ifelse(abs_d >= 1.5 & c_bound, "C", "B")))

mh_tbl <- tibble(Item = DIF_ITEMS, Subscale = ITEM_SUB[DIF_ITEMS], MH_chi2 = round(dif_mh$MH, 3),
                 p_adj = round(dif_mh$adjusted.p, 4), lnOR = round(mh_ln_or, 3),
                 ETS_delta = round(ets_delta, 3), ETS = ets_class,
                 SE = round(mh_se, 3),
                 CI_low = round(ci_low, 3), CI_high = round(ci_high, 3), ETS_2 = ets_2,
                 # Тот же NA-безопасный флаг sig, что и у ETS_2: без is.na() один NA в
                 # adjusted.p (реален на стратифицированной 2xK при n_ru = 61) давал бы
                 # NA в ячейке, а сумма ниже обнуляла бы весь счётчик в NA.
                 DIF_flag = ifelse(is.na(p_adj_v), NA_character_, ifelse(sig, "DIF!", "-")))

dif_lr <- tryCatch(difLogistic(Data = item_data, group = lang_vec, focal.name = 1, alpha = 0.05, type = "both", purify = TRUE, p.adjust.method = "BH"), error = function(e) NULL)
dif_lord <- tryCatch(difLord(Data = item_data, group = lang_vec, focal.name = 1, model = "1PL", alpha = 0.05, purify = TRUE, p.adjust.method = "BH"), error = function(e) NULL)

n_dif_mh <- sum(mh_tbl$DIF_flag == "DIF!", na.rm = TRUE)
n_na_mh  <- sum(is.na(mh_tbl$p_adj))   # пункты без оценки p (difMH вернул NA)
# Счётчики всех трёх методов — по BH-скорректированному p: три строки лога стоят
# рядом и сопоставимы только при одинаковом контроле множественности по 26 пунктам.
# Отсутствие adjusted.p означало бы МОЛЧАЛИВЫЙ 0 вместо счёта: у объекта без этого
# поля obj$adjusted.p равен NULL, сравнение даёт logical(0), а sum(logical(0)) равен
# 0 — неотличимо от измеренного «ни один пункт не показал DIF». Поэтому столбец
# проверяется явно у ОБОИХ счётчиков: состав полей difR зависит от версии пакета, а
# версия задана датой снапшота (PKG_SNAPSHOT в scripts/_setup.R), и её смена меняет
# пакет.
n_dif_adj <- function(obj, fn) {
  if (is.null(obj)) return(NA_integer_)
  if (!is.numeric(obj$adjusted.p))
    stop(sprintf("7_dif: %s() не вернул adjusted.p при p.adjust.method = \"BH\"", fn),
         call. = FALSE)
  sum(obj$adjusted.p < 0.05, na.rm = TRUE)
}
n_dif_lr   <- n_dif_adj(dif_lr,   "difLogistic")
n_dif_lord <- n_dif_adj(dif_lord, "difLord")

# Сохранение таблицы DIF MH для визуализации
write_csv_excel(mh_tbl, file.path(OUT, "dif_mh_table.csv"))

SEP <- strrep("=", 70)
sink(file.path(OUT, "dif_language_results.txt"))
cat(SEP, "\n")
cat(sprintf("  АНАЛИЗ DIF - язык (ru vs. kz)\n  n(ru)=%d n(kz)=%d пунктов=%d\n", n_ru, n_kz, length(DIF_ITEMS)))
cat(SEP, "\n\n")
cat("MANTEL-HAENSZEL (поправка BH)\n\n")
print(as.data.frame(mh_tbl), row.names = FALSE)
cat(sprintf("\nПунктов с DIF (MH, alpha=.05, BH): %d из %d\n", n_dif_mh, length(DIF_ITEMS)))
if (n_na_mh > 0)
  cat(sprintf("Пунктов без оценки p (difMH вернул NA, в счётчик не вошли): %d\n", n_na_mh))
cat(sprintf("ETS-классификация величины (|Delta|): A=%d, B=%d, C=%d\n",
            sum(mh_tbl$ETS == "A", na.rm = TRUE),
            sum(mh_tbl$ETS == "B", na.rm = TRUE),
            sum(mh_tbl$ETS == "C", na.rm = TRUE)))
cat(sprintf("ETS двойная классификация ETS_2 (Zwick 2012; знач. p_adj + односторонняя граница |D-DIF| > 1): A=%d, B=%d, C=%d\n",
            sum(mh_tbl$ETS_2 == "A", na.rm = TRUE),
            sum(mh_tbl$ETS_2 == "B", na.rm = TRUE),
            sum(mh_tbl$ETS_2 == "C", na.rm = TRUE)))
if (!is.null(dif_lr)) { cat(sprintf("DIF по логистической регрессии (alpha=.05, BH): %d из %d\n", n_dif_lr, length(DIF_ITEMS))) }
if (!is.null(dif_lord)) { cat(sprintf("DIF по chi2 Лорда (1PL, alpha=.05, BH): %d из %d\n", n_dif_lord, length(DIF_ITEMS))) }
sink()
