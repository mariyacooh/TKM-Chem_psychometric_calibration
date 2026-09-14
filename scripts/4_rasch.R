# =============================================================================
#  ШАГ 4 — МОДЕЛЬ РАША (1PL, TAM): калибровка пунктов и WLE-тета респондентов
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/Rasch/person_theta.csv      -> ШАГ 5, plot_rasch_wright
#         output/Rasch/rasch_item_report.csv -> plot_rasch_wright, report.Rmd
#         output/Rasch/rasch_excluded_items.csv -> write_report_text.R
#                                               (отсев нулевой дисперсии)
#         output/Rasch/rasch_model.RData     -> plot_rasch_icc
#         output/Rasch/rasch_report.txt      -> report.Rmd
#
#  ЕДИНСТВЕННЫЙ источник Theta (WLE) в пайплайне: person_theta.csv — единственное
#  ребро «анализ -> анализ». Отсюда и позиция шага: 4 стоит ПЕРЕД потребителями
#  (шаг 5), поэтому порядок номеров = порядок запуска.
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------------

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

load_pkgs(c("tidyverse", "readr", "TAM", "psych", "Matrix"))

ensure_dir("output/Rasch")

# ── Структура субшкал (исключённые пункты убраны в config.R / input/items.csv) ──
source("scripts/config.R")   # ITEMS (единый источник)

# ── Загрузка данных ────────────────────────────────────────────────────────
require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df_raw <- read_csv("output/cleaned_responses.csv")

all_item_cols <- intersect(ITEMS, names(df_raw))
X_raw <- df_raw %>% dplyr::select(all_of(all_item_cols)) %>% as.matrix()

# Пункты с нулевой дисперсией удаляются. Отсев ЗАПИСЫВАЕТСЯ АРТЕФАКТОМ, как в
# 6_2pl.R и 7_dif.R: молчаливое удаление развело бы набор пунктов Раша с наборами
# соседних шагов без следа. Артефакт пишется всегда, в том числе пустой (0 строк) —
# «ничего не удалено» тоже результат, и его отсутствие нельзя было бы отличить от
# невыполненной проверки. Консоль дублирует отсев диагностикой в stderr.
item_var <- apply(X_raw, 2, var, na.rm = TRUE)
zero_var_cols <- names(item_var[is.na(item_var) | item_var == 0])
write_csv_excel(data.frame(
  Item = zero_var_cols, Variance = unname(item_var[zero_var_cols]),
  stringsAsFactors = FALSE, check.names = FALSE
), "output/Rasch/rasch_excluded_items.csv")
if (length(zero_var_cols) > 0) {
  message("[4] Пункты нулевой дисперсии исключены: ", paste(zero_var_cols, collapse = ", "))
  all_item_cols <- setdiff(all_item_cols, zero_var_cols)
  X_raw <- df_raw %>% dplyr::select(all_of(all_item_cols)) %>% as.matrix()
}

mod_rasch <- tam.mml(resp = X_raw, irtmodel = "1PL")

# Сходимость проверяется так же строго, как у соседних моделей (fit_cfa() в
# 3_efa_cfa.R — lavInspect, ok_1pl/ok_2pl в 6_2pl.R — extract.mirt): tam.mml() при
# исчерпании итераций не падает, а возвращает объект, и трудности, фит, WLE-тета
# и WLE.rel ушли бы в артефакты без пометки. Тета отсюда — единственное ребро
# «анализ -> анализ» (шаг 5), поэтому дефект распространился бы дальше.
# Имя поля статуса между версиями TAM разнится: сначала явный флаг, иначе — упор
# в предел итераций; если ни того, ни другого нет, статус — честный NA, а не
# объявленная сходимость.
rasch_converged <- local({
  flag <- mod_rasch$converged
  if (!is.null(flag)) return(isTRUE(flag))
  it <- mod_rasch$iter; mx <- mod_rasch$control$maxiter
  if (is.null(it) || is.null(mx)) return(NA)
  it < mx
})
if (isTRUE(!rasch_converged))
  warning("Модель Раша не сошлась (упор в предел итераций tam.mml) — трудности, фит и тета ненадёжны.",
          call. = FALSE)

fit_stats <- tam.fit(mod_rasch)$itemfit

# Фит (tam.fit()$itemfit) и трудности (tam.mml()$item) — два разных объекта. Связывание
# идёт по ИМЕНИ пункта, а расхождение состава/порядка роняет шаг: при склейке по позиции
# (другая версия TAM, отсев пункта, иной itemfit) трудности молча привязались бы не к тем
# пунктам и ушли в rasch_item_report.csv, карту Райта и отчёт. Тот же стандарт, что у
# сверки raw_mat/lang_raw_kept в 9_distractor.R и у validate_items() в config.R.
items_mml <- colnames(X_raw)
fit_items <- as.character(fit_stats$parameter)
mml_items <- as.character(mod_rasch$item$item)
if (!setequal(fit_items, items_mml) || !setequal(mml_items, items_mml))
  stop("4_rasch: состав пунктов в tam.fit()$itemfit / tam.mml()$item разошёлся с ",
       "матрицей ответов — фит и трудности привязались бы не к тем пунктам.", call. = FALSE)

rasch_report <- tibble(
  Item       = items_mml,
  Outfit_MSQ = round(fit_stats$Outfit[match(items_mml, fit_items)], 3),
  Infit_MSQ  = round(fit_stats$Infit[ match(items_mml, fit_items)], 3),
  Difficulty = round(mod_rasch$item$xsi.item[match(items_mml, mml_items)], 3)
)

write_csv_excel(rasch_report, "output/Rasch/rasch_item_report.csv")

wle_est      <- tam.wle(mod_rasch)
person_theta <- wle_est$theta

person_theta_df <- tibble(
  ID    = df_raw$ID,
  Theta = wle_est$theta,
  Error = wle_est$error
)
write_csv_excel(person_theta_df, "output/Rasch/person_theta.csv")

# Текстовый отчет (без качественных суждений вроде "Redundancy" и "p-value Хорошо/Плохо")
sink("output/Rasch/rasch_report.txt", type = "output")
SEP  <- strrep("=", 68)
sep2 <- strrep("-", 68)

cat(SEP, "\n")
cat("  АНАЛИЗ ПО МОДЕЛИ РАША (1PL / Rasch)\n")
cat(SEP, "\n\n")
cat(sprintf("  N респондентов : %d\n", nrow(X_raw)))
cat(sprintf("  N пунктов      : %d\n", ncol(X_raw)))
cat("\n")

if (length(zero_var_cols) > 0)
  cat("!! Пункты нулевой дисперсии исключены из оценки: ",
      paste(zero_var_cols, collapse = ", "), "\n\n", sep = "")
if (isTRUE(!rasch_converged)) {
  cat("!! ОГОВОРКА: tam.mml() не достиг сходимости (упор в предел итераций).\n",
      "   Трудности, Infit/Outfit, WLE-тета и WLE.rel ниже получены из\n",
      "   несошедшейся оценки и ненадёжны; их потребитель — шаг 5.\n\n", sep = "")
} else if (is.na(rasch_converged)) {
  cat("!! Статус сходимости tam.mml() не определён: установленная версия TAM не\n",
      "   отдаёт ни флага сходимости, ни пары iter/maxiter.\n\n", sep = "")
}

cat(sep2, "\n")
cat("  1. ТРУДНОСТЬ ПУНКТОВ (логиты) -- от трудных к лёгким\n")
cat(sep2, "\n")
cat(sprintf("  %-6s  %9s\n", "Item", "b (logit)"))
cat(sprintf("  %s  %s\n", strrep("-", 6), strrep("-", 9)))
sorted_report <- rasch_report %>% arrange(desc(Difficulty))
for (i in seq_len(nrow(sorted_report))) {
  cat(sprintf("  %-6s  %+9.3f\n", sorted_report$Item[i], sorted_report$Difficulty[i]))
}
cat(sprintf("\n  M = %.3f  SD = %.3f\n\n", mean(rasch_report$Difficulty), sd(rasch_report$Difficulty)))

cat(sep2, "\n")
cat("  2. СООТВЕТСТВИЕ ПУНКТОВ МОДЕЛИ (Infit / Outfit MSQ)\n")
cat(sep2, "\n")
cat(sprintf("  %-6s  %7s  %7s\n", "Item", "Infit", "Outfit"))
cat(sprintf("  %s  %s  %s\n", strrep("-", 6), strrep("-", 7), strrep("-", 7)))
for (i in seq_len(nrow(rasch_report))) {
  cat(sprintf("  %-6s  %7.3f  %7.3f\n", rasch_report$Item[i], rasch_report$Infit_MSQ[i], rasch_report$Outfit_MSQ[i]))
}

cat("\n", sep2, "\n")
cat("  3. НАДЁЖНОСТЬ И ПАРАМЕТРЫ МОДЕЛИ\n")
cat(sep2, "\n")
cat(sprintf("  Надёжность WLE  : %.3f\n", wle_est$WLE.rel[[1]]))
cat(sprintf("  Deviance (-2LL) : %.1f\n", mod_rasch$deviance))
cat(sprintf("  AIC             : %.1f\n", mod_rasch$ic$AIC))
cat(sprintf("  BIC             : %.1f\n", mod_rasch$ic$BIC))

cat("\n", sep2, "\n")
# Описательная статистика латентной теты (перенесено из шага 1: тета рождается здесь)
th   <- person_theta
q_th <- quantile(th, c(.25, .5, .75))
sw   <- shapiro.test(th)
cat("  4. ЛАТЕНТНЫЕ ОЦЕНКИ РАША (theta, WLE) -- описательная статистика\n")
cat(sep2, "\n")
cat(sprintf("  N = %d\n", length(th)))
cat(sprintf("  M = %.3f  SD = %.3f  SE = %.3f\n", mean(th), sd(th), sd(th) / sqrt(length(th))))
cat(sprintf("  Min = %.3f  P25 = %.3f  Mdn = %.3f  P75 = %.3f  Max = %.3f\n",
            min(th), q_th[1], q_th[2], q_th[3], max(th)))
cat(sprintf("  IQR = %.3f\n", q_th[3] - q_th[1]))
cat(sprintf("  Skewness = %.3f\n", psych::skew(th)))
cat(sprintf("  Kurtosis (excess) = %.3f\n", psych::kurtosi(th)))
# Структура связок печатается ПЕРЕД Shapiro-Wilk, потому что объясняет его: сырой
# балл — достаточная статистика модели Раша, поэтому тета принимает столько же
# различных значений, сколько балл, а респонденты с полным баллом делят одну
# крайнюю оценку. Отклонение от нормальности при такой точечной массе структурно,
# и читать его как свойство способности нельзя. Бимодальность самого балла
# измеряет шаг 2 (Bimodality/extreme_patterns.csv).
th_tie <- max(table(th))
cat(sprintf("  Различных значений теты: %d на %d респондентов\n", length(unique(th)), length(th)))
cat(sprintf("  Наибольшая связка: %d чел. на theta = %.4f\n", th_tie, as.numeric(names(which.max(table(th))))))
cat(sprintf("  Shapiro-Wilk: W = %.4f, p = %.4f (при точечной массе отклонение структурно)\n",
            sw$statistic, sw$p.value))
sink()

# Модель сохраняется для графиков (карта Райта и ICC)
save(mod_rasch, person_theta, rasch_report, file = "output/Rasch/rasch_model.RData")
