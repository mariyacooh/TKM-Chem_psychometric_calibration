# =======================================================================
#  ШАГ 6 — 2PL IRT-МОДЕЛЬ (mirt): сравнение с 1PL, Q3, бифакторная 2PL
# -----------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/2PL/2pl_params.csv           -> plot_2pl_curves, report.Rmd
#         output/2PL/2pl_theta.csv            -> plot_2pl_curves (карта Райта)
#         output/2PL/2pl_model_status.csv     -> report.Rmd (флаг оговорки 1PL/2PL)
#         output/2PL/2pl_excluded_items.csv   -> write_report_text.R (отсев нулевой
#                                                дисперсии; пишется ВСЕГДА, пустой =
#                                                ничего не снято)
#         output/2PL/1pl_2pl_comparison.csv   -> report.Rmd
#         output/2PL/2pl_tif_sem.csv          -> report.Rmd
#         output/2PL/2pl_q3_*.csv             -> report.Rmd
#         output/2PL/2pl_irt_results.txt      -> report.Rmd
#         output/2PL/2pl_bifactor_loadings.csv, 2pl_bifactor_indices.csv,
#         2pl_bifactor_subscale_omega.csv     -> УСЛОВНО, только при сходимости
#                                                bfactor(); потребитель —
#                                                plot_bifactor_loadings, report.Rmd
#
#  Сравнение с шагом 4 (Раш) идёт по ЗАНОВО оценённой в mirt 1PL, а не по артефакту
#  TAM: зависимости от шага 4 у этого шага нет.
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

load_pkgs(c("tidyverse", "mirt"))

select <- dplyr::select
filter <- dplyr::filter

OUT <- "output/2PL"
ensure_dir(OUT)

source("scripts/config.R")   # ITEMS, ITEM_SUB (единый источник)

require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read_csv("output/cleaned_responses.csv") %>%
  mutate(across(all_of(ITEMS), as.integer))
item_data <- df %>%
  select(all_of(ITEMS)) %>%
  as.matrix()

# Пункт с нулевой дисперсией в IRT не идентифицируется. zero_var_cols в 4_rasch.R
# отсекает такие пункты перед tam.mml(); тот же фильтр здесь, иначе один и тот же
# набор пунктов сосед переваривает, а этот шаг обрывает. Всё ниже (включая spec_idx
# бифактора) берёт колонки из item_data, поэтому фильтр расходится сам.
item_var <- apply(item_data, 2, var, na.rm = TRUE)
zero_var <- names(item_var)[is.na(item_var) | item_var == 0]
# Отсев пишется артефактом всегда, в том числе пустым (0 строк): «ничего не удалено»
# тоже результат. Консоль дублирует его диагностикой в stderr.
write_csv_excel(data.frame(
  Item = zero_var, Variance = unname(item_var[zero_var]),
  stringsAsFactors = FALSE, check.names = FALSE
), file.path(OUT, "2pl_excluded_items.csv"))
if (length(zero_var)) {
  message("[6] Пункты нулевой дисперсии исключены: ", paste(zero_var, collapse = ", "))
  item_data <- item_data[, setdiff(colnames(item_data), zero_var), drop = FALSE]
}

# Базовые модели проверяются так же строго, как бифакторная ниже (bf_converged):
# от них зависят LRT, все a/b в 2pl_params.csv, M2*, таблица TIF/SEM и EAP-теты —
# при несошедшейся оценке всё это ушло бы в артефакты без единой пометки.
fit_1d <- function(itemtype) tryCatch(
  mirt(item_data, 1, itemtype = itemtype),
  error = function(e) {
    message(sprintf("[6] mirt(%s) упал: %s", itemtype, conditionMessage(e))); NULL
  })
mod_1pl <- fit_1d("Rasch")
mod_2pl <- fit_1d("2PL")
if (is.null(mod_1pl) || is.null(mod_2pl))
  stop("6_2pl: базовая 1PL/2PL модель не оценена — дальнейшие расчёты невозможны.",
       call. = FALSE)
ok_1pl <- isTRUE(extract.mirt(mod_1pl, "converged"))
ok_2pl <- isTRUE(extract.mirt(mod_2pl, "converged"))
base_converged <- ok_1pl && ok_2pl
if (!base_converged)
  warning("Базовая 1PL/2PL модель не сошлась — LRT, a/b, M2*, TIF/SEM и теты ненадёжны.",
          call. = FALSE)

# Статус базовых моделей — машиночитаемо, как omega_matrix_status.csv в
# 3_efa_cfa.R (БЛОК 3). Прозаичная оговорка в 2pl_irt_results.txt отчёту
# недоступна: 2pl_params.csv, 1pl_2pl_comparison.csv, 2pl_tif_sem.csv и
# 2pl_theta.csv он выводит прямыми tbl(), и без этого артефакта единственный
# статус, который report.Rmd НЕ мог бы оговорить, — статус моделей, от которых
# зависят все a/b, LRT, M2*, TIF/SEM и EAP-теты.
write_csv_excel(
  data.frame(Model = c("1PL", "2PL"), Converged = c(ok_1pl, ok_2pl)),
  file.path(OUT, "2pl_model_status.csv"))

# Сама модель — на диск, как шаг 4 кладёт mod_rasch в Rasch/rasch_model.RData.
# Это ВХОД для рисунков: кривые ICC строит штатный mirt::itemplot по объекту
# модели, а не по a/b из CSV, — иначе рисунок пересчитывал бы модель у себя, а
# слой рисунков в этом проекте только транскрибирует артефакты. Числа шага от
# записи не меняются: объект уже оценён выше.
# Свод write_report_text.R файл не перечисляет: OMIT_PATTERNS покрывает
# "\\.RData$" как сохранённые объекты моделей.
save(mod_1pl, mod_2pl, file = file.path(OUT, "2pl_model.RData"))

lrt <- anova(mod_1pl, mod_2pl)

params_2pl <- coef(mod_2pl, IRTpars = TRUE, simplify = TRUE)$items %>%
  as.data.frame() %>%
  rownames_to_column("Item") %>%
  mutate(
    Subscale   = ITEM_SUB[Item],
    p_value    = colMeans(item_data, na.rm = TRUE)[Item]
  ) %>%
  mutate(across(c(a, b, g, u, p_value), ~ round(., 3)))

params_1pl <- coef(mod_1pl, IRTpars = TRUE, simplify = TRUE)$items %>%
  as.data.frame() %>%
  rownames_to_column("Item") %>%
  select(Item, b_1pl = b) %>%
  mutate(b_1pl = round(b_1pl, 3))

params_full <- left_join(params_2pl, params_1pl, by = "Item")

m2_1pl <- tryCatch(M2(mod_1pl, type = "M2*"), error = function(e) NULL)
m2_2pl <- tryCatch(M2(mod_2pl, type = "M2*"), error = function(e) NULL)

# Сохранение параметров 2PL, сравнения моделей и оценок Theta (EAP) для визуализации
write_csv_excel(params_full, file.path(OUT, "2pl_params.csv"))
write_csv_excel(as.data.frame(lrt) %>% rownames_to_column("Model"), file.path(OUT, "1pl_2pl_comparison.csv"))
# EAP-теты — обязательный артефакт: 2PL сошлась (проверено выше), значит fscores()
# обязан считаться. Ошибка здесь — сбой, а не повод пропустить запись: без
# 2pl_theta.csv карта Райта в plot_2pl_curves.R не строится.
theta_pers <- tryCatch(fscores(mod_2pl, method = "EAP")[, 1],
  error = function(e) stop(sprintf(
    "6_2pl: fscores(EAP) упал на сошедшейся 2PL: %s", conditionMessage(e)), call. = FALSE))
write_csv_excel(data.frame(ID = df$ID, Theta = theta_pers), file.path(OUT, "2pl_theta.csv"))

# ── TIF/SEM: числовые характеристики информационной функции теста ──────
# Считается по тем же округлённым параметрам (a, b) и той же сетке theta,
# что и график plots/plot_2pl_curves.R (2pl_test_info), поэтому цифры ниже
# в точности описывают то, что изображено на рисунке.
theta_grid <- seq(-4, 4, by = 0.05)
ti <- vapply(theta_grid, function(th) {
  p <- 1 / (1 + exp(-params_full$a * (th - params_full$b)))
  sum(params_full$a^2 * p * (1 - p))
}, numeric(1))
sem <- 1 / sqrt(ti)

i_max <- which.max(ti)              # SEM = 1/sqrt(TI), значит здесь же минимум SEM
i_0   <- which.min(abs(theta_grid)) # ближайшая к theta = 0 точка сетки

# Диапазон theta, где SEM не выше порога; rel = 1 - SEM^2 (дисперсия theta = 1):
# SEM <= 0.5 соответствует rel >= 0.75, SEM <= 0.316 — rel >= 0.90
#
# Доля выборки считается по EAP-тетам, и в названии метрики это названо: EAP
# стянут к априорному распределению, поэтому на краях шкалы оценки сжимаются к
# центру и попадают в диапазон, где информации теста на самом деле мало. Доля
# описывает СЖАТИЕ оценок, а не покрытие выборки тестом; максимум EAP приведён
# рядом, чтобы сжатие было видно числом. Разброс самого балла измеряет шаг 2
# (Bimodality/extreme_patterns.csv).
sem_range <- function(threshold) {
  idx <- which(sem <= threshold)
  if (length(idx) == 0) c(NA_real_, NA_real_) else range(theta_grid[idx])
}
r75 <- sem_range(sqrt(1 - 0.75))
r90 <- sem_range(sqrt(1 - 0.90))

share_in <- function(rng) {
  if (is.null(theta_pers) || anyNA(rng)) return(NA_real_)
  mean(theta_pers >= rng[1] & theta_pers <= rng[2]) * 100
}
p75 <- share_in(r75)
p90 <- share_in(r90)

tif_summary <- tibble(
  Metric = c(
    "TI максимум",
    "theta в максимуме TI",
    "SEM минимум (в максимуме TI)",
    "TI при theta = 0",
    "SEM при theta = 0",
    "rel = 1 - SEM^2 при theta = 0",
    "theta от (SEM <= 0.50, rel >= 0.75)",
    "theta до (SEM <= 0.50, rel >= 0.75)",
    "% EAP-тет в диапазоне rel >= 0.75",
    "theta от (SEM <= 0.32, rel >= 0.90)",
    "theta до (SEM <= 0.32, rel >= 0.90)",
    "% EAP-тет в диапазоне rel >= 0.90",
    "EAP-тета минимум",
    "EAP-тета максимум"
  ),
  Value = round(c(
    ti[i_max], theta_grid[i_max], sem[i_max],
    ti[i_0], sem[i_0], 1 - sem[i_0]^2,
    r75[1], r75[2], p75,
    r90[1], r90[2], p90,
    if (is.null(theta_pers)) NA_real_ else min(theta_pers),
    if (is.null(theta_pers)) NA_real_ else max(theta_pers)
  ), 3)
)
write_csv_excel(tif_summary, file.path(OUT, "2pl_tif_sem.csv"))

# Запись отчета без качественных суждений и вербального шаблона
SEP <- strrep("=", 72)
sink(file.path(OUT, "2pl_irt_results.txt"))
cat(SEP, "\n")
cat(sprintf(" 2PL IRT-МОДЕЛЬ — ТКМ-Хим  n=%d | пунктов=%d\n", nrow(item_data), ncol(item_data)))
cat(SEP, "\n\n")

if (length(zero_var))
  cat("!! Пункты нулевой дисперсии исключены из оценки: ",
      paste(zero_var, collapse = ", "), "\n\n", sep = "")
if (!base_converged)
  cat("!! ОГОВОРКА: базовая 1PL и/или 2PL модель НЕ сошлась. Параметры, LRT, M2*,\n",
      "   TIF/SEM и теты ниже получены из несошедшейся оценки и ненадёжны.\n\n", sep = "")

cat("── СРАВНЕНИЕ МОДЕЛЕЙ: 1PL vs 2PL (LRT) ──────────────────────────────\n\n")
print(lrt)

if (!is.null(m2_1pl) && !is.null(m2_2pl)) {
  fmt_m2 <- function(m, label) {
    cat(sprintf(
      " %s: M2=%.2f df=%d p=%.4f RMSEA=%.3f[%.3f;%.3f] TLI=%.3f CFI=%.3f\n",
      label, m$M2, m$df, m$p, m$RMSEA, m$RMSEA_5, m$RMSEA_95, m$TLI, m$CFI
    ))
  }
  cat("\nАбсолютный фит (M2*):\n")
  fmt_m2(m2_1pl, "1PL")
  fmt_m2(m2_2pl, "2PL")
}

cat("\n\n── ПАРАМЕТРЫ 2PL ─────────────────────────────────────────────────────\n\n")
cat(sprintf(" %-6s %-4s %7s %7s %7s %7s\n", "Item", "Sub", "p-val", "a(disc)", "b(diff)", "b_1pl"))
cat(strrep("-", 45), "\n")
for (i in seq_len(nrow(params_full))) {
  r <- params_full[i, ]
  cat(sprintf(" %-6s %-4s %7.3f %7.3f %7.3f %7.3f\n", r$Item, r$Subscale, r$p_value, r$a, r$b, r$b_1pl))
}

cat(sprintf(
  "\n Дискриминация (a):  M=%.3f  SD=%.3f  min=%.3f  max=%.3f\n",
  mean(params_full$a), sd(params_full$a), min(params_full$a), max(params_full$a)
))
cat(sprintf(
  " Трудность     (b):  M=%.3f  SD=%.3f  min=%.3f  max=%.3f\n",
  mean(params_full$b), sd(params_full$b), min(params_full$b), max(params_full$b)
))

cat("\n\n── TIF / SEM (цифры к графику 2pl_test_info) ────────────────────────\n\n")
cat(sprintf(
  " Максимум информации:  TI=%.2f при theta=%+.2f (там же минимум SEM=%.3f)\n",
  ti[i_max], theta_grid[i_max], sem[i_max]
))
cat(sprintf(
  " При theta=0:          TI=%.2f, SEM=%.3f (rel = 1 - SEM^2 = %.3f)\n",
  ti[i_0], sem[i_0], 1 - sem[i_0]^2
))
if (!anyNA(r75)) {
  cat(sprintf(
    " SEM<=0.50 (rel>=.75): theta в [%+.2f; %+.2f]%s\n", r75[1], r75[2],
    if (!is.na(p75)) sprintf(" — %.1f%% EAP-тет", p75) else ""
  ))
} else {
  cat(" SEM<=0.50 (rel>=.75): не достигается ни при одном theta в [-4; 4]\n")
}
if (!anyNA(r90)) {
  cat(sprintf(
    " SEM<=0.32 (rel>=.90): theta в [%+.2f; %+.2f]%s\n", r90[1], r90[2],
    if (!is.na(p90)) sprintf(" — %.1f%% EAP-тет", p90) else ""
  ))
} else {
  cat(" SEM<=0.32 (rel>=.90): не достигается ни при одном theta в [-4; 4]\n")
}
if (!is.null(theta_pers)) {
  cat(sprintf(" Диапазон EAP-тет: [%+.2f; %+.2f]. Доли выше — по НИМ, поэтому они\n",
              min(theta_pers), max(theta_pers)))
  cat(" описывают сжатие оценок к априорному распределению, а не покрытие выборки тестом.\n")
}
sink()

# =======================================================================
#  YEN'S Q3: остаточные корреляции (проверка локальной независимости)
# =======================================================================
# Q3 (Yen, 1984) — корреляция остатков (наблюдаемый - ожидаемый ответ) по парам
# пунктов после вычитания влияния латентной черты. При локальной независимости
# ожидаемое среднее Q3 ~ -1/(k-1), а не 0. Пары флагуются по относительному
# критерию Christensen et al. (2017): Q3 > M(Q3) + 0.2. Полная матрица, все пары
# с центрированным Q3* и сводка пишутся в CSV И в общий отчёт 2pl_irt_results.txt.
q3_mat <- tryCatch(residuals(mod_2pl, type = "Q3"),
  error = function(e) { message("!! Ошибка Q3: ", conditionMessage(e)); NULL })

if (!is.null(q3_mat)) {
  diag(q3_mat) <- NA
  q3_vals <- q3_mat[upper.tri(q3_mat)]
  q3_mean <- mean(q3_vals, na.rm = TRUE)
  q3_sd   <- sd(q3_vals, na.rm = TRUE)
  q3_exp  <- -1 / (ncol(item_data) - 1)        # ожидаемое среднее при лок. независимости
  q3_crit <- q3_mean + 0.2                      # относительный порог Christensen (2017)

  q3_idx <- which(upper.tri(q3_mat), arr.ind = TRUE)
  q3_pairs <- tibble(
    Item_A   = rownames(q3_mat)[q3_idx[, "row"]],
    Item_B   = colnames(q3_mat)[q3_idx[, "col"]],
    Sub_A    = ITEM_SUB[rownames(q3_mat)[q3_idx[, "row"]]],
    Sub_B    = ITEM_SUB[colnames(q3_mat)[q3_idx[, "col"]]],
    Q3       = q3_mat[q3_idx],
    Q3_adj   = Q3 - q3_mean,                     # центрированный Q3*
    Same_sub = Sub_A == Sub_B,
    Flag     = Q3 > q3_crit
  ) %>%
    arrange(desc(Q3)) %>%
    mutate(across(c(Q3, Q3_adj), ~ round(., 3)))

  write_csv_excel(as.data.frame(round(q3_mat, 3)) %>% rownames_to_column("Item"),
            file.path(OUT, "2pl_q3_matrix.csv"))
  write_csv_excel(q3_pairs, file.path(OUT, "2pl_q3_pairs.csv"))

  flagged <- q3_pairs %>% filter(Flag)
  write_csv_excel(tibble(
    Metric = c("Mean_Q3", "Expected_Q3", "SD_Q3", "Max_Q3",
               "Crit_rel_M+0.2", "N_flagged", "N_pairs"),
    Value  = round(c(q3_mean, q3_exp, q3_sd, max(q3_pairs$Q3),
                     q3_crit, nrow(flagged), nrow(q3_pairs)), 3)
  ), file.path(OUT, "2pl_q3_summary.csv"))

  # Дописывание в общий текстовый отчёт
  sink(file.path(OUT, "2pl_irt_results.txt"), append = TRUE)
  cat("\n\n", SEP, "\n", sep = "")
  cat(" YEN'S Q3 — ОСТАТОЧНЫЕ КОРРЕЛЯЦИИ (локальная независимость)\n")
  cat(SEP, "\n\n")
  cat(sprintf(" M(Q3) = %.3f  (ожидаемое ~%.3f)   SD = %.3f   max = %.3f\n",
              q3_mean, q3_exp, q3_sd, max(q3_pairs$Q3)))
  cat(sprintf(" Порог Christensen (2017): Q3 > M+0.2 = %.3f\n", q3_crit))
  cat(sprintf(" Пар выше порога: %d из %d\n\n", nrow(flagged), nrow(q3_pairs)))
  if (nrow(flagged) > 0) {
    cat(sprintf(" %-6s %-6s %8s %8s %-5s %-5s %s\n",
                "Item_A", "Item_B", "Q3", "Q3*", "SubA", "SubB", "Same?"))
    cat(strrep("-", 54), "\n")
    for (i in seq_len(nrow(flagged))) {
      fr <- flagged[i, ]
      cat(sprintf(" %-6s %-6s %+8.3f %+8.3f %-5s %-5s %s\n",
                  fr$Item_A, fr$Item_B, fr$Q3, fr$Q3_adj, fr$Sub_A, fr$Sub_B,
                  if (fr$Same_sub) "da" else "-"))
    }
  } else {
    cat(" Нет пар выше относительного порога — локальная независимость не нарушена.\n")
  }
  cat("\n Топ-10 пар по Q3:\n")
  for (i in seq_len(min(10, nrow(q3_pairs)))) {
    tr <- q3_pairs[i, ]
    cat(sprintf("   %-6s %-6s  Q3=%+.3f  Q3*=%+.3f  %s\n",
                tr$Item_A, tr$Item_B, tr$Q3, tr$Q3_adj,
                if (tr$Same_sub) tr$Sub_A else paste0(tr$Sub_A, "/", tr$Sub_B)))
  }
  sink()
} else {
  message("!! Q3 не рассчитан (ошибка mirt::residuals).")
}


# =======================================================================
#  БИФАКТОРНАЯ 2PL (IRT): G + специфические факторы по субшкалам
# =======================================================================
spec_idx <- match(ITEM_SUB[colnames(item_data)], names(SUBSCALES))
mod_bf <- tryCatch(bfactor(item_data, model = spec_idx, technical = list(NCYCLES = 2000)),
  error = function(e) {
    message("!! Ошибка bfactor: ", conditionMessage(e))
    NULL
  }
)

bf_converged <- !is.null(mod_bf) && isTRUE(extract.mirt(mod_bf, "converged"))

if (bf_converged) {
  # Сравнение: одномерная 2PL vs бифакторная 2PL (вложенные модели)
  cmp_bf <- anova(mod_2pl, mod_bf)

  # Стандартизованные факторные нагрузки (G, S1..S3) и коммунальности
  sm <- summary(mod_bf)
  F <- as.matrix(sm$rotF)
  h2 <- as.numeric(sm$h2)
  u2 <- 1 - h2

  lam_g <- F[, 1]
  F_spec <- F[, -1, drop = FALSE]
  # каждая строка имеет ровно одну ненулевую специфическую нагрузку
  lam_s <- apply(F_spec, 1, function(r) r[which.max(abs(r))])
  s_name <- colnames(F_spec)[apply(abs(F_spec), 1, which.max)]

  # Экспорт нагрузок в формате для графика (Factor, Item, Beta)
  bf_loads <- bind_rows(
    tibble(Factor = "G", Item = rownames(F), Beta = round(lam_g, 3)),
    tibble(Factor = s_name, Item = rownames(F), Beta = round(lam_s, 3))
  )
  write_csv_excel(bf_loads, file.path(OUT, "2pl_bifactor_loadings.csv"))

  # Бифакторные индексы из IRT-нагрузок (Reise): ωH, ωt, ECV, PUC
  sum_g <- sum(lam_g)
  sum_s_sq <- sum(vapply(
    colnames(F_spec),
    function(s) sum(lam_s[s_name == s])^2, numeric(1)
  ))
  denom <- sum_g^2 + sum_s_sq + sum(u2)
  omega_h <- sum_g^2 / denom
  omega_t <- (sum_g^2 + sum_s_sq) / denom
  ecv <- sum(lam_g^2) / sum(lam_g^2 + lam_s^2)
  n_items <- nrow(F)
  n_within <- sum(vapply(unique(s_name), function(s) choose(sum(s_name == s), 2), numeric(1)))
  puc <- 1 - n_within / choose(n_items, 2)

  bf_indices <- tibble(
    Index = c("omega_h", "omega_total", "ECV", "PUC"),
    Value = round(c(omega_h, omega_t, ecv, puc), 3)
  )
  write_csv_excel(bf_indices, file.path(OUT, "2pl_bifactor_indices.csv"))

  # ω по субшкалам (Rodriguez/Reise/Haviland — та же логика, что и общая ω_h):
  #   omega_s  — полная надёжность субшкального балла (общий + специфический факторы),
  #   omega_hs — доля, приходящаяся ТОЛЬКО на специфический фактор S_k (за вычетом G).
  sub_omega <- lapply(sort(unique(s_name)), function(k) {
    in_k    <- s_name == k
    g_k     <- sum(lam_g[in_k])^2
    s_k     <- sum(lam_s[in_k])^2
    denom_k <- g_k + s_k + sum(u2[in_k])
    tibble(Subscale = k, n_items = sum(in_k),
           omega_s  = round((g_k + s_k) / denom_k, 3),
           omega_hs = round(s_k / denom_k, 3))
  }) %>% bind_rows()
  write_csv_excel(sub_omega, file.path(OUT, "2pl_bifactor_subscale_omega.csv"))

  # Построчные нагрузки для txt (широкий формат уже в 2pl_bifactor_loadings.csv):
  # λ на общий фактор G, λ на свой специфический S_k и коммунальность h².
  load_tab <- tibble(Item = rownames(F), Sub = s_name,
                     lam_G = round(lam_g, 3), lam_S = round(lam_s, 3),
                     h2 = round(h2, 3))

  # Дописывание в отчёт
  sink(file.path(OUT, "2pl_irt_results.txt"), append = TRUE)
  cat("\n\n", SEP, "\n", sep = "")
  cat(" БИФАКТОРНАЯ 2PL (IRT): G + S1/S2/S3\n")
  cat(SEP, "\n\n")
  cat("── СРАВНЕНИЕ: одномерная 2PL vs бифакторная 2PL ─────────────────────\n\n")
  print(cmp_bf)
  cat("\n── БИФАКТОРНЫЕ ИНДЕКСЫ (из IRT-нагрузок) ────────────────────────────\n")
  cat(sprintf("  omega_h (иерархическая) = %.3f\n", omega_h))
  cat(sprintf("  omega_total             = %.3f\n", omega_t))
  cat(sprintf("  ECV (объяснённая G)     = %.3f\n", ecv))
  cat(sprintf("  PUC                     = %.3f\n", puc))
  cat("\n── ω ПО СУБШКАЛАМ (omega_s полная / omega_hs за вычетом G) ───────────\n")
  cat(sprintf("  %-4s %6s %10s %10s\n", "Sub", "k", "omega_s", "omega_hs"))
  cat("  ", strrep("-", 32), "\n", sep = "")
  for (i in seq_len(nrow(sub_omega))) {
    r <- sub_omega[i, ]
    cat(sprintf("  %-4s %6d %10.3f %10.3f\n", r$Subscale, r$n_items, r$omega_s, r$omega_hs))
  }
  cat("\n── НАГРУЗКИ БИФАКТОРНОЙ 2PL (lam_G, lam_S по своему S_k, h2) ─────────\n")
  cat(sprintf("  %-6s %-4s %9s %9s %8s\n", "Item", "Sub", "lam_G", "lam_S", "h2"))
  cat("  ", strrep("-", 42), "\n", sep = "")
  for (i in seq_len(nrow(load_tab))) {
    r <- load_tab[i, ]
    cat(sprintf("  %-6s %-4s %9.3f %9.3f %8.3f\n", r$Item, r$Sub, r$lam_G, r$lam_S, r$h2))
  }
  sink()

} else {
  # Модель не сошлась — нагрузки/индексы не пишутся вовсе (файлов нет: output/
  # стартует пустым, чистая сборка в run_all.R).
  # Факт несходимости честно фиксируется в отчётном логе (его показывает report.Rmd
  # через log_block). Отсутствие индексов/нагрузок — не «забыли запустить скрипт»,
  # а результат: на этих данных специфические факторы не идентифицируются.
  sink(file.path(OUT, "2pl_irt_results.txt"), append = TRUE)
  cat("\n\n", SEP, "\n", sep = "")
  cat(" БИФАКТОРНАЯ 2PL (IRT): G + S1/S2/S3\n")
  cat(SEP, "\n\n")
  cat("Бифакторная 2PL НЕ СОШЛАСЬ на текущем наборе пунктов — нагрузки и индексы\n")
  cat("(omega_h, omega_total, ECV, PUC) не рассчитаны и намеренно не экспортированы.\n")
  cat("Вероятная причина: доминирующий общий фактор при почти нулевой специфической\n")
  cat("дисперсии субшкал (эмпирическая недоопределённость специфических факторов).\n")
  sink()
  message("!! Бифакторная 2PL не сошлась — нагрузки/индексы не экспортированы.")
}
