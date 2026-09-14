# =============================================================================
#  ШАГ 2 — БИМОДАЛЬНОСТЬ ВЫБОРКИ (крайние паттерны, чувствительность надёжности)
#    БЛОК 1 — распределение Score_Total, крайние паттерны, флаг
#    БЛОК 2 — надёжность и CFA на подвыборках (усечение краёв)
#    БЛОК 3 — градиент по границам среза
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/Bimodality/extreme_patterns.csv       -> report.Rmd (ветка оговорки)
#         output/Bimodality/score_total_frequency.csv  -> plot_score_distribution, report.Rmd
#         output/Bimodality/subsample_sensitivity.csv  -> report.Rmd
#         output/Bimodality/subsample_known_groups.csv -> report.Rmd
#         output/Bimodality/cut_sensitivity.csv        -> plot_bimodality_sensitivity, report.Rmd
#         output/Bimodality/cut_monotonicity.csv       -> plot_bimodality_sensitivity, report.Rmd
#                                                        (монотонность и охват как измерение)
#         output/Bimodality/bimodality_report.txt      -> report.Rmd
#
#  Шаг стоит ПЕРЕД моделями (шаги 3, 4, 6), хотя зависит только от шага 0:
#  alpha и omega растут с разбросом выборки, поэтому флаг бимодальности определяет,
#  как читаются их значения. Обоснование позиции — scripts/_artifacts.R.
#
#  Границы подвыборок ИЛЛЮСТРАТИВНЫ, а не установлены: естественного порога нет
#  (БЛОК 3), и результат — сам градиент, а не значение на одной выбранной границе.
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# =============================================================================

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

load_pkgs(c("psych", "GPArotation", "lavaan", "mirt", "TAM", "diptest"))

OUT <- "output/Bimodality"
ensure_dir(OUT)

# ── Структура субшкал (исключённые пункты убраны в config.R / input/items.csv) ──
source("scripts/config.R")   # ITEMS, SUBSCALES (единый источник)

require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read.csv("output/cleaned_responses.csv", fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE, check.names = FALSE)

X <- as.matrix(df[, ITEMS])
s <- df$Score_Total
k <- length(ITEMS)
n <- nrow(X)

# Нижняя граница «пола». Обоснование внешнее, а не подгонка под данные: при
# k пунктах с 4 вариантами чистое угадывание — Binom(k, 1/4), и на действующих
# 26 пунктах 95 % угадывающих набирают <= 10 (qbinom печатается в отчёте шага).
# Порог 8 консервативен: 9 и 10 остаются в середине, хотя ещё могут быть
# угадыванием. У верхней границы внешнего обоснования нет, поэтому «потолок»
# определён симметрично составу теста (k - 2), а не подобранным числом.
FLOOR_CUT <- 8L
CEIL_CUT  <- k - 2L

# =============================================================================
#  БЛОК 1 — распределение и крайние паттерны
# =============================================================================
# Моменты — смещённые оценки g1 и g2 (excess), теми же формулами, что у
# skew_biased()/kurt_excess() в 1_descriptive_stats.R: строка Score_Total в
# descriptive_stats_detailed.csv и коэффициент бимодальности ниже обязаны опираться
# на одно определение момента.
skew_biased <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); m3 <- mean((x - m)^3); m3 / m2^1.5 }
kurt_excess <- function(x) { x <- x[!is.na(x)]; m <- mean(x); m2 <- mean((x - m)^2); m4 <- mean((x - m)^4); m4 / m2^2 - 3 }

g1 <- skew_biased(s)
g2 <- kurt_excess(s)

# Коэффициент бимодальности BC = (g1^2 + 1) / (g2 + 3(n-1)^2 / ((n-2)(n-3))).
# Порогонезависим (границы пола и потолка в него не входят). Ссылочный уровень
# 5/9: значение равномерного распределения, выше него — признак бимодальности.
# BC чувствителен к платикуртозу, поэтому рядом считается тест провала Хартигана,
# который проверяет унимодальность напрямую, а не через моменты.
BC_REF <- 5 / 9
bc  <- (g1^2 + 1) / (g2 + 3 * (n - 1)^2 / ((n - 2) * (n - 3)))
dip <- diptest::dip.test(s)

extreme <- data.frame(
  N              = n,
  N_perfect      = sum(s == k),
  N_zero         = sum(s == 0),
  N_score_levels = length(unique(s)),
  Ceiling_cut    = CEIL_CUT,
  Floor_cut      = FLOOR_CUT,
  Pct_ceiling    = round(100 * mean(s >= CEIL_CUT), 1),
  Pct_floor      = round(100 * mean(s <= FLOOR_CUT), 1),
  Skewness       = round(g1, 3),
  Kurtosis       = round(g2, 3),
  BC             = round(bc, 4),
  BC_ref         = round(BC_REF, 4),
  Dip            = round(unname(dip$statistic), 5),
  Dip_p          = round(dip$p.value, 4),
  Bimodal        = bc > BC_REF,
  stringsAsFactors = FALSE
)
# Артефакт пишется ДО тяжёлых блоков: флаг, по которому ветвится оговорка отчёта,
# не должен теряться из-за несходимости модели на какой-нибудь подвыборке.
write_csv_excel(extreme, file.path(OUT, "extreme_patterns.csv"))

# Сетка баллов полная (0..k), а не только наблюдённая: форма таблицы не зависит
# от выборки, и пустой нижний хвост виден как нули, а не как отсутствующие строки.
grid   <- 0:k
cnt    <- as.integer(table(factor(s, levels = grid)))
freq   <- data.frame(
  Score   = grid,
  N       = cnt,
  Pct     = round(100 * cnt / n, 1),
  Cum_N   = cumsum(cnt),
  Cum_Pct = round(100 * cumsum(cnt) / n, 1),
  stringsAsFactors = FALSE
)
write_csv_excel(freq, file.path(OUT, "score_total_frequency.csv"))

if (isTRUE(extreme$Bimodal)) {
  message(sprintf("!! [2] Score_Total бимодален: BC = %.3f > %.3f, пол %.1f%%, потолок %.1f%%.",
                  bc, BC_REF, extreme$Pct_floor, extreme$Pct_ceiling))
}

# =============================================================================
#  БЛОК 2 — надёжность и CFA по подвыборкам
# =============================================================================
# Коэффициент шкалируемости Ловингера H по всем парам пунктов:
#   H = sum(Cov_ij) / sum(Cov_max_ij),  Cov_max_ij = min(p_i, p_j) - p_i*p_j,
# что тождественно классическому H = 1 - sum(F_ij)/sum(E_ij) при ошибке Гуттмана
# F_ij, считаемой ТОЛЬКО в направлении «сдан трудный, не сдан лёгкий».
# Нормировка идёт на максимально возможную при данных маргиналах ковариацию, а не
# на сумму ОБОИХ дискордантных ожиданий: у второй верхняя граница не равна единице
# и зависит от разброса трудностей (на идеальной шкале Гуттмана p = .8/.6/.4/.2
# она даёт 0.375 вместо 1).
loevinger_h <- function(M) {
  pp <- colMeans(M); nn <- nrow(M); num <- 0; den <- 0
  for (i in seq_len(ncol(M) - 1)) for (j in (i + 1):ncol(M)) {
    pij <- sum(M[, i] == 1 & M[, j] == 1) / nn
    num <- num + (pij - pp[i] * pp[j])
    den <- den + (min(pp[i], pp[j]) - pp[i] * pp[j])
  }
  unname(num / den)
}

# Гейт сходимости 2PL — тот же стандарт, что fit_scaled() ниже и ok_1pl/ok_2pl в
# 6_2pl.R: mirt() при исчерпании итераций не падает, а возвращает объект, поэтому
# tryCatch(error =) ловит только ЖЁСТКУЮ ошибку, а несошедшийся рефит ушёл бы
# в cut_sensitivity.csv, в свод и точкой на линию «mean 2PL a» наравне с
# сошедшимися. Статус трёхзначен: TRUE — сошлась, FALSE — объект есть, сходимости
# нет, NA — оценки нет вовсе. Число публикуется только при TRUE, иначе NA: снятая
# точка уменьшает число ОЦЕНЁННЫХ точек развёртки, а не участвует в выводе.
a_mean_2pl <- function(M, tag) {
  m  <- tryCatch(mirt::mirt(M, 1, itemtype = "2PL"), error = function(e) NULL)
  ok <- if (is.null(m)) NA else isTRUE(mirt::extract.mirt(m, "converged"))
  if (isFALSE(ok))
    message(sprintf("!! [2] 2PL не сошлась на подвыборке %s (n = %d) -- a_mean снят.",
                    tag, nrow(M)))
  list(conv = ok,
       a_mean = if (isTRUE(ok))
                  mean(coef(m, IRTpars = TRUE, simplify = TRUE)$items[, "a"], na.rm = TRUE)
                else NA_real_)
}

# Надёжность WLE со стороны Раша: тот же 1PL, что калибрует шаг 4, но на усечённой
# подвыборке. Артефакт шага 4 здесь не читается — ребро «анализ -> анализ» осталось
# бы единственным (шаг 4 -> шаг 5), а тета полной выборки к подвыборке всё равно не
# относится. Предикат сходимости — тот же трёхзначный, что rasch_converged в
# 4_rasch.R: tam.mml() при исчерпании итераций тоже возвращает объект, а имя поля
# статуса между версиями TAM разнится, поэтому сначала явный флаг, иначе упор в
# предел итераций, иначе NA.
tam_converged <- function(m) {
  flag <- m$converged
  if (!is.null(flag)) return(isTRUE(flag))
  it <- m$iter; mx <- m$control$maxiter
  if (is.null(it) || is.null(mx)) return(NA)
  it < mx
}
wle_rel <- function(M, tag) {
  mml <- tryCatch(TAM::tam.mml(M, irtmodel = "1PL"), error = function(e) NULL)
  ok  <- if (is.null(mml)) NA else tam_converged(mml)
  if (isFALSE(ok))
    message(sprintf("!! [2] 1PL не сошлась на подвыборке %s (n = %d) -- WLE_rel снята.",
                    tag, nrow(M)))
  list(conv = ok,
       wle = if (isTRUE(ok)) tryCatch(TAM::tam.wle(mml)$WLE.rel[[1]],
                                      error = function(e) NA_real_)
             else NA_real_)
}

# Базовые показатели подвыборки. omega_h считается, но НЕ интерпретируется:
# БЛОК 3 показывает, что на этих объёмах он скачет без закономерности. Колонка
# остаётся именно как свидетельство этой нестабильности; оговорка — в манифесте
# scripts/write_report_text.R и в тексте отчёта.
subset_stats <- function(i, tag) {
  M <- X[i, , drop = FALSE]
  # Предикат NA-безопасен, как zero_var_cols в 4_rasch.R: var() на одном наблюдении
  # (или на полностью пропущенном столбце) даёт NA, а NA в логическом индексе столбец
  # не отбрасывает, а ПОДСТАВЛЯЕТ столбец из NA. Матрица при этом не падает — alpha,
  # tetrachoric, omega и mirt обёрнуты в tryCatch, — и подвыборка ушла бы в
  # cut_sensitivity.csv строкой пропусков без диагностируемой причины.
  v <- apply(M, 2, var)
  M <- M[, !is.na(v) & v > 0, drop = FALSE]
  a_raw <- tryCatch(psych::alpha(M)$total$raw_alpha, error = function(e) NA_real_)
  tet <- tryCatch(psych::tetrachoric(M, smooth = FALSE)$rho, error = function(e) NULL)
  smoothed <- if (is.null(tet)) NA
              else min(eigen(tet, symmetric = TRUE, only.values = TRUE)$values) <= .Machine$double.eps
  rho <- if (!is.null(tet) && isTRUE(smoothed)) psych::cor.smooth(tet) else tet
  a_ord <- tryCatch(psych::alpha(rho)$total$raw_alpha, error = function(e) NA_real_)
  om <- tryCatch(psych::omega(rho, nfactors = 3, plot = FALSE, rotate = "oblimin"),
        error = function(e) NULL)
  a2 <- a_mean_2pl(M, tag)
  list(M = M, n = sum(i), alpha = a_raw, alpha_ord = a_ord,
       omega_t = if (!is.null(om)) om$omega.tot else NA_real_,
       omega_h = if (!is.null(om)) om$omega_h else NA_real_,
       H = loevinger_h(M), smoothed = smoothed,
       a_mean = a2$a_mean, conv_2pl = a2$conv)
}

# WLSMV = DWLS + scaled.shifted: наивные chisq/cfi/tli/rmsea при этом тесте не
# интерпретируются, а наивный pvalue не определён вовсе, поэтому берутся
# scaled-варианты — тот же набор ключей, что FI_KEYS в 3_efa_cfa.R.
FI <- c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
fit_scaled <- function(syntax, dat) {
  f <- tryCatch(lavaan::cfa(syntax, data = dat, estimator = "WLSMV", ordered = TRUE),
       error = function(e) NULL)
  if (is.null(f) || !isTRUE(lavaan::lavInspect(f, "converged"))) return(NULL)
  lavaan::fitMeasures(f, FI)
}
pick <- function(x, key) if (is.null(x)) NA_real_ else round(unname(x[[key]]), 3)

SAMPLES <- list(
  "FULL (all)"      = rep(TRUE, n),
  "NO-FLOOR (>8)"   = s > FLOOR_CUT,
  "NO-TAILS (9-25)" = s > FLOOR_CUT & s < k,
  "MID only (9-23)" = s > FLOOR_CUT & s < CEIL_CUT
)

sens <- do.call(rbind, lapply(names(SAMPLES), function(nm) {
  st    <- subset_stats(SAMPLES[[nm]], nm)
  items <- colnames(st$M)
  dat   <- as.data.frame(st$M)
  f_1f <- fit_scaled(paste("G =~", paste(items, collapse = " + ")), dat)
  ga <- lapply(SUBSCALES, function(v) intersect(v, items))
  ga <- ga[vapply(ga, length, integer(1)) >= 2]
  f_3f <- fit_scaled(paste(vapply(names(ga), function(g)
          paste0(g, " =~ ", paste(ga[[g]], collapse = " + ")), character(1)), collapse = "\n"), dat)
  wl <- wle_rel(st$M, nm)
  if (is.null(f_1f)) message(sprintf("!! [2] Однофакторная CFA не сошлась на подвыборке %s.", nm))
  if (is.null(f_3f)) message(sprintf("!! [2] 3-факторная CFA не сошлась на подвыборке %s.", nm))
  data.frame(Sample = nm, n = st$n,
             alpha = round(st$alpha, 3), alpha_ord = round(st$alpha_ord, 3),
             omega_t = round(st$omega_t, 3), omega_h = round(st$omega_h, 3),
             H = round(st$H, 3),
             WLE_rel = round(wl$wle, 3), Conv_WLE = wl$conv,
             a_mean = round(st$a_mean, 2), Conv_2PL = st$conv_2pl,
             Smoothed = st$smoothed,
             CFA_1F_cfi = pick(f_1f, "cfi.scaled"), CFA_1F_rmsea = pick(f_1f, "rmsea.scaled"),
             CFA_3F_cfi = pick(f_3f, "cfi.scaled"), CFA_3F_rmsea = pick(f_3f, "rmsea.scaled"),
             stringsAsFactors = FALSE)
}))
write_csv_excel(sens, file.path(OUT, "subsample_sensitivity.csv"))

# Валидность по известным группам на тех же подвыборках: проверка того, что
# отсутствие значимости (ISSUES.md 1.4) не создано усечением и не лечится им.
known <- do.call(rbind, lapply(names(SAMPLES), function(nm) {
  i <- SAMPLES[[nm]]; x <- df[i, , drop = FALSE]; y <- s[i]
  safe_p <- function(e) tryCatch(e, error = function(z) NA_real_)
  cr <- tryCatch(cor.test(x$GPA_ordinal, y, method = "spearman", exact = FALSE),
        error = function(z) NULL)
  data.frame(Sample = nm, n = sum(i),
             Course_KW    = round(safe_p(kruskal.test(y ~ factor(x$Course))$p.value), 3),
             Conference_t = round(safe_p(t.test(y ~ factor(x$Conference))$p.value), 3),
             Language_t   = round(safe_p(t.test(y ~ factor(x$Language))$p.value), 3),
             GPA_rho      = if (is.null(cr)) NA_real_ else round(unname(cr$estimate), 3),
             GPA_p        = if (is.null(cr)) NA_real_ else round(cr$p.value, 3),
             stringsAsFactors = FALSE)
}))
write_csv_excel(known, file.path(OUT, "subsample_known_groups.csv"))

# =============================================================================
#  БЛОК 3 — градиент по границам среза
# =============================================================================
# Естественного порога нет: показатели меняются без скачка, который выделил бы одну
# границу, поэтому значение на любой ОДНОЙ границе — артефакт её выбора, а не
# измерение. Монотонность НЕ предполагается: она измеряется по таблице cuts и уходит
# артефактом cut_monotonicity.csv. Результат этого блока — сам градиент, и он же вход
# оговорки об alpha/omega.
sweep_row <- function(sweep, cut, i) {
  st <- subset_stats(i, sprintf("%s Cut=%s", sweep, cut))
  data.frame(Sweep = sweep, Cut = cut, n = st$n,
             alpha = round(st$alpha, 3), omega_h = round(st$omega_h, 3),
             H = round(st$H, 3), a_mean = round(st$a_mean, 2),
             Conv_2PL = st$conv_2pl,
             stringsAsFactors = FALSE)
}
cuts <- rbind(
  do.call(rbind, lapply(6:12, function(lo) sweep_row("lower", lo, s > lo & s < CEIL_CUT))),
  do.call(rbind, lapply(22:k, function(hi) sweep_row("upper", hi, s > FLOOR_CUT & s < hi))),
  do.call(rbind, lapply(0:5,  function(t)  sweep_row("symmetric", t, s > (6 + t) & s < (k - t))))
)
write_csv_excel(cuts, file.path(OUT, "cut_sensitivity.csv"))

# Монотонность — свойство КОНКРЕТНОГО прогона, поэтому она машиночитаемый выход того
# же класса, что Bimodal в extreme_patterns.csv и Smoothed в omega_matrix_status.csv:
# подпись рисунка cut_sensitivity и проза report.Rmd ветвятся по этому артефакту, а
# не объявляют монотонность литералом. Расхождение литерала с напечатанным градиентом
# читатель получил бы как измеренный факт.
# Монотонность определена только при двух и более ОЦЕНЁННЫХ точках: при метрике,
# не оценившейся ни разу (2PL не сошлась или упала на каждой границе развёртки ->
# a_mean снят гейтом a_mean_2pl() и равен NA везде), diff() даёт
# numeric(0), а all(numeric(0)) равно TRUE -- отсутствие оценки предъявилось бы как
# пройденная проверка. Результат в этом случае NA, и агрегация по развёрткам
# трёхзначна сама собой: любой FALSE -> FALSE (немонотонность установлена), иначе
# любой NA -> NA (вывод невозможен), иначе TRUE.
# Пропуски снимаются ДО diff(), поэтому уцелевшие точки сравниваются как соседние,
# хотя между ними пропущены другие: вердикт относится к ОЦЕНЁННОМУ подмножеству
# развёртки, а не ко всей ей. Отсюда охват колонками N_assessed / N_points -- без него
# развёртка, оценённая в 4 точках из 5 (точка, снятая гейтом a_mean_2pl()), даёт то же
# TRUE, что оценённая во всех 5. Асимметрия при чтении охвата: подпоследовательность
# монотонной последовательности монотонна, поэтому пропуски способны создать TRUE, но
# не FALSE -- установленная на подмножестве немонотонность установлена и для всей
# развёртки.
# omega_h в проверку не входит намеренно: на этих объёмах он скачет без
# закономерности (раздел 5 текстовой выдачи) и монотонность по нему смысла не имеет.
mono_one <- function(x) {
  d <- diff(x[!is.na(x)])
  list(mono = if (!length(d)) NA else all(d <= 0) || all(d >= 0),
       n_assessed = sum(!is.na(x)), n_points = length(x))
}
mono_tbl <- do.call(rbind, lapply(c("alpha", "H", "a_mean"), function(v) {
  per <- lapply(split(cuts, cuts$Sweep), function(g) mono_one(g[[v]]))
  data.frame(Metric = v,
             Monotone   = all(vapply(per, function(p) p$mono, logical(1))),
             N_assessed = sum(vapply(per, function(p) p$n_assessed, integer(1))),
             N_points   = sum(vapply(per, function(p) p$n_points, integer(1))),
             stringsAsFactors = FALSE)
}))
write_csv_excel(mono_tbl, file.path(OUT, "cut_monotonicity.csv"))
# Ветки текстовой выдачи читают тот же вердикт именованным вектором.
mono_flag <- stats::setNames(mono_tbl$Monotone, mono_tbl$Metric)

# =============================================================================
#  Текстовая выдача
# =============================================================================
SEP  <- strrep("=", 68)
sep2 <- strrep("-", 68)

sink(file.path(OUT, "bimodality_report.txt"), type = "output")
cat(SEP, "\n", sep = "")
cat(sprintf("  БИМОДАЛЬНОСТЬ ВЫБОРКИ -- N = %d, пунктов: %d\n", n, k))
cat(SEP, "\n\n", sep = "")

cat(sep2, "\n  1. РАСПРЕДЕЛЕНИЕ SCORE_TOTAL\n", sep2, "\n", sep = "")
cat(sprintf("  M = %.2f  SD = %.2f  Min = %d  Max = %d\n", mean(s), sd(s), min(s), max(s)))
cat(sprintf("  Skewness = %.3f  Kurtosis (excess) = %.3f\n\n", g1, g2))
print(table(s))
cat(sprintf("\n  потолок (>= %d): %d (%.1f%%)\n", CEIL_CUT, sum(s >= CEIL_CUT), 100 * mean(s >= CEIL_CUT)))
cat(sprintf("  полный балл (= %d): %d (%.1f%%)\n", k, sum(s == k), 100 * mean(s == k)))
cat(sprintf("  пол (<= %d): %d (%.1f%%)\n", FLOOR_CUT, sum(s <= FLOOR_CUT), 100 * mean(s <= FLOOR_CUT)))
cat(sprintf("  середина (%d-%d): %d (%.1f%%), максимум наблюдений на один балл: %d\n",
            FLOOR_CUT + 1L, CEIL_CUT - 1L, sum(s > FLOOR_CUT & s < CEIL_CUT),
            100 * mean(s > FLOOR_CUT & s < CEIL_CUT),
            max(table(s[s > FLOOR_CUT & s < CEIL_CUT]))))
cat(sprintf("  различных значений балла: %d на %d респондентов\n", length(unique(s)), n))

cat("\n", sep2, "\n  2. МЕРА БИМОДАЛЬНОСТИ\n", sep2, "\n", sep = "")
cat(sprintf("  Коэффициент бимодальности BC = %.4f (ссылочный уровень 5/9 = %.4f)\n", bc, BC_REF))
# p теста Хартигана берётся интерполяцией по таблице квантилей, и при D выше
# наибольшего табулированного квантиля diptest отдаёт РОВНО 0. Печатать «0.0000»
# значило бы выдать предел таблицы за вычисленную вероятность.
cat(sprintf("  Тест провала Хартигана: D = %.5f, p %s\n", dip$statistic,
            if (dip$p.value < 1e-4) "< 0.0001 (ниже предела таблицы квантилей)"
            else sprintf("= %.4f", dip$p.value)))
cat(sprintf("  Вывод по BC: распределение %s\n",
            if (bc > BC_REF) "БИМОДАЛЬНО" else "унимодально"))
cat("  BC чувствителен к платикуртозу, поэтому тест Хартигана приведён рядом:\n")
cat("  он проверяет унимодальность напрямую, а не через моменты.\n")
cat(sprintf("\n  Доли краёв против конвенционального порога 15 %%: пол %.1f %%, потолок %.1f %%.\n",
            100 * mean(s <= FLOOR_CUT), 100 * mean(s >= CEIL_CUT)))

cat("\n", sep2, "\n  3. УРОВЕНЬ УГАДЫВАНИЯ (обоснование нижней границы)\n", sep2, "\n", sep = "")
cat(sprintf("  Binom(%d, 1/4): M = %.1f, SD = %.2f\n", k, k * .25, sqrt(k * .25 * .75)))
for (q in c(.90, .95, .99))
  cat(sprintf("    %.0f%% чистых угадывающих набирают <= %d\n", 100 * q, qbinom(q, k, .25)))
cat(sprintf("  Порог пола %d консервативен: баллы выше него, но ниже %d,\n",
            FLOOR_CUT, qbinom(.95, k, .25) + 1L))
cat("  от угадывания статистически неотличимы и остаются в середине.\n")

cat("\n", sep2, "\n  4. НАДЁЖНОСТЬ И CFA ПО ПОДВЫБОРКАМ\n", sep2, "\n", sep = "")
cat("  CFA_1F -- однофакторная, CFA_3F -- 3-факторная ICM (субшкалы из config.R).\n")
cat("  Индексы согласия МАСШТАБИРОВАННЫЕ (WLSMV = DWLS + scaled.shifted).\n")
cat("  omega_h приведён, но НЕ интерпретируется: см. раздел 5.\n")
cat("  Conv_2PL и Conv_WLE -- сходимость рефита 2PL и 1PL на подвыборке; при FALSE\n")
cat("  соответствующее число (a_mean, WLE_rel) снято и стоит пустым.\n\n")
print(sens, row.names = FALSE)
cat("\n  Валидность по известным группам на тех же подвыборках (p):\n\n")
print(known, row.names = FALSE)

cat("\n", sep2, "\n  5. ЧУВСТВИТЕЛЬНОСТЬ К ГРАНИЦАМ СРЕЗА\n", sep2, "\n", sep = "")
cat("  lower     -- нижняя граница подвижна, верхняя фиксирована\n")
cat("  upper     -- верхняя граница подвижна, нижняя фиксирована\n")
cat("  symmetric -- усечение с обоих концов одновременно\n")
cat("  Conv_2PL   -- сходимость рефита 2PL; при FALSE a_mean снят и стоит пустым\n\n")
print(cuts, row.names = FALSE)
# Ветки читают mono_flag, измеренный в БЛОКЕ 3 и записанный в cut_monotonicity.csv:
# один результат на весь шаг, а не второй счёт для текста.
# na.rm/which() обязательны: if (NA) -- ошибка с обрывом шага при открытом sink(),
# а индексация логическим вектором с NA дала бы лишний элемент NA в списке имён.
if (any(mono_flag, na.rm = TRUE))
  cat(sprintf("\n  Монотонны во всех трёх развёртках: %s.\n",
              paste(names(mono_flag)[which(mono_flag)], collapse = ", ")))
if (any(!mono_flag, na.rm = TRUE))
  cat(sprintf("  Немонотонны: %s -- монотонность нарушена не во всех развёртках.\n",
              paste(names(mono_flag)[which(!mono_flag)], collapse = ", ")))
if (anyNA(mono_flag))
  cat(sprintf("  Не оценивались (хотя бы в одной развёртке нет двух точек): %s.\n",
              paste(names(mono_flag)[is.na(mono_flag)], collapse = ", ")))
# Охват печатается рядом с вердиктом: TRUE по развёртке с пропусками получен на
# подмножестве точек, а не на всей развёртке.
partial <- mono_tbl$N_assessed < mono_tbl$N_points
if (any(partial))
  cat(sprintf(paste0("  Охват неполон, вердикт относится к оценённым точкам ",
                     "(немонотонность от пропусков не зависит): %s.\n"),
              paste(sprintf("%s -- %d точек из %d", mono_tbl$Metric[partial],
                            mono_tbl$N_assessed[partial], mono_tbl$N_points[partial]),
                    collapse = "; ")))
cat("  Естественного порога нет: значение на одной границе измерением не является.\n")
# Отрицательная средняя дискриминация 2PL — вырожденная оценка на малой подвыборке;
# помечается так же явно, как соседний omega_h.
degen <- cuts[!is.na(cuts$a_mean) & cuts$a_mean <= 0, ]
if (nrow(degen))
  cat(sprintf("  a_mean <= 0 (оценка 2PL вырождена, n мал): %s.\n",
              paste(sprintf("%s Cut=%s (n=%d, a_mean=%.2f)",
                            degen$Sweep, degen$Cut, degen$n, degen$a_mean),
                    collapse = "; ")))
# Снятая гейтом точка печатается рядом с вырождением и по той же причине: пустое
# a_mean читается как отсутствие ОЦЕНКИ на этой границе, а не как отсутствие
# данных -- прочие метрики строки посчитаны.
nonconv <- cuts[!(cuts$Conv_2PL %in% TRUE), ]
if (nrow(nonconv))
  cat(sprintf("  2PL не сошлась, a_mean снят (оценкой не является): %s.\n",
              paste(sprintf("%s Cut=%s (n=%d)", nonconv$Sweep, nonconv$Cut, nonconv$n),
                    collapse = "; ")))
cat("  omega_h скачет без закономерности: на этих объёмах он не интерпретируется.\n")

cat("\n", SEP, "\n", sep = "")
sink()
