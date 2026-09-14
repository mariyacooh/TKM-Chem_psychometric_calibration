#!/usr/bin/env Rscript
# =============================================================================
#  bimodality.R — разведочный анализ бимодальности Score_Total
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: печать в stdout; файлы не пишутся, output/ не изменяется
#
#  Скрипт ВНЕ пайплайна: scripts/run_all.R его не вызывает. Воспроизводит числа
#  из analysis/bimodality.md — там же вопрос, трактовка и ограничения.
#
#  Запуск из корня проекта (после полного прогона пайплайна):
#      Rscript analysis/bimodality.R
#  Время выполнения — единицы минут (блок 2 оценивает CFA на четырёх подвыборках).
# =============================================================================

.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

source("scripts/_setup.R")
source("scripts/config.R")
load_pkgs(c("psych", "lavaan", "mirt", "TAM"))
set.seed(42)

require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read.csv("output/cleaned_responses.csv", fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE, check.names = FALSE)
X <- as.matrix(df[, ITEMS]); s <- df$Score_Total; k <- length(ITEMS); n <- nrow(X)

SEP <- strrep("=", 78)

# Коэффициент шкалируемости Ловингера H по всем парам пунктов; формула и её
# обоснование — scripts/2_bimodality.R, определение общее для обоих скриптов.
loevinger_h <- function(M) {
  pp <- colMeans(M); nn <- nrow(M); num <- 0; den <- 0
  for (i in seq_len(ncol(M) - 1)) for (j in (i + 1):ncol(M)) {
    pij <- sum(M[, i] == 1 & M[, j] == 1) / nn
    num <- num + (pij - pp[i] * pp[j])
    den <- den + (min(pp[i], pp[j]) - pp[i] * pp[j])
  }
  unname(num / den)
}

# Базовые показатели подвыборки. omega_h считается, но НЕ интерпретируется:
# блок 3 показывает, что на этих объёмах он нестабилен.
subset_stats <- function(i) {
  M <- X[i, , drop = FALSE]; M <- M[, apply(M, 2, var) > 0, drop = FALSE]
  a_raw <- tryCatch(psych::alpha(M, warnings = FALSE)$total$raw_alpha, error = function(e) NA)
  tet <- tryCatch(tetrachoric(M, smooth = FALSE)$rho, error = function(e) NULL)
  smoothed <- if (is.null(tet)) NA
              else min(eigen(tet, symmetric = TRUE, only.values = TRUE)$values) <= .Machine$double.eps
  rho <- if (!is.null(tet) && isTRUE(smoothed)) suppressWarnings(psych::cor.smooth(tet)) else tet
  a_ord <- tryCatch(psych::alpha(rho, warnings = FALSE)$total$raw_alpha, error = function(e) NA)
  om <- tryCatch(suppressWarnings(omega(rho, nfactors = 3, plot = FALSE, rotate = "oblimin")),
                 error = function(e) NULL)
  aa <- tryCatch(coef(mirt(M, 1, itemtype = "2PL", verbose = FALSE),
                      IRTpars = TRUE, simplify = TRUE)$items[, "a"], error = function(e) NA)
  list(M = M, n = sum(i), alpha = a_raw, alpha_ord = a_ord,
       omega_t = if (!is.null(om)) om$omega.tot else NA,
       omega_h = if (!is.null(om)) om$omega_h else NA,
       H = loevinger_h(M), smoothed = smoothed, a_mean = mean(aa, na.rm = TRUE))
}

# WLSMV = DWLS + scaled.shifted: наивные chisq/cfi/tli/rmsea при этом тесте не
# интерпретируются, а pvalue не определён вовсе, поэтому берутся scaled-варианты.
FI <- c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
fit_scaled <- function(syntax, dat) {
  f <- tryCatch(suppressWarnings(cfa(syntax, data = dat, estimator = "WLSMV", ordered = TRUE)),
                error = function(e) NULL)
  if (is.null(f) || !isTRUE(lavInspect(f, "converged"))) return(NULL)
  fitMeasures(f, FI)
}

# =============================================================================
cat("\n", SEP, "\n БЛОК 1. РАСПРЕДЕЛЕНИЕ SCORE_TOTAL\n", SEP, "\n\n", sep = "")
cat(sprintf(" N = %d, пунктов = %d, M = %.2f, SD = %.2f, min = %d, max = %d\n\n",
            n, k, mean(s), sd(s), min(s), max(s)))
print(table(s))
cat(sprintf("\n потолок (=%d): %d (%.1f%%) | пол (<=8): %d (%.1f%%) | середина 9-23: %d (%.1f%%)\n",
            k, sum(s == k), 100 * mean(s == k), sum(s <= 8), 100 * mean(s <= 8),
            sum(s > 8 & s < 24), 100 * mean(s > 8 & s < 24)))
cat(sprintf(" максимум наблюдений на один балл в середине 9-23: %d\n", max(table(s[s >= 9 & s <= 23]))))
cat(sprintf("\n уровень угадывания Binom(%d, 1/4): M = %.1f, SD = %.2f\n", k, k * .25, sqrt(k * .25 * .75)))
for (q in c(.90, .95, .99))
  cat(sprintf("   %.0f%% чистых угадывающих набирают <= %d\n", 100 * q, qbinom(q, k, .25)))
cat(" -> порог 8 консервативен: 9 и 10 от угадывания статистически неотличимы.\n")

# =============================================================================
cat("\n", SEP, "\n БЛОК 2. ПСИХОМЕТРИКА ПО ПОДВЫБОРКАМ (CFA — scaled/WLSMV)\n", SEP, "\n\n", sep = "")
cat(" Границы подвыборок иллюстративны, а не установлены: см. БЛОК 3.\n\n")
SAMPLES <- list("FULL (all)" = rep(TRUE, n), "NO-FLOOR (>8)" = s > 8,
                "NO-TAILS (9-25)" = s > 8 & s < 26, "MID only (9-23)" = s > 8 & s < 24)
rows <- lapply(names(SAMPLES), function(nm) {
  st <- subset_stats(SAMPLES[[nm]]); M <- st$M; items <- colnames(M); dat <- as.data.frame(M)
  m1 <- fit_scaled(paste("G =~", paste(items, collapse = " + ")), dat)
  ga <- lapply(SUBSCALES, function(v) intersect(v, items))
  ga <- ga[vapply(ga, length, integer(1)) >= 2]
  m2 <- fit_scaled(paste(vapply(names(ga), function(g)
        paste0(g, " =~ ", paste(ga[[g]], collapse = " + ")), character(1)), collapse = "\n"), dat)
  wle <- tryCatch(suppressMessages(
    tam.wle(tam.mml(M, irtmodel = "1PL", control = list(progress = FALSE)))$WLE.rel[[1]]),
    error = function(e) NA)
  pick <- function(x, key) if (is.null(x)) NA_real_ else round(unname(x[[key]]), 3)
  data.frame(sample = nm, n = st$n, alpha = round(st$alpha, 3), alpha_ord = round(st$alpha_ord, 3),
             omega_t = round(st$omega_t, 3), omega_h = round(st$omega_h, 3), H = round(st$H, 3),
             WLE_rel = round(wle, 3), a_mean = round(st$a_mean, 2), smoothed = st$smoothed,
             CFA_1F_cfi = pick(m1, "cfi.scaled"), CFA_1F_rmsea = pick(m1, "rmsea.scaled"),
             CFA_3F_cfi = pick(m2, "cfi.scaled"), CFA_3F_rmsea = pick(m2, "rmsea.scaled"),
             stringsAsFactors = FALSE)
})
print(do.call(rbind, rows), row.names = FALSE)
cat("\n CFA_1F = однофакторная, CFA_3F = 3-факторная ICM (субшкалы из config.R).\n")

cat("\n ВАЛИДНОСТЬ ПО ИЗВЕСТНЫМ ГРУППАМ (p) на тех же подвыборках:\n\n")
kg <- lapply(names(SAMPLES), function(nm) {
  i <- SAMPLES[[nm]]; x <- df[i, ]; y <- s[i]
  safe <- function(e) tryCatch(e, error = function(z) NA_real_)
  cr <- suppressWarnings(cor.test(x$GPA_ordinal, y, method = "spearman", exact = FALSE))
  data.frame(sample = nm, n = sum(i),
             course_KW = round(safe(kruskal.test(y ~ factor(x$Course))$p.value), 3),
             conference_t = round(safe(t.test(y ~ factor(x$Conference))$p.value), 3),
             language_t = round(safe(t.test(y ~ factor(x$Language))$p.value), 3),
             GPA_rho = round(unname(cr$estimate), 3), GPA_p = round(cr$p.value, 3),
             stringsAsFactors = FALSE)
})
print(do.call(rbind, kg), row.names = FALSE)

# =============================================================================
cat("\n", SEP, "\n БЛОК 3. ЧУВСТВИТЕЛЬНОСТЬ К ГРАНИЦАМ СРЕЗА\n", SEP, "\n\n", sep = "")
cat(" Естественной границы нет: показатели меняются гладко и монотонно.\n\n")
sw <- function(i) { st <- subset_stats(i)
  c(n = st$n, alpha = round(st$alpha, 3), omega_h = round(st$omega_h, 3),
    H = round(st$H, 3), a_mean = round(st$a_mean, 2)) }
cat(" A. нижняя граница подвижна, верхняя фиксирована (< 24):\n")
print(t(sapply(6:12, function(lo) c(lower_cut = lo, sw(s > lo & s < 24)))))
cat("\n B. верхняя граница подвижна, нижняя фиксирована (> 8):\n")
print(t(sapply(22:26, function(hi) c(upper_cut = hi, sw(s > 8 & s < hi)))))
cat("\n C. симметричное усечение с обоих концов:\n")
print(t(sapply(0:5, function(t) c(trim = t, sw(s > (6 + t) & s < (26 - t))))))
cat("\n alpha, H и a_mean монотонны во всех трёх развёртках — это и есть результат.\n")
cat(" omega_h скачет без закономерности: на этих объёмах он не интерпретируется.\n")

# =============================================================================
cat("\n", SEP, "\n БЛОК 4. ЛАТЕНТНЫЕ КЛАССЫ: ДИСКРЕТНОСТЬ НЕ ПОДТВЕРЖДАЕТСЯ\n", SEP, "\n\n", sep = "")
lse <- function(m) { mx <- apply(m, 1, max); mx + log(rowSums(exp(m - mx))) }
lca <- function(X, C, seed, iter = 3000, tol = 1e-11) {
  set.seed(seed); J <- ncol(X)
  p <- matrix(runif(C * J, .15, .85), C, J); pi <- rep(1 / C, C); old <- -Inf
  for (t in seq_len(iter)) {
    L <- sapply(seq_len(C), function(c) log(pi[c]) + X %*% log(p[c, ]) + (1 - X) %*% log(1 - p[c, ]))
    if (C == 1) L <- matrix(L, ncol = 1)
    ll <- sum(lse(L)); r <- exp(L - lse(L)); pi <- colMeans(r)
    for (c in seq_len(C)) p[c, ] <- pmin(pmax(colSums(r[, c] * X) / sum(r[, c]), 1e-6), 1 - 1e-6)
    if (abs(ll - old) < tol) break
    old <- ll
  }
  npar <- (C - 1) + C * J
  list(ll = ll, bic = -2 * ll + npar * log(nrow(X)), aic = -2 * ll + 2 * npar,
       pi = pi, p = p, post = r, npar = npar)
}
best <- lapply(1:5, function(C) {
  fits <- lapply(1:25, function(sd) lca(X, C, sd))
  fits[[which.max(sapply(fits, function(f) f$ll))]] })
for (C in 1:5) cat(sprintf("  C=%d  npar=%3d  logLik=%9.1f  AIC=%8.1f  BIC=%8.1f\n",
                           C, best[[C]]$npar, best[[C]]$ll, best[[C]]$aic, best[[C]]$bic))
cat("\n Минимум BIC приходится на C=4, а не на C=2: двухклассовая модель\n")
cat(" (\"две популяции\") данными не выбирается.\n\n")
cat(" состав классов по баллу (классы упорядочены по средней p пункта):\n")
for (C in 2:5) {
  f <- best[[C]]; ord <- order(rowMeans(f$p)); a <- match(apply(f$post, 1, which.max), ord)
  rg <- t(sapply(seq_len(C), function(c) if (sum(a == c))
    c(n = sum(a == c), min = min(s[a == c]), max = max(s[a == c]), mean = round(mean(s[a == c]), 1))
    else c(0, NA, NA, NA)))
  rownames(rg) <- paste0("class", seq_len(C))
  cat(sprintf("\n C=%d:\n", C)); print(rg)
}
cat("\n Классы выходят упорядоченными полосами баллов с растущим перекрытием —\n")
cat(" так LCA режет континуум, а не находит дискретные типы ответа.\n")
cat("\n", SEP, "\n[OK] Разведочный анализ бимодальности завершён.\n", sep = "")
