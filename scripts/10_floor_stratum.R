# =============================================================================
#  ШАГ 10 — FLOOR-СТРАТА: ЧТО ИЗМЕРЕНО НА ПОЛУ И ЧТО ОСТАЁТСЯ БЕЗ НЕГО
# -----------------------------------------------------------------------------
#    БЛОК 1 — условная доля верных по пунктам ВНУТРИ floor-страты
#    БЛОК 2 — item-level 2PL на подвыборке БЕЗ floor-страты
#    БЛОК 3 — person-fit по всей выборке (lz/Zh, ошибки Гуттмана)
#    БЛОК 4 — смесь «чистое угадывание + 2PL»: доля случайных респондентов
#    БЛОК 5 — бифакторная 2PL со специфическим фактором на hub-кластере
#    БЛОК 6 — структура и надёжность на 20 пунктах без hub-кластера
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv              (шаг 0)
#         output/Bimodality/extreme_patterns.csv    (шаг 2 — граница пола)
#         output/2PL/2pl_model.RData                (шаг 6 — оценённые 1PL/2PL)
#         input/items.csv                           (через scripts/config.R)
#  ВЫХОД: output/FloorStratum/floor_item_proportions.csv -> plot_floor_conditional_p
#         output/FloorStratum/floor_group_tests.csv      -> plot_floor_conditional_p
#         output/FloorStratum/floor_contrasts.csv
#         output/FloorStratum/floor_verdict.csv
#         output/FloorStratum/nofloor_2pl_params.csv  УСЛОВНО (сходимость рефита)
#         output/FloorStratum/nofloor_2pl_status.csv
#         output/FloorStratum/person_fit.csv          построчный, вне свода
#         output/FloorStratum/person_fit_summary.csv
#         output/FloorStratum/mixture_posterior.csv    построчный, вне свода
#         output/FloorStratum/mixture_summary.csv
#         output/FloorStratum/mixture_by_stratum.csv
#         output/FloorStratum/hub_bifactor_status.csv
#         output/FloorStratum/hub_bifactor_loadings.csv  УСЛОВНО (сходимость)
#         output/FloorStratum/hub_bifactor_indices.csv   УСЛОВНО (сходимость)
#         output/FloorStratum/nohub_structure.csv
#         output/FloorStratum/nohub_eigenvalues.csv
#         output/FloorStratum/floor_stratum_report.txt
#
#  Шаг стоит ПОСЛЕ шага 6: граница пола берётся артефактом шага 2, а оценённая 2PL
#  полной выборки — артефактом шага 6, поэтому ни одна из этих моделей здесь не
#  оценивается заново и второго источника тех же a/b не возникает.
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

load_pkgs(c("psych", "GPArotation", "lavaan", "mirt"))

OUT <- "output/FloorStratum"
ensure_dir(OUT)

source("scripts/config.R")   # ITEMS, ITEM_SUB, SUBSCALES (единый источник)

require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read.csv("output/cleaned_responses.csv", fileEncoding = "UTF-8-BOM",
               stringsAsFactors = FALSE, check.names = FALSE)

X <- as.matrix(df[, ITEMS])
s <- df$Score_Total
k <- length(ITEMS)
n <- nrow(X)

# Граница пола читается артефактом шага 2, а не повторяется литералом: она выведена
# там из уровня угадывания (Binom(k, 1/4)) и вместе с ним обязана меняться.
require_input("output/Bimodality/extreme_patterns.csv", "scripts/2_bimodality.R (шаг 2)")
extreme   <- read.csv("output/Bimodality/extreme_patterns.csv", fileEncoding = "UTF-8-BOM",
                      stringsAsFactors = FALSE, check.names = FALSE)
FLOOR_CUT <- as.integer(extreme$Floor_cut[1])

# Уровень чистого угадывания: все пункты четырёхвариантные (input/answer_key.csv,
# по одному ключу на четыре варианта), отсюда 1/4. Тот же уровень задаёт нижнюю
# границу пола в 2_bimodality.R.
P_GUESS <- 0.25

# Граница «высокой» дискриминации по Baker & Kim (2004) — та же, по которой
# plot_2pl_curves.R относит пункт к классу high.
A_HIGH_CUT <- 1.35
# Порог «завышенной» дискриминации: конвенционального значения у него нет, он взят
# как круглая отметка, по которой в тексте статьи выделены Q05, Q13, Q19 и Q21.
A_EXTREME_CUT <- 3.0

# Hub-кластер: шесть пунктов, независимо выделяющиеся как вторая размерность в EFA
# (фактор ML3), как первый фактор ESEM, как ядро нарушений локальной независимости
# (11 из 14 флагированных пар Q3 лежат внутри этого набора) и как группа с
# наименьшими нагрузками на генеральный фактор бифакторной 2PL. Состав задан здесь
# литералом, потому что он не выводится ни из одного артефакта ОДНИМ правилом:
# четыре процедуры сходятся на нём независимо, и ни одна из них не является его
# определением.
HUB_ITEMS <- c("Q01", "Q06", "Q09", "Q11", "Q20", "Q26")
local({
  stale <- setdiff(HUB_ITEMS, ITEMS)
  if (length(stale))
    stop(sprintf(paste0(
      "10_floor_stratum: hub-пункты вне набора анализа: %s. Состав пунктов задаёт ",
      "input/items.csv; кластер обязан быть его подмножеством."),
      paste(stale, collapse = ", ")), call. = FALSE)
})
REST_ITEMS <- setdiff(ITEMS, HUB_ITEMS)
GROUP <- setNames(ifelse(ITEMS %in% HUB_ITEMS, "hub", "rest"), ITEMS)

SEP  <- strrep("=", 72)
sep2 <- strrep("-", 72)

# =============================================================================
#  БЛОК 1 — условная доля верных внутри floor-страты
# =============================================================================
floor_idx <- s <= FLOOR_CUT
Xf   <- X[floor_idx, , drop = FALSE]
n_f  <- nrow(Xf)
s_f  <- s[floor_idx]

# Структурная граница страты. Отбор по сумме баллов задаёт среднюю по 26 пунктам
# долю верных тождественно: M(балл)/k, и потолок этой доли равен FLOOR_CUT/k. Поэтому
# «доля по всем пунктам близка к 0.25» внутри страты — свойство самого отбора, а не
# измерение, и исход «равномерно повышенные доли на всех 26» внутри страты
# ненаблюдаем в принципе. Различимы здесь РАСПРЕДЕЛЕНИЕ доли между пунктами
# (Кокрен Q ниже) и КОНТРАСТ hub против остальных, а не общий уровень.
implied_p <- mean(s_f) / k
bound_p   <- FLOOR_CUT / k

# Доля верных по каждому пункту, точный биномиальный тест против P_GUESS и ДИ
# Клоппера-Пирсона (тот же вызов binom.test даёт и p, и ДИ — граница ДИ и решение
# теста не могут разойтись). Рядом — chi2 согласия по тем же двум ячейкам.
item_rows <- lapply(ITEMS, function(it) {
  cc <- sum(Xf[, it])
  bt <- binom.test(cc, n_f, p = P_GUESS)
  ex <- c(n_f * P_GUESS, n_f * (1 - P_GUESS))
  ob <- c(cc, n_f - cc)
  data.frame(Item = it, Subscale = unname(ITEM_SUB[it]), Group = unname(GROUP[it]),
             N = n_f, Correct = cc,
             P_correct = cc / n_f,
             CI_low  = bt$conf.int[1], CI_high = bt$conf.int[2],
             Chi2 = sum((ob - ex)^2 / ex), df = 1L,
             P_binom = bt$p.value,
             stringsAsFactors = FALSE)
})
floor_items <- do.call(rbind, item_rows)
# Поправка Холма на 26 пунктов: без неё при alpha = 0.05 одно превышение из 26
# ожидается по случайности, и вывод об «отдельных пунктах выше угадывания» стал бы
# следствием числа тестов.
floor_items$P_holm <- p.adjust(floor_items$P_binom, method = "holm")
floor_items$Above_guess <- floor_items$P_holm < 0.05 & floor_items$P_correct > P_GUESS
floor_items$Below_guess <- floor_items$P_holm < 0.05 & floor_items$P_correct < P_GUESS
floor_items[c("P_correct", "CI_low", "CI_high", "Chi2")] <-
  lapply(floor_items[c("P_correct", "CI_low", "CI_high", "Chi2")], round, 3)
floor_items[c("P_binom", "P_holm")] <-
  lapply(floor_items[c("P_binom", "P_holm")], round, 4)
write_csv_excel(floor_items, file.path(OUT, "floor_item_proportions.csv"))

# Групповой уровень. Объединённый биномиальный тест по всем ответам группы считает
# 26 ответов одного респондента независимыми наблюдениями и потому занижает p;
# он приводится РЯДОМ с тестом на уровне респондента (доля верных внутри группы
# пунктов у каждого человека — одно наблюдение), который этого допущения не делает.
group_sets <- list(hub = HUB_ITEMS, rest = REST_ITEMS, all = ITEMS)
group_rows <- lapply(names(group_sets), function(g) {
  its <- group_sets[[g]]
  M   <- Xf[, its, drop = FALSE]
  cc  <- sum(M)
  nn  <- length(M)
  bt  <- binom.test(cc, nn, p = P_GUESS)
  pp  <- rowMeans(M)
  tt  <- t.test(pp, mu = P_GUESS)
  wt  <- wilcox.test(pp, mu = P_GUESS)
  data.frame(Group = g, K_items = length(its), N_persons = n_f,
             Responses = nn, Correct = cc,
             P_pooled = cc / nn,
             CI_low = bt$conf.int[1], CI_high = bt$conf.int[2],
             P_binom_pooled = bt$p.value,
             Mean_person_prop = mean(pp), SD_person_prop = sd(pp),
             t = unname(tt$statistic), df = unname(tt$parameter),
             P_person_t = tt$p.value,
             V_wilcox = unname(wt$statistic), P_person_wilcox = wt$p.value,
             stringsAsFactors = FALSE)
})
floor_groups <- do.call(rbind, group_rows)
# Поправка Холма на две ЗАЯВЛЕННЫЕ группы (hub и остальные); строка all — описание
# страты целиком и в семью не входит: её уровень задан отбором (implied_p выше).
fam <- floor_groups$Group %in% c("hub", "rest")
floor_groups$P_person_holm <- NA_real_
floor_groups$P_person_holm[fam] <- p.adjust(floor_groups$P_person_t[fam], method = "holm")
num_cols <- c("P_pooled", "CI_low", "CI_high", "Mean_person_prop", "SD_person_prop", "t")
floor_groups[num_cols] <- lapply(floor_groups[num_cols], round, 3)
p_cols <- c("P_binom_pooled", "P_person_t", "P_person_wilcox", "P_person_holm")
floor_groups[p_cols] <- lapply(floor_groups[p_cols], round, 4)
write_csv_excel(floor_groups, file.path(OUT, "floor_group_tests.csv"))

# Контрасты. Три разных вопроса к одной таблице 73 x 26:
#   Cochran Q  — одинакова ли доля верных ВО ВСЕХ пунктах внутри страты; это
#                прямая проверка «чистого угадывания», не зависящая от того, что
#                общий уровень задан отбором;
#   парный тест hub против остальных — тот же контраст внутри респондента, поэтому
#                отбор по сумме на него не влияет так, как на уровни;
#   chi2 2x2   — тот же контраст на объединённых ответах (допущение независимости
#                то же, что у объединённого биномиального теста выше).
cochran_q <- function(M) {
  R <- rowSums(M); C <- colSums(M); kk <- ncol(M)
  den <- kk * sum(R) - sum(R^2)
  Q <- if (den == 0) NA_real_ else kk * (kk - 1) * sum((C - mean(C))^2) / den
  list(Q = Q, df = kk - 1L,
       p = if (is.na(Q)) NA_real_ else pchisq(Q, kk - 1L, lower.tail = FALSE))
}
cq      <- cochran_q(Xf)
d_pers  <- rowMeans(Xf[, HUB_ITEMS, drop = FALSE]) - rowMeans(Xf[, REST_ITEMS, drop = FALSE])
tt_pair <- t.test(d_pers)
wt_pair <- wilcox.test(d_pers)
tab2x2  <- matrix(c(sum(Xf[, HUB_ITEMS]),  n_f * length(HUB_ITEMS)  - sum(Xf[, HUB_ITEMS]),
                    sum(Xf[, REST_ITEMS]), n_f * length(REST_ITEMS) - sum(Xf[, REST_ITEMS])),
                  nrow = 2, dimnames = list(c("correct", "wrong"), c("hub", "rest")))
chi2x2 <- chisq.test(tab2x2, correct = FALSE)

floor_contrasts <- data.frame(
  Test = c("Cochran Q (равенство долей по 26 пунктам)",
           "Парный t (hub - остальные, внутри респондента)",
           "Парный Wilcoxon (hub - остальные)",
           "chi2 2x2 (hub против остальных, объединённые ответы)"),
  Statistic = round(c(cq$Q, unname(tt_pair$statistic), unname(wt_pair$statistic),
                      unname(chi2x2$statistic)), 3),
  df = c(cq$df, unname(tt_pair$parameter), NA_real_, unname(chi2x2$parameter)),
  P_value = round(c(cq$p, tt_pair$p.value, wt_pair$p.value, chi2x2$p.value), 4),
  Effect = c(NA_real_, round(mean(d_pers), 3), round(median(d_pers), 3), NA_real_),
  stringsAsFactors = FALSE
)
write_csv_excel(floor_contrasts, file.path(OUT, "floor_contrasts.csv"))

# Вердикт. Три исхода заданы ЗАРАНЕЕ и различаются по двум групповым тестам на
# уровне респондента (Холм на семью из двух). Четвёртая комбинация логически
# возможна, поэтому названа отдельно, а не свёрнута в один из трёх исходов.
hub_row  <- floor_groups[floor_groups$Group == "hub", ]
rest_row <- floor_groups[floor_groups$Group == "rest", ]
hub_above  <- isTRUE(hub_row$P_person_holm  < 0.05 && hub_row$Mean_person_prop  > P_GUESS)
rest_above <- isTRUE(rest_row$P_person_holm < 0.05 && rest_row$Mean_person_prop > P_GUESS)
outcome <- if (!hub_above && !rest_above) {
  "(a) уровень угадывания на обеих группах"
} else if (hub_above && !rest_above) {
  "(b) превышение только на hub-пунктах"
} else if (hub_above && rest_above) {
  "(c) повышенные доли на обеих группах"
} else {
  "(d) превышение только вне hub-пунктов"
}
outcome_reading <- c(
  "(a) уровень угадывания на обеих группах"   = "невовлечённость/угадывание",
  "(b) превышение только на hub-пунктах"      = "альтернативный путь решения (методологический фактор)",
  "(c) повышенные доли на обеих группах"      = "частичная сформированность навыка",
  "(d) превышение только вне hub-пунктов"     = "ни один из трёх заявленных исходов")[[outcome]]

floor_verdict <- data.frame(
  Metric = c("Outcome", "Reading",
             "N_floor", "Floor_cut", "Mean_score_floor",
             "Implied_mean_p", "Structural_bound_p", "P_guess",
             "Hub_mean_p", "Hub_p_holm", "Rest_mean_p", "Rest_p_holm",
             "Hub_minus_rest", "Hub_vs_rest_p",
             "Cochran_Q_p", "N_items_above_guess", "N_items_below_guess"),
  Value = c(outcome, outcome_reading,
            format(n_f), format(FLOOR_CUT), format(round(mean(s_f), 3)),
            format(round(implied_p, 3)), format(round(bound_p, 3)), format(P_GUESS),
            format(hub_row$Mean_person_prop), format(hub_row$P_person_holm),
            format(rest_row$Mean_person_prop), format(rest_row$P_person_holm),
            format(round(mean(d_pers), 3)), format(round(tt_pair$p.value, 4)),
            format(round(cq$p, 4)),
            format(sum(floor_items$Above_guess)), format(sum(floor_items$Below_guess))),
  stringsAsFactors = FALSE
)
write_csv_excel(floor_verdict, file.path(OUT, "floor_verdict.csv"))
message(sprintf("[10] Floor-страта (n = %d): исход %s — %s.", n_f, outcome, outcome_reading))

# =============================================================================
#  БЛОК 2 — item-level 2PL на подвыборке без floor-страты
# =============================================================================
# Средняя дискриминация на этой подвыборке уже опубликована шагом 2
# (Bimodality/subsample_sensitivity.csv, строка NO-FLOOR); здесь оцениваются
# ПОПУНКТНЫЕ a и b, которых там нет.
nofloor_idx <- s > FLOOR_CUT
Xn <- X[nofloor_idx, , drop = FALSE]
# Пункт нулевой дисперсии в IRT не идентифицируется — тот же фильтр, что в
# zero_var_cols (4_rasch.R) и в 6_2pl.R, считается на этой подвыборке заново.
v_n <- apply(Xn, 2, var, na.rm = TRUE)
zero_var_n <- names(v_n)[is.na(v_n) | v_n == 0]
if (length(zero_var_n)) {
  message("[10] Пункты нулевой дисперсии на подвыборке без пола: ",
          paste(zero_var_n, collapse = ", "))
  Xn <- Xn[, setdiff(colnames(Xn), zero_var_n), drop = FALSE]
}

# Гейт сходимости — тот же стандарт, что ok_2pl в 6_2pl.R и a_mean_2pl() в
# 2_bimodality.R: mirt() при исчерпании итераций возвращает объект, а не падает.
mod_nf <- tryCatch(mirt(Xn, 1, itemtype = "2PL"),
  error = function(e) { message("[10] mirt(2PL) без пола упал: ", conditionMessage(e)); NULL })
nf_conv <- if (is.null(mod_nf)) NA else isTRUE(extract.mirt(mod_nf, "converged"))
write_csv_excel(
  data.frame(Model = "2PL NO-FLOOR", N = nrow(Xn), K_items = ncol(Xn),
             Converged = nf_conv, Zero_var_items = paste(zero_var_n, collapse = " ")),
  file.path(OUT, "nofloor_2pl_status.csv"))

if (isTRUE(nf_conv)) {
  co <- coef(mod_nf, IRTpars = TRUE, simplify = TRUE)$items
  nofloor_params <- data.frame(
    Item = rownames(co), Subscale = unname(ITEM_SUB[rownames(co)]),
    Group = unname(GROUP[rownames(co)]),
    a = round(co[, "a"], 3), b = round(co[, "b"], 3),
    p_value = round(colMeans(Xn, na.rm = TRUE)[rownames(co)], 3),
    stringsAsFactors = FALSE, row.names = NULL
  )
  # Классификация, а не производное число: порог 1.35 — то самое утверждение о
  # 25 пунктах из 26, которое проверяется на этой подвыборке.
  nofloor_params$High_disc <- nofloor_params$a >= A_HIGH_CUT
  nofloor_params$Extreme_disc <- nofloor_params$a > A_EXTREME_CUT
  write_csv_excel(nofloor_params, file.path(OUT, "nofloor_2pl_params.csv"))
} else {
  nofloor_params <- NULL
  message("!! [10] 2PL без пола не сошлась — попунктные a/b не экспортированы.")
}

# =============================================================================
#  БЛОК 3 — person-fit по всей выборке
# =============================================================================
# 2PL полной выборки берётся ГОТОВОЙ из шага 6: повторная оценка той же модели на
# тех же данных завела бы второй источник тех же a/b.
require_input("output/2PL/2pl_model.RData", "scripts/6_2pl.R (шаг 6)")
load("output/2PL/2pl_model.RData")   # mod_1pl, mod_2pl
items_2pl <- colnames(extract.mirt(mod_2pl, "data"))

# lz в mirt называется Zh — стандартизованная логарифмическая правдоподобность
# паттерна ответов. Отрицательные значения означают паттерн менее вероятный, чем
# ожидает модель; порог -1.645 — односторонний 5% квантиль стандартной нормали.
ZH_CUT <- -1.645
pf <- tryCatch(personfit(mod_2pl), error = function(e) {
  message("!! [10] personfit упал: ", conditionMessage(e)); NULL })

# Ошибки Гуттмана: пара «лёгкий/трудный», где верным оказался ТРУДНЫЙ, а лёгкий
# нет. Трудность упорядочивается по доле верных полной выборки. Нормировка G* —
# на максимум, возможный при данном балле r: r * (k - r). При r = 0 и r = k
# максимум равен нулю, ошибок нет по построению, и G* не определена.
p_full <- colMeans(X)
ord_easy <- order(p_full, decreasing = TRUE)
Xg <- X[, ord_easy, drop = FALSE]
g_err <- apply(Xg, 1, function(x) sum(cumsum(x == 0)[x == 1]))
g_max <- s * (k - s)
g_star <- ifelse(g_max > 0, g_err / g_max, NA_real_)

stratum <- ifelse(floor_idx, "floor", "no-floor")
person_fit <- data.frame(
  ID = df$ID, Score = s, Stratum = stratum,
  Zh = if (is.null(pf)) NA_real_ else round(pf$Zh, 3),
  Guttman_G = g_err, Guttman_Gstar = round(g_star, 3),
  stringsAsFactors = FALSE
)
write_csv_excel(person_fit, file.path(OUT, "person_fit.csv"))

pf_summary <- do.call(rbind, lapply(c("floor", "no-floor", "all"), function(g) {
  i <- if (g == "all") rep(TRUE, n) else stratum == g
  zh <- person_fit$Zh[i]
  gs <- person_fit$Guttman_Gstar[i]
  data.frame(Stratum = g, N = sum(i),
             Zh_mean = round(mean(zh, na.rm = TRUE), 3),
             Zh_SD   = round(sd(zh, na.rm = TRUE), 3),
             N_misfit = sum(zh < ZH_CUT, na.rm = TRUE),
             Pct_misfit = round(100 * mean(zh < ZH_CUT, na.rm = TRUE), 1),
             Gstar_mean = round(mean(gs, na.rm = TRUE), 3),
             Gstar_SD   = round(sd(gs, na.rm = TRUE), 3),
             N_Gstar_undefined = sum(is.na(gs)),
             stringsAsFactors = FALSE)
}))
write_csv_excel(pf_summary, file.path(OUT, "person_fit_summary.csv"))

# =============================================================================
#  БЛОК 4 — смесь «чистое угадывание + 2PL»
# =============================================================================
# Два класса: класс R отвечает случайно (вероятность верного ответа P_GUESS на
# КАЖДОМ пункте, ответы независимы), класс T описывается 2PL с theta ~ N(0, 1).
# Оценивается доля класса R (Pi) — прямая оценка доли «случайных» респондентов.
# EM: E-шаг считает апостериорную вероятность класса R, M-шаг переоценивает 2PL
# с весами (1 - w) и обновляет Pi. Правдоподобие смеси считается ЗДЕСЬ по общей
# квадратуре, а не берётся из mirt: взвешенная оценка даёт правдоподобие в другой
# шкале, и сравнение с одноклассовой 2PL было бы несопоставимым.
Xm <- X[, items_2pl, drop = FALSE]
r_m <- rowSums(Xm)
k_m <- ncol(Xm)
theta_q <- seq(-6, 6, length.out = 61)
w_q <- dnorm(theta_q); w_q <- w_q / sum(w_q)

# log P(паттерн | 2PL), маргинализованное по theta квадратурой; вычитание максимума
# по строке держит exp() вне переполнения при любом числе пунктов.
loglik_2pl <- function(M, a, b) {
  lin <- sweep(outer(theta_q, b, "-"), 2, a, "*")
  P <- 1 / (1 + exp(-lin))
  LL <- M %*% t(log(P)) + (1 - M) %*% t(log1p(-P))
  mx <- apply(LL, 1, max)
  mx + log(as.vector(exp(LL - mx) %*% w_q))
}
loglik_guess <- r_m * log(P_GUESS) + (k_m - r_m) * log(1 - P_GUESS)

co0 <- coef(mod_2pl, IRTpars = TRUE, simplify = TRUE)$items
a_cur <- co0[items_2pl, "a"]; b_cur <- co0[items_2pl, "b"]
# База сравнения считается ТОЙ ЖЕ квадратурой, что и смесь, иначе разность
# правдоподобий смешивала бы эффект второго класса с эффектом другой квадратуры.
# Значение повторяет logLik модели 2PL из 2PL/1pl_2pl_comparison.csv, и повтор здесь
# осмысленный: совпадение с mirt — проверка того, что квадратура смеси не своя.
ll_2pl_only <- sum(loglik_2pl(Xm, a_cur, b_cur))

pi_cur <- 0.10
mix_iter <- 0L
mix_conv <- FALSE
mix_refit_ok <- TRUE
ll_mix <- NA_real_
w_post <- rep(NA_real_, n)
MIX_MAXIT <- 50L
MIX_TOL   <- 1e-5
for (it in seq_len(MIX_MAXIT)) {
  mix_iter <- it
  l2 <- loglik_2pl(Xm, a_cur, b_cur)
  # w = P(класс R | ответы); отношение считается в логарифмах
  w_post <- 1 / (1 + exp(log(1 - pi_cur) - log(pi_cur) + l2 - loglik_guess))
  mx <- pmax(log(pi_cur) + loglik_guess, log(1 - pi_cur) + l2)
  ll_mix <- sum(mx + log(exp(log(pi_cur) + loglik_guess - mx) + exp(log(1 - pi_cur) + l2 - mx)))
  pi_new <- mean(w_post)
  m_step <- tryCatch(mirt(Xm, 1, itemtype = "2PL", survey.weights = 1 - w_post),
    error = function(e) { message("!! [10] M-шаг смеси упал: ", conditionMessage(e)); NULL })
  if (is.null(m_step) || !isTRUE(extract.mirt(m_step, "converged"))) {
    mix_refit_ok <- FALSE
    message(sprintf("!! [10] M-шаг смеси не сошёлся на итерации %d — оценка смеси снята.", it))
    break
  }
  co_m <- coef(m_step, IRTpars = TRUE, simplify = TRUE)$items
  a_cur <- co_m[items_2pl, "a"]; b_cur <- co_m[items_2pl, "b"]
  if (abs(pi_new - pi_cur) < MIX_TOL) { pi_cur <- pi_new; mix_conv <- TRUE; break }
  pi_cur <- pi_new
}

mix_ok <- mix_refit_ok && mix_conv
n_par_mix <- 2L * k_m + 1L
n_par_2pl <- 2L * k_m
mix_summary <- data.frame(
  Metric = c("Pi_random", "N_posterior_gt_0.5", "Pct_posterior_gt_0.5",
             "logLik_mixture", "logLik_2PL_only", "AIC_mixture", "AIC_2PL_only",
             "BIC_mixture", "BIC_2PL_only", "EM_iterations", "Converged"),
  Value = c(
    if (mix_ok) format(round(pi_cur, 4)) else "",
    if (mix_ok) format(sum(w_post > 0.5)) else "",
    if (mix_ok) format(round(100 * mean(w_post > 0.5), 1)) else "",
    if (mix_ok) format(round(ll_mix, 2)) else "",
    format(round(ll_2pl_only, 2)),
    if (mix_ok) format(round(-2 * ll_mix + 2 * n_par_mix, 2)) else "",
    format(round(-2 * ll_2pl_only + 2 * n_par_2pl, 2)),
    if (mix_ok) format(round(-2 * ll_mix + log(n) * n_par_mix, 2)) else "",
    format(round(-2 * ll_2pl_only + log(n) * n_par_2pl, 2)),
    format(mix_iter), as.character(mix_ok)),
  stringsAsFactors = FALSE
)
write_csv_excel(mix_summary, file.path(OUT, "mixture_summary.csv"))

mix_post <- data.frame(ID = df$ID, Score = s, Stratum = stratum,
                       P_random = if (mix_ok) round(w_post, 4) else NA_real_,
                       stringsAsFactors = FALSE)
write_csv_excel(mix_post, file.path(OUT, "mixture_posterior.csv"))

mix_by_stratum <- do.call(rbind, lapply(c("floor", "no-floor", "all"), function(g) {
  i <- if (g == "all") rep(TRUE, n) else stratum == g
  wp <- if (mix_ok) w_post[i] else rep(NA_real_, sum(i))
  data.frame(Stratum = g, N = sum(i),
             N_random = if (mix_ok) sum(wp > 0.5) else NA_integer_,
             Pct_random = if (mix_ok) round(100 * mean(wp > 0.5), 1) else NA_real_,
             Mean_P_random = if (mix_ok) round(mean(wp), 3) else NA_real_,
             stringsAsFactors = FALSE)
}))
write_csv_excel(mix_by_stratum, file.path(OUT, "mixture_by_stratum.csv"))

# =============================================================================
#  БЛОК 5 — бифакторная 2PL со специфическим фактором на hub-кластере
# =============================================================================
# Бифакторная спецификация шага 6 привязывает специфические факторы к номинальным
# субшкалам, а hub-кластер их пересекает, поэтому представить его она не может.
# Здесь специфический фактор ровно один и задан кластером; NA у остальных пунктов
# означает «только генеральный фактор» (штатный плейсхолдер mirt::bfactor).
spec_hub <- ifelse(items_2pl %in% HUB_ITEMS, 1L, NA_integer_)
mod_hub <- tryCatch(bfactor(Xm, model = spec_hub, technical = list(NCYCLES = 2000)),
  error = function(e) { message("!! [10] Ошибка bfactor(hub): ", conditionMessage(e)); NULL })
hub_conv <- !is.null(mod_hub) && isTRUE(extract.mirt(mod_hub, "converged"))
write_csv_excel(
  data.frame(Model = "Bifactor 2PL: G + S_hub", K_items = k_m,
             K_specific = sum(items_2pl %in% HUB_ITEMS), Converged = hub_conv),
  file.path(OUT, "hub_bifactor_status.csv"))

hub_indices <- NULL
if (hub_conv) {
  cmp_hub <- anova(mod_2pl, mod_hub)
  sm <- summary(mod_hub)
  Fm <- as.matrix(sm$rotF)
  h2 <- as.numeric(sm$h2)
  u2 <- 1 - h2
  lam_g <- Fm[, 1]
  lam_s <- Fm[, 2]
  lam_s[is.na(lam_s)] <- 0     # пункт вне кластера специфической нагрузки не имеет

  hub_loads <- rbind(
    data.frame(Factor = "G", Item = rownames(Fm), Beta = round(lam_g, 3),
               stringsAsFactors = FALSE),
    data.frame(Factor = "S_hub", Item = rownames(Fm), Beta = round(lam_s, 3),
               stringsAsFactors = FALSE))
  write_csv_excel(hub_loads, file.path(OUT, "hub_bifactor_loadings.csv"))

  in_hub  <- rownames(Fm) %in% HUB_ITEMS
  sum_g   <- sum(lam_g)
  sum_s   <- sum(lam_s[in_hub])
  denom   <- sum_g^2 + sum_s^2 + sum(u2)
  common  <- sum(lam_g^2) + sum(lam_s^2)
  denom_h <- sum(lam_g[in_hub])^2 + sum_s^2 + sum(u2[in_hub])
  hub_indices <- data.frame(
    Index = c("omega_h", "omega_total", "ECV", "ECV_S_hub", "PUC",
              "omega_s_hub", "omega_hs_hub"),
    Value = round(c(
      sum_g^2 / denom,
      (sum_g^2 + sum_s^2) / denom,
      sum(lam_g^2) / common,
      sum(lam_s^2) / common,
      1 - choose(sum(in_hub), 2) / choose(nrow(Fm), 2),
      (sum(lam_g[in_hub])^2 + sum_s^2) / denom_h,
      sum_s^2 / denom_h), 3),
    stringsAsFactors = FALSE)
  write_csv_excel(hub_indices, file.path(OUT, "hub_bifactor_indices.csv"))
} else {
  cmp_hub <- NULL
  message("!! [10] Бифакторная 2PL с фактором на hub-кластере не сошлась — ",
          "нагрузки и индексы не экспортированы.")
}

# =============================================================================
#  БЛОК 6 — структура и надёжность на 20 пунктах без hub-кластера
# =============================================================================
# Коэффициент шкалируемости Ловингера H по всем парам пунктов; формула и её
# обоснование — scripts/2_bimodality.R, определение общее для всех трёх мест.
loevinger_h <- function(M) {
  pp <- colMeans(M); nn <- nrow(M); num <- 0; den <- 0
  for (i in seq_len(ncol(M) - 1)) for (j in (i + 1):ncol(M)) {
    pij <- sum(M[, i] == 1 & M[, j] == 1) / nn
    num <- num + (pij - pp[i] * pp[j])
    den <- den + (min(pp[i], pp[j]) - pp[i] * pp[j])
  }
  unname(num / den)
}

# WLSMV = DWLS + scaled.shifted: наивные chisq/cfi/tli/rmsea при этом тесте не
# интерпретируются, поэтому берутся scaled-варианты — тот же набор ключей, что
# FI_KEYS в 3_efa_cfa.R и FI в 2_bimodality.R.
FI <- c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
fit_scaled <- function(syntax, dat) {
  f <- tryCatch(lavaan::cfa(syntax, data = dat, estimator = "WLSMV", ordered = TRUE),
       error = function(e) NULL)
  if (is.null(f) || !isTRUE(lavaan::lavInspect(f, "converged"))) return(NULL)
  lavaan::fitMeasures(f, FI)
}
pick <- function(x, key) if (is.null(x)) NA_real_ else round(unname(x[[key]]), 3)

M20 <- X[, REST_ITEMS, drop = FALSE]
dat20 <- as.data.frame(M20)

a_raw20 <- tryCatch(psych::alpha(M20)$total$raw_alpha, error = function(e) NA_real_)
tet20 <- tryCatch(psych::tetrachoric(M20, smooth = FALSE)$rho, error = function(e) NULL)
smoothed20 <- if (is.null(tet20)) {
  NA
} else {
  min(eigen(tet20, symmetric = TRUE, only.values = TRUE)$values) <= .Machine$double.eps
}
rho20 <- if (!is.null(tet20) && isTRUE(smoothed20)) psych::cor.smooth(tet20) else tet20
a_ord20 <- tryCatch(psych::alpha(rho20)$total$raw_alpha, error = function(e) NA_real_)
om20 <- tryCatch(psych::omega(rho20, nfactors = 3, plot = FALSE, rotate = "oblimin"),
       error = function(e) NULL)

# mc.cores = 1 по той же причине, что в 3_efa_cfa.R: без него форкнутые процессы
# fa.parallel берут собственный поток ГПСЧ и число факторов гуляет при set.seed(42).
pa20 <- local({
  old <- options(mc.cores = 1)
  on.exit(options(old), add = TRUE)
  psych::fa.parallel(M20, fa = "both", fm = "ml", plot = FALSE, n.iter = 20, sim = FALSE)
})
eig20 <- eigen(cor(M20))$values
pa_pc20 <- if (!all(is.na(pa20$pc.simr))) pa20$pc.simr else pa20$pc.sim
pa_line20 <- if (!all(is.na(pa_pc20))) pa_pc20[seq_along(eig20)] else NA_real_
write_csv_excel(
  data.frame(Factor = seq_along(eig20), Eigenvalue = round(eig20, 4),
             PA_line = round(pa_line20, 4)),
  file.path(OUT, "nohub_eigenvalues.csv"))

f20_1f <- fit_scaled(paste("G =~", paste(REST_ITEMS, collapse = " + ")), dat20)
ga20 <- lapply(SUBSCALES, function(v) intersect(v, REST_ITEMS))
ga20 <- ga20[vapply(ga20, length, integer(1)) >= 2]
f20_3f <- fit_scaled(paste(vapply(names(ga20), function(g)
          paste0(g, " =~ ", paste(ga20[[g]], collapse = " + ")), character(1)), collapse = "\n"), dat20)
if (is.null(f20_1f)) message("!! [10] Однофакторная CFA на 20 пунктах не сошлась.")
if (is.null(f20_3f)) message("!! [10] 3-факторная CFA на 20 пунктах не сошлась.")

nohub_structure <- data.frame(
  K_items = length(REST_ITEMS), N = n,
  alpha = round(a_raw20, 3), alpha_ord = round(a_ord20, 3),
  omega_t = if (!is.null(om20)) round(om20$omega.tot, 3) else NA_real_,
  omega_h = if (!is.null(om20)) round(om20$omega_h, 3) else NA_real_,
  H = round(loevinger_h(M20), 3), Smoothed = smoothed20,
  nfact_FA = pa20$nfact, ncomp_PCA = pa20$ncomp,
  CFA_1F_cfi = pick(f20_1f, "cfi.scaled"), CFA_1F_tli = pick(f20_1f, "tli.scaled"),
  CFA_1F_rmsea = pick(f20_1f, "rmsea.scaled"), CFA_1F_srmr = pick(f20_1f, "srmr"),
  CFA_3F_cfi = pick(f20_3f, "cfi.scaled"), CFA_3F_tli = pick(f20_3f, "tli.scaled"),
  CFA_3F_rmsea = pick(f20_3f, "rmsea.scaled"), CFA_3F_srmr = pick(f20_3f, "srmr"),
  stringsAsFactors = FALSE)
write_csv_excel(nohub_structure, file.path(OUT, "nohub_structure.csv"))

# =============================================================================
#  Текстовая выдача
# =============================================================================
sink(file.path(OUT, "floor_stratum_report.txt"), type = "output")
cat(SEP, "\n", sep = "")
cat(sprintf("  FLOOR-СТРАТА -- N = %d, пунктов: %d, граница пола: <= %d\n", n, k, FLOOR_CUT))
cat(SEP, "\n\n", sep = "")

cat(sep2, "\n  1. УСЛОВНАЯ ДОЛЯ ВЕРНЫХ ВНУТРИ FLOOR-СТРАТЫ\n", sep2, "\n", sep = "")
cat(sprintf("  n = %d (%.1f%% выборки), M(балл) = %.2f, SD = %.2f\n",
            n_f, 100 * n_f / n, mean(s_f), sd(s_f)))
cat(sprintf("  Уровень чистого угадывания: %.2f (4 варианта на пункт)\n", P_GUESS))
cat(sprintf("  Средняя по 26 пунктам доля верных = M(балл)/k = %.3f;\n", implied_p))
cat(sprintf("  её ПОТОЛОК внутри страты задан отбором: %d/%d = %.3f.\n", FLOOR_CUT, k, bound_p))
cat("  Отсюда: общий уровень доли внутри страты -- свойство отбора, а не измерение,\n")
cat("  и исход «равномерно повышенные доли на всех 26» внутри неё ненаблюдаем.\n")
cat("  Измеряются РАСПРЕДЕЛЕНИЕ доли между пунктами и КОНТРАСТ hub/остальные.\n\n")
cat(sprintf("  %-6s %-4s %-5s %8s %7s %15s %9s %9s\n",
            "Item", "Sub", "Grp", "Correct", "p", "95% CI", "p(binom)", "p(Holm)"))
cat("  ", strrep("-", 68), "\n", sep = "")
for (i in seq_len(nrow(floor_items))) {
  r <- floor_items[i, ]
  cat(sprintf("  %-6s %-4s %-5s %8d %7.3f  [%.3f; %.3f] %9.4f %9.4f%s\n",
              r$Item, r$Subscale, r$Group, r$Correct, r$P_correct,
              r$CI_low, r$CI_high, r$P_binom, r$P_holm,
              if (r$Above_guess) "  ^" else if (r$Below_guess) "  v" else ""))
}
cat("  ^ -- доля значимо ВЫШЕ уровня угадывания, v -- значимо НИЖЕ (Холм, 26 тестов)\n")

cat("\n  Группы пунктов:\n\n")
print(floor_groups, row.names = FALSE)
cat("\n  Контрасты:\n\n")
print(floor_contrasts, row.names = FALSE)
cat(sprintf("\n  ВЕРДИКТ: %s -- %s.\n", outcome, outcome_reading))
cat(sprintf("  hub: M(доля) = %.3f, p(Холм) = %.4f; остальные: M(доля) = %.3f, p(Холм) = %.4f.\n",
            hub_row$Mean_person_prop, hub_row$P_person_holm,
            rest_row$Mean_person_prop, rest_row$P_person_holm))
cat(sprintf("  Разность внутри респондента: %+.3f (парный t: p = %.4f).\n",
            mean(d_pers), tt_pair$p.value))
cat(sprintf("  Равенство долей по всем %d пунктам (Cochran Q): Q = %.2f, df = %d, p = %.4f.\n",
            k, cq$Q, cq$df, cq$p))

cat("\n", sep2, "\n  2. 2PL НА ПОДВЫБОРКЕ БЕЗ FLOOR-СТРАТЫ\n", sep2, "\n", sep = "")
cat(sprintf("  n = %d, пунктов в оценке: %d, сходимость: %s\n",
            nrow(Xn), ncol(Xn), as.character(nf_conv)))
if (!is.null(nofloor_params)) {
  cat(sprintf("\n  %-6s %-4s %-5s %7s %8s %8s %s\n",
              "Item", "Sub", "Grp", "p-val", "a(disc)", "b(diff)", "класс"))
  cat("  ", strrep("-", 56), "\n", sep = "")
  for (i in seq_len(nrow(nofloor_params))) {
    r <- nofloor_params[i, ]
    cat(sprintf("  %-6s %-4s %-5s %7.3f %8.3f %8.3f %s\n",
                r$Item, r$Subscale, r$Group, r$p_value, r$a, r$b,
                if (r$Extreme_disc) sprintf("a > %.1f", A_EXTREME_CUT)
                else if (r$High_disc) sprintf("a >= %.2f", A_HIGH_CUT)
                else sprintf("a < %.2f", A_HIGH_CUT)))
  }
  below <- nofloor_params$Item[!nofloor_params$High_disc]
  extreme_items <- nofloor_params$Item[nofloor_params$Extreme_disc]
  cat(sprintf("\n  Дискриминация (a): M = %.3f, SD = %.3f, min = %.3f, max = %.3f\n",
              mean(nofloor_params$a), sd(nofloor_params$a),
              min(nofloor_params$a), max(nofloor_params$a)))
  cat(sprintf("  Трудность     (b): M = %.3f, SD = %.3f, min = %.3f, max = %.3f\n",
              mean(nofloor_params$b), sd(nofloor_params$b),
              min(nofloor_params$b), max(nofloor_params$b)))
  cat(sprintf("  Порог a >= %.2f достигают %d пунктов из %d; ниже порога: %s.\n",
              A_HIGH_CUT, sum(nofloor_params$High_disc), nrow(nofloor_params),
              if (length(below)) paste(below, collapse = ", ") else "нет"))
  cat(sprintf("  a > %.1f: %s.\n", A_EXTREME_CUT,
              if (length(extreme_items)) paste(extreme_items, collapse = ", ") else "нет"))
}

cat("\n", sep2, "\n  3. PERSON-FIT ПО ВСЕЙ ВЫБОРКЕ\n", sep2, "\n", sep = "")
cat(sprintf("  Zh (lz) из 2PL шага 6; порог несоответствия Zh < %.3f.\n", ZH_CUT))
cat("  G -- ошибки Гуттмана (лёгкий не сдан при сданном трудном), G* -- нормировка\n")
cat("  на максимум при данном балле r * (k - r); при r = 0 и r = k G* не определена.\n\n")
print(pf_summary, row.names = FALSE)

cat("\n", sep2, "\n  4. СМЕСЬ «ЧИСТОЕ УГАДЫВАНИЕ + 2PL»\n", sep2, "\n", sep = "")
cat(sprintf("  Класс R: вероятность верного ответа %.2f на каждом пункте.\n", P_GUESS))
cat("  Класс T: 2PL, theta ~ N(0, 1). Оценка EM, правдоподобие по общей квадратуре.\n\n")
print(mix_summary, row.names = FALSE)
if (mix_ok) {
  cat("\n  Распределение класса R по стратам:\n\n")
  print(mix_by_stratum, row.names = FALSE)
  cat("\n  Разность правдоподобий на границе пространства параметров (Pi = 0),\n")
  cat("  поэтому её распределение не chi2 и p-значение здесь не приводится:\n")
  cat("  сравнение идёт по AIC/BIC.\n")
}

cat("\n", sep2, "\n  5. БИФАКТОРНАЯ 2PL: G + S_hub\n", sep2, "\n", sep = "")
cat(sprintf("  Специфический фактор задан кластером: %s.\n", paste(HUB_ITEMS, collapse = ", ")))
if (hub_conv) {
  cat("\n  Сравнение: одномерная 2PL против бифакторной с S_hub\n\n")
  print(cmp_hub)
  cat("\n  Индексы:\n\n")
  print(hub_indices, row.names = FALSE)
  cat("\n  ECV_S_hub -- доля общей дисперсии, приходящаяся на специфический фактор\n")
  cat("  кластера; omega_hs_hub -- надёжность субшкального балла кластера за вычетом G.\n")
} else {
  cat("\n  Модель НЕ СОШЛАСЬ: нагрузки и индексы не рассчитаны.\n")
}

cat("\n", sep2, "\n  6. СТРУКТУРА И НАДЁЖНОСТЬ НА 20 ПУНКТАХ БЕЗ HUB-КЛАСТЕРА\n", sep2, "\n", sep = "")
cat(sprintf("  Сняты: %s. Осталось пунктов: %d.\n",
            paste(HUB_ITEMS, collapse = ", "), length(REST_ITEMS)))
cat("\n")
print(nohub_structure, row.names = FALSE)
cat("\n  Собственные значения корреляционной матрицы против линии параллельного анализа:\n\n")
print(utils::head(
  data.frame(Factor = seq_along(eig20), Eigenvalue = round(eig20, 3),
             PA_line = round(pa_line20, 3)), 6), row.names = FALSE)
sink()
