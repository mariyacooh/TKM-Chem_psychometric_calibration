# =============================================================================
#  ШАГ 3 — СТРУКТУРНАЯ ВАЛИДНОСТЬ И НАДЁЖНОСТЬ
#    БЛОК 1 — EFA
#    БЛОК 2 — CFA (1F / 3F ICM / бифактор)
#    БЛОК 3 — бифакторные индексы и omega
#    БЛОК 4 — ESEM (geomin), переиспользует 1F/3F CFA из БЛОКА 2
# -----------------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv   (шаг 0)
#         input/items.csv                (через scripts/config.R)
#  ВЫХОД: output/EFA/efa_eigenvalues.csv       -> plot_scree, report.Rmd
#         output/EFA/efa_parallel_analysis.csv -> plot_scree (число компонент PA)
#         output/EFA/efa_loadings.csv          -> plot_efa_heatmap, report.Rmd
#         output/CFA/cfa_fits.RData            -> plot_cfa_path, plot_cfa_loadings
#         output/CFA/cfa_loadings.csv          -> report.Rmd
#         output/CFA/cfa_loadings_compact.csv  -> write_report_text.R
#         output/CFA/cfa_bifactor_loadings.csv -> write_report_text.R; УСЛОВНО, только
#                                                 при сходимости бифакторной CFA
#         output/Bifactor/bifactor_fit_table.csv, omega_matrix_status.csv -> report.Rmd
#         output/ESEM/esem_loadings.csv        -> plot_esem_heatmap, report.Rmd
#         output/ESEM/esem_fit_table.csv, output/ESEM/esem_factor_correlations.csv
#                                              -> report.Rmd, write_report_text.R
#         (+ *.txt логи всех блоков -> report.Rmd)
#
#  ESEM — БЛОК 4 ЭТОГО шага; отдельного 3b_esem.R в пайплайне нет. При несходимости
#  ESEM файл esem_loadings.csv пишется ПУСТЫМ (0 строк) — это и есть сигнал.
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

load_pkgs(c("tidyverse", "readr", "lavaan", "psych", "GPArotation", "Matrix"))

select <- dplyr::select
filter <- dplyr::filter
alpha <- psych::alpha

ensure_dir("output/EFA")
ensure_dir("output/CFA")
ensure_dir("output/Bifactor")

# ── Структура субшкал (нотация Q) — ITEMS из config.R ───────────────────────
source("scripts/config.R")

# ── Загрузка данных ──────────────────────────────────────────────────────────
require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")
df <- read_csv("output/cleaned_responses.csv") %>%
  mutate(
    across(starts_with("Q"), as.integer)
  )

item_cols <- intersect(ITEMS, names(df))
item_data <- df %>% dplyr::select(all_of(item_cols)) %>% as.matrix()

# =============================================================================
#  БЛОК 1: EFA
# =============================================================================
# Корреляции EFA (KMO, Bartlett, fa.parallel, fa, scree) — ПИРСОНА (phi), НЕ
# тетрахорические, хотя CFA/omega/ESEM используют тетрахорическую/порядковую
# обработку. Это НАМЕРЕННО: тетрахорическая матрица на этих данных не положительно
# определена (ISSUES.md 1.1; 3 отрицательных собственных значения) и после
# сглаживания почти вырождена (cond ~1.5e11), поэтому KMO и ML-индексы согласия,
# обращающие матрицу, давали бы артефакты (KMO 0.94->0.50, RMSEA 0.05->0.51) при
# заведомо факторизуемых данных (WLSMV CFA, масштабированный RMSEA=0.041). phi занижает нагрузки —
# принятый компромисс; структура подтверждена тетрахорическими CFA/ESEM.
# Обоснование — DECISIONS.md D6.

sink("output/EFA/efa_results.txt", type = "output")
cat("=============================================================\n")
cat("  РАЗВЕДОЧНЫЙ ФАКТОРНЫЙ АНАЛИЗ (EFA) - n =", nrow(df), "\n")
cat("=============================================================\n\n")
cat("Прим.: корреляции — Пирсона (phi). Тетрахорическая матрица на этих данных не\n")
cat("положительно определена (ISSUES.md 1.1); после сглаживания почти вырождена,\n")
cat("поэтому KMO/RMSEA по ней были бы артефактами. phi занижает нагрузки (принятый\n")
cat("компромисс; структура подтверждена CFA/ESEM). Обоснование — DECISIONS.md D6.\n\n")

kmo <- KMO(item_data)
cat(sprintf("KMO = %.3f\n\n", kmo$MSA))

bart <- cortest.bartlett(item_data)
cat(sprintf("Bartlett: chi2(%.0f) = %.2f, p = %.4f\n\n", bart$df, bart$chisq, bart$p.value))

# fa = "both": нужна и компонентная ветка — линия сравнения на scree сопоставляется
# с собственными значениями PCA (eigen(cor) ниже), а не с fa.values. При fa = "fa"
# psych возвращает компонентные поля как NA, а не NULL.
#
# mc.cores = 1: fa.parallel ресэмплит через parallel::mclapply, а форкнутые процессы
# берут собственный поток ГПСЧ, поэтому без этой опции и nfact, и линия PA гуляют от
# прогона к прогону при одном и том же set.seed(42). mc.cores = 1 сводит mclapply к
# обычному lapply в родительском процессе, отчего результат воспроизводим; на
# n.iter = 20 это ~0.3 c.
pa <- local({
  old <- options(mc.cores = 1)
  on.exit(options(old), add = TRUE)
  fa.parallel(item_data, fa = "both", fm = "ml", plot = FALSE, n.iter = 20, sim = FALSE)
})
n_pa <- pa$nfact
cat(sprintf("Рекомендуемое число факторов (параллельный анализ, FA-ветка): %d\n", n_pa))
# Компонентная ветка того же анализа: именно с ней сопоставляются собственные
# значения PCA на scree-графике (маркер "PA: N components" в plot_scree.R). Число
# здесь и число на графике — одно и то же значение: рисунок читает его из артефакта
# ниже. Ветки называются на рисунке своими именами: маркер несёт КОМПОНЕНТЫ, тогда
# как рекомендация по числу ФАКТОРОВ — nfact_FA строкой выше и в этом же артефакте.
cat(sprintf("Число компонент (PCA-ветка; ей соответствует линия PA на scree-графике): %d\n\n",
            pa$ncomp))

# Оба числа уходят артефактом, потому что читаются они не только здесь. Правило
# отбора у параллельного анализа своё (fa.parallel удерживает ВЕДУЩИЕ компоненты до
# первой, ушедшей под линию сравнения), и вывести его заново по колонке PA_line —
# значит завести второе правило, совпадающее с первым не при всяком градиенте
# собственных значений. Маркер на scree-графике берёт ncomp_PCA отсюда.
write_csv_excel(
  data.frame(Metric = c("nfact_FA", "ncomp_PCA"), Value = c(pa$nfact, pa$ncomp)),
  "output/EFA/efa_parallel_analysis.csv")

eigenvalues <- eigen(cor(item_data, use = "pairwise.complete.obs"))$values

# Сохранение собственных значений для графиков
# Eigenvalue = собственные значения корреляционной матрицы (PCA), поэтому линию
# параллельного анализа берётся из компонентной ветки (pc.*), а не fa.*.
# sim = FALSE отключает симуляцию (pc.sim остаётся NA) и заполняет только
# ресэмплинг pc.simr; в дело идёт первое непустое из двух — не зависит от версии psych.
# Проверка на NA, а не на NULL: NA-вектор прошёл бы is.null() и PA_line ушла бы
# в CSV пустой, а plot_scree.R молча снял бы линию (has_pa = FALSE).
pa_pc   <- if (!all(is.na(pa$pc.simr))) pa$pc.simr else pa$pc.sim
pa_line <- if (!all(is.na(pa_pc))) pa_pc[seq_along(eigenvalues)] else NA_real_
write_csv_excel(data.frame(Factor = seq_along(eigenvalues), Eigenvalue = eigenvalues, PA_line = pa_line),
          "output/EFA/efa_eigenvalues.csv")

efa3 <- fa(item_data, nfactors = 3, rotate = "oblimin", fm = "ml", use = "pairwise")

cat("Факторные нагрузки (oblimin, cut=.3):\n")
print(efa3$loadings, cutoff = 0.30, sort = TRUE)

# Сохранение нагрузок EFA для тепловой карты
# Явный заголовок Item вместо безымянного индекса строк (иначе read_csv переименует в ...1)
efa_loadings_df <- as.data.frame(unclass(efa3$loadings))
efa_loadings_df <- cbind(Item = rownames(efa_loadings_df), efa_loadings_df)
write_csv_excel(efa_loadings_df, "output/EFA/efa_loadings.csv")

cat(sprintf("\nRMSEA = %.3f | TLI = %.3f | BIC = %.1f\n", efa3$RMSEA[1], efa3$TLI, efa3$BIC))
cat("Корреляции факторов (Phi):\n")
print(round(efa3$Phi, 3))
sink()

# Объект fa и собственные значения — на диск, как шаг 4 кладёт mod_rasch, а шаг 6
# mod_2pl. Это ВХОД для рисунков: scree и диаграмму факторов строят штатные
# psych::scree() и psych::fa.diagram(), которым нужен сам объект (или матрица
# корреляций), а не CSV с уже сведёнными числами. Слой рисунков при этом
# по-прежнему ничего не пересчитывает: модель оценена здесь, на шаге.
# Свод write_report_text.R файл не перечисляет: OMIT_PATTERNS покрывает
# "\\.RData$" как сохранённые объекты моделей.
efa_cor <- efa3$r   # корреляции Пирсона (phi), на которых оценён efa3 — см. D6
save(efa3, efa_cor, eigenvalues, pa_line, file = "output/EFA/efa_model.RData")

# =============================================================================
#  БЛОК 2: CFA
# =============================================================================

sink("output/CFA/cfa_results.txt", type = "output")
cat("=============================================================\n")
cat("  ПОДТВЕРЖДАЮЩИЙ ФАКТОРНЫЙ АНАЛИЗ (CFA) - n =", nrow(df), "\n")
cat("=============================================================\n\n")

cat("Прим.: индексы согласия — МАСШТАБИРОВАННЫЕ. estimator = \"WLSMV\" в lavaan — это\n")
cat("оценка DWLS с mean-and-variance-adjusted тестом (scaled.shifted), поэтому наивные\n")
cat("chisq/CFI/TLI/RMSEA из fitMeasures() посчитаны по нескорректированной статистике,\n")
cat("систематически оптимистичны и при этом тесте не интерпретируются, а наивный p не\n")
cat("определён вовсе. SRMR масштабирования не имеет и идёт как есть.\n\n")

# Ключи lavaan — масштабированные варианты (WLSMV = DWLS + scaled.shifted); имена
# элементов — короткие, под ними индексируются cat_fi() / fit_row() / get_fit_row().
FI_KEYS <- c(chisq          = "chisq.scaled",
             df             = "df.scaled",
             pvalue         = "pvalue.scaled",
             cfi            = "cfi.scaled",
             tli            = "tli.scaled",
             rmsea          = "rmsea.scaled",
             rmsea.ci.lower = "rmsea.ci.lower.scaled",
             rmsea.ci.upper = "rmsea.ci.upper.scaled",
             srmr           = "srmr")

# Индексы согласия под короткими именами FI_KEYS. Индексация по имени, а не по
# позиции: отсутствующий у lavaan ключ даёт NA, а не сдвиг всего набора.
fit_indices <- function(fit) {
  fi <- fitMeasures(fit, FI_KEYS)
  setNames(as.numeric(fi[FI_KEYS]), names(FI_KEYS))
}
na_indices <- function() setNames(rep(NA_real_, length(FI_KEYS)), names(FI_KEYS))

# Единая оценка модели для всех трёх спецификаций блока: жёсткая ошибка lavaan
# ловится (иначе шаг обрывается с открытым sink от cfa_results.txt — в автономном
# Rscript вывод остался бы перенаправленным до конца процесса), несходимость
# отдаётся флагом. Тот же контракт, что у БЛОКА 4 (get_fit_row) для этих же
# объектов: fitMeasures() на несошедшейся модели даёт правдоподобные, но
# недействительные индексы, поэтому проверка сходимости обязательна до вызова.
fit_cfa <- function(syntax, label) {
  f <- tryCatch(cfa(model = syntax, data = df, estimator = "WLSMV", ordered = TRUE),
       error = function(e) {
         cat(sprintf("  !! %s: lavaan упал: %s\n", label, conditionMessage(e))); NULL
       })
  if (is.null(f) || !isTRUE(lavInspect(f, "converged"))) list(fit = f, ok = FALSE)
  else list(fit = f, ok = TRUE)
}
# Печать шапки модели: числа — только для сошедшейся оценки.
cat_fi <- function(fi_v, ok) {
  if (ok) {
    cat(sprintf("  CFI = %.3f | TLI = %.3f | RMSEA = %.3f [%.3f; %.3f] | SRMR = %.3f\n\n",
                fi_v["cfi"], fi_v["tli"], fi_v["rmsea"],
                fi_v["rmsea.ci.lower"], fi_v["rmsea.ci.upper"], fi_v["srmr"]))
  } else {
    cat("  !! МОДЕЛЬ НЕ СОШЛАСЬ (оптимизатор не достиг сходимости) — fit-индексы недоступны.\n\n")
  }
}

# Ярлыки моделей — один источник на весь шаг: модель называется своей природой, а
# не позицией в своей таблице. Позиционный ярлык обозначал бы разные модели в
# таблице БЛОКА 3 (бифакторная CFA третьей строкой) и в таблице БЛОКА 4 (3F-ESEM
# третьей строкой), а обе попадают в свод соседними разделами. Имена ниже
# однозначны без нумерации, и модель, входящая в обе таблицы, несёт в них ОДИН
# ярлык.
MODEL_1F   <- "1F-CFA (однофакторная)"
MODEL_3F   <- "3F-ICM-CFA (3-факторная)"
MODEL_BI   <- "Бифакторная CFA"
MODEL_ESEM <- "3F-ESEM (geomin)"

# 1. Однофакторная
mono_syntax <- paste("Critical_Thinking =~", paste(item_cols, collapse = " + "))
f_1f <- fit_cfa(mono_syntax, MODEL_1F)
cfa_mono <- f_1f$fit; mono_ok <- f_1f$ok
fi_mono <- if (mono_ok) fit_indices(cfa_mono) else na_indices()

cat("── ОДНОФАКТОРНАЯ CFA (1F) ──────────────────────────────────\n")
cat_fi(fi_mono, mono_ok)

# 2. Трехфакторная
# Имена и число факторов приходят из SUBSCALES (то есть из input/items.csv), а не
# литералами: при литералах субшкала, добавленная в items.csv, осталась бы вне 3F и
# бифактора, тогда как 1F идёт по всему item_cols, — и обе таблицы согласия (БЛОКИ 3
# и 4) сравнивали бы модели на РАЗНЫХ наборах пунктов. Переименование субшкалы
# давало бы пустую правую часть синтаксиса, а жёсткую ошибку lavaan перехватывает
# fit_cfa() и отдаёт как несходимость, то есть ошибка конфигурации выглядела бы
# штатной деградацией (D3).
# Отсюда же проверка инварианта, на котором стоит сравнение моделей: набор пунктов
# у 1F, 3F, бифактора и ESEM один. Он ПРОВЕРЯЕТСЯ, а не предполагается — субшкалы
# обязаны покрывать item_cols целиком, а фактор с единственным индикатором в
# многофакторной модели не определён.
sub_avail <- lapply(SUBSCALES, function(v) intersect(v, item_cols))
local({
  uncovered <- setdiff(item_cols, unlist(sub_avail, use.names = FALSE))
  thin      <- names(sub_avail)[vapply(sub_avail, length, integer(1)) < 2L]
  if (length(uncovered) || length(thin))
    stop("[ERROR] состав субшкал не годится для 3-факторной и бифакторной моделей:",
         if (length(uncovered)) paste0("\n  вне субшкал остались пункты: ",
                                       paste(uncovered, collapse = ", ")) else "",
         if (length(thin)) paste0("\n  меньше двух пунктов в субшкале: ",
                                  paste(thin, collapse = ", ")) else "",
         "\n  Иначе 1F, 3F и бифактор оценивались бы на разных наборах пунктов, а их",
         "\n  индексы согласия попали бы в одну таблицу — исправьте input/items.csv.",
         call. = FALSE)
})
spec_lines <- vapply(names(sub_avail), function(g)
  sprintf("%s =~ %s", g, paste(sub_avail[[g]], collapse = " + ")), character(1))
cfa_syntax <- paste(spec_lines, collapse = "\n")
f_3f <- fit_cfa(cfa_syntax, MODEL_3F)
cfa_fit <- f_3f$fit; three_ok <- f_3f$ok
fi <- if (three_ok) fit_indices(cfa_fit) else na_indices()

cat("── 3-ФАКТОРНАЯ ICM-CFA (3F) ────────────────────────────────\n")
cat_fi(fi, three_ok)

# 3. Бифакторная
# Специфические факторы — те же строки, что в 3F, поэтому набор пунктов у обеих
# моделей один по построению. Ортогональность задаётся по ВСЕМ парам (общий фактор с
# каждым специфическим и специфические между собой), а не перечислением: при
# перечислении добавленная субшкала осталась бы коррелирующей с остальными, и модель
# перестала бы быть бифакторной, ничем этого не показав.
orth <- utils::combn(c("G", names(sub_avail)), 2,
                     function(p) sprintf("%s ~~ 0*%s", p[1], p[2]))
bifactor_syntax <- paste(
  c(paste0("G =~ ", paste(item_cols, collapse = " + ")), spec_lines, orth),
  collapse = "\n"
)
f_bi <- fit_cfa(bifactor_syntax, MODEL_BI)
cfa_bi <- f_bi$fit; bi_converged <- f_bi$ok

cat("── БИФАКТОРНАЯ CFA ─────────────────────────────────────────\n")
if (bi_converged) {
  fi_bi <- fit_indices(cfa_bi)
  cat_fi(fi_bi, TRUE)
} else {
  cat_fi(NULL, FALSE)
  cat("  Бифакторная модель на текущем наборе пунктов неустойчива; результаты ниже\n")
  cat("  приводятся только для однофакторной и 3-факторной ICM-CFA.\n\n")
}

# Сравнение. Отступ считается nchar(), а не шириной поля sprintf: у %-Ns ширина
# байтовая, и кириллический ярлык уехал бы влево.
cmp_row <- function(label, fi_v, ok) {
  pad <- strrep(" ", max(0, 26 - nchar(label)))
  if (ok)
    cat(sprintf("  %s%s CFI=%.3f  TLI=%.3f  RMSEA=%.3f  SRMR=%.3f\n",
                label, pad, fi_v["cfi"], fi_v["tli"], fi_v["rmsea"], fi_v["srmr"]))
  else
    cat(sprintf("  %s%s не сошлась — индексы недоступны\n", label, pad))
}
cat("── СРАВНЕНИЕ МОДЕЛЕЙ ───────────────────────────────────────\n")
cmp_row(MODEL_1F, fi_mono, mono_ok)
cmp_row(MODEL_3F, fi,      three_ok)
if (bi_converged) cmp_row(MODEL_BI, fi_bi, TRUE)
cat("\n")

# Нагрузки 3-факторной (только если модель сошлась — иначе оценки недействительны)
if (three_ok) {
  params <- parameterEstimates(cfa_fit, standardized = TRUE) %>%
    filter(op == "=~") %>%
    select(Factor = lhs, Item = rhs, B = est, SE = se, z = z, p = pvalue, Beta = std.all) %>%
    mutate(across(c(B, SE, z, Beta), ~ round(., 3)), p = round(p, 4))
  cat("── НАГРУЗКИ 3-ФАКТОРНАЯ (λ) ───────────────────────────────\n")
  print(as.data.frame(params), row.names = FALSE)

  # Сохранение нагрузок CFA для баров
  write_csv_excel(params, "output/CFA/cfa_loadings.csv")

  # Та же 3-факторная модель в компактной параметризации (Supplementary): только
  # стандартизованная нагрузка, по одной строке на пункт. Beta = std.all, то есть
  # est.std из standardizedSolution(), — второй оценки модели не требуется.
  loadings_compact <- params %>%
    select(subscale = Factor, item = Item, loading = Beta) %>%
    arrange(subscale, desc(loading))
  write_csv_excel(loadings_compact, "output/CFA/cfa_loadings_compact.csv")

  # Корреляции латентных факторов
  cat("\n── КОРРЕЛЯЦИИ МЕЖДУ ЛАТЕНТНЫМИ ФАКТОРАМИ ───────────────────\n")
  cors <- parameterEstimates(cfa_fit, standardized = TRUE) %>%
    filter(op == "~~", lhs != rhs, lhs %in% names(sub_avail))
  print(cors[, c("lhs", "rhs", "est", "std.all")], row.names = FALSE)
} else {
  cat("── НАГРУЗКИ 3-ФАКТОРНАЯ: пропущено (модель не сошлась) ─────\n")
  cat("   cfa_loadings.csv не записан: оценки несошедшейся модели недействительны.\n")
}

# Нагрузки бифакторной (только если модель сошлась)
if (bi_converged) {
  params_bi <- parameterEstimates(cfa_bi, standardized = TRUE) %>%
    filter(op == "=~") %>%
    select(Factor = lhs, Item = rhs, B = est, SE = se, z = z, p = pvalue, Beta = std.all) %>%
    mutate(across(c(B, SE, z, Beta), ~ round(., 3)), p = round(p, 4))
  cat("\n── НАГРУЗКИ БИФАКТОРНАЯ (λ) ───────────────────────────────\n")
  print(as.data.frame(params_bi), row.names = FALSE)

  # Сохранение бифакторных нагрузок для индексов
  write_csv_excel(params_bi, "output/CFA/cfa_bifactor_loadings.csv")
} else {
  cat("\n── НАГРУЗКИ БИФАКТОРНАЯ: пропущено (модель не сошлась) ────\n")
}

# Альфа Кронбаха
cat("\n── НАДЁЖНОСТЬ (α Кронбаха) ─────────────────────────────────\n")
for (s in names(SUBSCALES)) {
  cols_s <- intersect(SUBSCALES[[s]], names(df))
  if (length(cols_s) >= 2) {
    a <- psych::alpha(df[, cols_s])
    cat(sprintf("  %s: α = %.3f\n", s, a$total$raw_alpha))
  }
}
sink()

# lavaan fit-объекты сохраняются для рисования путевых диаграмм.
# Несошедшаяся модель НЕ сохраняется — иначе plot-скрипты нарисуют мусор; они читают
# объекты через exists() и корректно пропускают отсутствующий.
save_objs <- c(if (three_ok) "cfa_fit", if (bi_converged) "cfa_bi")
if (length(save_objs)) {
  save(list = save_objs, file = "output/CFA/cfa_fits.RData")
  if (!bi_converged)
    message("!! Бифакторная модель не сошлась — в cfa_fits.RData она не сохранена.")
  if (!three_ok)
    message("!! 3-факторная модель не сошлась — в cfa_fits.RData она не сохранена.")
} else {
  message("!! Ни одна CFA-модель не сошлась — cfa_fits.RData не записан.")
}

# cfa_mono/cfa_fit остаются в памяти — БЛОК 4 (ESEM) берёт их как 1F и 3F-ICM.

# =============================================================================
#  БЛОК 3: БИФАКТОРНЫЕ ИНДЕКСЫ И НАДЁЖНОСТЬ OMEGA
# =============================================================================

OUT_BI <- "output/Bifactor"

# Сводная таблица согласия трёх CFA для отчёта (модели уже оценены в блоке 2);
# ярлыки — те же MODEL_*, что в таблице БЛОКА 4, поэтому общая строка называется
# в обеих таблицах одинаково.
fit_row <- function(fi_v, label) {
  tibble(Model = label, Chisq = round(fi_v[["chisq"]], 2), df = fi_v[["df"]], p = round(fi_v[["pvalue"]], 4),
         CFI = round(fi_v[["cfi"]], 3), TLI = round(fi_v[["tli"]], 3), RMSEA = round(fi_v[["rmsea"]], 3),
         CI90 = sprintf("[%.3f;%.3f]", fi_v[["rmsea.ci.lower"]], fi_v[["rmsea.ci.upper"]]), SRMR = round(fi_v[["srmr"]], 3))
}
na_fit_row <- function(label) tibble(Model = label, Chisq = NA, df = NA, p = NA,
                                     CFI = NA, TLI = NA, RMSEA = NA, CI90 = "[NA;NA]", SRMR = NA)
fit_table <- bind_rows(
  if (mono_ok)      fit_row(fi_mono, MODEL_1F) else na_fit_row(MODEL_1F),
  if (three_ok)     fit_row(fi,      MODEL_3F) else na_fit_row(MODEL_3F),
  if (bi_converged) fit_row(fi_bi,   MODEL_BI) else na_fit_row(MODEL_BI)
)
write_csv_excel(fit_table, file.path(OUT_BI, "bifactor_fit_table.csv"))

# Бифакторные индексы (omega_h, ECV) из стандартизованных нагрузок бифакторной CFA
if (bi_converged) {
  lambda_wide <- params_bi %>% select(Factor, Item, Beta) %>%
    pivot_wider(names_from = Factor, values_from = Beta, values_fill = 0) %>%
    column_to_rownames("Item")
  g_loads  <- lambda_wide[, "G", drop = TRUE]
  spec_mat <- as.matrix(lambda_wide[, names(sub_avail), drop = FALSE])
  uniq_var <- pmax(0, 1 - rowSums(lambda_wide^2))
  # знаменатель ω_h: вклад специфических факторов — (Σλ_k)² по каждому фактору, не Σλ²
  spec_var <- sum(vapply(colnames(spec_mat), function(s) sum(spec_mat[, s])^2, numeric(1)))
  omega_h_manual <- sum(g_loads)^2 / (sum(g_loads)^2 + spec_var + sum(uniq_var))
  ecv_manual     <- sum(g_loads^2) / (sum(g_loads^2) + sum(spec_mat^2))
  # BifactorIndicesCalculator обязателен (install_deps.R гарантирует наличие).
  # Вызов прямой; ошибка — фатальна (re-raise), а не молчаливый NULL.
  bi_indices <- tryCatch(
    BifactorIndicesCalculator::bifactorIndices(as.matrix(lambda_wide)),
    error = function(e) stop(sprintf(
      "BifactorIndicesCalculator::bifactorIndices упал: %s", conditionMessage(e)),
      call. = FALSE)
  )
} else {
  omega_h_manual <- NA
  ecv_manual     <- NA
  bi_indices     <- NULL
}

# Надёжность Omega (psych, тетрахорическая матрица).
# Матрица берётся БЕЗ автосглаживания (smooth = FALSE) и сглаживается явно, потому что
# факт сглаживания — содержательная оговорка отчёта. psych сообщает о нём warning'ом
# в stderr, в sink() лога он не попадает, а report.Rmd не должен держать эту оговорку
# в прозе (как m3cfa_ok/bf2pl_ok, статус читается из артефакта).
# Ошибка оценки приходит условием, поэтому tryCatch делает fallback на polychoric().
tet_raw <- tryCatch(
  tetrachoric(item_data, smooth = FALSE)$rho,
  error = function(e) polychoric(item_data, smooth = FALSE)$rho)
tet_min_eig <- min(eigen(tet_raw, symmetric = TRUE, only.values = TRUE)$values)
tet_pd      <- tet_min_eig > .Machine$double.eps
tet_rho     <- if (tet_pd) tet_raw else psych::cor.smooth(tet_raw)
write_csv_excel(
  data.frame(Matrix = "tetrachoric", MinEigenvalue = round(tet_min_eig, 6),
             PositiveDefinite = tet_pd, Smoothed = !tet_pd),
  file.path(OUT_BI, "omega_matrix_status.csv"))
om_total <- tryCatch(omega(tet_rho, nfactors = 3, plot = FALSE, rotate = "oblimin"),
                     error = function(e) tryCatch(omega(tet_rho, nfactors = 1, plot = FALSE), error = function(e2) NULL))
alpha_raw_total <- psych::alpha(item_data)$total$raw_alpha
ord_alpha_total <- psych::alpha(tet_rho)$total$raw_alpha

SEP <- strrep("=", 70)
sink(file.path(OUT_BI, "bifactor_cfa_results.txt"))
cat(SEP, "\n"); cat("  БИФАКТОРНАЯ CFA - ТКМ-Хим | n =", nrow(df), "\n"); cat(SEP, "\n\n")
cat("СРАВНЕНИЕ МОДЕЛЕЙ (масштабированные индексы согласия, WLSMV)\n\n")
print(as.data.frame(fit_table), row.names = FALSE)
cat(sprintf("\nБИФАКТОРНЫЕ ИНДЕКСЫ:\n  omega_h = %.3f\n  ECV     = %.3f\n", omega_h_manual, ecv_manual))
if (!is.null(bi_indices)) {
  cat("\nИндексы (BifactorIndicesCalculator):\n")
  print(bi_indices)
}
if (bi_converged) {
  cat("\nБИФАКТОРНЫЕ НАГРУЗКИ\n\n"); print(as.data.frame(params_bi), row.names = FALSE)
} else {
  cat("\nБИФАКТОРНЫЕ НАГРУЗКИ: недоступны (модель не сошлась)\n")
}
sink()

sink(file.path(OUT_BI, "omega_reliability.txt"))
cat(SEP, "\n"); cat("  НАДЁЖНОСТЬ OMEGA - ТКМ-Хим\n\n")
cat(sprintf("  Тетрахорическая матрица: min eigenvalue = %.6f, положительно определена: %s\n",
            tet_min_eig, if (tet_pd) "да" else "нет"))
if (!tet_pd) cat("  Выполнено сглаживание (psych::cor.smooth) перед расчётом omega.\n")
cat("\n")
if (!is.null(om_total)) {
  cat(sprintf("  omega_t = %.3f\n  omega_h = %.3f\n  alpha_ord = %.3f\n  alpha_raw = %.3f\n",
              om_total$omega.tot, om_total$omega_h, ord_alpha_total, alpha_raw_total))
} else {
  cat(sprintf("  alpha_ord = %.3f\n  alpha_raw = %.3f\n  omega не сошлась\n", ord_alpha_total, alpha_raw_total))
}
sink()

# =============================================================================
#  БЛОК 4: ESEM (Exploratory Structural Equation Modeling)
# =============================================================================
#  ESEM = гибрид EFA и CFA. Трёхфакторная структура оценивается как в CFA (WLSMV,
#  ordered — те же настройки, что в блоке 2), но БЕЗ жёсткого зануления
#  кросс-нагрузок: все пункты грузятся на все факторы, а решение вращается
#  (geomin, косоугольное). Это снимает главный дефект ICM-CFA (запрет
#  кросс-нагрузок), из-за которого 3-факторная CFA систематически недооценивает
#  согласие и завышает корреляции факторов.
#
#  Три диагностики, которые ICM-CFA дать не может. Каждая измеряет своё, и вердикт
#  о размерности по ним не выносится (DECISIONS.md D3, D12):
#    (1) РАЗМЕРНОСТЬ — сравнение согласия 1F-CFA vs 3F-ICM-CFA vs 3F-ESEM;
#        единственная из трёх, относящаяся к числу измерений;
#    (2) РАЗЛИЧИМОСТЬ факторов вращения — их косоугольные корреляции: близость к 1
#        означает, что факторы эмпирически неразличимы;
#    (3) ВОСПРОИЗВЕДЕНИЕ АПРИОРНОЙ приписки пункт -> субшкала: сколько пунктов
#        грузятся на "свой" фактор и сколько дают кросс-нагрузки. Это согласие двух
#        РАЗМЕТОК, а не размерность; информативно лишь на выборке, где субшкалы
#        вообще различимы (дезаттенюированная r — B0 в
#        analysis/subscale_assignment.md).
#
#  Выход: output/ESEM/{esem_fit_table.csv, esem_loadings.csv,
#         esem_factor_correlations.csv, esem_results.txt}.
# =============================================================================
OUT_ESEM <- "output/ESEM"
ensure_dir(OUT_ESEM)
set.seed(42)   # ESEM детерминирован (WLSMV+geomin); зерно фиксировано ради воспроизводимости
N_FAC <- length(SUBSCALES)   # число субшкал в схеме (сейчас 3)

# Те же 1F и 3F-ICM CFA, что оценены в БЛОКЕ 2 (в памяти этой сессии), поэтому в
# таблице ниже они несут те же ярлыки MODEL_1F/MODEL_3F, что в таблице БЛОКА 3.
fit_1f <- cfa_mono   # однофакторная (Critical_Thinking)
fit_3f <- cfa_fit    # 3-факторная ICM-CFA

# ── ESEM: один EFA-блок из N_FAC факторов, все пункты на все факторы, ─────────
#    косоугольное вращение geomin. Каждый фактор помечен одним и тем же именем
#    блока efa("esem") — так lavaan вращает их вместе как единый EFA-блок.
esem_syntax <- paste(
  vapply(seq_len(N_FAC), function(k) {
    sprintf('efa("esem")*f%d =~ %s', k, paste(item_cols, collapse = " + "))
  }, character(1)),
  collapse = "\n"
)
fit_esem <- tryCatch(
  cfa(
    esem_syntax, data = df, estimator = "WLSMV", ordered = TRUE,
    rotation = "geomin"
  ),
  error = function(e) { message("  !! ESEM: ошибка оценки: ", conditionMessage(e)); NULL }
)
esem_ok <- !is.null(fit_esem) && isTRUE(lavInspect(fit_esem, "converged"))

# ── Таблица согласия: 1F-CFA vs 3F-ICM-CFA vs 3F-ESEM ────────────────────────
get_fit_row <- function(fit, label) {
  if (is.null(fit) || !isTRUE(lavInspect(fit, "converged"))) {
    return(tibble(Model = label, Chisq = NA, df = NA, p = NA, CFI = NA,
                  TLI = NA, RMSEA = NA, CI90 = "[NA;NA]", SRMR = NA))
  }
  fi_e <- fit_indices(fit)   # те же масштабированные ключи FI_KEYS, что в БЛОКЕ 2
  tibble(Model = label, Chisq = round(fi_e["chisq"], 2), df = fi_e["df"],
         p = round(fi_e["pvalue"], 4), CFI = round(fi_e["cfi"], 3),
         TLI = round(fi_e["tli"], 3), RMSEA = round(fi_e["rmsea"], 3),
         CI90 = sprintf("[%.3f;%.3f]", fi_e["rmsea.ci.lower"], fi_e["rmsea.ci.upper"]),
         SRMR = round(fi_e["srmr"], 3))
}
esem_fit_table <- bind_rows(
  get_fit_row(fit_1f,   MODEL_1F),
  get_fit_row(fit_3f,   MODEL_3F),
  get_fit_row(fit_esem, MODEL_ESEM)
)

# ── Нагрузки ESEM, привязка факторов к субшкалам, корреляции факторов ─────────
if (esem_ok) {
  std <- parameterEstimates(fit_esem, standardized = TRUE)

  load_long <- std %>% filter(op == "=~") %>%
    select(Factor = lhs, Item = rhs, Loading = std.all)

  # Матрица нагрузок (пункты x факторы f1..fk), в порядке ITEMS
  lambda <- load_long %>%
    pivot_wider(names_from = Factor, values_from = Loading) %>%
    arrange(match(Item, item_cols))
  fac_cols <- setdiff(names(lambda), "Item")

  # Вращение выдаёт факторы в произвольном порядке. Каждый ESEM-фактор привязывается
  # к субшкале жадно: по среднему |нагрузки| пунктов субшкалы на факторе (сначала
  # самое сильное соответствие, затем занятые фактор и субшкала снимаются).
  sub_names <- names(SUBSCALES)
  M <- matrix(NA_real_, nrow = length(fac_cols), ncol = length(sub_names),
              dimnames = list(fac_cols, sub_names))
  for (f in fac_cols) for (s in sub_names) {
    its <- intersect(SUBSCALES[[s]], lambda$Item)
    M[f, s] <- mean(abs(lambda[[f]][match(its, lambda$Item)]), na.rm = TRUE)
  }
  assign_fac <- setNames(rep(NA_character_, length(sub_names)), sub_names)  # subscale -> factor col
  Mtmp <- M
  for (i in seq_len(min(length(fac_cols), length(sub_names)))) {
    idx <- which(Mtmp == max(Mtmp, na.rm = TRUE), arr.ind = TRUE)[1, ]
    f <- rownames(Mtmp)[idx[1]]; s <- colnames(Mtmp)[idx[2]]
    assign_fac[s] <- f
    Mtmp[f, ] <- -Inf; Mtmp[, s] <- -Inf
  }

  # Колонки нагрузок переупорядочиваются и переименовываются так, чтобы порядок совпал
  # с субшкалами (S1->первая колонка и т.д.); заголовок несёт и субшкалу.
  ordered_facs <- assign_fac[sub_names]
  new_names <- sprintf("%s (~%s)", ordered_facs, sub_names)
  lambda_out <- lambda[, c("Item", ordered_facs)]
  names(lambda_out) <- c("Item", new_names)

  # Корреляции факторов (косоугольное решение), с ярлыками субшкал
  fcorr <- std %>% filter(op == "~~", lhs %in% fac_cols, rhs %in% fac_cols, lhs != rhs) %>%
    select(f1 = lhs, f2 = rhs, r = std.all) %>%
    mutate(
      s1 = sub_names[match(f1, ordered_facs)],
      s2 = sub_names[match(f2, ordered_facs)],
      Pair = sprintf("%s ~ %s", s1, s2),
      r = round(r, 3)
    ) %>% select(Pair, r)

  # Диагностики простой структуры
  lam_abs <- as.matrix(abs(lambda[, fac_cols]))
  target_col <- vapply(lambda$Item, function(it) {
    s <- ITEM_SUB[[it]]; assign_fac[[s]]
  }, character(1))
  target_idx <- match(target_col, fac_cols)
  dominant_idx <- max.col(lam_abs, ties.method = "first")
  pct_on_target <- mean(dominant_idx == target_idx) * 100
  # кросс-нагрузка: |lambda| >= .30 на не-целевом факторе
  cross_flags <- vapply(seq_len(nrow(lam_abs)), function(i) {
    any(lam_abs[i, -target_idx[i]] >= 0.30)
  }, logical(1))
  n_cross <- sum(cross_flags)
  max_fcorr <- max(abs(fcorr$r))

  # Пустой фактор — тот, который не доминирует НИ У ОДНОГО пункта. Критерий взят
  # по построению pct_on_target (доля считается по доминированию), а не по
  # абсолютному порогу |lambda|: фактор с одной заметной нагрузкой порог прошёл бы,
  # ничьей целью при этом не будучи. Привязка фактор -> субшкала полная по
  # построению, поэтому пустой фактор всё равно получает субшкалу по остатку и
  # обнуляет её вклад в долю.
  vacuous_facs <- setdiff(fac_cols, fac_cols[unique(dominant_idx)])

  # Разрешающая способность выборки — та же проверка, что B0 в
  # analysis/subscale_assignment.md: дезаттенюированная корреляция субшкал. При
  # r_disatt >= SEP_THRESHOLD субшкалы с точностью до надёжности измеряют одно и
  # то же, и доля пунктов на «своём» факторе о размерности не свидетельствует.
  SEP_THRESHOLD <- 0.90
  sub_scores <- sapply(names(SUBSCALES), function(s)
    rowSums(df[, intersect(SUBSCALES[[s]], names(df)), drop = FALSE]))
  sub_alpha <- vapply(names(SUBSCALES), function(s) {
    cols_s <- intersect(SUBSCALES[[s]], names(df))
    psych::alpha(as.data.frame(df[, cols_s]))$total$raw_alpha
  }, numeric(1))
  r_disatt <- cor(sub_scores) / sqrt(outer(sub_alpha, sub_alpha))
  max_disatt <- max(r_disatt[upper.tri(r_disatt)])
  subscales_separable <- max_disatt < SEP_THRESHOLD

  # Прирост согласия 3F-ESEM над 1F-CFA — единственная из трёх диагностик,
  # относящаяся к размерности. Строки esem_fit_table: 1 = 1F-CFA, 3 = 3F-ESEM.
  d_cfi   <- esem_fit_table$CFI[3]   - esem_fit_table$CFI[1]
  d_rmsea <- esem_fit_table$RMSEA[3] - esem_fit_table$RMSEA[1]
} else {
  lambda_out <- tibble()
  fcorr <- tibble()
  pct_on_target <- NA_real_; n_cross <- NA_integer_; max_fcorr <- NA_real_
}

# ── Запись артефактов ESEM ────────────────────────────────────────────────────
write_csv_excel(esem_fit_table, file.path(OUT_ESEM, "esem_fit_table.csv"))
write_csv_excel(lambda_out,     file.path(OUT_ESEM, "esem_loadings.csv"))
write_csv_excel(fcorr,          file.path(OUT_ESEM, "esem_factor_correlations.csv"))

sink(file.path(OUT_ESEM, "esem_results.txt"))
cat(SEP, "\n"); cat("  ESEM (geomin, WLSMV, ordered) - ТКМ-Хим | n =", nrow(df), "\n"); cat(SEP, "\n\n")

cat("СРАВНЕНИЕ МОДЕЛЕЙ (масштабированные индексы согласия, WLSMV)\n\n")
print(as.data.frame(esem_fit_table), row.names = FALSE)
cat("\nЧитается так: если 3F-ESEM улучшает согласие относительно 1F-CFA лишь\n")
cat("незначительно, дополнительные факторы почти ничего не объясняют сверх\n")
cat("общего — довод в пользу одномерности. Заметный прирост CFI/TLI и падение\n")
cat("RMSEA/SRMR у 3F-ESEM — довод против.\n\n")

if (esem_ok) {
  cat(SEP, "\n"); cat("НАГРУЗКИ ESEM (стандартизованные, geomin; |λ|>=.30 — простая структура)\n\n")
  lo <- lambda_out
  numcols <- setdiff(names(lo), "Item")
  lo[numcols] <- lapply(lo[numcols], function(x) round(x, 3))
  print(as.data.frame(lo), row.names = FALSE)

  cat("\n", SEP, "\n", sep = ""); cat("КОРРЕЛЯЦИИ ФАКТОРОВ (косоугольное решение)\n\n")
  print(as.data.frame(fcorr), row.names = FALSE)

  cat("\n", SEP, "\n", sep = ""); cat("ДИАГНОСТИКИ СТРУКТУРЫ\n\n")

  cat("  (1) РАЗМЕРНОСТЬ — прирост согласия 3F-ESEM над 1F-CFA:\n")
  if (is.na(d_cfi) || is.na(d_rmsea)) {
    cat("      недоступен: 1F-CFA или 3F-ESEM не сошлась, индексы NA.\n")
  } else {
    cat(sprintf("      dCFI = %+.3f, dRMSEA = %+.3f (ориентиры заметного прироста: >= +0.010 и <= -0.015).\n",
                d_cfi, d_rmsea))
  }
  cat("      Единственная из трёх диагностик, относящаяся к размерности.\n\n")

  cat(sprintf("  (2) РАЗЛИЧИМОСТЬ ФАКТОРОВ вращения — максимальная |корреляция факторов|: %.3f.\n\n",
              max_fcorr))

  cat(sprintf("  (3) ВОСПРОИЗВЕДЕНИЕ АПРИОРНОЙ ПРИПИСКИ пункт -> субшкала: %.0f%% (%d из %d)\n",
              pct_on_target, round(pct_on_target / 100 * nrow(lambda_out)), nrow(lambda_out)))
  cat(sprintf("      пунктов с максимальной нагрузкой на 'свой' фактор; кросс-нагрузок |λ|>=.30\n"))
  cat(sprintf("      на чужом факторе: %d из %d.\n", n_cross, nrow(lambda_out)))
  cat("      Мера согласия ДВУХ РАЗМЕТОК (объявленной и полученной вращением), а не\n")
  cat("      размерности: низкая доля одинаково совместима с одним фактором, с тремя\n")
  cat("      факторами не по объявленным границам и со слабым восстановлением вращения.\n\n")

  cat(sprintf("  Разрешающая способность выборки: max дезаттенюированная r субшкал = %.3f (порог %.2f).\n",
              max_disatt, SEP_THRESHOLD))
  if (!subscales_separable) {
    cat("  Субшкалы на этой выборке НЕРАЗЛИЧИМЫ: с точностью до надёжности измеряют одно и\n")
    cat("  то же. Диагностика (3) здесь не информативна ни в одну сторону — это ОГРАНИЧЕНИЕ\n")
    cat("  выборки, а не находка. Та же проверка и та же причина (бимодальность балла,\n")
    cat("  шаг 2) — B0 в analysis/subscale_assignment.md.\n")
  }
  if (length(vacuous_facs)) {
    cat(sprintf("  Факторов, не доминирующих ни у одного пункта: %d (%s). Привязка фактор ->\n",
                length(vacuous_facs), paste(vacuous_facs, collapse = ", ")))
    cat("  субшкала полная по построению, поэтому такой фактор получает субшкалу по остатку\n")
    cat("  и обнуляет её вклад в (3).\n")
  }

  cat("\n  Вердикт о размерности здесь НЕ выносится: вопрос зафиксирован открытым в обе\n")
  cat("  стороны (DECISIONS.md D3), и ни одна из диагностик выше его не закрывает.\n")
} else {
  cat("!! ESEM НЕ СОШЁЛСЯ (оптимизатор не достиг сходимости).\n")
  cat("   Несходимость многомерной модели сама по себе указывает на почти\n")
  cat("   одномерную структуру: данным не хватает информации, чтобы отделить\n")
  cat("   несколько факторов друг от друга.\n")
}
sink()

# Диагностика сходимости ESEM. Числа сошедшейся модели (доля пунктов на «своём»
# факторе, кросс-нагрузки, max|r|) не дублируются в консоль: их пишет блок
# ДИАГНОСТИКИ СТРУКТУРЫ в ESEM/esem_results.txt.
if (!esem_ok) {
  message("ESEM: модель не сошлась — результат записан (довод в пользу одномерности).")
}
