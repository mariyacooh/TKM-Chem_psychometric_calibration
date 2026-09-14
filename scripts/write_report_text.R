#!/usr/bin/env Rscript
# =============================================================================
#  write_report_text.R — машиночитаемый свод результатов: output/report.md и
#  output/report.json. Те же числа, что в output/report.html, но без
#  рисунков и без построчных данных респондентов.
# =============================================================================
#  Назначение — сверка черновика статьи с фактическими результатами прогона:
#  один файл на ~200 КБ вместо ~4 МБ report.html, где 99% объёма — встроенные
#  PNG. Оба выходных файла собираются за один проход по манифесту, поэтому
#  разойтись между собой не могут.
#
#  СЛОЙ ПЕРЕНОСА, НЕ РАСЧЁТА. Ни одно число здесь не вычисляется: скрипт
#  переносит содержимое артефактов output/ как есть. Производная статистика
#  (среднее, SD, доля) считалась бы вторым источником правды и могла бы
#  разойтись с шагом-производителем, поэтому её здесь нет — описательные
#  сводки берутся из тех артефактов, где их посчитал сам пайплайн (например
#  описательная статистика теты — Rasch/rasch_report.txt).
#
#  Числа в markdown форматируются format(trim = TRUE) — тем же вызовом, что
#  write_csv_excel() в scripts/_setup.R, при тех же options(digits = 7). Отсюда
#  текст ячейки в .md совпадает с текстом ячейки в .csv посимвольно.
#
#  Запуск (после полного прогона):  Rscript scripts/write_report_text.R
# =============================================================================

# Рабочая директория — корень проекта (этот файл лежит в scripts/).
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

source("scripts/_setup.R")   # options(digits = 7, ...), load_pkgs(), require_input()
source("scripts/config.R")   # ITEMS / SUBSCALES / EXCLUDED_ITEMS / ABSENT_ITEMS

load_pkgs(c("readr", "readxl", "jsonlite"))

OUT <- "output"
MD   <- file.path(OUT, "report.md")
JSON <- file.path(OUT, "report.json")

# Жёсткие входы, как у report.Rmd: без матрицы ответов описывать нечего, а без
# сводки отбора нечем подписать выборку — N берётся оттуда, а не выводится nrow().
require_input(file.path(OUT, c("cleaned_responses.csv", "preprocess_summary.csv")),
              "scripts/0_preprocess.R (шаг 0)")

# =============================================================================
#  МАНИФЕСТ: что попадает в свод
# =============================================================================
#  Порядок секций повторяет scripts/report.Rmd, чтобы раздел свода отображался
#  на раздел статьи. Поля записи:
#    path   путь относительно output/; он же ключ в report.json
#    kind   "csv" | "xlsx" (все листы) | "log" (текстовая выдача целиком)
#    title  подпись
#    md     "full" (по умолчанию) | "none" — запись идёт только в JSON
#    filter функция(df) -> df, сокращающая ТОЛЬКО markdown-версию; в JSON
#           таблица всегда полная
#    note   оговорка, печатается под подписью
#
#  Добавление артефакта в пайплайн требует строки здесь: непокрытый файл
#  output/ роняет прогон (проверка охвата ниже), а не выпадает из свода молча.

sec <- function(title, ..., intro = NULL) {
  list(title = title, intro = intro, entries = list(...))
}
art <- function(path, kind, title, md = "full", filter = NULL, note = NULL) {
  list(path = path, kind = kind, title = title, md = md, filter = filter, note = note)
}

# Шаги 4, 6 и 7 отсеивают пункты нулевой дисперсии независимо друг от друга, поэтому
# артефакт отсева у каждого свой. Пустая таблица — штатный результат: значит набор
# пунктов анализа (26) вошёл в шаг целиком.
ZERO_VAR_NOTE <- paste0(
  "Пустая таблица означает, что не снят ни один пункт. Отсев считается в каждом шаге ",
  "заново, поэтому наборы могут различаться между шагами 4, 6 и 7.")

# omega_h на подвыборках шага 2 скачет в диапазоне 0.16-0.81 без закономерности:
# объёмы для иерархической omega малы. Колонка остаётся именно как свидетельство
# этой нестабильности, а не как измерение.
OMEGA_H_NOTE <- paste0(
  "Колонка omega_h интерпретации не подлежит: на этих объёмах она нестабильна и ",
  "приведена как свидетельство нестабильности. Полновыборочное значение — в ",
  "Bifactor/omega_reliability.txt.")

# Рефиты шага 2 проходят тот же гейт сходимости, что модели шагов 3, 4 и 6
# (a_mean_2pl() и wle_rel() в 2_bimodality.R), поэтому статус несут колонками, а
# снятое число стоит пустым, а не значением несошедшейся оценки.
CONV_NOTE <- paste0(
  "Колонки Conv_2PL и Conv_WLE — сходимость рефита на подвыборке (пусто = оценки ",
  "нет вовсе). При FALSE соответствующее число (a_mean, WLE_rel) снято и стоит ",
  "пустым: несошедшаяся оценка оценкой не является и в градиенте не участвует.")

SECTIONS <- list(
  sec("Статус моделей",
      intro = paste0("Сходимость и состояние матриц — из артефактов шагов 3 и 6. ",
                     "Индексы согласия бифакторной CFA пишутся только при ",
                     "сходимости, поэтому NA в её строке в таблице CFA — это ",
                     "результат, а не пропуск."),
      art("2PL/2pl_model_status.csv", "csv", "Сходимость базовых 1PL и 2PL"),
      art("Bifactor/omega_matrix_status.csv", "csv",
          "Тетрахорическая матрица при расчёте omega: min eigenvalue, сглаживание")),

  sec("Выборка и структура теста",
      art("preprocess_summary.csv", "csv", "Отбор выборки: строки выгрузки, отсев, итог",
          note = paste0("Единственный источник чисел по отсеву: сколько строк пришло ",
                        "из Google Forms, сколько снято как пустые, сколько осталось.")),
      art("Descriptive/descriptive_stats_tables.csv", "csv",
          "Распределение по языку формы, курсу, конференциям, региону")),

  sec("Описательная статистика",
      art("Descriptive/descriptive_stats_detailed.csv", "csv",
          "Пункты и итоговые баллы: N, M, SD, квартили, форма распределения",
          note = paste0("Строки Score_* — общий балл и субшкалы, строки Q* — пункты. ",
                        "Shapiro-Wilk по дихотомическим пунктам не считался (NA).")),
      art("Descriptive/descriptive_stats_by_course.csv", "csv", "Баллы по курсам"),
      art("Descriptive/descriptive_stats.txt", "log", "Развёрнутая описательная статистика")),

  sec("Бимодальность выборки",
      intro = paste0("Шаг 2 измеряет разброс выборки ДО моделей: коэффициенты ",
                     "внутренней согласованности растут с ним, поэтому флаг ниже ",
                     "определяет, как читаются alpha и omega шага 3. Границы ",
                     "подвыборок иллюстративны — естественного порога нет, и ",
                     "результатом является сам градиент, а не значение на одной ",
                     "границе."),
      art("Bimodality/extreme_patterns.csv", "csv",
          "Крайние паттерны, коэффициент бимодальности, тест провала и флаг",
          note = paste0("Bimodal = BC > 5/9. Порог пола выведен из уровня угадывания ",
                        "Binom(k, 1/4), потолок — из состава теста (k - 2); оба ",
                        "записаны колонками Floor_cut и Ceiling_cut. Dip_p = 0 означает ",
                        "предел таблицы квантилей теста Хартигана (D выше наибольшего ",
                        "табулированного), а не нулевую вероятность.")),
      art("Bimodality/score_total_frequency.csv", "csv",
          "Частоты по каждому значению суммарного балла",
          note = "Сетка полная (0..k), поэтому пустой нижний хвост виден нулями."),
      art("Bimodality/subsample_sensitivity.csv", "csv",
          "Надёжность и согласие CFA при усечении крайних баллов",
          note = paste(OMEGA_H_NOTE, CONV_NOTE)),
      art("Bimodality/subsample_known_groups.csv", "csv",
          "Валидность по известным группам на тех же подвыборках",
          note = paste0("Отсутствие значимости (ISSUES.md 1.4) не создано усечением ",
                        "и не устраняется им.")),
      art("Bimodality/cut_sensitivity.csv", "csv",
          "Градиент показателей по границам среза (три развёртки)",
          note = paste(OMEGA_H_NOTE, CONV_NOTE)),
      art("Bimodality/cut_monotonicity.csv", "csv",
          "Монотонность показателей по развёрткам среза и охват развёрток",
          note = paste0("Монотонность ИЗМЕРЕНА по таблице выше, а не заявлена: ",
                        "Monotone = TRUE, если метрика меняется в одну сторону в КАЖДОЙ ",
                        "из трёх развёрток; пустое поле — метрика не оценивалась ни в ",
                        "одной развёртке. N_assessed / N_points — охват: сколько точек ",
                        "трёх развёрток оценено из общего их числа. Вердикт относится к ",
                        "ОЦЕНЁННЫМ точкам, и читается это асимметрично: пропуск способен ",
                        "создать TRUE (уцелевшие точки сравниваются как соседние), но не ",
                        "FALSE — немонотонность, установленная на подмножестве, ",
                        "установлена и для всей развёртки. omega_h в проверку не входит ",
                        "(см. оговорку выше). По этому артефакту ветвятся подпись ",
                        "рисунка cut_sensitivity и проза report.html.")),
      art("Bimodality/bimodality_report.txt", "log",
          "Развёрнутая выдача: распределение, мера бимодальности, подвыборки, градиент")),

  sec("Дистракторный анализ (CTT)",
      intro = paste0("Буквы вариантов раздаются внутри своей языковой формы: ",
                     "A — верный вариант формы по ключу, B в KZ и B в RU — разные ",
                     "дистракторы. Доли и корреляции считаны по выборке своей формы."),
      art("Distractor/distractor_pb_table.csv", "csv",
          "Доля выбора и точечно-бисериальная (item-rest) по каждому варианту")),

  sec("Разведочный факторный анализ (EFA)",
      art("EFA/efa_eigenvalues.csv", "csv", "Собственные значения и линия параллельного анализа"),
      art("EFA/efa_parallel_analysis.csv", "csv",
          "Параллельный анализ: число факторов (FA-ветка) и число компонент (PCA-ветка)",
          note = paste0("Единственный источник обоих чисел: ncomp_PCA — то же значение, ",
                        "что маркер «PA: N components» на scree-графике. Линия сравнения ",
                        "лежит колонкой PA_line в EFA/efa_eigenvalues.csv.")),
      art("EFA/efa_loadings.csv", "csv", "Факторные нагрузки (ML, oblimin, 3 фактора)"),
      art("EFA/efa_results.txt", "log", "Развёрнутая выдача EFA (KMO, Бартлетт, объяснённая дисперсия)")),

  sec("Подтверждающий факторный анализ (CFA)",
      art("Bifactor/bifactor_fit_table.csv", "csv",
          "Согласие: 1F-CFA vs 3F-ICM-CFA vs бифакторная CFA",
          note = paste0("Индексы масштабированные (WLSMV = DWLS + scaled.shifted): ",
                        "chisq/df/p и CFI/TLI/RMSEA — из *.scaled, SRMR — как есть. ",
                        "Модель названа своей природой: ярлык 1F-CFA и 3F-ICM-CFA ",
                        "означает в таблице ESEM ниже ТУ ЖЕ модель, что здесь.")),
      art("CFA/cfa_loadings.csv", "csv",
          "Нагрузки 3-факторной ICM-CFA: B, SE, z, p, стандартизованная Beta"),
      art("CFA/cfa_loadings_compact.csv", "csv",
          "Те же нагрузки 3-факторной ICM-CFA компактно: субшкала, пункт, Beta"),
      art("CFA/cfa_results.txt", "log", "Развёрнутая выдача CFA",
          note = paste0("Единственный источник альф Кронбаха по субшкалам (блок ",
                        "НАДЁЖНОСТЬ) — в CSV они не выведены."))),

  sec("ESEM",
      art("ESEM/esem_fit_table.csv", "csv",
          "Согласие: 1F-CFA vs 3F-ICM-CFA vs 3F-ESEM (масштабированные индексы, WLSMV)"),
      art("ESEM/esem_factor_correlations.csv", "csv", "Корреляции факторов ESEM (geomin, косоугольное)"),
      art("ESEM/esem_loadings.csv", "csv", "Нагрузки ESEM (стандартизованные, geomin)"),
      art("ESEM/esem_results.txt", "log", "Развёрнутая выдача ESEM")),

  sec("Надёжность и бифакторная модель",
      art("2PL/2pl_bifactor_indices.csv", "csv", "Индексы бифакторной 2PL: omega_h, omega_total, ECV, PUC"),
      art("2PL/2pl_bifactor_subscale_omega.csv", "csv",
          "Надёжность субшкал: omega_s (полная) и omega_hs (за вычетом общего фактора)"),
      art("2PL/2pl_bifactor_loadings.csv", "csv", "Нагрузки бифакторной 2PL (G и специфические S1-S3)"),
      art("CFA/cfa_bifactor_loadings.csv", "csv",
          "Нагрузки бифакторной CFA: B, SE, z, p, стандартизованная Beta",
          note = paste0("Условный артефакт: шаг 3 пишет его только при сходимости ",
                        "бифакторной CFA. На канонических данных она не сходится, и ",
                        "он штатно отсутствует.")),
      art("Bifactor/omega_reliability.txt", "log", "Расчёт omega по тетрахорической матрице",
          note = "Единственный источник alpha_ord и alpha_raw по всему тесту."),
      art("Bifactor/bifactor_cfa_results.txt", "log", "Бифакторная CFA: сравнение моделей и индексы")),

  sec("Модель Раша (1PL)",
      art("Rasch/rasch_item_report.csv", "csv", "Трудность пунктов и Infit/Outfit MSQ"),
      art("Rasch/rasch_excluded_items.csv", "csv",
          "Пункты, снятые по нулевой дисперсии перед калибровкой Раша",
          note = ZERO_VAR_NOTE),
      art("Rasch/rasch_report.txt", "log", "Развёрнутая выдача Раша",
          note = paste0("Содержит надёжность WLE, Deviance/AIC/BIC и описательную ",
                        "статистику теты (WLE) — построчные теты в свод не входят."))),

  sec("2PL IRT",
      art("2PL/2pl_params.csv", "csv", "Параметры 2PL: дискриминация a, трудность b, доля верных"),
      art("2PL/2pl_excluded_items.csv", "csv",
          "Пункты, снятые по нулевой дисперсии перед оценкой 2PL", note = ZERO_VAR_NOTE),
      art("2PL/1pl_2pl_comparison.csv", "csv", "Сравнение 1PL и 2PL: AIC, BIC, logLik, LRT"),
      art("2PL/2pl_tif_sem.csv", "csv",
          "TIF/SEM: максимум информации, значения при theta = 0, диапазоны приемлемой надёжности"),
      art("2PL/2pl_q3_summary.csv", "csv", "Локальная независимость: сводка по Yen Q3"),
      art("2PL/2pl_q3_pairs.csv", "csv", "Пары пунктов по Q3",
          filter = function(d) d[!is.na(d$Flag) & as.logical(d$Flag), , drop = FALSE],
          note = paste0("В markdown — только помеченные пары (Flag = TRUE); ",
                        "полный список всех пар есть в report.json.")),
      art("2PL/2pl_q3_matrix.csv", "csv", "Полная матрица Q3 (26 x 26)", md = "none",
          note = "Только в report.json: в markdown матрица нечитаема по ширине."),
      art("2PL/2pl_irt_results.txt", "log", "Развёрнутая выдача 2PL")),

  sec("DIF: языковое смещение (kz vs ru)",
      art("DIF/dif_mh_table.csv", "csv", "Mantel-Haenszel: chi2, скорректированный p, lnOR, ETS, флаг"),
      art("DIF/dif_excluded_items.csv", "csv",
          "Пункты, снятые по нулевой дисперсии перед расчётом DIF", note = ZERO_VAR_NOTE),
      art("DIF/dif_language_results.txt", "log",
          "Развёрнутая выдача DIF (MH, логистическая регрессия, chi2 Лорда)")),

  sec("Валидность по известным группам (ANOVA)",
      art("ANOVA/group_tests_significance.csv", "csv",
          "Сводка значимости омнибусных групповых сравнений"),
      art("ANOVA/anova_results.xlsx", "xlsx", "Полные таблицы ANOVA (все листы книги)",
          note = paste0("Авторитетный источник по этому разделу: листы содержат ",
                        "таблицы целиком, включая публикационные (PubTable).")),
      art("ANOVA/anova_results.txt", "log", "Развёрнутая выдача ANOVA",
          note = paste0("Таблицы в этом логе печатались как tibble и обрезаны — ",
                        "по ширине (строка «more variables» вместо части колонок) и ",
                        "по длине названий регионов. При расхождении верны листы ",
                        "ANOVA/anova_results.xlsx, а не этот лог."))),

  sec("Floor-страта: угадывание, пересчёт без пола, hub-кластер",
      intro = paste0(
        "Шаг 10. Отбор в страту идёт по сумме баллов, поэтому средняя по всем ",
        "пунктам доля верных внутри неё тождественно равна M(балл)/k и сверху ",
        "ограничена Floor_cut/k: общий УРОВЕНЬ доли внутри страты задан отбором, а ",
        "не измерен. Измерены распределение доли между пунктами (Cochran Q) и ",
        "контраст hub-кластера против остальных пунктов."),
      art("FloorStratum/floor_verdict.csv", "csv",
          "Вердикт по floor-страте и структурная граница отбора",
          note = paste0("ЕДИНЫЙ источник исхода: Outcome выбирается из трёх ",
                        "заявленных заранее (плюс четвёртая комбинация, названная ",
                        "отдельно) по двум групповым тестам на уровне респондента ",
                        "с поправкой Холма.")),
      art("FloorStratum/floor_item_proportions.csv", "csv",
          "Доля верных по каждому пункту внутри floor-страты против уровня угадывания"),
      art("FloorStratum/floor_group_tests.csv", "csv",
          "Те же доли по группам пунктов: hub-кластер и остальные",
          note = paste0("Объединённый биномиальный тест считает 26 ответов одного ",
                        "респондента независимыми и потому занижает p; вывод несут ",
                        "колонки уровня респондента (t и Уилкоксон).")),
      art("FloorStratum/floor_contrasts.csv", "csv",
          "Контрасты внутри страты: Cochran Q, парный hub/остальные, chi2 2x2"),
      art("FloorStratum/nofloor_2pl_status.csv", "csv",
          "Сходимость рефита 2PL на подвыборке без floor-страты"),
      art("FloorStratum/nofloor_2pl_params.csv", "csv",
          "Попунктные a и b 2PL на подвыборке без floor-страты",
          note = paste0("Средняя дискриминация на этой же подвыборке опубликована ",
                        "шагом 2 (Bimodality/subsample_sensitivity.csv, строка ",
                        "NO-FLOOR); здесь — попунктные значения.")),
      art("FloorStratum/person_fit_summary.csv", "csv",
          "Person-fit по стратам: Zh (lz) и ошибки Гуттмана"),
      art("FloorStratum/mixture_summary.csv", "csv",
          "Смесь «чистое угадывание + 2PL»: доля класса случайных ответов",
          note = paste0("Доля класса лежит на границе пространства параметров ",
                        "(Pi = 0), поэтому разность правдоподобий не имеет ",
                        "распределения chi2 и сравнение идёт по AIC/BIC. Строки ",
                        "*_2PL_only повторяют logLik и AIC/BIC модели 2PL из ",
                        "2PL/1pl_2pl_comparison.csv: база сравнения обязана быть ",
                        "посчитана той же квадратурой, что и смесь, и совпадение с ",
                        "mirt — проверка того, что квадратура не своя.")),
      art("FloorStratum/mixture_by_stratum.csv", "csv",
          "Тот же класс случайных ответов в разбивке по стратам"),
      art("FloorStratum/hub_bifactor_status.csv", "csv",
          "Сходимость бифакторной 2PL со специфическим фактором на hub-кластере"),
      art("FloorStratum/hub_bifactor_indices.csv", "csv",
          "Индексы бифакторной 2PL с S_hub: omega_h, ECV, ECV_S_hub, PUC"),
      art("FloorStratum/hub_bifactor_loadings.csv", "csv",
          "Нагрузки бифакторной 2PL с S_hub (G и специфический фактор кластера)"),
      art("FloorStratum/nohub_structure.csv", "csv",
          "Надёжность, число факторов и согласие CFA на 20 пунктах без hub-кластера"),
      art("FloorStratum/nohub_eigenvalues.csv", "csv",
          "Собственные значения тех же 20 пунктов против линии параллельного анализа"),
      art("FloorStratum/floor_stratum_report.txt", "log",
          "Развёрнутая выдача шага 10"))
)

# Артефакты, сознательно не входящие в свод. Каждый — с причиной: свод должен
# показывать границу своего охвата, а не умалчивать о ней.
OMITTED <- list(
  list(path = "cleaned_responses.csv",
       reason = "построчная матрица ответов респондентов"),
  list(path = "answer_key.csv",
       reason = "тексты верных вариантов (kz/ru), не статистики"),
  list(path = "ANOVA/anova_prepared_data.csv",
       reason = "построчные данные респондентов"),
  list(path = "ANOVA/gpa_valid_data.csv",
       reason = "построчные данные респондентов"),
  list(path = "Rasch/person_theta.csv",
       reason = "построчные WLE-теты; их описательная статистика — в Rasch/rasch_report.txt"),
  list(path = "2PL/2pl_theta.csv",
       reason = "построчные EAP-теты; их сводка — в 2PL/2pl_tif_sem.csv и 2PL/2pl_irt_results.txt"),
  list(path = "FloorStratum/person_fit.csv",
       reason = "построчные Zh и ошибки Гуттмана; их сводка — в FloorStratum/person_fit_summary.csv"),
  list(path = "FloorStratum/mixture_posterior.csv",
       reason = paste0("построчная апостериорная вероятность класса случайных ответов; ",
                       "её сводка — в FloorStratum/mixture_summary.csv и mixture_by_stratum.csv")),
  list(path = "Distractor/distractor_analysis.txt",
       reason = paste0("50 КБ текстов вариантов и кросс-таблиц по уровням балла; ",
                       "числовая часть — в Distractor/distractor_pb_table.csv"))
)

# Классы файлов, не перечисляемые поимённо: рисунки, бинарные объекты моделей и
# сами отчёты.
OMIT_PATTERNS <- c(
  "(^|/)plots/",                       # каталоги рисунков
  "\\.(png|tiff|tif|pdf|svg|jpg)$",    # рисунки вне plots/
  "\\.RData$",                         # объекты моделей (lavaan, TAM, mirt, psych)
  "^report\\.html$",                   # полный отчёт
  "^report\\.(md|json)$"               # результат этого скрипта
)

# =============================================================================
#  ПРОВЕРКА ОХВАТА
# =============================================================================
# Артефакт, не описанный ни манифестом, ни списком исключений, не попал бы в
# свод молча — и сверка статьи прошла бы мимо его чисел. Расхождение = ошибка,
# как в run_all.R для списка шагов.
local({
  on_disk <- list.files(OUT, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  listed  <- c(unlist(lapply(SECTIONS, function(s) vapply(s$entries, `[[`, character(1), "path"))),
               vapply(OMITTED, `[[`, character(1), "path"))
  rest <- setdiff(on_disk, listed)
  for (p in OMIT_PATTERNS) rest <- rest[!grepl(p, rest)]
  if (length(rest)) {
    stop(sprintf(paste0(
      "write_report_text: артефакты output/ не покрыты манифестом: %s.\n",
      "Опишите каждый в SECTIONS (входит в свод) или в OMITTED (с причиной) — ",
      "иначе его числа выпадут из сверки без единого сообщения."),
      paste(rest, collapse = ", ")), call. = FALSE)
  }
})

# =============================================================================
#  ФОРМАТИРОВАНИЕ
# =============================================================================

# Текст ячейки. Числа — format(trim = TRUE), как в write_csv_excel(), поэтому
# markdown воспроизводит запись CSV посимвольно; NA -> пусто; вертикальная черта
# и переводы строк экранируются, иначе разъезжается разметка таблицы.
md_cell <- function(col) {
  txt <- if (is.numeric(col)) format(col, trim = TRUE) else as.character(col)
  txt[is.na(col)] <- ""
  txt <- gsub("[\r\n]+", " ", txt)
  gsub("|", "\\|", txt, fixed = TRUE)
}

md_table <- function(df) {
  if (!nrow(df) || !ncol(df)) return("_(таблица пуста)_\n")
  cells <- lapply(df, md_cell)
  hdr <- gsub("|", "\\|", names(df), fixed = TRUE)
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(vapply(cells, `[[`, character(1), i), collapse = " | "), " |")
  }, character(1))
  paste0(paste(c(paste0("| ", paste(hdr, collapse = " | "), " |"),
                 paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
                 rows), collapse = "\n"), "\n")
}

# Текстовая выдача идёт в ограждённом блоке; ограждение из тильд не конфликтует
# с обратными кавычками внутри логов.
md_pre <- function(txt) paste0("~~~\n", txt, "\n~~~\n")

read_table <- function(path) as.data.frame(
  readr::read_csv(path))

write_utf8 <- function(txt, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)   # без BOM: JSON-парсеры на нём падают
  invisible(path)
}

# =============================================================================
#  СБОРКА
# =============================================================================

md <- character(0)
add <- function(...) md <<- c(md, ...)

tables_json <- list()   # ключ = путь артефакта (для xlsx: путь#лист)
logs_json   <- list()
absent      <- list()   # условные артефакты, которых нет после этого прогона

# N переносится строкой артефакта шага 0, а не считается nrow() по построчной
# матрице: свод — слой переноса, и второе, самостоятельно выведенное значение того
# же числа снимало бы это правило ровно там, где оно объявлено. Отсутствие строки —
# рассогласование со шагом 0, поэтому падение, а не молчаливый integer(0).
n_resp <- local({
  s <- read_table(file.path(OUT, "preprocess_summary.csv"))
  v <- s$Value[s$Metric == "Итоговая выборка"]
  if (length(v) != 1L)
    stop("write_report_text: в preprocess_summary.csv нет строки «Итоговая выборка» — ",
         "манифест свода разошёлся с шагом 0.", call. = FALSE)
  as.integer(v)
})
stamp  <- format(Sys.Date())

add(sprintf("# ТКМ-Хим: машиночитаемый свод результатов\n"))
add(sprintf(paste0("Сгенерировано: %s | Источник: `output/` | Генератор: ",
                   "`scripts/write_report_text.R` | Парная машинная версия: ",
                   "`output/report.json`\n"), stamp))
add(paste0(
  "Свод содержит те же числа, что `output/report.html`, без рисунков и без ",
  "построчных данных респондентов. Каждая таблица подписана путём к артефакту ",
  "в `output/` — тот же путь служит ключом в JSON, поэтому любое значение ",
  "прослеживается до файла, который его записал.\n"))
add(paste0(
  "Ни одно значение здесь не пересчитывается: скрипт переносит содержимое ",
  "артефактов как есть, в той же записи, в какой их сохранил пайплайн. ",
  "Расхождение между числом в тексте статьи и числом здесь означает ошибку в ",
  "статье, а не другую версию расчёта.\n"))

add("## Параметры набора пунктов\n")
add(md_table(data.frame(
  Параметр = c("N респондентов", "Пунктов в анализе", "Исключены из анализов",
               "Отсутствуют в схеме форм"),
  Значение = c(as.character(n_resp), as.character(length(ITEMS)),
               paste(EXCLUDED_ITEMS, collapse = ", "),
               paste(ABSENT_ITEMS, collapse = ", ")),
  check.names = FALSE)))
add("")

sub_lbl <- c(S1 = "Интерпретация", S2 = "Анализ", S3 = "Оценка")
struct <- data.frame(
  Субшкала = names(SUBSCALES),
  Название = unname(sub_lbl[names(SUBSCALES)]),
  Пунктов  = vapply(SUBSCALES, length, integer(1)),
  Пункты   = vapply(SUBSCALES, function(x) paste(x, collapse = ", "), character(1)),
  check.names = FALSE, row.names = NULL
)
add("Структура субшкал (источник — `input/items.csv` через `scripts/config.R`):\n")
add(md_table(struct))
add("")

emit_table <- function(e, key, df, section) {
  add(sprintf("### %s\n", e$title))
  add(sprintf("Артефакт: `output/%s`%s\n", e$path,
              if (identical(key, e$path)) "" else sprintf(" — лист `%s`",
                                                          sub("^.*#", "", key))))
  if (!is.null(e$note)) add(sprintf("> %s\n", e$note))
  tables_json[[key]] <<- list(section = section, title = e$title,
                              n_rows = nrow(df), columns = I(names(df)), data = df)
  if (identical(e$md, "none")) {
    add(sprintf("_Строк: %d. В markdown не разворачивается; полностью — в `output/report.json` по ключу `%s`._\n",
                nrow(df), key))
  } else {
    shown <- if (is.function(e$filter)) e$filter(df) else df
    if (nrow(shown) < nrow(df)) {
      add(sprintf("_Показано %d из %d строк; полная таблица — в `output/report.json`._\n",
                  nrow(shown), nrow(df)))
    }
    add(md_table(shown))
  }
  add("")
}

mark_absent <- function(e, section, why) {
  absent[[length(absent) + 1L]] <<- list(path = e$path, section = section,
                                         title = e$title, reason = why)
  add(sprintf("### %s\n", e$title))
  add(sprintf("_Артефакт `output/%s` отсутствует: %s._\n", e$path, why))
  add("")
}

for (s in SECTIONS) {
  add(sprintf("## %s\n", s$title))
  if (!is.null(s$intro)) add(sprintf("%s\n", s$intro))
  for (e in s$entries) {
    path <- file.path(OUT, e$path)
    if (!file.exists(path)) {
      # Условный артефакт (модель не сошлась) — отсутствие видимо, как в report.Rmd.
      mark_absent(e, s$title, "шаг-производитель его не записал")
      next
    }
    if (identical(e$kind, "csv")) {
      emit_table(e, e$path, read_table(path), s$title)
    } else if (identical(e$kind, "xlsx")) {
      add(sprintf("### %s\n", e$title))
      add(sprintf("Артефакт: `output/%s`\n", e$path))
      if (!is.null(e$note)) add(sprintf("> %s\n", e$note))
      add("")
      for (sh in readxl::excel_sheets(path)) {
        d <- as.data.frame(readxl::read_excel(path, sheet = sh, .name_repair = "minimal"))
        emit_table(list(path = e$path, title = sprintf("%s — лист `%s`", e$title, sh),
                        md = "full", filter = NULL, note = NULL),
                   paste0(e$path, "#", sh), d, s$title)
      }
    } else {
      txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      add(sprintf("### %s\n", e$title))
      add(sprintf("Артефакт: `output/%s`\n", e$path))
      if (!is.null(e$note)) add(sprintf("> %s\n", e$note))
      logs_json[[e$path]] <- list(section = s$title, title = e$title, text = txt)
      add(md_pre(txt))
      add("")
    }
  }
}

add("## Не входит в свод\n")
add(paste0("Артефакты `output/`, сознательно оставленные за границей свода. ",
           "Файлы на месте — их содержимое просто не переносится сюда.\n"))
add(md_table(data.frame(
  Артефакт = vapply(OMITTED, function(o) sprintf("`output/%s`", o$path), character(1)),
  Причина  = vapply(OMITTED, `[[`, character(1), "reason"),
  check.names = FALSE)))
add(paste0("\nПомимо перечисленного не переносятся рисунки (`*/plots/**`), ",
           "сохранённые объекты моделей (`*.RData`) и сам `output/report.html`.\n"))

write_utf8(paste(md, collapse = "\n"), MD)

# JSON: meta -> tables -> logs -> absent -> omitted. digits = NA отдаёт полную
# точность double; при дефолтных 4 знаках значения молча округлились бы и
# разошлись с CSV.
payload <- list(
  meta = list(
    generated = stamp,
    generator = "scripts/write_report_text.R",
    source = "output/",
    r_version = as.character(getRversion()),
    n_respondents = n_resp,
    n_items = length(ITEMS),
    # I() удерживает массив: под auto_unbox = TRUE вектор из одного пункта
    # (EXCLUDED_ITEMS сейчас — только Q02) свернулся бы в скаляр, и тип поля
    # менялся бы вместе с набором пунктов.
    items = I(ITEMS),
    subscales = lapply(SUBSCALES, I),
    excluded_items = I(EXCLUDED_ITEMS),
    absent_items = I(ABSENT_ITEMS),
    derived_values = FALSE   # весь контент перенесён из артефактов без пересчёта
  ),
  tables = tables_json,
  logs = logs_json,
  absent = absent,
  omitted = OMITTED
)

write_utf8(jsonlite::toJSON(payload, dataframe = "rows", na = "null",
                            digits = NA, auto_unbox = TRUE, pretty = 2),
           JSON)

message(sprintf("[report_text] %s (%.0f КБ), %s (%.0f КБ): таблиц %d, логов %d, отсутствует %d",
            MD, file.size(MD) / 1024, JSON, file.size(JSON) / 1024,
            length(tables_json), length(logs_json), length(absent)))
