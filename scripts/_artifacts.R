# =============================================================================
#  _artifacts.R — контракт зависимостей между шагами пайплайна (артефакты в output/).
# =============================================================================
#  Source-ится из scripts/_setup.R (шаги анализа) и scripts/plots/_plot_utils.R
#  (графики), поэтому require_input()/optional_input() доступны в любом скрипте.
#
#  Шаги не вызывают друг друга: связь только через файлы. Producer пишет артефакт
#  в output/, consumer его читает. Отсутствующий вход означает НЕВЫПОЛНЕННЫЙ
#  предыдущий шаг, а не «нет данных»: продолжать нельзя, иначе результат выйдет
#  тихо неполным. Ровно два санкционированных поведения:
#
#    require_input()  — жёсткая зависимость: файл ОБЯЗАН быть, иначе stop().
#    optional_input() — условный артефакт: producer пишет его не всегда (модель не
#                       сошлась). Возвращает TRUE/FALSE и ВСЕГДА печатает [SKIP]
#                       с причиной, поэтому пропуск виден в логе.
#
#  Третьего варианта (тихий file.exists() без сообщения) в пайплайне нет.
#
# -----------------------------------------------------------------------------
#  ГРАФ ЗАВИСИМОСТЕЙ (номер файла = позиция в порядке запуска, см. run_all.R)
# -----------------------------------------------------------------------------
#  Корни (input/, вне пайплайна):
#    input/items.csv                  -> scripts/config.R (структура пунктов; читают ВСЕ)
#    input/answer_key.csv             -> шаг 0
#    input/region_map.csv             -> шаг 0
#    input/google_forms_responses.xlsx-> шаг 0 и шаг 9 (буквы вариантов из сырого листа)
#
#  Списки выходов ниже ПОЛНЫЕ: артефакта, которого в них нет, шаг не пишет. Стрелка
#  «->» ведёт к потребителю ВНУТРИ пайплайна (другой шаг или график) -- это и есть
#  рёбра графа. Артефакт без стрелки для пайплайна терминален: его читают только
#  report.Rmd и свод write_report_text.R, потребители всего output/. Полнота
#  проверяема: манифест свода (SECTIONS + OMITTED + OMIT_PATTERNS) обязан покрывать
#  каждый файл output/, иначе прогон падает, поэтому расхождение с ним ловится.
#
#  Шаг 0  0_preprocess.R    вход: input/google_forms_responses.xlsx + input/*.csv
#    cleaned_responses.csv  -> шаги 1-10, plot_descriptive_density; ЦЕНТРАЛЬНЫЙ артефакт
#    answer_key.csv         терминальный даже для свода: только строка в его OMITTED
#    preprocess_summary.csv отбор выборки (строки выгрузки, отсев, итог);
#                           ЕДИНСТВЕННЫЙ источник этих чисел
#
#  Шаг 1  1_descriptive_stats.R        вход: cleaned_responses.csv
#    Descriptive/descriptive_stats_detailed.csv
#    Descriptive/descriptive_stats_tables.csv -> plot_sample_composition; частоты по
#                                          языку, курсу, самооценке и конференциям
#    Descriptive/descriptive_stats.txt
#
#  Шаг 2  2_bimodality.R               вход: cleaned_responses.csv
#    Bimodality/extreme_patterns.csv       -> plot_score_distribution; флаг бимодальности
#                                             выборки (BC, тест провала, доли краёв)
#    Bimodality/score_total_frequency.csv  -> plot_score_distribution; частоты по баллам
#    Bimodality/cut_sensitivity.csv        -> plot_bimodality_sensitivity; градиент по
#                                             границам среза
#    Bimodality/cut_monotonicity.csv       -> plot_bimodality_sensitivity, report.Rmd;
#                                             монотонность трёх метрик как ИЗМЕРЕНИЕ
#                                             (трёхзначная: TRUE / FALSE / NA) плюс охват
#                                             развёрток (N_assessed / N_points), по нему
#                                             ветвятся подпись рисунка и проза отчёта
#    Bimodality/subsample_sensitivity.csv  надёжность и CFA по усечениям
#    Bimodality/subsample_known_groups.csv валидность по известным группам на тех же
#                                          подвыборках
#    Bimodality/bimodality_report.txt
#
#  Шаг 3  3_efa_cfa.R                  вход: cleaned_responses.csv
#    EFA/efa_eigenvalues.csv       -> plot_scree
#    EFA/efa_parallel_analysis.csv -> plot_scree; ЕДИНЫЙ источник числа компонент PA
#    EFA/efa_loadings.csv          -> plot_efa_heatmap
#    EFA/efa_results.txt
#    CFA/cfa_fits.RData            -> plot_cfa_path, plot_cfa_loadings; УСЛОВНО
#                                     (сохраняются только сошедшиеся модели; ни одна
#                                      не сошлась => файл не пишется вовсе)
#    CFA/cfa_loadings.csv          УСЛОВНО (только при сходимости 3-факторной ICM-CFA)
#    CFA/cfa_loadings_compact.csv  УСЛОВНО (то же), в свод write_report_text.R
#    CFA/cfa_bifactor_loadings.csv УСЛОВНО (только при сходимости бифакторной CFA)
#    CFA/cfa_results.txt
#    Bifactor/bifactor_fit_table.csv, Bifactor/omega_matrix_status.csv <- флаги, по
#                                     которым ветвится report.Rmd, а не проза
#    Bifactor/omega_reliability.txt, Bifactor/bifactor_cfa_results.txt
#    ESEM/esem_loadings.csv        -> plot_esem_heatmap; ПУСТОЙ файл (0 строк) означает
#                                     несошедшийся ESEM, и рисунок пропускается с
#                                     видимым [SKIP] -- отсутствие файла означает
#                                     невыполненный шаг и роняет график
#    ESEM/esem_fit_table.csv, ESEM/esem_factor_correlations.csv, ESEM/esem_results.txt
#
#  Шаг 4  4_rasch.R                    вход: cleaned_responses.csv
#    Rasch/person_theta.csv        -> ШАГ 5, plot_rasch_wright; ЕДИНСТВЕННЫЙ источник
#                                     Theta (WLE)
#    Rasch/rasch_item_report.csv   -> plot_rasch_wright
#    Rasch/rasch_model.RData       -> plot_rasch_icc
#    Rasch/rasch_excluded_items.csv отсев нулевой дисперсии; пишется ВСЕГДА, пустой
#                                   файл = ничего не снято
#    Rasch/rasch_report.txt
#
#  Шаг 5  5_course_descriptives.R      вход: cleaned_responses.csv + Rasch/person_theta.csv
#    Descriptive/descriptive_stats_by_course.csv  каталог общий с шагом 1
#
#  Шаг 6  6_2pl.R                      вход: cleaned_responses.csv
#    2PL/2pl_params.csv            -> plot_2pl_curves
#    2PL/2pl_theta.csv             -> plot_2pl_curves (карта Райта)
#    2PL/2pl_model.RData           -> plot_2pl_curves, ШАГ 10; оценённые 1PL и 2PL как
#                                     объекты: ICC рисует mirt по модели, шаг 10 берёт
#                                     ту же 2PL для person-fit и старта смеси
#    2PL/2pl_model_status.csv      статус сходимости 1PL/2PL; флаг ветвления report.Rmd
#    2PL/2pl_excluded_items.csv    отсев нулевой дисперсии (всегда)
#    2PL/2pl_q3_pairs.csv          -> plot_q3_heatmap; УСЛОВНО (residuals(type = "Q3")
#                                     мог не посчитаться — ветка q3_mat в 6_2pl.R)
#    2PL/2pl_q3_summary.csv        -> plot_q3_heatmap; ЕДИНЫЙ источник порога отбора
#                                     пар (Crit_rel_M+0.2), УСЛОВНО (то же)
#    2PL/1pl_2pl_comparison.csv, 2PL/2pl_tif_sem.csv, 2PL/2pl_q3_matrix.csv,
#    2PL/2pl_irt_results.txt
#    2PL/2pl_bifactor_loadings.csv -> plot_bifactor_loadings; УСЛОВНО (сходимость
#                                     bfactor()); без него рисунка нет вовсе
#    2PL/2pl_bifactor_indices.csv  -> plot_bifactor_loadings; УСЛОВНО (то же); без него
#                                     рисунок строится БЕЗ панели плашек
#    2PL/2pl_bifactor_subscale_omega.csv  УСЛОВНО (то же)
#
#  Шаг 7  7_dif.R                      вход: cleaned_responses.csv
#    DIF/dif_mh_table.csv          -> plot_dif_mh
#    DIF/dif_excluded_items.csv    отсев нулевой дисперсии (всегда)
#    DIF/dif_language_results.txt
#
#  Шаг 8  8_anova_known_groups.R       вход: cleaned_responses.csv
#    ANOVA/anova_prepared_data.csv -> plot_anova_validity
#    ANOVA/gpa_valid_data.csv      -> plot_anova_validity
#    ANOVA/group_tests_significance.csv  флаг оговорок report.Rmd
#    ANOVA/anova_results.txt, ANOVA/anova_results.xlsx <- единственная книга xlsx в output/
#
#  Шаг 9  9_distractor.R    вход: cleaned_responses.csv + input/google_forms_responses.xlsx
#    Distractor/distractor_pb_table.csv -> plot_distractor_heatmap
#    Distractor/distractor_analysis.txt
#
#  Шаг 10 10_floor_stratum.R  вход: cleaned_responses.csv + ШАГ 2 (extreme_patterns.csv,
#                             граница пола) + ШАГ 6 (2pl_model.RData, оценённая 2PL)
#    FloorStratum/floor_item_proportions.csv -> plot_floor_conditional_p; доля верных по
#                                        каждому пункту внутри floor-страты и её тест
#                                        против уровня угадывания
#    FloorStratum/floor_group_tests.csv -> plot_floor_conditional_p; те же доли по двум
#                                        группам пунктов (hub-кластер и остальные)
#    FloorStratum/floor_contrasts.csv   Cochran Q, парный контраст hub/остальные, chi2 2x2
#    FloorStratum/floor_verdict.csv     исход из трёх заявленных плюс структурная граница
#                                        страты; ЕДИНЫЙ источник вердикта
#    FloorStratum/nofloor_2pl_params.csv УСЛОВНО (сходимость рефита): попунктные a и b на
#                                        подвыборке без пола
#    FloorStratum/nofloor_2pl_status.csv статус того же рефита; пишется ВСЕГДА
#    FloorStratum/person_fit.csv        построчные Zh и ошибки Гуттмана; в свод не входит
#    FloorStratum/person_fit_summary.csv сводка person-fit по стратам
#    FloorStratum/mixture_posterior.csv построчная апостериорная вероятность класса
#                                        случайных ответов; в свод не входит
#    FloorStratum/mixture_summary.csv   доля класса случайных ответов, AIC/BIC, сходимость
#    FloorStratum/mixture_by_stratum.csv тот же класс в разбивке по стратам
#    FloorStratum/hub_bifactor_status.csv сходимость бифакторной 2PL с S_hub; ВСЕГДА
#    FloorStratum/hub_bifactor_loadings.csv УСЛОВНО (сходимость bfactor)
#    FloorStratum/hub_bifactor_indices.csv  УСЛОВНО (то же); ECV_S_hub — численная оценка
#                                        вклада фактора метода
#    FloorStratum/nohub_structure.csv   надёжность, число факторов и согласие CFA на 20
#                                        пунктах без hub-кластера
#    FloorStratum/nohub_eigenvalues.csv собственные значения тех же 20 пунктов
#    FloorStratum/floor_stratum_report.txt
#
#  Рёбра «анализ -> анализ» — два, и оба по одной причине: величина, уже
#  посчитанная шагом-производителем, вторым счётом не заводится.
#    шаг 4 -> шаг 5 (Theta): 4_rasch.R стоит ПЕРЕД шагом 5 по номеру, порядок
#      номеров = порядок запуска. Шаг 8 тету не читает — зависимой переменной она не
#      является (DECISIONS.md D9);
#    шаги 2 и 6 -> шаг 10: граница пола выведена шагом 2 из уровня угадывания, а 2PL
#      полной выборки оценена шагом 6, поэтому шаг 10 берёт обе готовыми. Отсюда его
#      номер: он стоит после обоих производителей.
#
#  Остальные шаги зависят только от шага 0, поэтому их номера свободны, и порядок
#  между ними выражает не зависимость, а область действия результата:
#    шаг 2 (бимодальность) стоит ПЕРЕД моделями, потому что его флаг определяет,
#      как читаются alpha/omega шага 3 и оценки способности шагов 4 и 6;
#    шаг 9 (дистракторы) стоит ПОСЛЕ них как лист: его CTT-скрининг не меняет ни
#      одного числа шагов 1-8, состав пунктов задаёт input/items.csv, а не он.
#      Цена позиции: сверка позиционного маппинга столбцов xlsx с
#      cleaned_responses.csv (raw_mat/lang_raw_kept в 9_distractor.R) — единственная
#      независимая проверка этого маппинга в пайплайне, и на позиции 9 она сообщает
#      о расхождении после всех моделей, а не в начале прогона.
#
#  Графики (scripts/plots/*.R) — листья, читают артефакты своего шага:
#    plot_descriptive_density  <- шаг 0 (cleaned_responses.csv)
#    plot_sample_composition   <- шаг 1 (Descriptive/descriptive_stats_tables.csv)
#    plot_score_distribution   <- шаг 2 (Bimodality/score_total_frequency.csv,
#                                        Bimodality/extreme_patterns.csv)
#    plot_bimodality_sensitivity                 <- шаг 2 (Bimodality/cut_sensitivity.csv,
#                                                   Bimodality/cut_monotonicity.csv)
#    plot_scree                                  <- шаг 3 (EFA/efa_eigenvalues.csv,
#                                                   EFA/efa_parallel_analysis.csv)
#    plot_efa_heatmap                            <- шаг 3 (EFA/efa_loadings.csv)
#    plot_cfa_path, plot_cfa_loadings            <- шаг 3, УСЛОВНО (CFA/cfa_fits.RData)
#    plot_esem_heatmap                           <- шаг 3 (ESEM/esem_loadings.csv)
#    plot_rasch_icc                              <- шаг 4 (Rasch/rasch_model.RData)
#    plot_rasch_wright                           <- шаг 4 (Rasch/person_theta.csv,
#                                                   Rasch/rasch_item_report.csv)
#    plot_2pl_curves                             <- шаг 6 (2PL/2pl_params.csv, 2pl_theta.csv)
#    plot_q3_heatmap                             <- шаг 6, УСЛОВНО (2PL/2pl_q3_pairs.csv,
#                                                   2pl_q3_summary.csv): без них [SKIP]
#    plot_bifactor_loadings                      <- шаг 6, УСЛОВНО ДВАЖДЫ и по-разному:
#                                                   без 2pl_bifactor_loadings.csv рисунка
#                                                   нет вовсе (quit со статусом 0), без
#                                                   2pl_bifactor_indices.csv он строится
#                                                   БЕЗ панели плашек omega_h/ECV/PUC.
#                                                   Оба пропуска видимы как [SKIP]
#    plot_dif_mh                                 <- шаг 7 (DIF/dif_mh_table.csv)
#    plot_anova_validity                         <- шаг 8 (ANOVA/anova_prepared_data.csv, gpa_valid_data.csv)
#    plot_distractor_heatmap                     <- шаг 9 (Distractor/distractor_pb_table.csv)
#    plot_floor_conditional_p                    <- шаг 10 (FloorStratum/floor_item_proportions.csv,
#                                                   floor_group_tests.csv) + шаг 2
#                                                   (Bimodality/extreme_patterns.csv, граница пола)
#
#  Отчёт (render_report_html.R -> report.Rmd) — потребитель всего output/. Он
#  единственный деградирует по частям: отсутствующая таблица/рисунок/лог
#  заменяется ВИДИМОЙ пометкой в HTML с указанием шага, а не пропускается молча.
#
#  Свод (write_report_text.R -> output/report.md + .json) — второй потребитель всего
#  output/, без рисунков и без построчных данных респондентов. Слой переноса: числа
#  не пересчитываются, поэтому вторым источником правды он не становится. Охват
#  проверяется: артефакт output/, не описанный ни манифестом свода, ни его списком
#  исключений, роняет прогон — молча выпасть из свода он не может.
# =============================================================================

# Жёсткая зависимость по артефакту: все пути обязаны существовать.
# `produced_by` — шаг-производитель (например "scripts/4_rasch.R (шаг 4)").
require_input <- function(paths, produced_by) {
  absent <- paths[!file.exists(paths)]
  if (length(absent)) {
    stop(sprintf(
      "Отсутствует обязательный вход: %s. Артефакт пишет %s — выполните этот шаг (или весь пайплайн: Rscript scripts/run_all.R).",
      paste(absent, collapse = ", "), produced_by
    ), call. = FALSE)
  }
  invisible(paths)
}

# Условная зависимость: producer пишет артефакт не всегда (например, бифакторная
# модель не сошлась). Пропуск ЛЕГАЛЕН, но не бесшумен — печатается [SKIP] с
# причиной. Возвращает TRUE, если все пути на месте.
optional_input <- function(paths, produced_by, reason) {
  absent <- paths[!file.exists(paths)]
  if (length(absent)) {
    message(sprintf("[SKIP] Нет условного входа: %s (пишет %s). Причина: %s.",
                    paste(absent, collapse = ", "), produced_by, reason))
    return(FALSE)
  }
  TRUE
}

# Отдельная ось того же контракта: вход НА МЕСТЕ, но его содержимое разошлось с
# ожиданиями потребителя. Литералы уровней фактора, объявленные заново поверх тех,
# что породил шаг-producer, сверяются с фактическими значениями: устаревший набор
# иначе деградирует МОЛЧА -- factor(levels = <старые>) даёт сплошные NA, фильтр по
# !is.na() оставляет пустую панель, а индексация по имени колонки возвращает NULL,
# который tibble() снимает без ошибки.
# Предикат -- пустое пересечение, а не полное совпадение: набор, где какого-то
# уровня законно нет (например без четверокурсников), ложным срабатыванием не будет.
need_levels <- function(x, lv, what) {
  present <- as.character(unique(x))
  if (!length(intersect(present, lv))) {
    stop(sprintf(
      "УСТАРЕВШИЕ уровни для %s: ни один из объявленных (%s) не найден во входе (в данных: %s). Литералы разошлись с шагом-производителем.",
      what, paste(lv, collapse = ", "), paste(present, collapse = ", ")
    ), call. = FALSE)
  }
  lv
}
