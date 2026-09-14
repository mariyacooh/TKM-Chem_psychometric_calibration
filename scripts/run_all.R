#!/usr/bin/env Rscript
# =============================================================================
#  run_all.R — ПОЛНЫЙ прогон: шаги 0-10 и все графики в ОДНОМ процессе R.
#  Тяжёлые пакеты (tidyverse, lavaan, mirt, TAM, difR, semPlot) загружаются один
#  раз, а не заново в каждом из ~25 холодных вызовов Rscript.
#
#  Каждый скрипт самодостаточен: сам делает set.seed(42), source(_setup.R)/
#  config.R, читает свои входные файлы из output/ и пишет свои артефакты. Каждый
#  источится в собственном окружении, поэтому переменные между шагами не
#  пересекаются; загруженные пакеты общие. Движок (run_one, наборы пакетов,
#  сводка) — scripts/_run_lib.R, общий с scripts/run_plots.R.
#
#  Запуск из корня проекта:  Rscript scripts/run_all.R
#  Только графики (без пересчёта):  Rscript scripts/run_plots.R
# =============================================================================

t_start <- Sys.time()

# Рабочая директория — корень проекта (этот файл лежит в scripts/).
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

# Чистая сборка: весь output/ пересоздаётся из input/ за один прогон, поэтому
# старт с пустого каталога — ни один шаг не подхватит устаревший артефакт
# прошлого прогона (шаг 0 сразу создаёт output/ заново). Именно этим полный
# прогон отличается от run_plots.R, который output/ сохраняет.
unlink("output", recursive = TRUE, force = TRUE)

# Общие бутстрап-функции (load_pkgs) и движок прогона (run_one, наборы пакетов).
source("scripts/_setup.R")
source("scripts/_run_lib.R")

pkg_secs <- load_pkgs_timed(PKGS_ANALYSIS)

# =============================================================================
#  ПОРЯДОК ШАГОВ = ПОРЯДОК НОМЕРОВ
# =============================================================================
#  Номер в имени файла выражает позицию в графе зависимостей, поэтому список ниже
#  просто отсортирован. Рёбер «анализ -> анализ» два:
#  Theta (WLE) — шаг 4 (4_rasch.R) пишет output/Rasch/person_theta.csv, шаг 5 его
#  читает и без него падает (require_input), а не выдаёт результат без Theta;
#  floor-страта — шаг 10 (10_floor_stratum.R) читает границу пола у шага 2
#  (Bimodality/extreme_patterns.csv) и оценённую 2PL у шага 6 (2PL/2pl_model.RData),
#  поэтому стоит после обоих и ни одной из этих величин не заводит заново.
#  Все остальные шаги зависят только от шага 0 (output/cleaned_responses.csv), и
#  порядок между ними выражает область действия результата, а не зависимость: шаг 2
#  измеряет бимодальность выборки ДО моделей (его флаг определяет чтение alpha/omega
#  шага 3 и оценок способности шагов 4 и 6), шаг 9 — лист, чей CTT-скрининг ни одного
#  числа шагов 1-8 не меняет. Обоснование и цена позиции шага 9 — scripts/_artifacts.R.
#  Полный граф producer -> consumer — scripts/_artifacts.R.
steps <- file.path("scripts", c(
  "0_preprocess.R",            # 0: input/ -> output/cleaned_responses.csv
  "1_descriptive_stats.R",     # 1: <- шаг 0
  "2_bimodality.R",            # 2: <- шаг 0            -> флаг бимодальности выборки
  "3_efa_cfa.R",               # 3: <- шаг 0            (EFA/CFA/Bifactor/ESEM)
  "4_rasch.R",                 # 4: <- шаг 0            -> person_theta.csv
  "5_course_descriptives.R",   # 5: <- шаг 0 + ШАГ 4 (Theta)
  "6_2pl.R",                   # 6: <- шаг 0
  "7_dif.R",                   # 7: <- шаг 0
  "8_anova_known_groups.R",    # 8: <- шаг 0
  "9_distractor.R",            # 9: <- шаг 0 + сырой xlsx
  "10_floor_stratum.R"         # 10: <- шаг 0 + ШАГ 2 (граница пола) + ШАГ 6 (2PL)
))

# Список шагов задан явно (он же — авторитетный порядок), поэтому он обязан совпадать
# с набором scripts/[0-9]*_*.R на диске: файл вне списка не попадёт в прогон без
# единого сообщения. Сверка до запуска; расхождение — ошибка.
# Порядок сверки — ЧИСЛОВОЙ по префиксу имени, а не лексикографический: с
# двузначным номером сортировка строк ставит "10_" перед "1_" (символ "0" идёт
# раньше "_"), и проверка непрерывности получила бы 0, 10, 1, ... вместо 0, 1, ..., 10.
# Номер остаётся позицией в графе; лексикографическое совпадение с ней — нет.
by_step_num <- function(paths) paths[order(as.integer(sub("^([0-9]+)_.*$", "\\1", basename(paths))))]
local({
  on_disk <- by_step_num(Sys.glob("scripts/[0-9]*_*.R"))
  listed  <- by_step_num(steps)
  if (!identical(on_disk, listed)) {
    stop(sprintf(paste0(
      "run_all: список шагов расходится с scripts/[0-9]*_*.R.\n",
      "  только на диске : %s\n  только в списке : %s\n",
      "Обновите вектор steps (и нумерацию) — иначе шаг молча не выполнится."),
      paste(setdiff(on_disk, listed), collapse = ", "),
      paste(setdiff(listed, on_disk), collapse = ", ")), call. = FALSE)
  }
  # Номера обязаны идти без пропусков от 0: пропуск ломает связь
  # «номер = позиция в графе».
  nums <- as.integer(sub("^([0-9]+)_.*$", "\\1", basename(listed)))
  if (!identical(nums, seq_along(nums) - 1L)) {
    stop(sprintf("run_all: номера шагов не непрерывны от 0: %s.",
                 paste(nums, collapse = ", ")), call. = FALSE)
  }
})

timings <- run_scripts(c(steps, plot_scripts()))

report_timings(timings, pkg_secs, t_start)
