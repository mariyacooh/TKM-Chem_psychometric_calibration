#!/usr/bin/env Rscript
# =============================================================================
#  run_plots.R — пересборка ТОЛЬКО графиков поверх уже готовых артефактов output/.
# =============================================================================
#  Для правок оформления (цвет, палитра, подписи, размеры, тема) в
#  scripts/plots/*.R и scripts/plots/_plot_utils.R: ничего не пересчитывается,
#  шаги 0-10 не запускаются, output/ НЕ стирается — графики читают те же CSV/RData,
#  что и в прошлом полном прогоне, и перезаписывают свои рисунки.
#
#  Отличия от scripts/run_all.R (общий движок — scripts/_run_lib.R):
#    - output/ сохраняется (полный прогон его стирает);
#    - грузится только PKGS_PLOTS, без mirt/difR/car/rstatix и т.п.;
#    - шаги 0-10 не запускаются, поэтому артефакты обязаны быть на месте.
#
#  ГРАНИЦА: это НЕ инкрементальная сборка. Артефакты берутся как есть, поэтому
#  правка математики в scripts/[0-9]*_*.R здесь не отразится — нужен полный
#  прогон. Стирания output/ нет, значит рисунок удалённого скрипта графиков
#  останется на диске (и в отчёте) до следующего run_all.R.
#
#  Запуск из корня проекта:  Rscript scripts/run_plots.R
#                            Rscript scripts/render_report_html.R   # отчёт после
# =============================================================================

t_start <- Sys.time()

# Рабочая директория — корень проекта (этот файл лежит в scripts/).
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

# Общие бутстрап-функции (load_pkgs, require_input) и движок прогона.
source("scripts/_setup.R")
source("scripts/_run_lib.R")

# Единая проверка «пайплайн вообще прогонялся»: без неё отсутствие output/ дало бы
# по одинаковому падению на каждый скрипт графиков вместо одного внятного
# сообщения. Каждый график всё равно проверяет свои входы сам
# (require_input/optional_input в scripts/_artifacts.R), поэтому здесь достаточно
# центрального артефакта шага 0.
require_input("output/cleaned_responses.csv", "scripts/0_preprocess.R (шаг 0)")

pkg_secs <- load_pkgs_timed(PKGS_PLOTS)

timings <- run_scripts(plot_scripts())

report_timings(timings, pkg_secs, t_start)
