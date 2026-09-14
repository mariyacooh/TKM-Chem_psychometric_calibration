#!/usr/bin/env Rscript
# =============================================================================
#  render_report_html.R — рендер единого отчёта scripts/report.Rmd в
#  output/report.html.
#  Зависимости (rmarkdown) ставятся заранее через scripts/install_deps.R;
#  здесь только проверка наличия и рендер — без установки на лету. pandoc —
#  системная зависимость (см. README.ru.md), R-пакет pandoc не тянем.
# =============================================================================

# Рабочая директория — корень проекта (этот файл лежит в scripts/).
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

# Воспроизводимые числовые опции для процесса рендера (render() исполняет
# report.Rmd в этом же процессе). Дефолты R -> форматирование чисел в отчёте не
# зависит от ~/.Rprofile. Синхронизировать с scripts/_setup.R.
options(digits = 7, OutDec = ".", scipen = 0,
        contrasts = c("contr.treatment", "contr.poly"))

if (!requireNamespace("rmarkdown", quietly = TRUE))
  stop("Пакет rmarkdown не установлен. Установите зависимости: Rscript scripts/install_deps.R.", call. = FALSE)
if (!rmarkdown::pandoc_available())
  stop("pandoc не найден. Установите системный pandoc (см. README.ru.md).", call. = FALSE)

# Остатки предыдущего рендера в scripts/: report_cache/ может подать устаревший
# вывод чанка в новый рендер, report_files/ и report*.html дублируют
# output/report.html. Ни run_all.R, ни run_plots.R в scripts/ не пишут, поэтому
# чистит их сам рендер — чистота не зависит от оркестратора (go.py) и одинакова
# на хосте и в контейнере.
local({
  leftovers <- c(Sys.glob("scripts/report_files"), Sys.glob("scripts/report_cache"),
                 Sys.glob("scripts/report*.html"), Sys.glob("scripts/report.knit.md"))
  if (length(leftovers)) unlink(leftovers, recursive = TRUE, force = TRUE)
})

rmarkdown::render("scripts/report.Rmd", output_dir = "output")
