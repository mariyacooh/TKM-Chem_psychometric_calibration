# =============================================================================
#  _run_lib.R — общий движок консолидированного прогона в ОДНОМ процессе R.
# =============================================================================
#  Source-ится точками входа scripts/run_all.R (шаги 0-10 + графики) и
#  scripts/run_plots.R (только графики) ПОСЛЕ фиксации рабочей директории на
#  корне проекта и source("scripts/_setup.R").
#
#  Здесь живёт всё, что у обеих точек входа общее: наборы пакетов, изолированный
#  запуск одного скрипта с замером времени, печать сводки и код возврата. Что
#  именно запускать и что чистить перед запуском — решает точка входа, не этот файл.
# =============================================================================

# =============================================================================
# Наборы пакетов
# =============================================================================
# Пакеты грузятся ОДИН раз на процесс: последующие library() в самих скриптах
# становятся почти бесплатными no-op — в этом весь выигрыш консолидированного
# прогона против ~25 холодных стартов Rscript.
#
# Это наборы ПОДКЛЮЧЕНИЯ (library), не установки: канонический список установки —
# scripts/install_deps.R, он же надмножество (плюс knitr/rmarkdown для рендера
# отчёта и jsonlite для свода). PKGS_PLOTS — подмножество PKGS_ANALYSIS.
#
# PKGS_ANALYSIS — объединённый набор шагов 0-10 И графиков (полный прогон).
# PKGS_PLOTS    — строгое подмножество: только то, что нужно графикам
#                 (см. library() в scripts/plots/*.R; ggplot2 приходит с
#                 tidyverse). Прогон «только графики» не платит за mirt, difR,
#                 car, rstatix и прочий инструментарий шагов. Пакет, не попавший
#                 в набор, всё равно подключится собственным library() внутри
#                 скрипта — набор влияет на время, не на корректность.
#                 TAM в набор не входит: rasch_model.RData читается load() как
#                 обычный S3-список (mod_rasch$resp, mod_rasch$item в
#                 plot_rasch_icc.R), а load() класса пакета не требует.
PKGS_ANALYSIS <- c("tidyverse", "readr", "readxl", "lavaan", "psych", "GPArotation",
                   "Matrix", "TAM", "mirt", "difR", "rstatix", "effectsize", "car",
                   "writexl", "semPlot", "patchwork", "ggrepel", "CTT",
                   "BifactorIndicesCalculator", "diptest")
PKGS_PLOTS    <- c("tidyverse", "lavaan", "semPlot", "patchwork", "ggrepel")

# Подключить набор пакетов через load_pkgs() (единый fail-fast путь из _setup.R:
# недостающий пакет -> stop с указанием запустить install_deps.R, а не тихая
# компиляция из исходников). Возвращает затраченные секунды для сводки.
load_pkgs_timed <- function(pkgs) {
  t_pkg <- Sys.time()
  load_pkgs(pkgs)
  secs <- as.numeric(difftime(Sys.time(), t_pkg, units = "secs"))
  message(sprintf("[run] Пакеты загружены за %.2f c (%d шт.)", secs, length(pkgs)))
  secs
}

# Источить один скрипт в изолированном окружении, замерив время.
run_one <- function(path) {
  message(sprintf("\n>>> %s", path))
  t0 <- Sys.time()
  env <- new.env(parent = globalenv())
  # Обезвредить quit()/q(): скрипт (например, plot_bifactor_loadings.R) вызывает
  # quit() чтобы аккуратно пропустить себя. В отдельном процессе это нормально,
  # но в общей сессии это убило бы весь процесс и все последующие шаги. Отсюда
  # подмена quit/q ловимым сигналом -> ранний выход ТОЛЬКО текущего скрипта.
  quit_signal <- function(save = "default", status = 0, runLast = TRUE) {
    stop(structure(
      class = c("scriptQuit", "error", "condition"),
      list(message = sprintf("script quit(status=%s)", status), call = NULL)
    ))
  }
  assign("quit", quit_signal, envir = env)
  assign("q", quit_signal, envir = env)
  # В одном процессе на пути поиска одновременно висят пакеты всех шагов, поэтому
  # car::recode может замаскировать dplyr::recode (несовместимая сигнатура) и т.п.
  # В отдельном процессе каждый скрипт грузит tidyverse последним и dplyr
  # побеждает. Тот же исход восстанавливается явно: dplyr-версии часто маскируемых
  # глаголов кладутся прямо в окружение скрипта, чтобы порядок пакетов не влиял.
  for (.fn in c("recode", "select", "filter", "mutate", "rename", "summarise",
                "summarize", "arrange", "count", "lag", "first", "last")) {
    assign(.fn, getExportedValue("dplyr", .fn), envir = env)
  }
  ok <- tryCatch({
    sys.source(path, envir = env, chdir = FALSE)
    TRUE
  }, scriptQuit = function(e) {
    message("  (скрипт вышел рано через quit() -- шаг пропущен, сессия продолжается)")
    TRUE
  }, error = function(e) {
    message("  !! ОШИБКА: ", conditionMessage(e))
    FALSE
  })
  while (sink.number() > 0) sink()   # закрыть sink, оставшийся от упавшего скрипта
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("<<< %-28s %7.2f c  %s", basename(path), dt, if (ok) "OK" else "FAIL"))
  data.frame(script = basename(path), seconds = round(dt, 2), ok = ok, stringsAsFactors = FALSE)
}

# Источить набор скриптов по порядку; вернуть таблицу времён.
#
# Прогон ПРЕРЫВАЕТСЯ на первом падении. Продолжение оставляло бы output/ частично
# собранным, а собранный по нему отчёт неотличим от отчёта по чистому прогону:
# отсутствующий артефакт выглядит там ровно как условный (модель не сошлась).
# Обрыв гарантирует, что существующий отчёт описывает ПОЛНЫЙ прогон. Возврат
# частичной таблицы (без stop) оставлен намеренно: сводку и код возврата печатает
# report_timings(), и упавший шаг обязан попасть в неё, а не потеряться в
# raw-ошибке R.
run_scripts <- function(paths) {
  rows <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    rows[[i]] <- run_one(paths[[i]])
    if (!rows[[i]]$ok) {
      skipped <- length(paths) - i
      if (skipped > 0) {
        message(sprintf(
          "!! Прогон прерван: не запускались %d последующих скриптов (%s ...).",
          skipped, basename(paths[[i + 1L]])))
      }
      rows <- rows[seq_len(i)]
      break
    }
  }
  do.call(rbind, rows)
}

# Графики — листья графа зависимостей: каждый читает артефакты своего шага и ни
# один не пишет вход другого, поэтому порядок внутри набора не важен (сортировка —
# для стабильного лога). Пустой набор означает, что скрипты графиков потерялись;
# молча отрендерить отчёт без рисунков нельзя.
plot_scripts <- function() {
  paths <- sort(Sys.glob("scripts/plots/plot_*.R"))
  if (!length(paths)) {
    stop("Не найдено ни одного scripts/plots/plot_*.R — нечего строить.", call. = FALSE)
  }
  paths
}

# Сводка по времени + код возврата. Упавший скрипт => exit 1, чтобы оркестратор
# (go.py, CI) не считал прогон успешным при частично собранном output/.
report_timings <- function(timings, pkg_secs, t_start) {
  total_secs <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  message("\n==== СВОДКА ПО ВРЕМЕНИ (один процесс) ====")
  # print() пишет в stdout, а весь остальной лог прогона (>>>, <<<, !!) идёт в stderr.
  # Перехват держит сводку в том же потоке — иначе она оказалась бы в другом файле,
  # чем строки шагов, которые она подытоживает. message()-эквивалента печати
  # data.frame нет.
  message(paste(utils::capture.output(print(timings, row.names = FALSE)), collapse = "\n"))
  message(sprintf("\nЗагрузка пакетов : %.2f c", pkg_secs))
  message(sprintf("Скрипты (сумма)  : %.2f c", sum(timings$seconds)))
  message(sprintf("ИТОГО R WALL     : %.2f c", total_secs))
  if (any(!timings$ok)) {
    message("\n!! Упавшие скрипты: ",
            paste(timings$script[!timings$ok], collapse = ", "))
    quit(status = 1)
  }
  invisible(timings)
}
