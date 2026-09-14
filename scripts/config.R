# =============================================================================
#  config.R — единый источник структуры пунктов и исключений
# =============================================================================
#  Структура пунктов (субшкалы, исключения, отсутствующие пункты, заметки по
#  каждому пункту) хранится ОДИН раз в input/items.csv. Этот файл её читает и
#  разворачивает в имена, используемые всеми скриптами пайплайна (весь пайплайн
#  на R). Единый источник => препроцессинг, анализы и графики не могут разойтись
#  в том, какие пункты участвуют в работе.
#
#  Схема items.csv (по строке на каждый слот, отсортировано по Q):
#    item      Q01..Q29
#    subscale  S1 / S2 / S3 (естественная принадлежность; ПУСТО => пункт
#              отсутствует в схеме Google Forms, т.е. absent)
#    excluded  ПУСТО => пункт включён (по умолчанию); TRUE => исключён из
#              Score_Total и всех анализов (труъ-токены: true/t/1/yes/x)
#    note      свободный комментарий по пункту (не используется в расчётах)
#
#  Три состояния выводятся из двух колонок:
#    included  subscale задана, excluded пусто
#    excluded  excluded=TRUE (subscale задана)
#    absent    subscale пуста
#
#  Чтобы вернуть/исключить пункт или перенести его в другую субшкалу —
#  правится ОДНА ячейка в items.csv, а не два скрипта.
#
#  Использование из корня проекта (Rscript scripts/<имя>.R или RStudio-проект):
#    source("scripts/config.R")
# =============================================================================

# Только база R: config.R source-ится и скриптами графиков, и report.Rmd,
# где tidyverse может быть не загружен.

# Найти input/items.csv. config.R лежит в scripts/, items.csv — в соседней input/
# под корнем проекта; корень вычисляется из ofile (при source() путь известен).
# Запасные пути — по соглашению пайплайна (рабочая директория = корень проекта).
.items_csv <- local({
  candidates <- character(0)
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(e) e$ofile))
  if (length(frame_files)) {
    root <- dirname(dirname(normalizePath(
      frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE)))
    candidates <- c(candidates, file.path(root, "input", "items.csv"))
  }
  candidates <- c(candidates, "input/items.csv", "../input/items.csv")
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop("config.R: не найден input/items.csv (искал: ",
         paste(candidates, collapse = ", "), ")", call. = FALSE)
  }
  hit[[1]]
})

# items.csv — UTF-8 с BOM (utf-8-sig), как и рукописные ключи и все промежуточные
# output/*.csv: единая кодировка во всём пайплайне. Читается fileEncoding=
# "UTF-8-BOM" (снимает BOM). Если BOM пропадёт, read.csv оставит невидимый префикс
# ﻿ на первой ячейке и колонка item перестанет матчиться — отсюда громкое падение.
# config.R — только базовый R (без _setup.R), проверка встроена.
local({
  .bom <- readBin(.items_csv, "raw", n = 3L)
  if (!(length(.bom) == 3L && all(.bom == as.raw(c(0xEF, 0xBB, 0xBF)))))
    stop("config.R: ", .items_csv, " без BOM, ожидался utf-8-sig (BOM) ",
         "(читается fileEncoding='UTF-8-BOM') — кириллица испортится.", call. = FALSE)
})

.items_df <- utils::read.csv(
  .items_csv, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE,
  colClasses = "character", na.strings = character(0)
)
.items_df$item     <- trimws(.items_df$item)
.items_df$subscale <- trimws(.items_df$subscale)
.items_df$excluded <- trimws(.items_df$excluded)

# excluded: пусто => включён (по умолчанию); труъ-токен => исключён.
.is_excluded <- tolower(.items_df$excluded) %in% c("true", "t", "1", "yes", "x")
# absent: пункт не предъявлялся (нет субшкалы). absent имеет приоритет над excluded.
.is_absent   <- .items_df$subscale == ""

.included <- .items_df[!.is_excluded & !.is_absent, , drop = FALSE]

# Пункты, отсутствующие в схеме Google Forms
ABSENT_ITEMS <- .items_df$item[.is_absent]

# Пункты предъявлялись и оценивались, но исключены из Score_Total и ВСЕХ анализов.
# Остаются колонками в очищенных данных. Причины — в колонке note файла items.csv.
EXCLUDED_ITEMS <- .items_df$item[.is_excluded & !.is_absent]

# Структура субшкал (исключённые/отсутствующие пункты уже убраны). Порядок
# субшкал — по первому появлению во included-строках; порядок пунктов — по CSV.
SUBSCALES <- local({
  ord <- unique(.included$subscale)
  s <- lapply(ord, function(g) .included$item[.included$subscale == g])
  names(s) <- ord
  s
})

# Пункты, участвующие в анализе (сейчас 26; было 27 до исключения Q02 — DECISIONS.md D4)
ITEMS <- unlist(SUBSCALES, use.names = FALSE)

# Соответствие пункт -> субшкала (выровнено по порядку ITEMS)
ITEM_SUB <- setNames(.included$subscale, .included$item)[ITEMS]

# Все предъявлявшиеся пункты (только для анализа дистракторов, который по
# замыслу смотрит и на варианты ответов исключённых пунктов)
ALL_ADMINISTERED <- sort(c(ITEMS, EXCLUDED_ITEMS))

rm(.items_csv, .items_df, .included, .is_excluded, .is_absent)

# -----------------------------------------------------------------------------
#  validate_items(found, context)
#  Вызывается в каждом скрипте графиков после чтения промежуточного файла.
#  Громко падает, если файл был создан до текущих исключений — график никогда
#  не должен строиться по устаревшим данным.
# -----------------------------------------------------------------------------
validate_items <- function(found, context) {
  found <- as.character(found)
  stale <- intersect(found, EXCLUDED_ITEMS)
  if (length(stale) > 0) {
    stop(sprintf(
      "УСТАРЕВШИЙ ВХОД для %s: содержит исключённые пункты %s.\n  Файл создан до текущих исключений — сначала перезапустите соответствующий скрипт анализа.",
      context, paste(stale, collapse = ", ")
    ), call. = FALSE)
  }
  missing <- setdiff(ITEMS, found)
  if (length(missing) > 0) {
    warning(sprintf(
      "! %s: во входных данных отсутствуют ожидаемые пункты: %s",
      context, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}
