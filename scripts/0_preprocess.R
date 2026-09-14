#!/usr/bin/env Rscript
# =============================================================================
#  ШАГ 0 — ПРЕПРОЦЕССИНГ GOOGLE FORMS
# -----------------------------------------------------------------------------
#  ВХОД : input/google_forms_responses.xlsx  (сырые ответы)
#         input/answer_key.csv               (ключ верных ответов kz/ru)
#         input/region_map.csv               (нормализация регионов)
#         input/items.csv                    (через scripts/config.R)
#  ВЫХОД: output/cleaned_responses.csv  -> ЦЕНТРАЛЬНЫЙ артефакт: шаги 1-9,
#                                          plot_descriptive_density, report.Rmd
#         output/preprocess_summary.csv -> свод write_report_text.R (ЖЁСТКИЙ вход:
#                                          N берётся оттуда); ЕДИНСТВЕННЫЙ источник
#                                          чисел отбора выборки
#         output/answer_key.csv        -> терминальный (никто не читает)
#
#  Корень графа зависимостей: без этого шага не выполняется ни один следующий.
#  Полный граф и правила отказа — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------------
#
#  Позиционный маппинг столбцов, бинарная оценка ответов по ключу, маппинг
#  демографии, отсев пустых записей. Пишет output/cleaned_responses.csv и
#  output/answer_key.csv в кодировке utf-8-sig (BOM + LF); формат чисел в
#  колонках фиксирован (см. fmt_float / render_int_col ниже).
#
#  Ключ ответов и таблица регионов живут в input/answer_key.csv и
#  input/region_map.csv (единый источник, без хардкода кириллицы в коде).
#
#  Структура файла (подтверждена анализом), индексы столбцов заданы 0-based;
#  в R к ним прибавляется 1:
#    Col 0  — Отметка времени; Col 1 — язык; Col 2 — начать (KZ);
#    Cols 3-7 KZ демография; Cols 8-34 KZ ответы Q01-Q29 (27, без Q17/Q25);
#    Col 35 — начать (RU); Cols 36-40 RU демография; Cols 41-67 RU ответы.
# =============================================================================

local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/_setup.R")   # csv_field / write_csv_excel (CSV utf-8-sig)
library(readxl)
source("scripts/config.R")   # EXCLUDED_ITEMS, SUBSCALES (единый источник структуры)

# ── Позиционный маппинг (индексы 0-based) ────────────────────────────────────
LANG_COL <- 1L
KZ_DEMO <- c(region = 3L, gpa_range = 4L, course = 5L, conference = 6L, gpa_text = 7L)
RU_DEMO <- c(region = 36L, course = 37L, gpa_range = 38L, conference = 39L, gpa_text = 40L)

# item_num -> c(kz_col, ru_col)  (0-based); порядок как в форме
ITEM_TO_COLS <- list(
  "1"=c(8,41),  "2"=c(9,42),  "3"=c(10,43), "4"=c(11,44), "5"=c(12,45),
  "6"=c(13,46), "7"=c(14,47), "8"=c(15,48), "9"=c(16,49), "10"=c(17,50),
  "11"=c(18,51),"12"=c(19,52),"13"=c(20,53),"14"=c(21,54),"15"=c(22,55),
  "16"=c(23,56),"18"=c(24,57),"19"=c(25,58),"20"=c(26,59),"21"=c(27,60),
  "22"=c(28,61),"23"=c(29,62),"24"=c(30,63),"26"=c(31,64),"27"=c(32,65),
  "28"=c(33,66),"29"=c(34,67)
)
ITEM_NUMS <- as.integer(names(ITEM_TO_COLS))
ITEM_KEYS <- sprintf("Q%02d", ITEM_NUMS)

EXCLUDED_FROM_TOTAL <- EXCLUDED_ITEMS   # из config.R

# Score_Total и субшкальные баллы берут состав пунктов из РАЗНЫХ множеств:
# Score_Total — предъявленные (ITEM_KEYS) минус EXCLUDED_ITEMS, субшкалы — SUBSCALES
# из items.csv. Равенство Score_Total = Score_S1 + Score_S2 + Score_S3 держится только
# пока эти множества совпадают: пункт, предъявленный без субшкалы (пустая ячейка
# subscale), попал бы в Score_Total и выпал из субшкал, а колонки разъехались бы молча.
# Отсюда структурная проверка совпадения — до чтения xlsx, чтобы падение указывало на
# items.csv, а не на данные.
local({
  scored  <- setdiff(ITEM_KEYS, EXCLUDED_FROM_TOTAL)
  in_subs <- unlist(SUBSCALES, use.names = FALSE)
  dup     <- unique(in_subs[duplicated(in_subs)])
  no_sub  <- setdiff(scored, in_subs)    # оценивается, но не входит ни в одну субшкалу
  no_col  <- setdiff(in_subs, ITEM_KEYS) # субшкала ссылается на непредъявленный пункт
  if (length(dup) || length(no_sub) || length(no_col)) {
    stop("[ERROR] состав пунктов рассогласован между input/items.csv и схемой формы:",
         if (length(no_sub)) paste0("\n  входит в Score_Total, но без субшкалы: ",
                                    paste(no_sub, collapse = ", ")) else "",
         if (length(no_col)) paste0("\n  субшкала ссылается на непредъявленный пункт: ",
                                    paste(no_col, collapse = ", ")) else "",
         if (length(dup))    paste0("\n  пункт указан в нескольких субшкалах: ",
                                    paste(dup, collapse = ", ")) else "",
         "\n  Иначе Score_Total != Score_S1 + ... — исправьте input/items.csv.",
         call. = FALSE)
  }
})

# ── Ключ ответов и карта регионов (input/) ───────────────────────────────────
# Входы шага — корни графа: их производит не пайплайн, а репозиторий. Проверка идёт
# до чтения, иначе assert_bom() падает невнятным "cannot open file".
require_input(c("input/answer_key.csv", "input/region_map.csv"),
              "репозиторий (input/, версионируется)")
assert_bom("input/answer_key.csv", TRUE)
.ak <- read.csv("input/answer_key.csv", fileEncoding = "UTF-8-BOM",
                stringsAsFactors = FALSE, colClasses = "character")
ANSWER_KZ <- setNames(.ak$kz, .ak$item)
ANSWER_RU <- setNames(.ak$ru, .ak$item)
# Проверка пустых ключей — ниже, после определения normalize(): is_correct()
# сравнивает НОРМАЛИЗОВАННЫЕ строки, поэтому и пустота ключа проверяется по ней.
assert_bom("input/region_map.csv", TRUE)
.rm <- read.csv("input/region_map.csv", fileEncoding = "UTF-8-BOM",
                stringsAsFactors = FALSE, colClasses = "character")
REGION_PAT <- .rm$pattern
REGION_CAN <- .rm$canonical

# ── Вспомогательные функции ──────────────────────────────────────────────────

# Символы, снимаемые normalize(): « » \ " ' ‹ ›  (U+00AB,00BB,005C,0022,0027,2039,203A).
# Литеральный бэкслеш (U+005C) входит в набор намеренно.
.QUOTE_CHARS <- c("\u00AB", "\u00BB", "\u005C", "\u0022", "\u0027", "\u2039", "\u203A")

normalize <- function(text) {
  text <- ifelse(is.na(text), "", as.character(text))
  text <- cyr_tolower(trimws(text))   # локале-независимая свёртка кириллицы
  for (ch in .QUOTE_CHARS) text <- gsub(ch, "", text, fixed = TRUE)   # снять кавычки
  text <- sub("[.,;:!?…]+$", "", text)                          # rstrip пунктуации
  text <- sub("^[a-dа-г]\\.\\s*", "", text, perl = TRUE)   # префикс "a." / "а."
  text <- gsub("\\s+", " ", text, perl = TRUE)                       # схлопнуть пробелы
  text <- trimws(text)
  # Снятие пробела перед пунктуацией — НЕСУЩЕЕ для оценивания правило, не косметика:
  # без него часть верных ответов не совпадает с ключом и Score_Total меняется
  # (безопасный порядок правки — DECISIONS.md D7). Изменение — только вместе с
  # перепроверкой Score_Total.
  text <- gsub("\\s+([:;,.!?…])", "\\1", text, perl = TRUE)
  gsub("ё", "е", text, fixed = TRUE)                       # ё -> е
}

# Пустой ключ фатален: is_correct() тогда засчитал бы ЛЮБОЙ ответ (grepl("", x)).
# Проверяется НОРМАЛИЗОВАННАЯ форма: сырой непустой ключ из одних кавычек/пунктуации
# normalize() сводит к "", поэтому проверка сырого trimws() такой ключ пропустила бы.
.norm_kz <- vapply(ANSWER_KZ, normalize, character(1))
.norm_ru <- vapply(ANSWER_RU, normalize, character(1))
.empty_key <- !nzchar(.norm_kz) | !nzchar(.norm_ru)   # normalize(NA) -> "" тоже ловится
if (any(.empty_key)) {
  stop("[ERROR] answer_key.csv: пустой ключ у пунктов ",
       paste(names(ANSWER_KZ)[.empty_key], collapse = ", "),
       " — заполните input/answer_key.csv (иначе is_correct() засчитает любой ответ)",
       call. = FALSE)
}

is_correct <- function(response, correct) {
  r <- normalize(response); cc <- normalize(correct)
  if (!nzchar(r)) return(0L)
  if (r == cc) return(1L)
  if (grepl(cc, r, fixed = TRUE)) return(1L)                          # ключ содержится в ответе
  if (nchar(r) >= 15L && grepl(r, cc, fixed = TRUE)) return(1L)       # ответ — часть ключа
  0L
}

# detect_lang() / resolve_lang() / is_empty() — в scripts/_setup.R: те же правила
# нужны шагу 9 (выбор столбцов сырых текстов вариантов из того же xlsx), а два
# набора регулярных выражений на одних данных расходятся между собой.

# get_val: "" для "", "nan", "none" (но НЕ для "-", в отличие от is_empty)
get_val <- function(vals, col0) {
  col1 <- col0 + 1L
  if (col1 <= length(vals)) {
    v <- trimws(as.character(vals[[col1]]))
    if (tolower(v) %in% c("", "nan", "none")) "" else v
  } else ""
}

# Соглашение о свёртке бина GPA в число, РАЗНОЕ для двух форм бина:
#   закрытый ("2.67 – 3.32", "3.67 – 4.0") -> середина;
#   открытый ("Ниже 2.0", "2.0-ден төмен") -> ГРАНИЦА бина, то есть 2.0.
# Середины у открытого бина нет, и граница выбрана намеренно, а не за неимением
# лучшего: у зачисленных студентов фактический GPA прижат к верху такого бина,
# поэтому 2.0 смещает оценку меньше, чем формальная середина 0–2.0. На текущих
# данных под открытый бин попадают 2 респондента (1 KZ + 1 RU).
.GPA_OPEN_BIN <- "төмен|ниже|below|жоғары|выше|above"

map_gpa_range <- function(val) {
  if (is.na(val) || !nzchar(trimws(val))) return(NA_real_)
  s <- gsub(",", ".", val, fixed = TRUE)
  nums <- regmatches(s, gregexpr("[0-9]+[.][0-9]+|[0-9]+", s))[[1]]
  fl <- as.numeric(nums)
  if (length(fl) == 2L) return(round((fl[1] + fl[2]) / 2, 3))
  if (length(fl) == 1L) {
    # Одиночное число ожидается только у открытого бина. Иная формулировка с одним
    # числом означает, что список вариантов формы разошёлся с этим соглашением:
    # значение всё равно берётся как есть, но факт попадает в диагностику прогона.
    if (!grepl(.GPA_OPEN_BIN, cyr_tolower(val), perl = TRUE))
      message("!! GPA: одиночное число вне открытого бина, взято как есть: ", val)
    return(fl[1])
  }
  if (length(fl) > 2L)
    message("!! GPA: более двух чисел в значении, свёртка невозможна -> NA: ", val)
  NA_real_
}

map_gpa_text_to_num <- function(val) {
  v <- normalize(val)
  has <- function(...) any(vapply(c(...), function(x) grepl(x, v, fixed = TRUE), logical(1)))
  # Порядок — от самой специфичной формулировки к самой общей; первый матч
  # выигрывает, поэтому охранные !grepl не нужны. Крайние («значительно/
  # айтарлықтай») проверяются раньше общих «жоғары»/«төмен», иначе те бы
  # перехватили «... айтарлықтай жоғары/төмен».
  if (has("значительно выше", "айтарлықтай жоғары",
          "үздік 10", "топ 10")) return(5L)                 # значительно выше среднего
  if (has("значительно ниже", "айтарлықтай төмен",
          "орта деңгейден айтарлықтай төмен")) return(1L)   # значительно ниже среднего
  if (has("выше среднего", "орта деңгейден жоғары",
          "жоғары")) return(4L)                             # выше среднего
  if (has("ниже среднего", "орта деңгейден төмен")) return(2L)   # ниже среднего
  if (has("орта деңгейде", "на уровне среднего")) return(3L)     # на уровне среднего
  NA_integer_
}

map_course <- function(val) {
  m <- regmatches(as.character(val), regexpr("\\b[1-4]\\b", as.character(val), perl = TRUE))
  if (length(m) && nzchar(m[1])) as.integer(m[1]) else NA_integer_
}

map_conference <- function(val) {
  v <- normalize(val)
  has <- function(...) any(vapply(c(...), function(x) grepl(x, v, fixed = TRUE), logical(1)))
  if (has("да", "иә", "yes", "ия")) return(1L)   # да,иә,yes,ия
  if (has("нет", "жоқ", "no")) return(0L)         # нет,жоқ,no
  NA_integer_
}

normalize_region <- function(val) {
  if (is.na(val) || !nzchar(trimws(val))) return(NA_character_)
  v <- gsub("ё", "е", cyr_tolower(trimws(val)), fixed = TRUE)   # lower + ё->е
  hit <- which(vapply(REGION_PAT, function(p) grepl(p, v, fixed = TRUE), logical(1)))
  if (length(hit)) return(REGION_CAN[hit[1]])
  trimws(val)
}

# ── CSV-запись: utf-8-sig (BOM + LF), минимальное квотирование ────────────────

# Форматирование float: минимальные значащие цифры; целочисленные -> "N.0".
fmt_float <- function(x) {
  # as.character даёт кратчайшую запись для диапазона GPA 0-5; целочисленные
  # значения отдельно -> "N.0" (float-колонка всегда с дробной частью).
  ifelse(is.na(x), "",
    ifelse(x == round(x), sprintf("%.1f", x), as.character(x)))
}

# Колонка целых: если есть NA -> float-рендер ("3.0"/""); иначе int ("3").
render_int_col <- function(x) {
  if (any(is.na(x))) ifelse(is.na(x), "", sprintf("%.1f", as.numeric(x)))
  else as.character(as.integer(x))
}

# csv_field / write_csv_excel (utf-8-sig, LF) — общие в scripts/_setup.R.

# ── Основной проход ──────────────────────────────────────────────────────────
main <- function() {
  forms_file <- "input/google_forms_responses.xlsx"
  require_input(forms_file, "репозиторий (input/, версионируется)")

  # Всё читается как текст; первая строка — заголовок формы (снимается ниже).
  raw <- read_excel(forms_file, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  raw[is.na(raw)] <- ""                 # пустые ячейки -> ""
  raw <- raw[-1, , drop = FALSE]        # убрать строку заголовка
  rownames(raw) <- NULL
  n_raw <- nrow(raw)

  rec_list <- vector("list", n_raw)
  lang_stats <- c(kz = 0L, ru = 0L, unknown = 0L)

  for (idx in seq_len(n_raw)) {
    vals <- as.list(raw[idx, ])

    lang_raw <- if (length(vals) > LANG_COL) trimws(as.character(vals[[LANG_COL + 1L]])) else ""
    # Заполненность обеих форм считается всегда: она нужна resolve_lang() как
    # запасное правило, а счётчик unknown — только для итоговой строки лога.
    kz_filled <- sum(vapply(ITEM_TO_COLS, function(cc) {
      c0 <- cc[1]; c0 + 1L <= length(vals) && !is_empty(vals[[c0 + 1L]])
    }, logical(1)))
    ru_filled <- sum(vapply(ITEM_TO_COLS, function(cc) {
      c0 <- cc[2]; c0 + 1L <= length(vals) && !is_empty(vals[[c0 + 1L]])
    }, logical(1)))
    if (is.na(detect_lang(lang_raw)))
      lang_stats["unknown"] <- lang_stats["unknown"] + 1L   # язык не распознан -> резолв по заполненности
    resp_lang <- resolve_lang(lang_raw, kz_filled, ru_filled)
    lang_stats[resp_lang] <- lang_stats[resp_lang] + 1L

    demo <- if (resp_lang == "kz") KZ_DEMO else RU_DEMO
    region_raw     <- get_val(vals, demo[["region"]])
    gpa_range_raw  <- get_val(vals, demo[["gpa_range"]])
    course_raw     <- get_val(vals, demo[["course"]])
    conference_raw <- get_val(vals, demo[["conference"]])
    gpa_text_raw   <- get_val(vals, demo[["gpa_text"]])

    item_scores <- integer(length(ITEM_NUMS))
    names(item_scores) <- ITEM_KEYS
    for (j in seq_along(ITEM_NUMS)) {
      cc <- ITEM_TO_COLS[[j]]
      col0 <- if (resp_lang == "kz") cc[1] else cc[2]
      response <- get_val(vals, col0)
      key <- ITEM_KEYS[j]
      correct <- if (resp_lang == "kz") ANSWER_KZ[[key]] else ANSWER_RU[[key]]
      item_scores[j] <- is_correct(response, correct)
    }

    n_answered <- sum(vapply(ITEM_TO_COLS, function(cc) {
      col0 <- if (resp_lang == "kz") cc[1] else cc[2]
      !is_empty(get_val(vals, col0))
    }, logical(1)))

    score_total <- sum(item_scores[!(names(item_scores) %in% EXCLUDED_FROM_TOTAL)])

    rec_list[[idx]] <- list(
      ID = idx, Language = resp_lang,
      GPA_numeric = map_gpa_range(gpa_range_raw),
      GPA_ordinal = map_gpa_text_to_num(gpa_text_raw),
      GPA_text_raw = if (nzchar(gpa_text_raw)) gpa_text_raw else NA_character_,
      Course = map_course(course_raw),
      Conference = map_conference(conference_raw),
      Region = normalize_region(region_raw),
      item_scores = item_scores,
      Score_Total = as.integer(score_total),
      n_answered = n_answered
    )
  }

  # Фактический N = число НЕпустых записей (nrow), а не литерал — денумератор нигде
  # не хардкодится (см. DECISIONS.md D5). На текущих данных пустых записей нет,
  # поэтому N=253; исследование закрыто, новых ответов не ожидается.
  keep <- vapply(rec_list, function(r) r$n_answered != 0L, logical(1))
  rec_list <- rec_list[keep]
  n_clean <- length(rec_list)

  # Собрать data.frame в каноническом порядке колонок.
  item_mat <- t(vapply(rec_list, function(r) r$item_scores, integer(length(ITEM_KEYS))))
  colnames(item_mat) <- ITEM_KEYS
  out <- data.frame(
    ID = seq_len(n_clean),
    Language = vapply(rec_list, function(r) r$Language, character(1)),
    GPA_numeric = vapply(rec_list, function(r) r$GPA_numeric, numeric(1)),
    GPA_ordinal = vapply(rec_list, function(r) as.integer(r$GPA_ordinal), integer(1)),
    GPA_text_raw = vapply(rec_list, function(r) r$GPA_text_raw, character(1)),
    Course = vapply(rec_list, function(r) as.integer(r$Course), integer(1)),
    Conference = vapply(rec_list, function(r) as.integer(r$Conference), integer(1)),
    Region = vapply(rec_list, function(r) r$Region, character(1)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  out <- cbind(out, as.data.frame(item_mat, check.names = FALSE))
  out$Score_Total <- vapply(rec_list, function(r) r$Score_Total, integer(1))
  # Субшкальные баллы считаются ОДИН раз здесь; шаги 1/1a/8 и side-анализы
  # читают готовые колонки Score_S1..S3, а не пересчитывают rowSums каждый сам.
  # Состав субшкалы берётся целиком, без отбора по наличию колонки: совпадение
  # SUBSCALES со схемой формы гарантировано проверкой выше, поэтому фильтр по
  # пересечению только скрыл бы рассогласование, занижая Score_S* без ошибки.
  for (s in names(SUBSCALES)) {
    out[[paste0("Score_", s)]] <- as.integer(rowSums(out[, SUBSCALES[[s]], drop = FALSE]))
  }

  # Рендер колонок в строковую форму для CSV.
  render <- data.frame(
    ID = as.character(out$ID),
    Language = out$Language,
    GPA_numeric = fmt_float(out$GPA_numeric),
    GPA_ordinal = render_int_col(out$GPA_ordinal),
    GPA_text_raw = out$GPA_text_raw,
    Course = render_int_col(out$Course),
    Conference = render_int_col(out$Conference),
    Region = out$Region,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  for (k in ITEM_KEYS) render[[k]] <- as.character(out[[k]])
  render$Score_Total <- as.character(out$Score_Total)
  for (s in names(SUBSCALES)) {
    render[[paste0("Score_", s)]] <- as.character(out[[paste0("Score_", s)]])
  }

  dir.create("output", showWarnings = FALSE, recursive = TRUE)
  write_csv_excel(render, "output/cleaned_responses.csv")

  # answer_key.csv (item,kz_answer,ru_answer) — в порядке ключа.
  ak_out <- data.frame(item = .ak$item, kz_answer = .ak$kz, ru_answer = .ak$ru,
                       stringsAsFactors = FALSE, check.names = FALSE)
  write_csv_excel(ak_out, "output/answer_key.csv")

  # Провенанс отбора выборки: сколько строк пришло из выгрузки, сколько снято как
  # пустые, сколько осталось. Ни один другой шаг этих чисел не выводит, а статья
  # отчитывается по отсеву, поэтому они идут в артефакт. Консоль под них не годится:
  # она отдана диагностике и не сохраняется.
  #
  # Доли форм KZ/RU здесь НЕ пишутся — их считает шаг 1
  # (Descriptive/descriptive_stats_tables.csv), и второй источник тех же чисел мог бы
  # с ним разойтись. Из языковой статистики уникален только резолв по заполненности:
  # сколько записей пришлось отнести к форме не по тексту ответа о языке, а по тому,
  # в какой форме заполнено больше пунктов (см. resolve_lang в scripts/_setup.R).
  write_csv_excel(data.frame(
    Metric = c("Строк в выгрузке", "Пустых (удалено)", "Итоговая выборка",
               "Язык резолвлен по заполненности ответов"),
    Value  = c(n_raw, n_raw - n_clean, n_clean, lang_stats[["unknown"]]),
    stringsAsFactors = FALSE, check.names = FALSE
  ), "output/preprocess_summary.csv")

  invisible(out)
}

main()
