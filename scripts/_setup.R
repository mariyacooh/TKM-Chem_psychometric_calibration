# =============================================================================
#  _setup.R — общие бутстрап-функции для R-скриптов анализа.
# =============================================================================
#  Source-ится один раз в начале каждого скрипта анализа, сразу после блока,
#  фиксирующего рабочую директорию на корне проекта:
#
#      local({
#        if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#          root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
#          while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
#          if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
#        }
#      })
#      source("scripts/_setup.R")
#
#  Единая точка для ~15 строк шаблонного кода (рабочая директория, установка
#  пакетов, закрытие sink, создание выходных папок). Структура пунктов живёт в
#  scripts/config.R — source-ится отдельно; этот файл трогает только окружение R.
# =============================================================================

# =============================================================================
# Воспроизводимые числовые/статистические опции (не зависят от ~/.Rprofile хоста)
# =============================================================================
# Ни одна из них не задаётся иначе, поэтому без явной фиксации пользовательский
# .Rprofile становится скрытым входом: digits меняет значащие цифры в каждом
# write_csv_excel(), OutDec — десятичный разделитель во всех format()/as.character,
# scipen — порог экспоненты, contrasts — кодировку ANOVA в 8_anova. Значения =
# штатные дефолты R, поэтому фиксация ничего не меняет на чистом хосте, но
# устраняет .Rprofile как переменную. Дублируется в scripts/plots/_plot_utils.R и
# scripts/render_report_html.R (точки входа графиков и отчёта) — при правке
# синхронизировать.
options(digits = 7, OutDec = ".", scipen = 0,
        contrasts = c("contr.treatment", "contr.poly"))

# =============================================================================
# Версия R сверяется с пином .R-version (fail-fast)
# =============================================================================
# Пин объявлен авторитетным (README.ru.md, «Если R в системе нет»), а его
# значение продублировано руками в теге базы docker/Dockerfile, в `image:`
# docker/compose.yaml и в `IMAGE` container.py. Без исполняемой сверки расхождение
# молчаливо: смена .R-version не роняет ни прогон, ни сборку, контейнер
# поднимается на прежнем R, а дрейф патча на хосте (apt отдаёт соседний 4.6.x)
# проходит без сообщения.
# Место выбрано по охвату: _setup.R source-ится каждым шагом, рендером отчёта,
# сводом И install_deps.R, поэтому сверка покрывает и прогон, и сборку образа —
# .R-version копируется в образ рядом с install_deps.R (COPY в docker/Dockerfile),
# так что несовпадение тега базы с пином роняет саму сборку.
local({
  if (!file.exists(".R-version"))
    stop("Не найден .R-version: пин версии R авторитетен и обязан быть на месте ",
         "(рабочая директория: ", getwd(), ").", call. = FALSE)
  pin <- trimws(readLines(".R-version", warn = FALSE)[1])
  cur <- as.character(getRversion())
  if (!identical(cur, pin))
    stop(sprintf(paste0(
      "Версия R %s не совпадает с пином .R-version (%s). Бинарники PPM привязаны ",
      "к МИНОРНОЙ версии, а воспроизводимость прогона — к патчу. Установите ",
      "пиновый патч (README.ru.md, «Если R в системе нет») либо смените .R-version осознанно; ",
      "в контейнере пин задаёт тег базы в docker/Dockerfile."),
      cur, pin), call. = FALSE)
})

# Закрыть sink()-соединения, оставшиеся от предыдущего (прерванного) запуска.
reset_sink <- function() {
  while (sink.number() > 0) sink()
  invisible(NULL)
}

# =============================================================================
# Дата снапшота PPM — версии пакетов зафиксированы датой, а не «сегодняшним днём»
# =============================================================================
# Роль та же, что у .R-version для самого R: набор версий задан один раз и не
# зависит от дня установки. Незакреплённый источник — PPM `latest` под Linux,
# cran.r-project.org под Windows/macOS — отдаёт версии на день установки, и
# свежая установка расходится с опубликованным прогоном по двум линиям.
#
# Числа: величина, чувствительная к версии пакета, сдвигается молча — верхняя
# граница ДИ eta2 из effectsize (DECISIONS.md D10, где закрепление снапшота и
# названо порядком разбора).
#
# Работоспособность: снапшот бывает несогласован сам с собой, причём НЕ на всех
# платформах сразу — бинарники под Windows/macOS и под Linux собираются по
# отдельности и в разные дни. В 2026-07-28 windows-бинарник stringfish (собран
# 2026-07-21 под RcppParallel 5.1.11-2) не грузится с лежащим там же RcppParallel
# 6.1.1 — `LoadLibrary failure: The specified procedure could not be found` (6.x
# меняет ABI TBB). Через qs2 и SimDesign это уносит mirt, то есть шаги 4-7; обход —
# PKG_SNAPSHOT_OVERRIDE ниже.
PKG_SNAPSHOT <- "2026-07-28"

# Пакеты, версия которых берётся из ДРУГОГО снапшота — обход той самой
# несогласованности: бинарник в PKG_SNAPSHOT собран под более старую версию
# зависимости, чем лежит там же. Снимается, когда в снапшоте появится stringfish,
# собранный под RcppParallel 6.x (сверяется по полю Built в его DESCRIPTION).
#
# Обход привязан к платформе, потому что несогласованность привязана к ней же.
# Linux-снапшот (__linux__/noble на ту же дату) согласован: stringfish 0.19.0
# собран 2026-07-28 уже под RcppParallel 6.1.1 и грузится с ним. Понижение
# RcppParallel там не лечит, а ЛОМАЕТ — `undefined symbol:
# _ZN3tbb6detail2r122cache_aligned_allocateEm`, то есть ровно тот же обрыв
# mirt/difR, только зеркальный, и падает он на сборке образа (docker/Dockerfile,
# RUN install_deps.R). Проверено в rocker/tidyverse:4.6.0 на обеих ветках.
PKG_SNAPSHOT_OVERRIDE <- if (Sys.info()[["sysname"]] == "Linux") {
  character(0)
} else {
  c(RcppParallel = "2026-07-20")
}

# Репозиторий с бинарными сборками для текущей ОС на дату снапшота. Обычный CRAN
# (cran.r-project.org) под Linux отдаёт ТОЛЬКО исходники -> каждый пакет
# компилируется (RcppArmadillo, lavaan, mirt, TAM, semPlot -- минуты вместо
# секунд). Posit Package Manager (PPM) отдаёт бинарники под Linux, но лишь когда
# HTTPUserAgent объявляет сборку R, а кодовое имя ОС в URL совпадает. Бинарники
# у PPM привязаны к МИНОРНОЙ версии R (4.6); версия зафиксирована в .R-version,
# чтобы у PPM всегда были совпадающие бинарники. Windows/macOS берут бинарники
# из того же снапшота, но по общему URL — путь с кодовым именем ОС нужен только
# Linux.
binary_repo <- function(snapshot = PKG_SNAPSHOT) {
  if (Sys.info()[["sysname"]] != "Linux")
    return(c(CRAN = sprintf("https://packagemanager.posit.co/cran/%s", snapshot)))
  codename <- NA_character_
  if (file.exists("/etc/os-release")) {
    hit <- grep("^VERSION_CODENAME=", readLines("/etc/os-release", warn = FALSE), value = TRUE)
    if (length(hit)) codename <- gsub('"', '', sub('.*=', '', hit[1]))
  }
  if (is.na(codename) || !nzchar(codename)) codename <- "noble"  # цель по умолчанию; как в контейнере main253
  options(HTTPUserAgent = sprintf(
    "R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
  ))
  c(CRAN = sprintf("https://packagemanager.posit.co/cran/__linux__/%s/%s", codename, snapshot))
}

# Тип пакета для install.packages(). Под Windows/macOS дефолтный "both" уходит в
# исходники, когда версия в src/contrib новее собранной: SimDesign 2.26 в src/contrib
# при 2.25 в бинарниках даёт сборку из исходников, а под Windows это ещё и требует
# Rtools, которого на хосте может не быть. "binary" оставляет только собранные —
# то же обещание «никакой компиляции», что и у PPM под Linux, где бинарники
# отдаются по source-путям и тип остаётся дефолтным.
pkg_type <- function() {
  if (Sys.info()[["sysname"]] == "Linux") getOption("pkgType") else "binary"
}

# Проверить наличие пакетов из `pkgs` и подключить их. НЕ устанавливает на лету:
# зависимости ставятся один раз явным шагом (scripts/install_deps.R -> binary_repo()),
# иначе на свежем Linux-хосте недостающий пакет тянулся бы из исходников (компиляция).
# При отсутствии пакета — падение с понятным указанием, а не молчаливая компиляция.
load_pkgs <- function(pkgs) {
  missing <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(missing)) {
    stop(sprintf(
      "Не установлены пакеты: %s. Установите зависимости один раз: Rscript scripts/install_deps.R.",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# Создать выходную директорию (рекурсивно, без предупреждений); вернуть путь невидимо.
ensure_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  invisible(path)
}

# =============================================================================
# Регистро-независимое приведение к нижнему регистру (кириллица + латиница)
# =============================================================================
# base::tolower() складывает ASCII A-Z в любой локали, но НЕ-ASCII (кириллицу)
# складывает через LC_CTYPE — под локалью C свёртка молча не срабатывает, и
# scoring/детект языка/маппинг регионов ломаются. Кириллица складывается явным
# chartr(), чтобы корректность НЕ зависела от переменной окружения. Пары
# (RU 32 буквы + KZ 9 букв) выровнены поштучно; enc2utf8() гарантирует, что
# chartr работает по кодовым точкам, а не по байтам.
.CYR_UPPER <- "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯӘҒҚҢӨҰҮҺІ"
.CYR_LOWER <- "абвгдеёжзийклмнопрстуфхцчшщъыьэюяәғқңөұүһі"
cyr_tolower <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))   # ASCII складывается локале-независимо
  chartr(.CYR_UPPER, .CYR_LOWER, x)          # кириллица — явной картой
}

# =============================================================================
# Языковая форма респондента (единый источник правил для шагов 0 и 9)
# =============================================================================
# Шаг 0 выбирает по языку столбцы ответов и демографии, шаг 9 — столбцы сырых
# текстов вариантов из того же xlsx. Правило живёт здесь ровно один раз, потому что
# по своему набору регулярных выражений на шаг наборы расходятся по двум линиям:
# по составу токенов (набор без казахского «қаз» и с якорем «^kz» часть строк не
# распознаёт) и по наличию запасного разрешения (без резолва по заполненности
# нераспознанный язык роняет проверку выравнивания шага 9 на записи, которую шаг 0
# штатно спасает).

# Пусто: "", "nan", "none", "-" и NA (сырая ячейка xlsx без значения).
is_empty <- function(val) {
  v <- as.character(val)
  is.na(v) | tolower(trimws(v)) %in% c("", "nan", "none", "-")
}

# Язык по тексту ответа на вопрос о языке; NA, если не распознан.
detect_lang <- function(val) {
  v <- cyr_tolower(trimws(val))
  # Только названия языка: "иә"/"жоқ" — словарь map_conference(), и проверялись бы
  # ДО всего ru-списка, отправляя любой ответ с "иә" в kz.
  for (m in c("қазақ", "казах", "kz",
              "каз", "kazakh", "қаз")) {  # қазақ,казах,kz,каз,kazakh,қаз
    if (grepl(m, v, fixed = TRUE)) return("kz")
  }
  for (m in c("русск", "рус", "ru", "russian",
              "орыс")) {            # русск,рус,ru,russian,орыс
    if (grepl(m, v, fixed = TRUE)) return("ru")
  }
  NA_character_
}

# Итоговая форма: текст языка, а при нераспознанном — та форма, где заполнено
# больше пунктов (равенство -> kz). Возвращает всегда "kz" либо "ru".
resolve_lang <- function(val, kz_filled, ru_filled) {
  lg <- detect_lang(val)
  if (is.na(lg)) lg <- if (kz_filled >= ru_filled) "kz" else "ru"
  lg
}

# =============================================================================
# Проверка BOM при чтении CSV (fail-fast против рассинхрона кодировок)
# =============================================================================
# Источники input/items.csv, answer_key.csv, region_map.csv и все промежуточные
# output/*.csv — utf-8-sig (BOM), читаются fileEncoding="UTF-8-BOM"; для items.csv
# наличие BOM проверяет сам config.R и падает без него. Если фактический BOM файла
# разойдётся с объявленной кодировкой при чтении — кириллица молча испортится. Первые
# байты сверяются, расхождение — громкое падение: кодировка не молчаливое допущение.
assert_bom <- function(path, expect_bom) {
  raw <- readBin(path, "raw", n = 3L)
  has_bom <- length(raw) == 3L && all(raw == as.raw(c(0xEF, 0xBB, 0xBF)))
  if (has_bom != expect_bom) {
    stop(sprintf(
      "%s: BOM %s, ожидался %s — кодировка рассинхронизирована (кириллица испортится).",
      path, if (has_bom) "присутствует" else "отсутствует",
      if (expect_bom) "utf-8-sig (BOM)" else "UTF-8 без BOM"),
      call. = FALSE)
  }
  invisible(path)
}

# =============================================================================
# Запись CSV в кодировке utf-8-sig (BOM) — Excel открывает по двойному клику
# =============================================================================
# Единственный писатель CSV в пайплайне. BOM utf-8-sig + переводы строк LF =>
# Excel по двойному клику декодирует UTF-8 (без BOM он читает кириллицу как
# символы локальной кодовой страницы). Квотирование минимальное (поле квотируется
# только при наличии , " CR или LF). Числовые колонки форматируются через
# format() — те же значащие цифры, что в консоли R, без float-шума as.character;
# NA => пустое поле; строковые колонки идут как есть (шаги 0/1/1a подают их уже
# отформатированными). Обратное чтение через readr::read_csv снимает BOM
# прозрачно; базовый read.csv в пайплайне читает с fileEncoding = "UTF-8-BOM".

# Поле квотируется только если в нём , " CR или LF; NA -> пустое поле.
csv_field <- function(s) {
  s <- ifelse(is.na(s), "", as.character(s))
  need <- grepl("[,\"\r\n]", s)
  q <- gsub("\"", "\"\"", s, fixed = TRUE)
  ifelse(need, paste0("\"", q, "\""), s)
}

write_csv_excel <- function(df, path) {
  render_col <- function(col) {
    if (is.numeric(col)) {
      txt <- format(col, trim = TRUE)
      txt[is.na(col)] <- ""
      csv_field(txt)
    } else {
      csv_field(col)
    }
  }
  header <- paste(vapply(names(df), csv_field, character(1)), collapse = ",")
  body <- lapply(df, render_col)
  n <- nrow(df)
  lines <- if (n > 0) vapply(seq_len(n), function(i) paste(vapply(body, `[[`, character(1), i), collapse = ","), character(1)) else character(0)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw("﻿"), con)
  writeBin(charToRaw(enc2utf8(paste0(paste(c(header, lines), collapse = "\n"), "\n"))), con)
  invisible(df)
}

# =============================================================================
# Контракт зависимостей между шагами (артефакты output/)
# =============================================================================
# require_input() / optional_input() + полный граф producer -> consumer живут в
# scripts/_artifacts.R; scripts/plots/_plot_utils.R source-ит его отдельно, поэтому
# графики получают те же функции. Отсутствующий обязательный вход = stop.
source("scripts/_artifacts.R", local = TRUE)
