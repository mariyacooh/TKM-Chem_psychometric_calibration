#!/usr/bin/env Rscript
# =============================================================================
#  install_deps.R — единственная точка установки R-зависимостей пайплайна.
#  Ставит весь набор БИНАРНИКАМИ из снапшота PPM на дату PKG_SNAPSHOT —
#  репозиторий и тип пакета даёт binary_repo() / pkg_type() из _setup.R. Версия R
#  зафиксирована в .R-version, чтобы у PPM всегда были совпадающие бинарники и
#  ничто не собиралось из исходников; дата снапшота фиксирует ВЕРСИИ пакетов, без
#  неё установка зависела бы от дня. Сам пайплайн НИЧЕГО не ставит на лету —
#  только library(); при отсутствии пакета load_pkgs() падает с указанием
#  запустить этот скрипт.
#
#  Запуск:  Rscript scripts/install_deps.R
#  В контейнере ставится на этапе сборки образа (docker/Dockerfile), поэтому
#  отдельного шага установки в go.py нет.
# =============================================================================

# Рабочая директория — корень проекта (этот файл лежит в scripts/).
.args <- commandArgs(trailingOnly = FALSE)
.file <- sub("^--file=", "", .args[grep("^--file=", .args)])
.root <- if (length(.file)) normalizePath(file.path(dirname(.file), ".."), winslash = "/") else getwd()
setwd(.root)

source("scripts/_setup.R")

# Единый канонический список пакетов пайплайна (шаги 0-9, графики, отчёт).
pkgs <- c(
  "tidyverse", "readxl", "readr", "lavaan", "psych", "GPArotation", "Matrix",
  "TAM", "mirt", "difR", "rstatix", "effectsize", "car", "writexl", "semPlot",
  "patchwork", "ggrepel", "knitr", "rmarkdown", "CTT", "BifactorIndicesCalculator",
  "jsonlite", "diptest",
  # Рисунки в виде вывода статпрограмм, а не самодельных раскладок:
  # tidySEM::graph_sem — путевые схемы CFA/ESEM (ggplot2, раскладка сеткой имён);
  # WrightMap::wrightMap — карта Райта в том же виде, что печатает ConQuest.
  "tidySEM", "WrightMap"
)

repo <- binary_repo()
type <- pkg_type()

# Установка версий из PKG_SNAPSHOT_OVERRIDE. Идёт ПЕРВОЙ: пакеты списка тянут
# переопределённый как зависимость, и без этого шага он приехал бы из основного
# снапшота в версии, под которую его зависимые бинарники не собраны. Набор пуст
# там, где снапшот согласован сам с собой (под Linux — см. _setup.R): тогда шаг
# ничего не делает и сообщает об этом, чтобы пустой обход не читался как сбой.
install_overrides <- function() {
  if (!length(PKG_SNAPSHOT_OVERRIDE)) {
    cat("[install_deps] Переопределений снапшота нет.\n")
    return(invisible(NULL))
  }
  for (p in names(PKG_SNAPSHOT_OVERRIDE)) {
    snap <- PKG_SNAPSHOT_OVERRIDE[[p]]
    want <- available.packages(repos = binary_repo(snap), type = type)
    if (!p %in% rownames(want)) {
      stop(sprintf("%s отсутствует в снапшоте %s — переопределение версии неприменимо.", p, snap),
           call. = FALSE)
    }
    # Сравнение через package_version(), а не строками: в поле Version репозитория
    # разделитель ревизии дефис ("5.1.11-2"), а packageVersion() нормализует его в
    # точку ("5.1.11.2"), и строковое равенство не срабатывало бы никогда —
    # переопределённый пакет переустанавливался бы при каждом запуске.
    want_ver <- package_version(want[p, "Version"])
    have_ver <- if (p %in% rownames(installed.packages())) packageVersion(p) else NULL
    if (!is.null(have_ver) && have_ver == want_ver) {
      cat(sprintf("[install_deps] %s %s (снапшот %s) — уже установлен.\n",
                  p, format(want_ver), snap))
      next
    }
    cat(sprintf("[install_deps] %s: %s -> %s из снапшота %s\n",
                p, if (is.null(have_ver)) "нет" else format(have_ver), format(want_ver), snap))
    install.packages(p, repos = binary_repo(snap), type = type, dependencies = FALSE)
  }
}

# Пакеты, подлежащие установке: отсутствующие ЛИБО стоящие в версии, отличной от
# снапшотной. Именной критерий сам по себе недостаточен: дата снапшота объявлена
# тем, что фиксирует ВЕРСИИ, а пакет, уже стоящий в библиотеке в другой версии, при
# отборе по имени остаётся вне снапшота молча — verify_loadable() ниже видит
# загружаемость, а не версию, поэтому прогон уходил бы на версиях дня установки.
# Так и сдвигается величина, чувствительная к версии: верхняя граница ДИ eta2 из
# effectsize (DECISIONS.md D10).
#
# Сравнение через package_version(), а не строками — по той же причине, что в
# install_overrides(): поле Version репозитория несёт дефис ревизии ("0.7-2",
# "4.3-25"), packageVersion() нормализует его в точку, и строковое равенство
# объявило бы устаревшими 6 пакетов действующего набора при совпадающих версиях,
# то есть переустанавливало бы их каждый запуск.
# Пакет, которого в снапшоте нет вовсе, не трогается: его версию сверять не с чем.
off_snapshot <- function(pkgs, avail) {
  have <- installed.packages()[, "Package"]
  vapply(pkgs, function(p) {
    if (!p %in% have) return(TRUE)
    if (!p %in% rownames(avail)) return(FALSE)
    packageVersion(p) != package_version(avail[p, "Version"])
  }, logical(1))
}

# Проверка загружаемости — по каждому пакету списка, после установки.
# installed.packages() видит только ИМЯ пакета, поэтому мимо проверки по именам
# проходят молча два отказа: сорванная установка транзитивной зависимости
# (SimDesign у mirt) и бинарник, не совпавший по ABI с зависимостью (stringfish,
# собранный под одну версию RcppParallel, при установленной другой — обе стороны
# несовпадения ловятся здесь: под Windows 5.x-сборка при 6.x, под Linux 6.x-сборка
# при понижённом 5.x). Без этой проверки оба отказа дают «Готово» на установке и
# падение уже на шаге пайплайна — с ошибкой, в которой не видно ни установки, ни
# её причины.
verify_loadable <- function(pkgs) {
  bad <- character(0)
  for (p in pkgs) {
    err <- tryCatch({ loadNamespace(p); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(err)) bad <- c(bad, sprintf("%s: %s", p, gsub("\n", " ", err)))
  }
  if (length(bad)) {
    stop(sprintf("Установлены, но не загружаются:\n  %s", paste(bad, collapse = "\n  ")),
         call. = FALSE)
  }
  cat(sprintf("[install_deps] Загружаются все %d пакет(ов).\n", length(pkgs)))
}

cat(sprintf("[install_deps] Репозиторий: %s (тип: %s)\n", repo[["CRAN"]], type))
install_overrides()

avail    <- available.packages(repos = repo, type = type)
new_pkgs <- pkgs[off_snapshot(pkgs, avail)]
if (!length(new_pkgs)) {
  cat("[install_deps] Все пакеты стоят в версиях снапшота.\n")
} else {
  cat(sprintf("[install_deps] Устанавливаю %d пакет(ов): %s\n",
              length(new_pkgs), paste(new_pkgs, collapse = ", ")))
  install.packages(new_pkgs, repos = repo, type = type)
  still_missing <- new_pkgs[!new_pkgs %in% installed.packages()[, "Package"]]
  if (length(still_missing))
    stop(sprintf("Не удалось установить: %s", paste(still_missing, collapse = ", ")), call. = FALSE)
}

verify_loadable(pkgs)
cat("[install_deps] Готово.\n")
