# =============================================================================
#  ГРАФИК: характеристические кривые пунктов (ICC, Раш)
# =============================================================================

# Бутстрап: рабочая директория — корень проекта, затем общие функции графиков
local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/plots/_plot_utils.R")

library(tidyverse)
library(TAM)

# КОНФИГУРАЦИЯ
width  <- W_2COL   # дюймы
height <- 5.4      # дюймы — панель на пункт, 26 пунктов сеткой
dpi    <- PLOT_DPI

model_path <- "output/Rasch/rasch_model.RData"
require_input(model_path, "scripts/4_rasch.R (шаг 4)")

loaded_objs <- load(model_path) # mod_rasch, person_theta, rasch_report
# Трудности берутся из rasch_report, который 4_rasch.R собрал СВЯЗЫВАНИЕМ ПО ИМЕНИ
# (match по колонке item плюс setequal-проверка состава). Пара
# mod_rasch$item$xsi.item + colnames(mod_rasch$resp) даёт тот же вектор только пока
# порядок строк $item совпадает с порядком столбцов $resp: при ином порядке (другая
# версия TAM, снятый по нулевой дисперсии пункт) трудности молча привязались бы не к
# тем пунктам. Артефакт старше этой таблицы роняет рисунок, а не подставляет позицию.
if (!"rasch_report" %in% loaded_objs)
  stop(sprintf(paste0("УСТАРЕВШИЙ ВХОД для Rasch ICC: в %s нет rasch_report ",
                      "(есть: %s) — сначала перезапустите 4_rasch.R."),
               model_path, paste(loaded_objs, collapse = ", ")), call. = FALSE)
source("scripts/config.R")
items <- rasch_report$Item
validate_items(items, "Rasch ICC (rasch_model.RData)")

# Кривые рисует ШТАТНЫЙ TAM: plot(mod_rasch, type = "expected") — тот же вид,
# что печатает ConQuest, панель на пункт, модельная кривая ожидаемой оценки плюс
# НАБЛЮДЁННЫЕ средние по группам способности (observed = TRUE). Наблюдённых точек
# у самодельной кривой P = 1/(1+exp(-(theta-b))) не было вовсе: она показывала
# модель, но не её согласие с данными.
#
# Про выбор режима (проверено на действующем прогоне):
#  * type = "items" рисует кривые КАТЕГОРИЙ, и TAM кладёт в каждую панель легенду
#    Cat0/Cat1 (exp./obs.) — на 26 панелях легенда занимает большую часть панели,
#    а кривая сжимается в полоску;
#  * overlay = TRUE накладывает не пункты, а категории ОДНОГО пункта, страницы
#    всё равно идут по пункту;
#  * png/tiff без "%d" в имени страницы не нумерует, поэтому многостраничный
#    вывод оставил бы в артефакте только последний пункт (Q29).
# Отсюда par(mfrow): страницы TAM тайлятся в одну, и все 26 пунктов попадают в
# файл. fix.devices оставлен по умолчанию — при FALSE plot.tam.mml падает на
# своей внутренней переменной old.par.mar.
NC <- 5L                                   # колонок в сетке панелей
NR <- ceiling(length(items) / NC)

draw_icc <- function() {
  op <- par(mfrow = c(NR, NC),
            mar = c(1.8, 1.8, 1.5, 0.4), oma = c(2.2, 2.2, 3.4, 0.5),
            cex.axis = 0.5, cex.lab = 0.55, tcl = -0.2, mgp = c(1, 0.25, 0))
  on.exit(try(par(op), silent = TRUE), add = TRUE)
  # cex.main мелкий намеренно: заголовок панели у TAM длинный
  # ("Expected Scores Curve - Item Q01") и при большем кегле обрезается шириной
  # панели.
  par(cex.main = 0.45)
  plot(mod_rasch, items = seq_along(items), type = "expected",
       export = FALSE, ask = FALSE, observed = TRUE, package = "graphics")

  # Заголовочный блок — общим mtext_block() во внешнее поле oma: кегли из
  # cex_pub() и перенос по словам, поэтому подзаголовок не обрезается правым
  # краем, а разворачивается во вторую строку.
  #
  # Общих подписей осей нет намеренно: TAM подписывает оси В КАЖДОЙ панели
  # ("Ability" / "Score"), и внешние подписи оказались бы вторым набором тех же
  # ярлыков.
  mtext_block(
    title    = "Expected score curves (Rasch 1PL)",
    subtitle = sprintf("%d items; blue line — model, black — observed group means. The slope is fixed at 1, so items differ only in difficulty b.",
                       length(items)),
    width_in = width, outer = TRUE
  )
}

output_path <- file.path("output/Rasch/plots", paste0("rasch_icc.", PLOT_FORMAT))
save_base_plot(draw_icc, output_path, width, height, dpi = dpi)
