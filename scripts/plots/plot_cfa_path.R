# =============================================================================
#  ГРАФИК: схема измерительной модели 3-факторной CFA
# =============================================================================
#  Рисунок строит tidySEM (prepare_graph -> plot), то есть ШТАТНЫЙ вывод
#  SEM-пакета, а не самодельная раскладка: узлы — белые прямоугольники
#  (наблюдаемые) и овалы (латентные), связи — чёрные стрелки, ковариации —
#  штриховые двусторонние дуги. Это тот же идиом, в котором путевую схему
#  печатают AMOS и Mplus, поэтому вид рисунка не приходится подбирать вручную.
#
#  Раскладка задаётся СЕТКОЙ ИМЁН (аргумент layout у prepare_graph): 26 пунктов
#  идут строками во второй колонке, фактор встаёт в первой колонке напротив
#  середины своего блока. Порядок строк — порядок SUBSCALES, поэтому пункты
#  одного фактора идут непрерывным блоком и принадлежность читается по тому, из
#  какого овала приходит стрелка.
#
#  Величины — стандартизованные нагрузки (колонка est_std, которую prepare_graph
#  приносит уже посчитанной lavaan). Те же нагрузки с доверительными интервалами
#  и индексы согласия — на профиле (scripts/plots/plot_cfa_loadings.R).
#
#  Рисунок бифакторной модели строит plot_bifactor_loadings.R — путевая
#  диаграмма бифакторной модели нечитаема при любой настройке на текущем числе
#  индикаторов.
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

library(lavaan)
library(tidySEM)
library(ggplot2)

# КОНФИГУРАЦИЯ (save_ggplot и подписи субшкал — из _plot_utils.R)
dpi <- PLOT_DPI
# Высоту задаёт число пунктов: 26 строк по ~0.32 дюйма. Ширина уже полной полосы —
# в раскладке всего две колонки узлов, и на полной полосе связи тянулись бы через
# пустое поле. H_MAX (9.25") не превышается, проверка в check_pub_size().
w1  <- 5.4   # дюймы
h1  <- 9.0   # дюймы

data_file <- "output/CFA/cfa_fits.RData"
# УСЛОВНЫЙ вход: шаг 3 пишет cfa_fits.RData только при сходимости хотя бы одной
# CFA-модели и имеет явную ветку «ни одна не сошлась — файл не записан»
# (save_objs в 3_efa_cfa.R). Отсутствие файла — не невыполненный шаг, а честный
# результат, поэтому optional_input (видимый [SKIP]), а не require_input с
# советом перезапустить шаг 3.
if (!optional_input(data_file, "scripts/3_efa_cfa.R (шаг 3, БЛОК 2)",
                    "ни одна CFA-модель не сошлась")) {
  quit(save = "no", status = 0)
}

loaded_objs <- load(data_file) # cfa_fit (и cfa_bi — только если бифакторная модель сошлась в шаге 3)
# Файл есть, но 3-факторной модели в нём нет: save_objs в 3_efa_cfa.R кладёт только
# сошедшиеся, поэтому при несошедшейся 3-факторной файл существует без cfa_fit.
# Тот же легальный исход, что и отсутствие файла, — диаграмму строить не из чего.
if (!"cfa_fit" %in% loaded_objs) {
  message("[SKIP] ", data_file, " не содержит cfa_fit (есть: ",
          paste(loaded_objs, collapse = ", "),
          "): 3-факторная CFA не сошлась в шаге 3 — диаграмма пропущена.")
  quit(save = "no", status = 0)
}
source("scripts/config.R")
validate_items(lavaan::lavNames(cfa_fit, "ov"), "схема измерительной модели CFA (cfa_fits.RData)")

output_path <- paste0("output/CFA/plots/cfa_path_diagram.", PLOT_FORMAT)

lat_names <- names(SUBSCALES)
man_order <- unlist(SUBSCALES, use.names = FALSE)

# --- Сетка имён под layout ---------------------------------------------------
# Пустая строка "" — свободная клетка. Первая колонка несёт факторы, вторая —
# пункты; фактор ставится напротив СЕРЕДИНЫ своего блока, поэтому его связи
# расходятся симметрично вверх и вниз.
lay <- matrix("", nrow = length(man_order), ncol = 2)
lay[, 2] <- man_order
for (s in lat_names) {
  rows <- match(SUBSCALES[[s]], man_order)
  lay[rows[ceiling(length(rows) / 2)], 1] <- s
}

g <- prepare_graph(cfa_fit, layout = lay,
                   rect_width = 1.5, rect_height = 0.62,
                   ellipses_width = 1.5, ellipses_height = 1.0,
                   spacing_x = 3.4, spacing_y = 1.0,
                   text_size = 3.2, fix_coord = FALSE)

fmt_est <- function(x) formatC(x, digits = 2, format = "f")

edges_df <- g$edges
# Дисперсии сняты: остаточные (op "~~" на самом пункте) tidySEM рисует петлёй у
# узла, а дисперсии факторов при std.lv несут константу 1.00. Величина остатка
# при этом осмысленна (1-lambda^2), но она полностью определена нагрузкой,
# которая на рисунке уже есть.
edges_df$show <- !(edges_df$op == "~~" & edges_df$lhs == edges_df$rhs)

is_load <- edges_df$op == "=~"
edges_df$label[is_load]        <- fmt_est(edges_df$est_std[is_load])
edges_df$connect_from[is_load] <- "right"   # из овала вправо, в левый край пункта
edges_df$connect_to[is_load]   <- "left"
edges_df$curvature[is_load]    <- NA        # нагрузки — прямые, как в выводе AMOS

is_cov <- which(edges_df$op == "~~" & edges_df$lhs != edges_df$rhs)
edges_df$connect_from[is_cov] <- "left"     # дуги уходят влево, в свободное поле
edges_df$connect_to[is_cov]   <- "left"
# Кривизна РАЗНАЯ по парам: при одинаковой три дуги ложатся друг на друга.
# Дальняя пара (S1~S3) идёт шире соседних.
cov_span <- abs(match(edges_df$lhs[is_cov], lat_names) -
                match(edges_df$rhs[is_cov], lat_names))
edges_df$curvature[is_cov] <- ifelse(cov_span == 1, 32, 80)
# Дуги остаются, а ЧИСЛА ковариаций уходят в подпись под рисунком. Причина
# геометрическая: tidySEM ставит подпись связи в середину ПРЯМОЙ между узлами, а
# для несоседней пары S1~S3 середина приходится ровно на овал S2 — подпись
# пропадает под ним. При меньшей кривизне она вылезает на веер нагрузок S2 и
# сливается с ними ("0.82" вплотную к "0.85"). Подпись под рисунком читается
# однозначно при любой геометрии, поэтому числа берутся оттуда.
cov_pairs <- sprintf("%s–%s %s", edges_df$lhs[is_cov], edges_df$rhs[is_cov],
                     fmt_est(edges_df$est_std[is_cov]))
edges_df$label[is_cov] <- ""
g$edges <- edges_df

# Имя узла без второй строки: по умолчанию tidySEM подписывает узел ещё и
# дисперсией, а она здесь снята.
nodes_df <- g$nodes
nodes_df$label <- nodes_df$name
g$nodes <- nodes_df

p_path <- plot(g) +
  # Запас слева: дуга дальней пары уходит за координаты узлов, и при стандартном
  # расширении осей ggplot обрезает её по краю панели.
  scale_x_continuous(expand = expansion(mult = c(0.35, 0.03))) +
  labs(
    title    = "Three-factor ICM-CFA: measurement model",
    subtitle = paste0("Standardized estimates   ",
                      paste(sub_label(names(SUBSCALES)), collapse = "   |   ")),
    caption  = paste0("Solid arrows are standardized loadings; dashed double arrows are factor covariances, ",
                      paste(cov_pairs, collapse = ", "),
                      ".\nResidual variances are omitted. The same loadings with 95% CI are in the loadings figure.")
  ) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0),
    plot.subtitle = element_text(size = 8.5, colour = "grey20", hjust = 0),
    plot.caption  = element_text(size = 7, colour = "grey35", hjust = 0)
  )

save_ggplot(p_path, output_path, w1, h1, dpi = dpi)
