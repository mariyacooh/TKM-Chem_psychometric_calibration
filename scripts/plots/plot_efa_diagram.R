# =============================================================================
#  ГРАФИК: диаграмма факторной структуры EFA (psych::fa.diagram)
# =============================================================================
#  Рисунок строит ШТАТНАЯ psych::fa.diagram() по объекту `fa` — то есть вывод
#  того самого пакета, которым оценён разведочный блок шага 3, а не самодельная
#  раскладка. Вид узнаваемый: пункты слева прямоугольниками, факторы справа
#  овалами, у стрелки — нагрузка, пункты отсортированы по фактору и по величине.
#
#  Дополняет тепловую карту (plot_efa_heatmap.R), а не заменяет её: карта несёт
#  ВСЕ нагрузки всех трёх факторов, включая мелкие и кросс-нагрузки, диаграмма —
#  только структуру «пункт -> его фактор». Поэтому здесь simple = TRUE (по
#  ведущей нагрузке на пункт) и порог cut: без них 26 пунктов дали бы 78 стрелок
#  и диаграмма перестала бы отвечать на свой единственный вопрос.
#
#  ВАЖНО про содержание: EFA считается по корреляциям ПИРСОНА (phi), а не
#  тетрахорически — причина и цена в DECISIONS.md D6. Нагрузки здесь занижены
#  относительно тетрахорических, и это свойство шага, а не рисунка.
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

library(psych)

# КОНФИГУРАЦИЯ
width  <- W_2COL   # дюймы
height <- 9.2      # дюймы — строка на пункт при 26 пунктах, у предела полосы (H_MAX)
dpi    <- PLOT_DPI
CUT    <- 0.30     # порог показа нагрузки, тот же, что печатает efa_results.txt

model_path <- "output/EFA/efa_model.RData"
require_input(model_path, "scripts/3_efa_cfa.R (шаг 3, БЛОК 1)")

loaded_objs <- load(model_path) # efa3, efa_cor, eigenvalues, pa_line
# Объект `fa` — единственный вход этого рисунка: fa.diagram() читает из него и
# нагрузки, и Phi. Артефакт старше рисунка роняет его, а не подставляет пустую
# структуру.
if (!"efa3" %in% loaded_objs)
  stop(sprintf(paste0("УСТАРЕВШИЙ ВХОД для диаграммы EFA: в %s нет efa3 ",
                      "(есть: %s) — сначала перезапустите 3_efa_cfa.R."),
               model_path, paste(loaded_objs, collapse = ", ")), call. = FALSE)
source("scripts/config.R")
validate_items(rownames(unclass(efa3$loadings)), "диаграмма EFA (efa_model.RData)")

output_path <- paste0("output/EFA/plots/efa_fa_diagram.", PLOT_FORMAT)

# Номер фактора — ПОЗИЦИЯ КОЛОНКИ, имя ML остаётся в скобках. psych отдаёт
# нагрузки упорядоченными по объяснённой дисперсии, но имена ML нумеруют факторы
# по внутреннему порядку извлечения: на действующем прогоне колонки идут
# ML2, ML3, ML1 при SS loadings 7.403 / 2.436 / 0.909, поэтому подпись "ML1" на
# рисунке пришлась бы на самый СЛАБЫЙ фактор (3.5% дисперсии). То же правило
# действует на тепловой карте (plot_efa_heatmap.R) — иначе два рисунка одного
# шага противоречили бы друг другу. Имя ML сохранено, иначе рисунок не сводится
# с efa_results.txt и таблицей отчёта.
efa_lab <- efa3
ml_names <- colnames(unclass(efa_lab$loadings))
new_names <- sprintf("F%d (%s)", seq_along(ml_names), ml_names)
colnames(efa_lab$loadings) <- new_names
if (!is.null(efa_lab$Phi)) dimnames(efa_lab$Phi) <- list(new_names, new_names)

draw_diagram <- function() {
  # marg — поля самой fa.diagram (bottom, left, top, right): сверху место под
  # заголовок и подзаголовок, снизу под подпись, иначе они лягут на узлы.
  psych::fa.diagram(
    efa_lab,
    cut    = CUT,
    simple = TRUE,
    sort   = TRUE,
    digits = 2,
    main   = "",          # заголовок ставится ниже, чтобы задать кегль и выключку
    # marg — поля fa.diagram (bottom, left, top, right). Снизу шире остальных:
    # при узком поле нижний пункт садится на подпись под рисунком. Сверху — под
    # заголовок и подзаголовок mtext_block() (строка заголовка 1.05, строка
    # подзаголовка 0.72 при line_top = 0.35).
    marg   = c(2.8, 0.4, 2.6, 0.4),
    # cex мельче: на 26 пунктах рамки идут вплотную, и при большем кегле имя
    # пункта выходит за свою рамку на соседнюю.
    cex    = 0.5,
    # e.size больше значения по умолчанию (0.05): подпись фактора несёт и номер,
    # и имя ML ("F1 (ML2)"), и в узкий овал она не входит.
    e.size = 0.09
  )
  # Заголовок, подзаголовок и подпись — общим mtext_block(): кегли из cex_pub()
  # и перенос по словам по ширине холста, поэтому строка длиннее полосы набора не
  # уходит за правый край, а разворачивается во вторую.
  mtext_block(
    title    = "Exploratory factor analysis: factor structure (ML, oblimin)",
    subtitle = sprintf("Pearson (phi) correlations; loadings below %.2f and non-leading loadings are not drawn", CUT),
    caption  = "Each item is drawn to its leading factor only; the full loading pattern, small and cross-loadings included, is in the heatmap.",
    width_in = width, outer = FALSE
  )
}

save_base_plot(draw_diagram, output_path, width, height, dpi = dpi)
