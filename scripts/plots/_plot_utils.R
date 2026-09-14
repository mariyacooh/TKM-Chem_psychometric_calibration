# =============================================================================
#  _plot_utils.R — общая палитра, тема и функции сохранения для скриптов графиков.
# =============================================================================
#  Source-ится в начале каждого скрипта scripts/plots/*.R, сразу после блока,
#  фиксирующего рабочую директорию на корне проекта:
#
#      local({
#        if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#          root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
#          while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
#          if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
#        }
#      })
#      source("scripts/plots/_plot_utils.R")
#
#  Централизует публикационный формат вывода, ширины полос набора, палитру
#  Okabe-Ito, общую тему рисунков и сохранение в несколько форматов сразу.
# =============================================================================

# Воспроизводимые числовые опции для отдельного запуска графика (в run_all.R они
# уже выставлены через _setup.R). Дефолты R -> формат подписей осей/меток не
# зависит от ~/.Rprofile. Синхронизировать с scripts/_setup.R.
options(digits = 7, OutDec = ".", scipen = 0,
        contrasts = c("contr.treatment", "contr.poly"))

# =============================================================================
# Форматы вывода
# =============================================================================
# Каждый рисунок пишется СРАЗУ в оба формата, потому что у них разные читатели:
#   tiff — публикационный носитель (LZW, 300x300 dpi, требование журналов);
#   png  — то, что показывает output/report.html, поскольку тег <img> в браузере
#          TIFF не отображает вовсе.
# Одного формата не хватает: только tiff обнуляет рисунки отчёта, только png не
# годится в подачу. PLOT_FORMAT — тот из набора, чьи пути записаны в report.Rmd.
#
# КАТАЛОГ РАЗВОДИТ ФОРМАТЫ, а не только расширение: в plots/ лежит по ОДНОМУ файлу
# на рисунок (формат отчёта), остальные форматы набора уходят в plots/<формат>/.
# Иначе каталог анализа держит вдвое больше файлов, чем в нём рисунков, и один
# рисунок читается как два разных: имена соседних записей различаются только
# расширением, а превью TIFF файловый менеджер обычно не строит.
PLOT_FORMATS <- c("tiff", "png")
PLOT_FORMAT  <- "png"
PLOT_DPI     <- 300

# =============================================================================
# Полосы набора: ширина рисунка задаётся полосой журнала, а не экраном
# =============================================================================
# Рисунок, нарисованный шириной 12-14 дюймов, в вёрстке сжимается до полосы, и
# вместе с ним сжимается КАЖДАЯ подпись: 8 pt на макете 14" превращаются в 4 pt на
# полосе 7.1". Отсюда и нечитаемость, и наезжающие подписи — не размер шрифта, а
# масштаб холста. Поэтому ширина берётся из этих констант, а размер шрифта
# остаётся тем, каким он будет в напечатанной статье.
W_1COL <- 3.46   # дюймы — одна колонка (88 мм)
W_2COL <- 7.09   # дюймы — полная полоса (180 мм)
H_MAX  <- 9.25   # дюймы — предельная высота полосы набора

# Ширина сверх полной полосы означает, что подписи в вёрстке уменьшатся. Сообщение,
# а не stop(): pdf/свободный формат для просмотра остаётся законным применением.
check_pub_size <- function(path, width, height) {
  if (width > W_2COL + 1e-9)
    message(sprintf("[SIZE] %s: ширина %.2f\" больше полной полосы %.2f\" — в вёрстке подписи уменьшатся.",
                    path, width, W_2COL))
  if (height > H_MAX + 1e-9)
    message(sprintf("[SIZE] %s: высота %.2f\" больше полосы набора %.2f\".",
                    path, height, H_MAX))
  invisible(NULL)
}

# Подзаголовок или подпись длиннее полосы обрезается устройством МОЛЧА: текст
# просто уходит за край рисунка. Перенос по словам ставит на его место лишнюю
# строку, которая видна. Предел в символах выводится из ШИРИНЫ рисунка (~18
# знаков на дюйм при theme_pub на 8 pt), иначе одна и та же константа резала бы
# по месту на полной полосе и не спасала на узком холсте.
wrap_lab <- function(x, width_in = W_2COL) {
  paste(strwrap(x, width = max(24L, floor(width_in * 18))), collapse = "\n")
}

# =============================================================================
# Кегль подписей base-графики — тот же, что у theme_pub()
# =============================================================================
# ggplot считает размер подписи от base_size темы, base-устройство — от своего
# pointsize (12 по умолчанию), поэтому cex, подобранный на глаз в одном скрипте,
# давал на рисунке пакета другой кегль, чем rel() в теме на соседнем. Пересчёт
# здесь один на весь набор, и оба слоя ссылаются на одни и те же rel-множители
# theme_pub: заголовок 1.15, подзаголовок 0.9, подпись под рисунком 0.8.
PUB_BASE_SIZE <- 9    # = base_size у theme_pub()
PUB_POINTSIZE <- 12   # pointsize base-устройств по умолчанию

cex_pub <- function(rel) PUB_BASE_SIZE * rel / PUB_POINTSIZE

# Заголовочный блок для base-графики (psych::scree, psych::fa.diagram,
# plot.tam.mml, WrightMap): заголовок и подзаголовок над рисунком, подпись под
# ним, кегли из cex_pub(), перенос по словам из wrap_lab().
#
# mtext() рисует ОДНУ строку на вызов и текст длиннее полосы обрезает молча,
# поэтому перенос разворачивается в несколько вызовов. Блок растёт снизу вверх:
# нижняя строка подзаголовка стоит у самого рисунка, заголовок — над ней, и от
# числа строк не съезжает ни то, ни другое.
#
# `outer` = TRUE для рисунка с сеткой панелей (подписи идут во внешнее поле oma),
# FALSE для одиночной панели (поле mar). Место под блок отводит ВЫЗЫВАЮЩИЙ: здесь
# известен только кегль, а не то, сколько строк поля осталось у рисунка.
mtext_block <- function(title = NULL, subtitle = NULL, caption = NULL,
                        width_in = W_2COL, outer = TRUE,
                        line_top = 0.35, line_bottom = 0.30) {
  lines_of <- function(x) strsplit(wrap_lab(x, width_in), "\n", fixed = TRUE)[[1]]

  y <- line_top
  if (!is.null(subtitle)) {
    sub_l <- lines_of(subtitle)
    for (i in rev(seq_along(sub_l))) {
      mtext(sub_l[i], side = 3, line = y, outer = outer,
            cex = cex_pub(0.9), col = "grey25", adj = 0)
      y <- y + 0.72
    }
  }
  if (!is.null(title)) {
    ttl_l <- lines_of(title)
    for (i in rev(seq_along(ttl_l))) {
      mtext(ttl_l[i], side = 3, line = y, outer = outer,
            cex = cex_pub(1.15), font = 2, adj = 0)
      y <- y + 1.05
    }
  }
  if (!is.null(caption)) {
    cap_l <- lines_of(caption)
    yb <- line_bottom + (length(cap_l) - 1L) * 0.62
    for (i in seq_along(cap_l)) {
      mtext(cap_l[i], side = 1, line = yb, outer = outer,
            cex = cex_pub(0.8), col = "grey35", adj = 0)
      yb <- yb - 0.62
    }
  }
  invisible(NULL)
}

# Частоты по корзинам, посчитанные заранее. geom_histogram() в ggplot2 4.x
# достраивает по краям пустые корзины с NA-границами, и при заданных limits они
# на каждый print дают предупреждение "removed 2 rows containing missing values",
# то есть шум, неотличимый в логе от настоящей потери данных. Здесь границы
# корзин заданы явно, от версии ggplot2 не зависят и NA не создают.
bin_counts <- function(x, binwidth) {
  x <- x[!is.na(x)]
  lo <- floor(min(x) / binwidth) * binwidth
  hi <- ceiling(max(x) / binwidth) * binwidth
  brk <- seq(lo, hi, by = binwidth)
  h <- graphics::hist(x, breaks = brk, plot = FALSE, right = FALSE)
  data.frame(mid = h$mids, count = h$counts)
}

# Качественная палитра Okabe-Ito (безопасна при нарушениях цветовосприятия)
OK_PAL <- c("#0072B2", "#E69F00", "#009E73", "#D55E00",
            "#CC79A7", "#56B4E9", "#F0E442", "#000000")

# Заливка ряда, который НИЧЕГО не кодирует: одна гистограмма, одна серия
# столбцов, полоса частот. Цвет из OK_PAL на таком ряде декоративен — он
# различает ряды, которых нет, а рядом со штатным выводом пакетов (все они
# чёрно-белые: fa.diagram, scree, tidySEM, WrightMap, TAM) читается как значимый
# признак. Цвет остаётся там, где кодирует переменную: субшкалу, группу, флаг,
# величину нагрузки.
FILL_NEUTRAL <- "grey72"
LINE_NEUTRAL <- "grey30"

# =============================================================================
# Общая тема публикационного рисунка
# =============================================================================
# Одна тема на весь набор: рисунки статьи читаются как одна серия, а размеры
# шрифта заданы в пунктах ИТОГОВОЙ полосы (base_size = 9 -> подписи осей 8 pt,
# как требует большинство журналов). Поля минимальны: вёрстка добавляет свои.
theme_pub <- function(base_size = 9, grid = c("both", "x", "y", "none")) {
  grid <- match.arg(grid)
  gline <- element_line(linewidth = 0.25, colour = "grey88")
  theme_bw(base_size = base_size) +
    theme(
      panel.border      = element_rect(colour = "grey35", linewidth = 0.4, fill = NA),
      panel.grid.major.x = if (grid %in% c("both", "x")) gline else element_blank(),
      panel.grid.major.y = if (grid %in% c("both", "y")) gline else element_blank(),
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey93", colour = "grey35", linewidth = 0.4),
      strip.text        = element_text(size = rel(0.95), face = "bold",
                                       margin = margin(2.5, 2.5, 2.5, 2.5)),
      plot.title        = element_text(size = rel(1.15), face = "bold", hjust = 0,
                                       margin = margin(b = 3)),
      plot.subtitle     = element_text(size = rel(0.9), colour = "grey25", hjust = 0,
                                       margin = margin(b = 5)),
      plot.caption      = element_text(size = rel(0.8), colour = "grey35", hjust = 0,
                                       margin = margin(t = 5)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      axis.title        = element_text(size = rel(1.0)),
      axis.text         = element_text(size = rel(0.9), colour = "grey15"),
      legend.title      = element_text(size = rel(0.95)),
      legend.text       = element_text(size = rel(0.9)),
      legend.key.size   = unit(0.34, "cm"),
      legend.margin     = margin(0, 0, 0, 0),
      legend.box.spacing = unit(0.15, "cm"),
      plot.margin       = margin(4, 5, 4, 4)
    )
}

# =============================================================================
# Подписи и цвета субшкал — один источник на все рисунки
# =============================================================================
# Набор субшкал задаёт input/items.csv (SUBSCALES в scripts/config.R), поэтому
# подпись и цвет выводятся ПО ИМЕНИ субшкалы, а не перечисляются литералами в каждом
# рисунке. При перечислении субшкала, добавленная в items.csv, давала бы на рисунке
# NA вместо подписи и отсутствующий цвет, причём молча: панель фасета остаётся,
# только без имени.
# Английские названия живут здесь: рисунки публикационные и подписаны по-английски,
# тогда как русские названия субшкал принадлежат отчёту и своду. Незнакомое имя
# показывается как есть -- ряд из легенды не выпадает.
SUB_NAME_EN <- c(S1 = "Interpretation", S2 = "Analysis", S3 = "Evaluation")

# Короткие названия — только для подписи в узкой полосе фасета (strip.text.y
# тепловых карт), где полное название расширяет полосу за счёт самой карты.
SUB_NAME_EN_SHORT <- c(S1 = "Interpret", S2 = "Analysis", S3 = "Evaluation")

sub_label <- function(subs, short = FALSE) {
  nm  <- as.character(subs)
  map <- if (short) SUB_NAME_EN_SHORT else SUB_NAME_EN
  en  <- unname(map[nm])
  ifelse(is.na(en), nm, sprintf("%s: %s", nm, en))
}

# Цвет на субшкалу по её позиции в SUBSCALES. Индекс цикличен: набор цветов конечен,
# а NA-цвет роняет отрисовку.
sub_palette <- function(subs) {
  nm <- as.character(subs)
  setNames(OK_PAL[(seq_along(nm) - 1L) %% length(OK_PAL) + 1L], nm)
}

# =============================================================================
# Карта Райта — общая раскладка для Раша и 2PL
# =============================================================================
# Построение карты Райта на базе ggplot2, ggrepel и patchwork: раздельные панели
# респондентов и пунктов в едином масштабе логитов. Пунктирная оранжевая линия
# по нулевому логиту, закрашенные столбцы гистограммы, точки по точному Difficulty
# с подписями через ggrepel и вертикальная линия-разделитель между панелями.
#
# `items` — таблица с колонками Item, Difficulty.
wright_map <- function(theta, items, title = NULL, subtitle = NULL, caption = NULL,
                       binwidth = 0.25, item_prop = 0.55, width_in = W_2COL) {
  stopifnot(all(c("Item", "Difficulty") %in% names(items)))
  theta <- theta[!is.na(theta)]

  rng <- range(c(theta, items$Difficulty), na.rm = TRUE)
  lo <- floor(rng[1] / binwidth) * binwidth
  hi <- ceiling(rng[2] / binwidth) * binwidth

  counts_df <- bin_counts(theta, binwidth)
  counts_df <- counts_df[counts_df$count > 0, ]

  y_limits <- c(lo - 0.15, hi + 0.15)
  y_breaks <- seq(floor(lo), ceiling(hi), by = 1)

  # ЛЕВАЯ ПАНЕЛЬ: гистограмма способностей респондентов
  p_left <- ggplot2::ggplot(counts_df) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = OK_PAL[4], linewidth = 0.5) +
    ggplot2::geom_rect(ggplot2::aes(ymin = mid - binwidth/2, ymax = mid + binwidth/2,
                                     xmin = 0, xmax = count),
                       fill = FILL_NEUTRAL, colour = "grey35", linewidth = 0.3) +
    ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks) +
    ggplot2::scale_x_reverse(expand = ggplot2::expansion(mult = c(0.05, 0.02))) +
    ggplot2::labs(y = NULL, x = "Respondents") +
    theme_pub(grid = "none") +
    ggplot2::theme(
      panel.border       = ggplot2::element_blank(),
      axis.line.y        = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(colour = "grey35", linewidth = 0.4),
      axis.text.y        = ggplot2::element_blank(),
      axis.ticks.y       = ggplot2::element_blank(),
      axis.title.y       = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = ggplot2::rel(0.85)),
      axis.title.x       = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold", hjust = 0.5,
                                                 margin = ggplot2::margin(t = 6)),
      plot.margin        = ggplot2::margin(4, 0, 4, 4)
    )

  # РАЗДЕЛИТЕЛЬНАЯ ПАНЕЛЬ: вертикальная линия между Respondents и Items до нижней оси.
  p_div <- ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 0, y = y_limits[1], yend = y_limits[2]),
                          colour = "grey35", linewidth = 0.5) +
    ggplot2::scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(4, 0, 4, 0))

  # ПРАВАЯ ПАНЕЛЬ: точки по точной сложности (Difficulty) и сдвиг по X (jitter),
  # подписи разводит ggrepel без округления до корзины, подпись Items снизу по центру.
  set.seed(1)
  items_pts <- items
  items_pts$x_jit <- stats::runif(nrow(items_pts), min = 0.12, max = 0.88)

  p_right <- ggplot2::ggplot(items_pts, ggplot2::aes(x = x_jit, y = Difficulty)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = OK_PAL[4], linewidth = 0.5) +
    ggplot2::geom_point(shape = 21, fill = FILL_NEUTRAL, colour = "grey20",
                        size = 2.3, stroke = 0.3) +
    ggrepel::geom_text_repel(ggplot2::aes(label = Item), family = "sans", size = 2.6,
                             colour = "grey15", seed = 1, max.overlaps = Inf,
                             box.padding = 0.25, point.padding = 0.15,
                             segment.size = 0.2, segment.colour = "grey60",
                             direction = "both") +
    ggplot2::scale_y_continuous(limits = y_limits, breaks = y_breaks, position = "right") +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::labs(y = "Logit scale", x = "Items") +
    theme_pub(grid = "none") +
    ggplot2::theme(
      panel.border       = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(colour = "grey35", linewidth = 0.4),
      axis.line.y.right  = ggplot2::element_line(colour = "grey35", linewidth = 0.4),
      axis.text.y.right  = ggplot2::element_text(size = ggplot2::rel(0.85), colour = "grey15"),
      axis.ticks.y.right = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 8), size = ggplot2::rel(1.0)),
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_line(colour = "grey35", linewidth = 0.3),
      axis.title.x       = ggplot2::element_text(size = ggplot2::rel(1.0), face = "bold", hjust = 0.5,
                                                 margin = ggplot2::margin(t = 6)),
      plot.margin        = ggplot2::margin(4, 6, 4, 0)
    )

  p_combined <- patchwork::wrap_plots(p_left, p_div, p_right, widths = c(1, 0.015, 1.45))

  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p_combined <- p_combined + patchwork::plot_annotation(
      title = title, subtitle = subtitle, caption = caption,
      theme = theme_pub() + ggplot2::theme(panel.border = ggplot2::element_blank())
    )
  }

  print(p_combined)
}

# =============================================================================
# Сохранение
# =============================================================================
# Расширение в `path` игнорируется: имя файла собирается из основы и каждого
# формата набора. Так вызывающий код не перечисляет форматы у себя и не может
# записать половину набора.
#
# Формат отчёта (PLOT_FORMAT) ложится по самому `path`, остальные — в подкаталог
# со своим именем: plots/<рисунок>.png и plots/tiff/<рисунок>.tiff. Пути отчёта
# от этого не меняются (report.Rmd ссылается только на формат отчёта), а каталог
# анализа держит ровно по одному файлу на рисунок.
fmt_paths <- function(path, formats, report_format = PLOT_FORMAT) {
  base <- sub("\\.[^./\\\\]+$", "", path)
  out <- ifelse(formats == report_format,
                paste0(base, ".", formats),
                file.path(dirname(base), formats, paste0(basename(base), ".", formats)))
  setNames(out, formats)
}

# Устройство под один формат. Для tiff — LZW и res = dpi (300x300 точек на дюйм).
open_one_dev <- function(path, width, height, format, dpi) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  switch(format,
    pdf  = pdf(path, width = width, height = height),
    svg  = svg(path, width = width, height = height),
    tiff = ,
    tif  = tiff(path, width = width, height = height, units = "in", res = dpi,
                compression = "lzw"),
    png  = png(path, width = width, height = height, units = "in", res = dpi),
    stop(sprintf("_plot_utils: неизвестный формат рисунка '%s'.", format), call. = FALSE)
  )
  invisible(path)
}

# Сохранить ggplot во ВСЕ форматы набора. Один print() на устройство: ggsave() не
# используется, чтобы tiff и png шли через одинаковый путь отрисовки и не
# расходились по размеру шрифта.
save_ggplot <- function(plot_obj, path, width, height,
                        formats = PLOT_FORMATS, dpi = PLOT_DPI) {
  check_pub_size(path, width, height)
  paths <- fmt_paths(path, formats)
  for (f in formats) {
    open_one_dev(paths[[f]], width, height, f, dpi)
    on.exit(if (dev.cur() != 1L) dev.off(), add = TRUE)
    print(plot_obj)
    dev.off()
    on.exit()
  }
  invisible(unname(paths))
}

# То же для base-графики (semPaths, TAM::plot, ...): `draw` — функция без
# аргументов, рисующая на текущем устройстве. Вызывается по разу на формат,
# поэтому парный dev.off() держит здесь, а не в вызывающем коде: при выводе в
# несколько форматов "устройство открывает один, закрывает другой" перестаёт
# работать в принципе.
save_base_plot <- function(draw, path, width, height,
                           formats = PLOT_FORMATS, dpi = PLOT_DPI) {
  check_pub_size(path, width, height)
  paths <- fmt_paths(path, formats)
  for (f in formats) {
    open_one_dev(paths[[f]], width, height, f, dpi)
    on.exit(if (dev.cur() != 1L) dev.off(), add = TRUE)
    draw()
    dev.off()
    on.exit()
  }
  invisible(unname(paths))
}

# =============================================================================
# Контракт зависимостей между шагами (артефакты output/)
# =============================================================================
# Графики — листья графа: каждый читает артефакты своего шага анализа.
# require_input() (жёсткий вход -> stop) и optional_input() (условный артефакт ->
# видимый [SKIP]) плюс полный граф — в scripts/_artifacts.R, общий с scripts/_setup.R.
source("scripts/_artifacts.R", local = TRUE)
