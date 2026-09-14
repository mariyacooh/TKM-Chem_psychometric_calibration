# =============================================================================
#  ГРАФИК: собственные значения (scree plot, EFA)
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
library(psych)

# КОНФИГУРАЦИЯ
width  <- W_2COL   # дюймы
height <- 3.9      # дюймы
dpi    <- PLOT_DPI

color_pa   <- "#D55E00"
color_th   <- "#009E73"

data_file  <- "output/EFA/efa_eigenvalues.csv"
pa_file    <- "output/EFA/efa_parallel_analysis.csv"
model_file <- "output/EFA/efa_model.RData"
require_input(c(data_file, pa_file, model_file), "scripts/3_efa_cfa.R (шаг 3, БЛОК 1)")

scree_df <- read_csv(data_file)
# Приведение к numeric (PA_line из одних NA иначе читается как logical)
scree_df$Factor     <- as.numeric(scree_df$Factor)
scree_df$Eigenvalue <- as.numeric(scree_df$Eigenvalue)
scree_df$PA_line    <- as.numeric(scree_df$PA_line)
n_items <- nrow(scree_df)
source("scripts/config.R")
if (n_items != length(ITEMS)) stop(sprintf("УСТАРЕВШИЙ ВХОД для scree plot: собственных значений %d, а пунктов анализа %d — сначала перезапустите 3_efa_cfa.R.", n_items, length(ITEMS)), call. = FALSE)

# Линия сравнения PA — колонка PA_line; число компонент — ncomp_PCA из артефакта
# шага 3. Пересчёт по линии (sum(Eigenvalue > PA_line)) завёл бы второе правило
# отбора: fa.parallel удерживает ВЕДУЩИЕ компоненты до первой, ушедшей под линию, а
# сумма считает всякую всплывшую над ней, поэтому маркер рисунка расходился бы с
# числом, напечатанным в EFA/efa_results.txt. Единственный источник — шаг 3.
has_pa <- any(!is.na(scree_df$PA_line))
pa_tbl <- read_csv(pa_file)
n_pa   <- pa_tbl$Value[pa_tbl$Metric == "ncomp_PCA"]
if (length(n_pa) != 1L || is.na(n_pa))
  stop(sprintf("УСТАРЕВШИЙ ВХОД для scree plot: в %s нет строки ncomp_PCA — сначала перезапустите 3_efa_cfa.R.",
               pa_file), call. = FALSE)

# Сам рисунок строит ШТАТНАЯ psych::scree() по той же корреляционной матрице, на
# которой шаг 3 оценил EFA (efa_cor = efa3$r, корреляции Пирсона — D6). Она даёт
# узнаваемый вид psych: обе ветки (PC и FA) с горизонтальной линией на lambda = 1.
loaded_objs <- load(model_file) # efa3, efa_cor, eigenvalues, pa_line
if (!"efa_cor" %in% loaded_objs)
  stop(sprintf(paste0("УСТАРЕВШИЙ ВХОД для scree plot: в %s нет efa_cor ",
                      "(есть: %s) — сначала перезапустите 3_efa_cfa.R."),
               model_file, paste(loaded_objs, collapse = ", ")), call. = FALSE)

# СВЕРКА: psych::scree() считает собственные значения у себя, из матрицы. Если
# они разойдутся с efa_eigenvalues.csv, рисунок покажет одни числа, а отчёт —
# другие, причём молча. Расхождение = ошибка, как несовпадение состава пунктов.
eig_own <- eigen(efa_cor, symmetric = TRUE, only.values = TRUE)$values
max_dev <- max(abs(eig_own - scree_df$Eigenvalue))
if (max_dev > 1e-6)
  stop(sprintf(paste0("plot_scree: собственные значения матрицы efa_cor расходятся с ",
                      "efa_eigenvalues.csv на %.2g — рисунок и отчёт показали бы разные числа. ",
                      "Перезапустите 3_efa_cfa.R целиком."), max_dev), call. = FALSE)

draw_scree <- function() {
  # Кегли осей — из cex_pub() по тем же rel-множителям, что у theme_pub():
  # рисунок одиночной панели должен читаться в одном ряду с рисунками ggplot.
  op <- par(mar = c(3.6, 3.6, 3.2, 0.8), mgp = c(2.1, 0.6, 0), tcl = -0.25,
            cex.axis = cex_pub(0.9), cex.lab = cex_pub(1.0))
  on.exit(try(par(op), silent = TRUE), add = TRUE)

  psych::scree(efa_cor, factors = TRUE, pc = TRUE, main = "")

  # Линия сравнения PA и маркеры правил отбора идут ПОВЕРХ штатного вывода:
  # psych::scree() параллельный анализ не рисует вовсе, а без него с рисунка
  # исчезло бы правило, по которому шаг 3 удержал ncomp_PCA компонент.
  if (has_pa) {
    lines(scree_df$Factor, scree_df$PA_line, col = color_pa, lty = 2, lwd = 1.4)
    abline(v = n_pa, col = color_pa, lty = 3, lwd = 1.2)
  }
  abline(v = 3, col = color_th, lty = 3, lwd = 1.2)

  usr <- par("usr")
  y_at <- usr[4] - diff(usr[3:4]) * c(0.06, 0.13)
  text(3 + n_items * 0.015, y_at[1], "Theory: 3 factors",
       col = color_th, cex = cex_pub(0.85), font = 2, adj = 0)
  if (has_pa) {
    text(n_pa + n_items * 0.015, y_at[2], sprintf("PA: %d components", n_pa),
         col = color_pa, cex = cex_pub(0.85), font = 2, adj = 0)
  }

  # Заголовочный блок — общий mtext_block(): кегли из cex_pub() (те же, что у
  # theme_pub на рисунках ggplot) и перенос по словам по ширине холста. Число
  # строк подзаголовка при этом не зашивается в код: одной строкой он в полосу
  # набора не входит и обрезался бы правым краем МОЛЧА.
  mtext_block(
    title    = "Scree plot with parallel analysis",
    subtitle = paste("psych::scree output: PC and FA eigenvalue branches with the lambda = 1 line.",
                     "Orange dashed — parallel-analysis comparison line;",
                     "dotted verticals — components retained by each rule."),
    width_in = width, outer = FALSE
  )
}

output_path <- file.path("output/EFA/plots", paste0("efa_scree.", PLOT_FORMAT))
save_base_plot(draw_scree, output_path, width, height, dpi = dpi)
