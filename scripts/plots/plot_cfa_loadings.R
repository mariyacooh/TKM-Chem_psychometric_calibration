# ========================================================================
# НАГРУЗКИ 3-ФАКТОРНОЙ ICM-CFA
# ========================================================================
# ВХОД: output/CFA/cfa_fits.RData (объект cfa_fit) — шаг 3, БЛОК 2
# ВЫХОД: CFA/plots/cfa_loadings_forest.* — стандартизованные нагрузки с 95% ДИ
#
# Прежний рисунок этого скрипта — cfa_path_diagram_detailed.* — путевая диаграмма
# в круговой раскладке. На 26 индикаторах она нечитаема при любой настройке:
# подписи рёбер сходятся к центру и накладываются друг на друга, а сама величина
# нагрузки с рисунка не снимается. Здесь те же числа показаны точечным профилем с
# доверительными интервалами: значение читается по оси, а порог существенности —
# опорной линией. Схему модели (какой пункт к какому фактору) даёт
# scripts/plots/plot_cfa_path.R.
#
# Тепловой карты корреляций факторов (cfa_factor_correlations.*) здесь тоже нет:
# при трёх факторах она несла ТРИ числа на девяти плитках, из которых три
# фиксированы спецификацией, а три — зеркало остальных. Те же три корреляции
# стоят подписью под схемой измерительной модели (cov_pairs в plot_cfa_path.R) и
# в таблице отчёта, где рядом с ними есть ДИ.
# ========================================================================

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
library(lavaan)

# --- настройки публикационного вывода ---
dpi <- PLOT_DPI

OUT <- "output/CFA"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ========================================================================
# ЗАГРУЗКА fit_3f (3-ФАКТОРНАЯ МОДЕЛЬ)
# ========================================================================

# Автозагрузка cfa_fit из сохранённых результатов CFA. Путь ровно один:
# source("scripts/plots/_plot_utils.R") выше уже требует корень проекта.
if (!exists("fit_3f")) {
  cfa_data_file <- file.path(OUT, "cfa_fits.RData")
  # УСЛОВНЫЙ вход: шаг 3 пишет cfa_fits.RData только при сходимости хотя бы одной
  # CFA-модели (save_objs в 3_efa_cfa.R), поэтому optional_input, а не require_input.
  if (!optional_input(cfa_data_file, "scripts/3_efa_cfa.R (шаг 3, БЛОК 2)",
                      "ни одна CFA-модель не сошлась")) {
    quit(save = "no", status = 0)
  }
  loaded_objs <- load(cfa_data_file)
  # Файл есть, но 3-факторной модели в нём нет: save_objs кладёт только сошедшиеся,
  # значит 3-факторная не сошлась — тот же легальный исход, что и отсутствие файла.
  if (!"cfa_fit" %in% loaded_objs) {
    message("[SKIP] ", cfa_data_file, " не содержит cfa_fit (есть: ",
            paste(loaded_objs, collapse = ", "),
            "): 3-факторная CFA не сошлась в шаге 3 — её рисунки пропущены.")
    quit(save = "no", status = 0)
  }
  fit_3f <- cfa_fit
}

plots_dir <- file.path(OUT, "plots")
dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

source("scripts/config.R")
validate_items(lavaan::lavNames(fit_3f, "ov"),
               "нагрузки 3F-ICM-CFA (cfa_fits.RData)")

# Цвет точки кодирует СУБШКАЛУ пункта — по канонической палитре sub_palette() из
# _plot_utils.R, а не по позиции ряда. Это не дубль заголовка панели: три блока
# по 26 строк идут стопкой на одном холсте, и принадлежность строки блоку читается
# по точке, а не по тому, под какой полосой она оказалась.
SUB_PAL <- sub_palette(names(SUBSCALES))

# Стандартизованное решение с доверительными интервалами; нагрузки — строки "=~".
std_sol <- lavaan::standardizedSolution(fit_3f, ci = TRUE, level = 0.95)

# ========================================================================
# ПРОФИЛЬ НАГРУЗОК -> cfa_loadings_forest
# ========================================================================

SALIENT <- 0.30   # порог существенности нагрузки
STRONG  <- 0.70   # ориентир «сильная нагрузка», общепринятый в отчётах по CFA

# Ширина объявлена рядом с рисунком: её же берёт wrap_lab() для переноса подписей.
W_FOREST <- W_2COL * 0.72
H_FOREST <- 6.6

load_df <- std_sol %>%
  filter(op == "=~") %>%
  transmute(
    Factor = lhs, Item = rhs,
    Beta = est.std, Lo = ci.lower, Hi = ci.upper
  ) %>%
  mutate(
    Panel = factor(sub_label(Factor), levels = sub_label(names(SUBSCALES))),
    # Сортировка по нагрузке общая, но в ICM-CFA пункт принадлежит ровно одному
    # фактору, а фасеты идут с free_y, поэтому каждая панель получает свой
    # непересекающийся кусок общего порядка — то есть сортировку внутри фактора.
    Item = fct_reorder(Item, Beta)
  )

# Индексы согласия — МАСШТАБИРОВАННЫЕ. Модель оценена WLSMV (DWLS +
# scaled.shifted), поэтому наивные chisq/CFI/TLI/RMSEA посчитаны по
# нескорректированной статистике и при этом тесте не интерпретируются, а наивный
# pvalue не определён вовсе (lavaan отдаёт NA, и sprintf положил бы на рисунок
# строку "p = NA"). Набор ключей — тот же, что FI_KEYS в 3_efa_cfa.R: иначе
# подпись рисунка несёт другие CFI/RMSEA той же модели, чем таблица
# bifactor_fit_table.csv в том же отчёте.
FI_KEYS_SCALED <- c(chisq = "chisq.scaled", df = "df.scaled",
                    pvalue = "pvalue.scaled", cfi = "cfi.scaled",
                    tli = "tli.scaled", rmsea = "rmsea.scaled", srmr = "srmr")
fit_indices <- setNames(as.numeric(lavaan::fitMeasures(fit_3f, FI_KEYS_SCALED)),
                        names(FI_KEYS_SCALED))

fit_text <- sprintf(
  "Scaled (WLSMV): chi2(%.0f) = %.2f, p = %.3f; CFI = %.3f, TLI = %.3f, RMSEA = %.3f, SRMR = %.3f",
  fit_indices["df"], fit_indices["chisq"], fit_indices["pvalue"],
  fit_indices["cfi"], fit_indices["tli"],
  fit_indices["rmsea"], fit_indices["srmr"]
)

# Пределы оси выводятся из данных, как в plot_bifactor_loadings.R: литеральные
# limits цензурируют значения вне диапазона (oob = censor), и граница ДИ за
# литералом снимала бы весь усик geom_linerange МОЛЧА, оставляя точку без
# интервала. При ДИ внутри [0; 1] пределы равны [0; 1.02].
x_lo <- min(0,    floor(min(load_df$Lo, na.rm = TRUE) * 10) / 10)
x_hi <- max(1.02, ceiling(max(load_df$Hi, na.rm = TRUE) * 10) / 10)

p_load <- ggplot(load_df, aes(x = Beta, y = Item, colour = Factor)) +
  geom_vline(xintercept = SALIENT, linetype = "dashed", colour = "grey55", linewidth = 0.35) +
  geom_vline(xintercept = STRONG,  linetype = "dotted", colour = "grey55", linewidth = 0.35) +
  geom_linerange(aes(xmin = Lo, xmax = Hi), linewidth = 0.45) +
  geom_point(size = 1.6) +
  facet_grid(Panel ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = SUB_PAL, guide = "none") +
  scale_x_continuous(limits = c(x_lo, x_hi), breaks = seq(-1, 1, 0.2),
                     expand = expansion(mult = 0.01)) +
  labs(
    title = "Three-factor ICM-CFA: standardized item loadings",
    subtitle = wrap_lab(fit_text, width_in = W_FOREST),
    x = expression("Standardized loading " * lambda * " (95% CI)"),
    y = "Item",
    caption = wrap_lab(sprintf("Dashed line: salience threshold %.2f. Dotted line: %.2f. Panels are the intended subscales; items are ordered by loading within a panel.",
                               SALIENT, STRONG), width_in = W_FOREST)
  ) +
  theme_pub(grid = "x") +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.y = element_line(linewidth = 0.2, colour = "grey92"))

forest_file <- file.path(plots_dir, paste0("cfa_loadings_forest.", PLOT_FORMAT))
save_ggplot(p_load, forest_file, W_FOREST, H_FOREST, dpi = dpi)
