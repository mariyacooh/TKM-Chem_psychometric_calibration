# =============================================================================
#  ГРАФИК: DIF Mantel-Haenszel (ru vs. kz) — статистика и величина эффекта
# =============================================================================
#  Две панели на общей оси пунктов:
#    слева  — статистика MH chi2 (значимость),
#    справа — lnOR с 95% ДИ (величина и НАПРАВЛЕНИЕ различия).
#
#  Панель величины эффекта добавлена потому, что по одной chi2 нельзя сказать ни
#  насколько велико смещение, ни в чью пользу: прежний рисунок отсылал за lnOR в
#  таблицу отчёта, хотя обе величины лежат в одном артефакте dif_mh_table.csv.
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
library(ggplot2)
library(patchwork)

# КОНФИГУРАЦИЯ
width  <- W_2COL   # дюймы
height <- 6.4      # дюймы
dpi    <- PLOT_DPI

color_dif    <- OK_PAL[4]   # киноварь — DIF после поправки BH
color_nodif  <- OK_PAL[6]   # голубой  — без DIF

data_file <- "output/DIF/dif_mh_table.csv"
require_input(data_file, "scripts/7_dif.R (шаг 7)")

mh_tbl <- read_csv(data_file)
source("scripts/config.R")
validate_items(mh_tbl$Item, "DIF MH plot (dif_mh_table.csv)")

# Колонки величины эффекта объявлены здесь поверх того, что пишет шаг 7, поэтому
# их наличие проверяется явно: артефакт, созданный до появления lnOR/CI, иначе
# дал бы пустую правую панель при живом коде возврата 0.
EFF_COLS <- c("lnOR", "CI_low", "CI_high")
absent <- setdiff(EFF_COLS, names(mh_tbl))
if (length(absent))
  stop(sprintf(paste0("plot_dif_mh: в dif_mh_table.csv нет колонок %s. Артефакт ",
                      "старше панели величины эффекта — нужен полный прогон."),
               paste(absent, collapse = ", ")), call. = FALSE)

# Порядок пунктов — общий для обеих панелей: разный порядок сделал бы строки
# несопоставимыми, а панели рядом читаются построчно.
plot_df <- mh_tbl %>%
  mutate(
    Panel = factor(sub_label(Subscale, short = TRUE),
                   levels = sub_label(names(SUBSCALES), short = TRUE)),
    Item  = fct_reorder(Item, MH_chi2),
    Flag  = ifelse(DIF_flag == "-", "no DIF", "DIF (BH-adjusted p < .05)")
  )
FLAG_PAL <- c("no DIF" = color_nodif, "DIF (BH-adjusted p < .05)" = color_dif)

chi_ref <- qchisq(0.95, df = 1)

p_chi <- ggplot(plot_df, aes(x = MH_chi2, y = Item, fill = Flag)) +
  geom_col(width = 0.72, colour = "grey30", linewidth = 0.15) +
  geom_vline(xintercept = chi_ref, linetype = "dashed", colour = "grey35", linewidth = 0.4) +
  scale_fill_manual(values = FLAG_PAL, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_grid(Panel ~ ., scales = "free_y", space = "free_y") +
  labs(x = expression("MH " * chi^2), y = "Item") +
  theme_pub(grid = "x") +
  theme(strip.text.y = element_text(angle = 0))

# lnOR > 0 означает преимущество фокальной группы (kz) при равном общем балле.
# Нулевая линия названа в подписи: без неё знак читается только по осям.
p_eff <- ggplot(plot_df, aes(x = lnOR, y = Item, colour = Flag)) +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.4) +
  geom_linerange(aes(xmin = CI_low, xmax = CI_high), linewidth = 0.45) +
  geom_point(size = 1.4) +
  # Легенда снята здесь, а не собрана обеими панелями: обе кодируют один и тот же
  # признак Flag, но разными эстетиками (fill слева, colour справа), поэтому
  # plot_layout(guides = "collect") выдавал бы её ДВАЖДЫ — плашкой и точкой.
  scale_colour_manual(values = FLAG_PAL, guide = "none") +
  facet_grid(Panel ~ ., scales = "free_y", space = "free_y") +
  # Подпись оси пунктов снята ЗДЕСЬ, а не на обеих панелях: строки у панелей
  # общие, слева они подписаны именами пунктов и заголовком оси, и второй
  # ярлык "Item" у безымянной оси называл бы то, чего на ней не видно.
  labs(x = "ln(OR) with 95% CI", y = NULL) +
  theme_pub(grid = "x") +
  theme(strip.text.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p_dif <- (p_chi | p_eff) +
  plot_layout(widths = c(1, 1), guides = "collect") +
  plot_annotation(
    title = "Differential item functioning, Mantel-Haenszel: kz (focal) against ru (reference)",
    subtitle = wrap_lab(sprintf("Left: MH chi-squared; the dashed line is the UNADJUSTED chi-squared(1, .95) = %.2f and is a reference mark only, since the decision rule is the BH-adjusted p. Right: effect size; ln(OR) > 0 favours the focal group at equal total score.",
                                chi_ref)),
    theme = theme_pub() +
      theme(plot.title = element_text(size = 10, face = "bold"),
            plot.subtitle = element_text(size = 7.5, colour = "grey25"))
  ) &
  theme(legend.position = "bottom")

output_path <- file.path("output/DIF/plots", paste0("dif_mh.", PLOT_FORMAT))
save_ggplot(p_dif, output_path, width, height, dpi = dpi)
