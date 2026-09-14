# =============================================================================
#  ГРАФИК: карта Райта (Раш) — распределение способностей против трудностей
# =============================================================================
#  Раскладку строит wright_map() из scripts/plots/_plot_utils.R — она же у карты
#  2PL: карты двух моделей читаются рядом только пока у них совпадают оси,
#  порядок панелей и правило подписи.
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
library(ggrepel)

# КОНФИГУРАЦИЯ
width  <- W_2COL * 0.72   # дюймы
height <- 6.2             # дюймы
dpi    <- PLOT_DPI

person_path <- "output/Rasch/person_theta.csv"
report_path <- "output/Rasch/rasch_item_report.csv"

require_input(c(person_path, report_path), "scripts/4_rasch.R (шаг 4)")

person_theta_df <- read_csv(person_path)
rasch_report    <- read_csv(report_path)
source("scripts/config.R")
validate_items(rasch_report$Item, "Wright map (rasch_item_report.csv)")

person_theta <- person_theta_df$Theta
BINW <- 0.25   # логиты

items_df <- rasch_report %>%
  transmute(Item, Difficulty)

draw_wright <- function() {
  wright_map(
    theta    = person_theta,
    items    = items_df,
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    binwidth = BINW,
    width_in = width
  )
}

output_path <- file.path("output/Rasch/plots", paste0("rasch_wright_map.", PLOT_FORMAT))
save_base_plot(draw_wright, output_path, width, height, dpi = dpi)
