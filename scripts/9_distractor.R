# =======================================================================
#  ШАГ 9 — АНАЛИЗ ДИСТРАКТОРОВ (CTT-скрининг пунктов)
# -----------------------------------------------------------------------
#  ВХОД : output/cleaned_responses.csv          (шаг 0; бинарная матрица + язык)
#         input/google_forms_responses.xlsx     (сырые буквы вариантов)
#         input/items.csv                       (через scripts/config.R)
#  ВЫХОД: output/Distractor/distractor_pb_table.csv -> plot_distractor_heatmap, report.Rmd
#         output/Distractor/distractor_analysis.txt -> report.Rmd
#
#  Единственный шаг, читающий сырой xlsx ПОСЛЕ шага 0: бинарная матрица не хранит
#  выбранную букву. Строки xlsx выравниваются с cleaned_responses.csv (проверка ниже).
#  Полный граф — scripts/_artifacts.R; порядок — scripts/run_all.R.
# -----------------------------------------------------------------------

# Бутстрап: рабочая директория — корень проекта, затем общие функции настройки
local({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    root <- normalizePath(dirname(rstudioapi::getActiveDocumentContext()$path), winslash = "/")
    while (!file.exists(file.path(root, "scripts", "config.R")) && root != dirname(root)) root <- dirname(root)
    if (file.exists(file.path(root, "scripts", "config.R"))) setwd(root)
  }
})
source("scripts/_setup.R")
reset_sink()
set.seed(42)

load_pkgs(c("tidyverse", "readxl", "CTT"))

select <- dplyr::select; filter <- dplyr::filter; mutate <- dplyr::mutate

# Пути от корня проекта: source("scripts/_setup.R") выше уже его требует, поэтому
# вариант «рабочая директория = scripts/» не поддерживается. OUT — литерал, а не
# dirname(csv_path): читаемый и записываемый каталоги разойтись не могут.
csv_path  <- "output/cleaned_responses.csv"
xlsx_path <- "input/google_forms_responses.xlsx"

require_input(csv_path, "scripts/0_preprocess.R (шаг 0)")
require_input(xlsx_path, "репозиторий (input/, версионируется)")

OUT <- "output/Distractor"
ensure_dir(OUT)

source("scripts/config.R")   # ITEMS, ITEM_SUB, ALL_ADMINISTERED (единый источник)

ALL_27 <- ALL_ADMINISTERED

LANG_COL <- 2L
KZ_COLS  <- 9:35
RU_COLS  <- 42:68

df_bin <- read_csv(csv_path) %>%
  mutate(across(all_of(ITEMS), as.integer))

# Тот же контракт типов, что у шага 0: без col_types типы столбцов угадываются, и
# строковая запись ячейки разошлась бы с шагом 0 — вместе с буквами дистракторов
# разошёлся бы и позиционный маппинг, единственная независимая проверка которого здесь.
df_raw <- read_excel(xlsx_path, col_names = FALSE, col_types = "text",
                     .name_repair = "minimal")
if (grepl("Отметка|Timestamp|время", as.character(df_raw[1, 1]), ignore.case = TRUE))
  df_raw <- df_raw[-1, ]

n_xlsx <- nrow(df_raw)

# Язык строки определяется ТОЙ ЖЕ функцией, что и в шаге 0 (resolve_lang из
# scripts/_setup.R). Собственный набор регулярных выражений здесь означал бы два
# расходящихся правила на одних данных, и расходятся они по двум линиям. Набор
# токенов: шаг 0 ищет "kz" в любой позиции и знает казахский токен "қаз", тогда
# как якорь "^kz" без "қаз" часть строк не распознаёт. Запасное разрешение по
# заполненности: без него нераспознанный язык даёт пустую строку raw_mat, её
# убирает фильтр answered != 0, и проверка выравнивания ниже обрывает ВЕСЬ шаг на
# записи, которую шаг 0 штатно спасает.
count_filled <- function(cols)
  Reduce(`+`, lapply(cols, function(j) as.integer(!is_empty(df_raw[[j]]))))
kz_filled <- count_filled(KZ_COLS)
ru_filled <- count_filled(RU_COLS)
lang_xlsx <- vapply(seq_len(n_xlsx), function(i)
  resolve_lang(df_raw[[LANG_COL]][i], kz_filled[i], ru_filled[i]), character(1))

raw_mat <- matrix(NA_character_, nrow = n_xlsx, ncol = length(ALL_27), dimnames = list(NULL, ALL_27))
for (i in seq_len(n_xlsx))
  raw_mat[i, ] <- as.character(df_raw[i, if (lang_xlsx[i] == "kz") KZ_COLS else RU_COLS])

# 0_preprocess.R отбрасывает пустые записи (n_answered == 0) и переиндексирует ID,
# поэтому cleaned_responses.csv — это ПОДМНОЖЕСТВО строк xlsx в том же порядке, а не
# первые N. Склейка по позиции (seq_len(min(...))) для такого подмножества неверна:
# отброшенная ВНУТРЕННЯЯ пустая строка сдвигает хвост, и сырой ответ одного
# респондента попал бы к баллам другого. Тот же фильтр воспроизведён здесь,
# чтобы строки raw_mat соответствовали строкам df_bin по РЕСПОНДЕНТУ; выравнивание
# затем жёстко проверяется — при рассинхроне громкое падение, а не порча анализа.
EMPTY_TOK <- c("", "nan", "none", "-")   # как is_empty() в scripts/_setup.R
answered  <- vapply(seq_len(n_xlsx), function(i) {
  v <- trimws(raw_mat[i, ])
  sum(!is.na(v) & !(tolower(v) %in% EMPTY_TOK))
}, integer(1))
raw_mat <- raw_mat[answered != 0L, , drop = FALSE]
# Язык, из столбцов которого фактически взята строка raw_mat (та же ветка выше).
lang_raw_kept <- lang_xlsx[answered != 0L]

if (nrow(raw_mat) != nrow(df_bin)) {
  stop(sprintf(
    paste0("9_distractor: строки xlsx не выровнены с cleaned_responses.csv ",
           "(непустых в xlsx = %d, записей в cleaned = %d). Перезапустите 0_preprocess.R ",
           "или проверьте детект языка/фильтр пустых записей."),
    nrow(raw_mat), nrow(df_bin)), call. = FALSE)
}
n_use <- nrow(df_bin)

# Язык респондента берётся из cleaned_responses.csv (его пишет 0_preprocess.R —
# единый источник). Дистракторный анализ ведётся ОТДЕЛЬНО по языковым формам,
# поэтому язык обязан совпадать с тем, из каких столбцов xlsx взята строка
# raw_mat: расхождение означало бы, что текст ответа и балл относятся к разным
# формам. Оба шага резолвят язык одной и той же resolve_lang(), поэтому проверка
# ниже — инвариант (сработает, только если cleaned_responses.csv отстал от xlsx),
# а не рабочий фильтр: нераспознанный язык разрешается по заполненности в обоих
# шагах одинаково, и строка на этом не теряется.
lang_vec <- as.character(df_bin$Language)
if (!identical(lang_vec, lang_raw_kept)) {
  stop("9_distractor: детект языка в cleaned_responses.csv разошёлся с xlsx — ",
       "текст ответа и балл относились бы к разным языковым формам.", call. = FALSE)
}
LANGS <- c(kz = "KZ", ru = "RU")

bin_all <- matrix(NA_integer_, nrow = n_use, ncol = length(ALL_27), dimnames = list(NULL, ALL_27))
for (item in ALL_27)
  if (item %in% names(df_bin))
    bin_all[, item] <- as.integer(df_bin[[item]])

total_score <- rowSums(df_bin[, ITEMS], na.rm = TRUE)

coded_mat   <- matrix(NA_character_, nrow = n_use, ncol = length(ALL_27), dimnames = list(NULL, ALL_27))
item_legend <- list()

# Порядок вариантов детерминирован и локале-независим: по частоте (убыв.), ничьи
# разбиваются по тексту в БАЙТОВОМ порядке (method="radix", не зависит от
# LC_COLLATE). sort(table(...)) здесь не годится: он упорядочивает кириллицу по
# локали хоста (glibc ставит «ё» рядом с «е», radix — по кодовой точке), и буква
# A/B/C/D дистрактора менялась бы между окружениями.
freq_order <- function(x, decreasing = TRUE) {
  tb  <- table(x)
  sgn <- if (decreasing) -1L else 1L
  names(tb)[order(sgn * as.integer(tb), names(tb), method = "radix")]
}

# Правильный вариант определяется по БИНАРНОЙ оценке is_correct(), а не по
# частоте текста ответа. KZ- и RU-формы записывают верный ответ РАЗНЫМИ строками
# (input/answer_key.csv хранит отдельные колонки kz/ru), поэтому выбор самой
# частой строки по объединённой выборке дал бы казахскую формулировку, а русская
# попала бы в дистракторы с пометкой IsCorrect = FALSE.
# Дистракторы между формами сопоставить нельзя — полных списков вариантов в
# данных нет, есть только тексты ответов, — поэтому буквы раздаются ВНУТРИ своей
# языковой формы, а N/Pct/pbis считаются по её собственной выборке.
for (item in ALL_27) {
  raw_j  <- raw_mat[, item]
  bin_j  <- bin_all[, item]
  valid  <- !is.na(raw_j) & !is.na(bin_j) & nchar(trimws(raw_j)) > 0 & raw_j != "NA"
  if (sum(valid) < 5) next

  item_legend[[item]] <- list()
  for (lg in names(LANGS)) {
    sel <- valid & lang_vec == lg
    if (!any(sel)) next
    raw_l <- raw_j[sel]; bin_l <- bin_j[sel]
    # Все верные формулировки этой формы -> A; остальные -> B, C, ... по частоте.
    # Если в форме нет ни одного верного ответа, буква A не выдаётся вовсе —
    # правильного варианта в наблюдениях нет, и назначать его произвольно нельзя.
    corr_l  <- freq_order(raw_l[bin_l == 1])
    wrong_l <- freq_order(raw_l[!(raw_l %in% corr_l)])
    opt_map <- c(setNames(rep("A", length(corr_l)), corr_l),
                 setNames(LETTERS[seq_along(wrong_l) + 1L], wrong_l))
    item_legend[[item]][[lg]] <- opt_map
    coded_mat[sel, item] <- unname(opt_map[raw_l])
  }
}

# Доли и point-biserial считаются в границах языковой формы: вариант из KZ-формы
# предъявлялся только KZ-респондентам, поэтому делить его частоту на всю выборку
# (n_use) и коррелировать с баллами обеих форм нельзя.
pb_rows <- list()
for (item in ALL_27) {
  # Корреляция считается с ОСТАТОЧНЫМ баллом (item-rest): в total_score входят все
  # ITEMS, поэтому для анализируемого пункта его собственный вклад вычитается —
  # иначе пункт коррелирует сам с собой и pbis завышен (part-whole overlap).
  # Q02 в total_score не входит вовсе, для него остаток равен полному баллу.
  # total_score считается rowSums(na.rm = TRUE), т.е. NA = 0; вычитание такое же.
  own_j      <- bin_all[, item]
  rest_score <- if (item %in% ITEMS) total_score - ifelse(is.na(own_j), 0L, own_j) else total_score
  for (lg in names(LANGS)) {
    in_lg <- lang_vec == lg
    n_lg  <- sum(in_lg)
    if (n_lg == 0) next
    coded <- coded_mat[in_lg, item]
    tot_l <- rest_score[in_lg]
    opts  <- sort(unique(coded[!is.na(coded)]), method = "radix")
    if (length(opts) == 0) next
    for (opt in opts) {
      is_opt  <- (!is.na(coded)) & coded == opt
      n_opt   <- sum(is_opt)
      pb      <- if (n_opt >= 2 && n_opt <= (n_lg - 2))
        tryCatch(cor(as.integer(is_opt), tot_l, use = "complete.obs"), error = function(e) NA_real_)
      else NA_real_
      pb_rows[[length(pb_rows)+1]] <- tibble(
        Item      = item,
        Language  = unname(LANGS[[lg]]),
        InCFA     = item %in% ITEMS,
        Subscale  = if (item %in% ITEMS) ITEM_SUB[item] else "-",
        Option    = opt,
        IsCorrect = opt == "A",
        N         = n_opt,
        Pct       = round(100 * n_opt / n_lg, 1),
        pbis      = round(pb, 3)
      )
    }
  }
}
pb_tbl <- bind_rows(pb_rows)
if (nrow(pb_tbl) == 0) {
  pb_tbl <- tibble(Item = character(), Language = character(), InCFA = logical(), Subscale = character(), Option = character(), IsCorrect = logical(), N = integer(), Pct = numeric(), pbis = numeric())
}

# Сохранение таблицы distractor pb для визуализации
write_csv_excel(pb_tbl, file.path(OUT, "distractor_pb_table.csv"))

# CTT обязателен (load_pkgs выше). Вызов прямой; ошибка — фатальна (re-raise),
# а не молчаливый NULL, иначе блок анализа дистракторов молча исчез бы из отчёта.
# Отдельно по каждой языковой форме: буквы вариантов осмысленны только внутри
# своей формы, поэтому ключ "A" указывает на верный вариант в каждой из них.
da <- lapply(names(LANGS), function(lg) {
  sel <- lang_vec == lg
  tryCatch(
    CTT::distractor.analysis(coded_mat[sel, ITEMS, drop = FALSE],
                             rep("A", length(ITEMS))),
    error = function(e) stop(sprintf(
      "CTT::distractor.analysis (%s) упал: %s", LANGS[[lg]], conditionMessage(e)), call. = FALSE)
  )
})
names(da) <- names(LANGS)

# Запись текстового отчета без качественных суждений и вербального шаблона статьи
SEP <- strrep("=", 72)
sink(file.path(OUT, "distractor_analysis.txt"))
tryCatch({
  cat(SEP, "\n")
  cat(sprintf(" АНАЛИЗ ДИСТРАКТОРОВ — ТКМ-Хим  n=%d | %d пунктов\n", n_use, length(ALL_27)))
  cat(SEP, "\n\n")

  cat("── КОДИРОВКА ВАРИАНТОВ (A = правильный, отмечен *) ───────────────────\n")
  cat("   Буквы раздаются внутри своей языковой формы: B в KZ и B в RU — разные\n")
  cat("   варианты. A — верный ответ формы по ключу ответов.\n")
  for (item in ALL_27) {
    leg <- item_legend[[item]]; if (is.null(leg) || !length(leg)) next
    sub <- if (item %in% ITEMS) ITEM_SUB[item] else "excl"
    cat(sprintf("\n%s [%s]:\n", item, sub))
    for (lg in names(leg)) {
      cat(sprintf("  [%s]\n", LANGS[[lg]]))
      map_l <- leg[[lg]]
      for (txt in names(map_l))
        cat(sprintf("    %s%s  %s\n", map_l[[txt]],
                    if (map_l[[txt]] == "A") " *" else "  ", substr(txt, 1, 80)))
    }
  }

  cat("\n\n", SEP, "\n POINT-BISERIAL ПО ВАРИАНТАМ (в границах языковой формы)\n", SEP, "\n")
  cat(" pbis = item-rest: балл сравнения не включает сам пункт (для пунктов из ITEMS).\n\n")
  cat(sprintf(" %-6s %-4s %-5s %-4s %-5s %6s %6s %8s\n", "Item","Sub","Lang","Opt","Corr","N","Pct%","pbis"))
  cat(strrep("-",52),"\n")
  for (item in ALL_27) {
    rows <- pb_tbl %>% filter(Item == item); if (nrow(rows)==0) next
    for (i in seq_len(nrow(rows))) {
      r <- rows[i,]
      cat(sprintf(" %-6s %-4s %-5s %-4s %-5s %6d %5.1f%% %8s\n",
                  r$Item, r$Subscale, r$Language, r$Option, if(r$IsCorrect) "YES" else "   ", r$N, r$Pct,
                  ifelse(is.na(r$pbis),"  NA", sprintf("%.3f",r$pbis))))
    }; cat("\n")
  }

  cat("\n── CTT::distractor.analysis (отдельно по языковым формам) ────────────\n")
  for (lg in names(LANGS)) {
    cat(sprintf("\n[%s]\n", LANGS[[lg]]))
    for (item in ITEMS) { if (!is.null(da[[lg]][[item]])) { cat(sprintf("\n%s:\n",item)); print(da[[lg]][[item]]) } }
  }

  pb_correct <- pb_tbl %>% filter(IsCorrect, !is.na(pbis)) %>% pull(pbis)
  mean_val <- if (length(pb_correct) > 0) mean(pb_correct, na.rm=TRUE) else 0
  min_val  <- if (length(pb_correct) > 0) min(pb_correct, na.rm=TRUE) else 0
  max_val  <- if (length(pb_correct) > 0) max(pb_correct, na.rm=TRUE) else 0
  sd_val   <- if (length(pb_correct) > 1) sd(pb_correct, na.rm=TRUE) else 0

  cat("\n\n",SEP,"\n СВОДКА\n",SEP,"\n")
  cat(sprintf(" pbis правильных ответов (по пунктам x языковым формам, n=%d):\n", length(pb_correct)))
  cat(sprintf("   M=%.3f  SD=%.3f  min=%.3f  max=%.3f\n", mean_val, sd_val, min_val, max_val))
}, finally = {
  while (sink.number() > 0) sink()
})
