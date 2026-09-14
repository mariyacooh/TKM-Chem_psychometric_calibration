#!/usr/bin/env python3
# =============================================================================
#  item_cues.py — поверхностные подсказки в вариантах ответа и целостность данных
# -----------------------------------------------------------------------------
#  ВХОД : input/google_forms_responses.xlsx  (сырые ответы и тексты вариантов)
#         input/answer_key.csv               (ключ верных ответов kz/ru)
#  ВЫХОД: печать в stdout; файлы не пишутся, output/ не изменяется
#
#  Скрипт ВНЕ пайплайна: scripts/run_all.R его не вызывает, output/ не читается.
#  Воспроизводит числа из analysis/item_cues.md — там же вопрос, трактовка и
#  ограничения. Оценивание ответов перенесено из scripts/0_preprocess.R (тот же
#  позиционный маппинг ITEM_TO_COLS) и сверено по объёму выборки, распределению
#  суммарного балла и долям краёв.
#
#  Запуск из корня проекта (полный прогон пайплайна не нужен):
#      python3 analysis/item_cues.py
#  Зависимости: openpyxl (та же, что у subscale_assignment.py). Время — секунды,
#  кроме блока B3 (перестановочный тест, ~20 с).
#
#  Два блока, отвечающих на РАЗНЫЕ вопросы.
#    Блок A — конструктная валидность ПУНКТОВ: можно ли выбрать верный вариант,
#      не читая условие, по одной длине варианта. Свойство самих текстов;
#      не зависит от того, как отвечали респонденты.
#    Блок B — целостность ДАННЫХ: есть ли записи, несовместимые с независимым
#      заполнением формы. Свойство выборки; о качестве пунктов не говорит.
#
#  Блок A содержит две встроенные проверки ПРОТИВ находки (A3, A4): подсказка
#  может существовать в текстах и при этом не использоваться респондентами.
#  Разделение обязательно — A1/A2 меряют уязвимость, A3/A4 её реализацию.
# =============================================================================

import csv
import math
import random
import re
import statistics as st
import sys
from collections import Counter

from openpyxl import load_workbook

ROOT = "."

# Позиционный маппинг столбцов xlsx (0-based), дублирует scripts/0_preprocess.R:
# item -> (kz_col, ru_col). Совпадение с analysis/subscale_assignment.py
# обязательно: расхождение означает дрейф одной из реализаций.
ITEM_TO_COLS = {
    1: (8, 41), 2: (9, 42), 3: (10, 43), 4: (11, 44), 5: (12, 45), 6: (13, 46),
    7: (14, 47), 8: (15, 48), 9: (16, 49), 10: (17, 50), 11: (18, 51),
    12: (19, 52), 13: (20, 53), 14: (21, 54), 15: (22, 55), 16: (23, 56),
    18: (24, 57), 19: (25, 58), 20: (26, 59), 21: (27, 60), 22: (28, 61),
    23: (29, 62), 24: (30, 63), 26: (31, 64), 27: (32, 65), 28: (33, 66),
    29: (34, 67),
}
LANG_COL = 1
REGION_COL = (3, 36)          # (kz, ru) — «текущее местоположение»
EXCLUDED = "Q02"              # DECISIONS.md D4: экстремальный языковой DIF
N_OPTIONS = 4                 # вариантов на пункт (выбор одного из четырёх)
SEED = 42                     # как в analysis/bimodality.R
N_PERM = 3000                 # итераций перестановочного теста B3


# --- нормализация текстов -----------------------------------------------------
# Сопоставление ответа с ключом: ключ в answer_key.csv записан без буквенного
# префикса варианта и без финальной пунктуации, в выгрузке они присутствуют, а
# у части пунктов вариант в форме ДЛИННЕЕ ключа (ключ — его начало). Поэтому
# сопоставление идёт по префиксу нормализованного текста, а не по равенству.
# Кавычки снимаются целиком: в выгрузке встречается `« LifeChem »` против
# `«LifeChem»` в ключе (ru Q24), и пробел внутри кавычек ломал бы префикс.
PREFIX_RE = re.compile(r"^[A-DА-Г][\.\)]\s*")
QUOTE_RE = re.compile(r"[«»\"“”‹›']")


def norm(s):
    s = (s or "").strip().replace("ё", "е")
    s = QUOTE_RE.sub("", PREFIX_RE.sub("", s))
    s = re.sub(r"\s+", " ", s)
    s = re.sub(r"\s+([:;,.!?])", r"\1", s)   # снятые кавычки оставляют пробел
    return s.rstrip(" .;").lower()


def visible(s):
    """Текст варианта без буквенного префикса — то, что читает респондент."""
    return PREFIX_RE.sub("", (s or "").strip())


def read_key(path):
    key = {}
    with open(path, encoding="utf-8-sig") as fh:
        for row in csv.DictReader(fh):
            key[row["item"]] = (row["kz"].strip(), row["ru"].strip())
    return key


def read_rows(path):
    wb = load_workbook(path, read_only=True, data_only=True)
    rows = list(wb.worksheets[0].iter_rows(min_row=2, values_only=True))
    wb.close()
    return rows


def build(rows, key):
    """Оценивание: -> (recs, opts, keyopt, items). Сверяется тестом A0."""
    items = [f"Q{n:02d}" for n in ITEM_TO_COLS]
    scored = [q for q in items if q != EXCLUDED]

    opts = {}
    for li, lang in ((0, "kz"), (1, "ru")):
        for n, q in zip(ITEM_TO_COLS, items):
            col = ITEM_TO_COLS[n][li]
            seen = Counter(r[col] for r in rows if r[col] is not None)
            opts[(lang, q)] = seen

    keyopt = {}
    for li, lang in ((0, "kz"), (1, "ru")):
        for n, q in zip(ITEM_TO_COLS, items):
            target = norm(key[q][li])
            cands = [o for o in opts[(lang, q)] if norm(o).startswith(target[:60])]
            if len(cands) != 1:
                sys.exit(f"Ключ не опознан однозначно: {lang} {q} ({len(cands)} совпадений)")
            keyopt[(lang, q)] = cands[0]

    recs = []
    for r in rows:
        lang = "ru" if r[ITEM_TO_COLS[1][1]] is not None else "kz"
        li = 1 if lang == "ru" else 0
        ans = {q: r[ITEM_TO_COLS[n][li]] for n, q in zip(ITEM_TO_COLS, items)}
        corr = {q: int(ans[q] == keyopt[(lang, q)]) for q in items}
        recs.append({
            "lang": lang,
            "ans": ans,
            "corr": corr,
            "score": sum(corr[q] for q in scored),
            "ts": r[0],
            "region": r[REGION_COL[li]],
        })
    return recs, opts, keyopt, scored


def is_key_longest(opts, keyopt, lang, q):
    o = list(opts[(lang, q)])
    return max(o, key=lambda x: len(visible(x))) == keyopt[(lang, q)]


def longest_opt(opts, lang, q):
    return max(opts[(lang, q)], key=lambda x: len(visible(x)))


# --- A0: сверка оценивания с пайплайном --------------------------------------
def block_a0(recs, scored):
    sc = [r["score"] for r in recs]
    print("=== A0. Сверка оценивания (ожидаются числа пайплайна) ===")
    print(f"  N = {len(recs)} (kz {sum(r['lang'] == 'kz' for r in recs)} / "
          f"ru {sum(r['lang'] == 'ru' for r in recs)}); пунктов {len(scored)}")
    print(f"  Score_Total: M = {st.mean(sc):.2f}, SD = {st.stdev(sc):.2f}, "
          f"Mdn = {st.median(sc):.0f}, диапазон {min(sc)}-{max(sc)}")
    print(f"  пол (<= 8): {sum(s <= 8 for s in sc)} ({100 * sum(s <= 8 for s in sc) / len(sc):.1f} %); "
          f"потолок (>= 24): {sum(s >= 24 for s in sc)} "
          f"({100 * sum(s >= 24 for s in sc) / len(sc):.1f} %); "
          f"полный балл: {sum(s == len(scored) for s in sc)}")
    ref = (15.89, 7.87, 73, 77, 45)
    got = (round(st.mean(sc), 2), round(st.stdev(sc), 2),
           sum(s <= 8 for s in sc), sum(s >= 24 for s in sc),
           sum(s == len(scored) for s in sc))
    print(f"  СВЕРКА: {'совпадает' if got == ref else 'РАСХОЖДЕНИЕ ' + str(got)} "
          f"с M = 15.89, SD = 7.87, 73 / 77 / 45")


# --- A1: ключ как самый длинный вариант --------------------------------------
def block_a1(opts, keyopt, scored):
    print("\n=== A1. Является ли ключ самым длинным вариантом ===")
    for lang in ("kz", "ru"):
        hits = [is_key_longest(opts, keyopt, lang, q) for q in scored]
        ranks, kl, dl = [], [], []
        for q in scored:
            o = list(opts[(lang, q)])
            lens = {x: len(visible(x)) for x in o}
            ranks.append(sorted(o, key=lambda x: -lens[x]).index(keyopt[(lang, q)]) + 1)
            kl.append(lens[keyopt[(lang, q)]])
            dl += [v for x, v in lens.items() if x != keyopt[(lang, q)]]
        print(f"  {lang}: ключ самый длинный в {sum(hits)} из {len(scored)} пунктов "
              f"({100 * sum(hits) / len(scored):.0f} %); случайно ожидается "
              f"{100 / N_OPTIONS:.0f} %")
        print(f"      средний ранг ключа по длине {st.mean(ranks):.2f} "
              f"(1 = самый длинный; случайно {(N_OPTIONS + 1) / 2:.1f})")
        print(f"      длина ключа {st.mean(kl):.0f} знаков против "
              f"{st.mean(dl):.0f} у дистракторов")
        print("      пункты, где ключ НЕ самый длинный: "
              + ", ".join(q for q in scored if not is_key_longest(opts, keyopt, lang, q)))


# --- A2: балл стратегии, не читающей условие ---------------------------------
def block_a2(recs, opts, keyopt, scored):
    print("\n=== A2. Балл стратегии «всегда самый длинный вариант» ===")
    sc = [r["score"] for r in recs]
    for lang in ("kz", "ru"):
        n = sum(is_key_longest(opts, keyopt, lang, q) for q in scored)
        print(f"  {lang}: {n} из {len(scored)} = {100 * n / len(scored):.0f} % — "
              f"условие не прочитано ни разу")
        below = sum(s < n for s in sc)
        print(f"      респондентов с баллом НИЖЕ этой стратегии: {below} из "
              f"{len(sc)} ({100 * below / len(sc):.0f} %)")
    print(f"  средний балл выборки: {st.mean(sc):.2f} из {len(scored)} = "
          f"{100 * st.mean(sc) / len(scored):.0f} %")


# --- A3: проверка ПРОТИВ находки — трудность пунктов с подсказкой и без ------
def block_a3(recs, opts, keyopt, scored):
    print("\n=== A3. Проверка ПРОТИВ: труднее ли пункты без подсказки ===")
    for lang in ("kz", "ru"):
        g = [r for r in recs if r["lang"] == lang]
        with_cue = [q for q in scored if is_key_longest(opts, keyopt, lang, q)]
        without = [q for q in scored if not is_key_longest(opts, keyopt, lang, q)]
        p_w = st.mean(sum(r["corr"][q] for r in g) / len(g) for q in with_cue)
        p_o = st.mean(sum(r["corr"][q] for r in g) / len(g) for q in without)
        print(f"  {lang}: с подсказкой (n = {len(with_cue)}) p = {p_w:.3f}; "
              f"без (n = {len(without)}) p = {p_o:.3f}; разность {p_w - p_o:+.3f}")
    print("  Трактовка: разность около нуля => подсказка в текстах ЕСТЬ, но")
    print("  систематического использования её респондентами не видно.")


# --- A4: проверка ПРОТИВ — частота выбора самого длинного варианта -----------
def block_a4(recs, opts, scored):
    print("\n=== A4. Проверка ПРОТИВ: как часто выбирают самый длинный вариант ===")
    groups = (("пол (<= 8)", lambda r: r["score"] <= 8),
              ("середина (9-23)", lambda r: 9 <= r["score"] <= 23),
              ("потолок (>= 24)", lambda r: r["score"] >= 24))
    for name, sel in groups:
        g = [r for r in recs if sel(r)]
        rate = st.mean(
            sum(1 for q in scored if r["ans"][q] == longest_opt(opts, r["lang"], q))
            / len(scored) for r in g)
        print(f"  {name:16s} n = {len(g):3d}: самый длинный вариант "
              f"{100 * rate:.1f} % ответов (случайно {100 / N_OPTIONS:.0f} %)")
    print("  Трактовка: у потолка показатель высок ПОТОМУ, что ключ обычно самый")
    print("  длинный, а не наоборот — величина смешана с правильностью и")
    print("  самостоятельным доводом не является. Информативна нижняя строка:")
    print("  пол не отличается от случайного выбора и по этому признаку тоже.")


# --- B1: побайтово идентичные векторы ответов --------------------------------
def block_b1(recs, scored):
    print("\n=== B1. Побайтово идентичные векторы ответов ===")
    sig = {}
    for r in recs:
        sig.setdefault(tuple(r["ans"][q] for q in scored), []).append(r)
    print(f"  различных векторов: {len(sig)} на {len(recs)} респондентов")
    dups = sorted((v for v in sig.values() if len(v) > 1), key=len, reverse=True)
    for v in dups:
        wrong = [q for q in scored if v[0]["corr"][q] == 0]
        ts = sorted(r["ts"] for r in v)
        span = ts[-1] - ts[0]
        print(f"  n = {len(v):2d}  балл {v[0]['score']:2d}  форма {v[0]['lang']}  "
              f"неверных {len(wrong)}  разброс отправок {span}")
        if 0 < len(wrong) <= 3:
            print(f"        неверные пункты: {', '.join(wrong)}")
        if len(wrong) > 3:
            print(f"        регионы: {', '.join(str(r['region']) for r in v)}")
    print("  Вырожденные случаи: при балле 26 вектор единственен по построению,")
    print("  при 25 их всего 26 x 3 — совпадение там ожидаемо и ничего не значит.")
    print("  Информативны только группы с БОЛЬШИМ числом неверных ответов.")
    return dups


# --- B2: вероятность совпадения при независимом заполнении ------------------
def block_b2(recs, scored, dups):
    print("\n=== B2. Вероятность идентичного вектора при независимом заполнении ===")
    target = None
    for v in dups:
        if sum(v[0]["corr"][q] == 0 for q in scored) > 3:
            target = v
            break
    if target is None:
        print("  групп с большим числом неверных ответов нет — блок неприменим")
        return None
    lang = target[0]["lang"]
    g = [r for r in recs if r["lang"] == lang]
    vec = tuple(target[0]["ans"][q] for q in scored)

    # Маргинальные частоты вариантов внутри той же языковой формы: оценка
    # СВЕРХУ, поскольку она уже впитала общую тягу к популярным дистракторам.
    p = 1.0
    for q, a in zip(scored, vec):
        c = Counter(r["ans"][q] for r in g)
        p *= c[a] / sum(c.values())
    pairs = math.comb(len(g), 2) * p
    print(f"  группа: n = {len(target)}, балл {target[0]['score']}, форма {lang}")
    print(f"  P(один респондент воспроизводит этот вектор) = {p:.3e}")
    print(f"  ожидаемое число совпадающих ПАР среди {len(g)} записей = {pairs:.2e}")
    return lang, g, scored


# --- B3: перестановочный тест на максимальную кратность --------------------
def block_b3(ctx):
    print("\n=== B3. Перестановочный тест: максимальная кратность вектора ===")
    if ctx is None:
        print("  блок неприменим")
        return
    lang, g, scored = ctx
    dists = []
    for q in scored:
        c = Counter(r["ans"][q] for r in g)
        tot = sum(c.values())
        dists.append((list(c), [v / tot for v in c.values()]))
    rnd = random.Random(SEED)
    mult = Counter()
    for _ in range(N_PERM):
        cnt = Counter(
            tuple(rnd.choices(o, weights=w)[0] for o, w in dists)
            for _ in range(len(g)))
        mult[max(cnt.values())] += 1
    print(f"  {N_PERM} симуляций по {len(g)} независимых записей формы {lang}")
    print(f"  максимальная кратность идентичного вектора: "
          f"{dict(sorted(mult.items()))}")
    print("  Наблюдалось 4 при неверных ответах на 20 пунктах из 26.")


def main():
    key = read_key(f"{ROOT}/input/answer_key.csv")
    rows = read_rows(f"{ROOT}/input/google_forms_responses.xlsx")
    recs, opts, keyopt, scored = build(rows, key)

    block_a0(recs, scored)
    block_a1(opts, keyopt, scored)
    block_a2(recs, opts, keyopt, scored)
    block_a3(recs, opts, keyopt, scored)
    block_a4(recs, opts, scored)
    dups = block_b1(recs, scored)
    ctx = block_b2(recs, scored, dups)
    block_b3(ctx)


if __name__ == "__main__":
    main()
