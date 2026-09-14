#!/usr/bin/env python3
# =============================================================================
#  subscale_assignment.py — проверка приписки пункт -> субшкала
# -----------------------------------------------------------------------------
#  ВХОД : input/google_forms_responses.xlsx  (сырые ответы)
#         input/answer_key.csv               (ключ верных ответов kz/ru)
#         input/items.csv                    (объявленная приписка)
#  ВЫХОД: печать в stdout; файлы не пишутся, output/ не изменяется
#
#  Скрипт ВНЕ пайплайна: scripts/run_all.R его не вызывает, output/ не читается.
#  Воспроизводит числа из analysis/subscale_assignment.md — там же вопрос,
#  трактовка и ограничения. На Python (не R) по требованию задачи; оценивание
#  ответов перенесено из scripts/0_preprocess.R и сверено по N и составу выборки.
#
#  Запуск из корня проекта (полный прогон пайплайна не нужен):
#      python3 analysis/subscale_assignment.py
#  Зависимости: numpy, scipy, openpyxl. Время выполнения — около минуты.
#
#  Батарея из двух уровней, отвечающих на РАЗНЫЕ вопросы.
#    Уровень A — идентичность пункта: столбец, названный Qnn, действительно несёт
#      вопрос Qnn и оценивается его ключом. Детерминирован, даёт доказательство.
#    Уровень B — ярлык субшкалы: ведёт ли пункт себя эмпирически как член
#      объявленной субшкалы. Статистический, доказательства дать не может:
#      фальсифицирует приписку, но не подтверждает её, а при эмпирически
#      неразличимых субшкалах не делает и этого (проверка мощности B0).
#
#  Чего не проверяет НИ ОДИН тест здесь: сплошное переименование субшкал
#  (S1<->S3 целиком). Статистика внутренней структуры зависит от РАЗБИЕНИЯ и не
#  зависит от имён групп, поэтому переименование ей невидимо по построению. Оно
#  решается только сверкой с содержательной спецификацией теста (тест B5).
# =============================================================================

import csv
import re
import sys
from itertools import permutations

import numpy as np
from openpyxl import load_workbook
from scipy.optimize import brentq
from scipy.stats import multivariate_normal, norm

ROOT = "."

# Позиционный маппинг столбцов xlsx (0-based), дублирует scripts/0_preprocess.R:
# item -> (kz_col, ru_col). Не принимается на веру — проверяется тестами A2/A3.
ITEM_TO_COLS = {
    1: (8, 41), 2: (9, 42), 3: (10, 43), 4: (11, 44), 5: (12, 45), 6: (13, 46),
    7: (14, 47), 8: (15, 48), 9: (16, 49), 10: (17, 50), 11: (18, 51),
    12: (19, 52), 13: (20, 53), 14: (21, 54), 15: (22, 55), 16: (23, 56),
    18: (24, 57), 19: (25, 58), 20: (26, 59), 21: (27, 60), 22: (28, 61),
    23: (29, 62), 24: (30, 63), 26: (31, 64), 27: (32, 65), 28: (33, 66),
    29: (34, 67),
}
ITEM_KEYS = [f"Q{n:02d}" for n in ITEM_TO_COLS]
LANG_COL = 1
QUOTE_CHARS = ["«", "»", "\", """, "'", "‹", "›"]
TRUE_TOKENS = ("true", "t", "1", "yes", "x")
EMPTY_TOKENS = ("", "nan", "none", "-")

RNG = np.random.default_rng(42)
N_BOOT = 2000
N_PERM = 20000
# Порог различимости субшкал: дезаттенюированная корреляция выше него означает,
# что субшкалы измеряют одно и то же с точностью до надёжности, и тесты B1-B4
# теряют мощность.
R_DISATT_MAX = 0.90

RESULTS = []


def report(tag, ok, title, *lines):
    RESULTS.append((tag, ok))
    print(f"\n[{ {True: 'PASS', False: 'FAIL', None: 'INFO'}[ok] }] {tag} — {title}")
    for ln in lines:
        if ln:
            print(f"       {ln}")


# ─────────────────────────────────────────────────────────────────────────────
#  Чтение входов и оценивание (правила scripts/0_preprocess.R)
# ─────────────────────────────────────────────────────────────────────────────


def read_bom_csv(path):
    with open(path, encoding="utf-8-sig") as fh:
        return list(csv.DictReader(fh))


def normalize(text):
    t = "" if text is None else str(text)
    t = t.strip().lower()
    for ch in QUOTE_CHARS:
        t = t.replace(ch, "")
    t = re.sub(r"[.,;:!?…]+$", "", t)
    t = re.sub(r"^[a-dа-г]\.\s*", "", t)
    t = re.sub(r"\s+", " ", t).strip()
    t = re.sub(r"\s+([:;,.!?…])", r"\1", t)
    return t.replace("ё", "е")


def is_correct(response, correct):
    r, cc = normalize(response), normalize(correct)
    if not r:
        return 0
    if r == cc or cc in r:
        return 1
    if len(r) >= 15 and r in cc:
        return 1
    return 0


def is_empty(val):
    return val is None or str(val).strip().lower() in EMPTY_TOKENS


def detect_lang(val):
    s = str(val).strip().lower()
    for m in ("қазақ", "казах", "kz", "каз", "kazakh", "қаз"):
        if m in s:
            return "kz"
    for m in ("русск", "рус", "ru", "russian", "орыс"):
        if m in s:
            return "ru"
    return None


def load_raw():
    wb = load_workbook(f"{ROOT}/input/google_forms_responses.xlsx", read_only=True, data_only=True)
    rows = list(wb.active.values)
    header = ["" if v is None else str(v) for v in rows[0]]
    body = [["" if v is None else str(v) for v in r] for r in rows[1:]]
    return header, body


def score_matrix(body, answer_key):
    """Бинарная матрица respondents x ITEM_KEYS и язык формы каждого респондента."""
    scores, langs = [], []
    for vals in body:
        get = lambda c: vals[c].strip() if c < len(vals) else ""
        kz_filled = sum(not is_empty(get(c[0])) for c in ITEM_TO_COLS.values())
        ru_filled = sum(not is_empty(get(c[1])) for c in ITEM_TO_COLS.values())
        lg = detect_lang(get(LANG_COL)) or ("kz" if kz_filled >= ru_filled else "ru")
        j = 0 if lg == "kz" else 1
        row, answered = [], 0
        for n, cols in ITEM_TO_COLS.items():
            resp = get(cols[j])
            if not is_empty(resp):
                answered += 1
            if str(resp).strip().lower() in ("", "nan", "none"):
                resp = ""
            row.append(is_correct(resp, answer_key[f"Q{n:02d}"][j]))
        if answered:
            scores.append(row)
            langs.append(lg)
    return np.array(scores, dtype=float), np.array(langs)


def load_items():
    subs, absent, excluded = {}, [], []
    for r in read_bom_csv(f"{ROOT}/input/items.csv"):
        item, sub = r["item"].strip(), r["subscale"].strip()
        if sub == "":
            absent.append(item)
        elif r["excluded"].strip().lower() in TRUE_TOKENS:
            excluded.append(item)
        else:
            subs.setdefault(sub, []).append(item)
    return subs, absent, excluded


# ─────────────────────────────────────────────────────────────────────────────
#  УРОВЕНЬ A — идентичность пункта
# ─────────────────────────────────────────────────────────────────────────────


def test_a1_structure(subs, absent, excluded):
    """Состав items.csv согласован со схемой формы: без дублей, сирот и разрывов.

    Повторяет структурную проверку scripts/0_preprocess.R. Проходит при ЛЮБОЙ
    приписке, лишь бы она была полной и однозначной, поэтому от подмены ярлыка не
    защищает — это предусловие остальных тестов, а не проверка приписки.
    """
    in_subs = [i for g in subs.values() for i in g]
    dup = sorted({i for i in in_subs if in_subs.count(i) > 1})
    scored = [k for k in ITEM_KEYS if k not in excluded]
    no_sub = sorted(set(scored) - set(in_subs))
    no_col = sorted(set(in_subs) - set(ITEM_KEYS))
    ghost = sorted(set(absent) & set(ITEM_KEYS))
    ok = not (dup or no_sub or no_col or ghost)
    report("A1", ok, "структура items.csv против схемы формы",
           f"пунктов в субшкалах: {len(in_subs)}; оценивается: {len(scored)}",
           f"дубли: {dup or '-'}; без субшкалы: {no_sub or '-'}; "
           f"ссылка на непредъявленный: {no_col or '-'}; absent со столбцом: {ghost or '-'}")
    return ok


ANCHOR_RE = re.compile(r"[0-9]+(?:[.,][0-9]+)?|[A-Za-z]{3,}")


def anchors(text):
    """Языконезависимые якоря стима: числа и латинские токены."""
    t = text.replace("₂", "2").replace("²", "2")
    out = set()
    for m in ANCHOR_RE.findall(t):
        m = m.lower().replace(",", ".")
        out.add(str(float(m)) if re.match(r"^[0-9]+\.[0-9]+$", m) else m)
    return out


def test_a2_pair_alignment(header):
    """Столбцы KZ и RU одного пункта несут один и тот же вопрос.

    Перевод не сверяется — сверяются якоря, инвариантные к языку. Рассогласование
    пары означает, что ответы на два РАЗНЫХ вопроса сведены в столбец Qnn, после
    чего любая субшкальная приписка бессмысленна независимо от items.csv.
    """
    bad = []
    for n, (kz, ru) in ITEM_TO_COLS.items():
        a_kz, a_ru = anchors(header[kz]), anchors(header[ru])
        union = a_kz | a_ru
        jac = len(a_kz & a_ru) / len(union) if union else 1.0
        if jac < 0.5:
            bad.append(f"  Q{n:02d}: Jaccard={jac:.2f}, расхождение {sorted(a_kz ^ a_ru)[:6]}")
    report("A2", not bad, "выравнивание пар столбцов KZ/RU (якоря стима)",
           f"пар проверено: {len(ITEM_TO_COLS)}; рассогласованных: {len(bad)}", *bad)
    return not bad


def column_options(body, item_to_cols):
    """Множество наблюдённых нормализованных вариантов ответа по каждому столбцу."""
    opts = {}
    for n, cols in item_to_cols.items():
        for lang, col in zip(("kz", "ru"), cols):
            seen = {normalize(r[col]) for r in body if col < len(r) and not is_empty(r[col])}
            opts[(n, lang)] = {o for o in seen if o}
    return opts


def key_hits(opts, answer_key, item_to_cols):
    """(нет ключа в своём столбце, ключ найден в чужом) для данного маппинга."""
    own_miss, foreign = [], []
    for n in item_to_cols:
        for lang, key in zip(("kz", "ru"), answer_key[f"Q{n:02d}"]):
            k = normalize(key)
            if not any(k == o or k in o or (len(o) >= 15 and o in k) for o in opts[(n, lang)]):
                own_miss.append(f"Q{n:02d}/{lang}")
            for m in item_to_cols:
                if m != n and any(k == o or k in o for o in opts[(m, lang)]):
                    foreign.append(f"Q{n:02d}/{lang} -> столбец Q{m:02d}")
    return own_miss, foreign


def test_a3_key_in_column(body, answer_key):
    """Ключ Qnn встречается среди вариантов СВОЕГО столбца и ни одного чужого.

    Прямая проверка того, что ключ применён к нужному вопросу. Сдвиг маппинга —
    например на один пункт из-за отсутствующих Q17/Q25 — обнуляет попадание в
    своём столбце либо даёт попадание в чужом.
    """
    own_miss, foreign = key_hits(column_options(body, ITEM_TO_COLS), answer_key, ITEM_TO_COLS)
    ok = not own_miss and not foreign
    report("A3", ok, "ключ ответа принадлежит своему столбцу и только ему",
           f"ключей проверено: {2 * len(ITEM_TO_COLS)}",
           f"нет в своём столбце: {own_miss or '-'}",
           f"найден в чужом столбце: {foreign or '-'}")
    return ok


def test_a4_shift_sensitivity(body, answer_key):
    """Контроль мощности A3: на СДВИНУТОМ маппинге A3 обязан упасть.

    Прохождение A3 без этого контроля не означает ничего: тест, проходящий всегда,
    не отличает верный маппинг от неверного.
    """
    nums = list(ITEM_TO_COLS)
    shifted = {nums[i]: ITEM_TO_COLS[nums[(i + 1) % len(nums)]] for i in range(len(nums))}
    own_miss, foreign = key_hits(column_options(body, shifted), answer_key, shifted)
    caught = len(own_miss)
    ok = caught == 2 * len(nums)
    report("A4", ok, "контроль мощности A3: сдвиг маппинга на 1 пункт обязан ловиться",
           f"сдвинутых ключей поймано как отсутствующие в своём столбце: {caught} "
           f"из {2 * len(nums)} (ожидается всё)",
           f"вдобавок опознано в чужом столбце: {len(foreign)}")
    return ok


def test_a5_score_identity(X_all, subs):
    """Score_Total == сумма субшкальных баллов на всех респондентах."""
    idx = {k: i for i, k in enumerate(ITEM_KEYS)}
    included = [i for g in subs.values() for i in g]
    total = X_all[:, [idx[i] for i in included]].sum(axis=1)
    parts = sum(X_all[:, [idx[i] for i in g]].sum(axis=1) for g in subs.values())
    ok = bool(np.all(total == parts))
    report("A5", ok, "Score_Total == Score_S1 + Score_S2 + Score_S3",
           f"респондентов: {X_all.shape[0]}; расхождений: {int(np.sum(total != parts))}")
    return ok


# ─────────────────────────────────────────────────────────────────────────────
#  УРОВЕНЬ B — ярлык субшкалы
# ─────────────────────────────────────────────────────────────────────────────
#  Все корреляции пункт-субшкала выводятся из ковариационной матрицы пунктов:
#  corr(i, sum_G) = sum_{j in G} C_ij / (sd_i * sqrt(sum_{j,k in G} C_jk)). Это
#  точный Пирсон, но бутстрап и перестановки требуют одной матрицы на выборку, а
#  не пересчёта по парам.


def cov_of(X):
    return np.cov(X, rowvar=False)


def r_item_group(C, i, g):
    """Корреляция пункта i с суммарным баллом группы g (индексы столбцов)."""
    if not g:
        return np.nan
    den = np.sqrt(C[i, i]) * np.sqrt(C[np.ix_(g, g)].sum())
    return C[i, g].sum() / den if den > 0 else np.nan


def item_subscale_matrix(C, groups, names, k):
    """Матрица k x len(names): своя субшкала — с поправкой на перекрытие."""
    M = np.full((k, len(names)), np.nan)
    for i in range(k):
        for j, s in enumerate(names):
            M[i, j] = r_item_group(C, i, [m for m in groups[s] if m != i])
    return M


def partition_stat(C, assign, names, k):
    """Критерий разбиения: средняя по пунктам разность (своя субшкала - чужие)."""
    groups = {s: [i for i in range(k) if assign[i] == s] for s in names}
    tot = 0.0
    for i in range(k):
        own = assign[i]
        r_own = r_item_group(C, i, [m for m in groups[own] if m != i])
        r_oth = [r_item_group(C, i, groups[s]) for s in names if s != own]
        tot += r_own - float(np.mean(r_oth))
    return tot / k


def tetrachoric(x, y):
    """Тетрахорическая корреляция пары бинарных пунктов (MLE по таблице 2x2)."""
    p1, p2 = float(np.mean(x)), float(np.mean(y))
    if min(p1, p2) in (0.0, 1.0):
        return np.nan
    h, k = norm.ppf(1 - p1), norm.ppf(1 - p2)
    target = float(np.mean((x == 1) & (y == 1)))

    def f(r):
        return multivariate_normal(mean=[0, 0], cov=[[1, r], [r, 1]]).cdf([-h, -k]) - target

    try:
        return float(brentq(f, -0.999, 0.999, xtol=1e-4))
    except ValueError:
        return float(np.sign(f(0.0)) * 0.999)


def alpha(X):
    k = X.shape[1]
    vt = X.sum(axis=1).var(ddof=1)
    return k / (k - 1) * (1 - X.var(axis=0, ddof=1).sum() / vt) if vt > 0 else np.nan


def meng_z(r1, r2, r12, n):
    """z-тест Мэна-Розенталя-Рубина: сравнение двух зависимых корреляций.

    r1, r2 — корреляции пункта с двумя субшкальными баллами; r12 — корреляция
    самих баллов между собой. Общий пункт делает корреляции зависимыми, поэтому
    независимое сравнение по SE каждой из них завышало бы разброс разности.
    """
    r1 = np.clip(r1, -0.999, 0.999)
    r2 = np.clip(r2, -0.999, 0.999)
    r12 = np.clip(r12, -0.999, 0.999)
    rbar2 = (r1 ** 2 + r2 ** 2) / 2
    f = min((1 - r12) / (2 * (1 - rbar2)), 1.0)
    h = (1 - f * rbar2) / (1 - rbar2)
    return (np.arctanh(r1) - np.arctanh(r2)) * np.sqrt((n - 3) / (2 * (1 - r12) * h))


def bh(pvals):
    """Поправка Бенджамини-Хохберга; возвращает q-значения в исходном порядке."""
    p = np.asarray(pvals, dtype=float)
    m = len(p)
    order = np.argsort(p)
    q = np.empty(m)
    prev = 1.0
    for rank, i in enumerate(order[::-1]):
        prev = min(prev, p[i] * m / (m - rank))
        q[i] = prev
    return q


def test_b0_power(X, groups, names, tag):
    """Предусловие мощности: различимы ли субшкалы эмпирически вообще.

    Если субшкальные баллы с точностью до надёжности измеряют одно и то же,
    тесты B1-B4 не могут ни подтвердить приписку, ни опровергнуть её, и их итог
    читается как «нет данных», а не как «верно».
    """
    n, k = X.shape
    a = {s: alpha(X[:, groups[s]]) for s in names}
    lines = [f"alpha (все {k} пунктов): {alpha(X):.3f}"]
    lines += [f"alpha({s}) = {a[s]:.3f}  (пунктов: {len(groups[s])})" for s in names]
    dis = []
    for i in range(len(names)):
        for j in range(i + 1, len(names)):
            s1, s2 = names[i], names[j]
            r = float(np.corrcoef(X[:, groups[s1]].sum(axis=1), X[:, groups[s2]].sum(axis=1))[0, 1])
            rd = r / np.sqrt(a[s1] * a[s2])
            dis.append(rd)
            lines.append(f"r({s1},{s2}) = {r:.3f}   дезаттенюированная: {rd:.3f}")
    T = np.array([[1.0 if i == j else tetrachoric(X[:, i], X[:, j]) for j in range(k)]
                  for i in range(k)])
    ev = np.linalg.eigvalsh(np.nan_to_num(T, nan=0.0))[::-1]
    lines.append("собственные значения тетрахорической матрицы (1-4): "
                 + ", ".join(f"{e:.2f}" for e in ev[:4]))
    lines.append(f"доля дисперсии 1-го фактора: {ev[0] / k:.3f}; ev1/ev2 = {ev[0] / ev[1]:.2f}")
    powered = max(dis) < R_DISATT_MAX
    lines.append(f"максимум дезаттенюированной корреляции: {max(dis):.3f} "
                 f"(порог различимости {R_DISATT_MAX})")
    lines.append(f"=> мощность уровня B: {'есть' if powered else 'НЕТ, субшкалы эмпирически неразличимы'}")
    report(tag, None, "предусловие: различимость субшкал", *lines)
    return powered


def test_b1_discrimination(X, M, names, cols, assign, powered, tag):
    """Пункт коррелирует со СВОЕЙ субшкалой сильнее, чем с любой чужой.

    Простое сравнение величин ловит только знак разности, поэтому для каждого
    пункта считается z-тест зависимых корреляций против САМОЙ СИЛЬНОЙ чужой
    субшкалы, односторонний (значим только перевес чужой), с поправкой BH по всем
    пунктам. Значимый перевес = свидетельство ошибочного ярлыка; незначимый
    перевес = шум, различить приписку на этих данных нельзя.
    """
    n, k = X.shape
    groups = {s: [i for i in range(k) if assign[i] == s] for s in names}
    rows, pvals = [], []
    for i in range(k):
        own = names.index(assign[i])
        other = max((j for j in range(len(names)) if j != own), key=lambda j: M[i, j])
        g_own = [m for m in groups[names[own]] if m != i]
        r12 = float(np.corrcoef(X[:, g_own].sum(axis=1), X[:, groups[names[other]]].sum(axis=1))[0, 1])
        z = meng_z(M[i, own], M[i, other], r12, n)
        p = float(norm.cdf(z))          # односторонний: чужая субшкала сильнее своей
        rows.append((cols[i], names[own], names[other], M[i, own] - M[i, other], z))
        pvals.append(p)
    q = bh(pvals)
    flipped = [(r, qq) for r, qq in zip(rows, q) if r[3] < 0]
    sig = [(r, qq) for r, qq in zip(rows, q) if r[3] < 0 and qq < 0.05]
    ok = not sig
    report(tag, ok, "дискриминация пункт-субшкала (z-тест зависимых корреляций, BH)",
           f"argmax совпал с объявленной субшкалой: {k - len(flipped)}/{k}",
           f"перевес чужой субшкалы значим после BH: {len(sig)}",
           *[f"  {r[0]}: объявлен {r[1]}, сильнее с {r[2]} (dr={r[3]:+.3f}, z={r[4]:+.2f}, q={qq:.3f})"
             + ("  ЗНАЧИМО" if qq < 0.05 else "") for r, qq in flipped],
           "" if powered else "мощности нет (B0): незначимость здесь не подтверждает приписку")
    return ok


def test_b2_bootstrap(X, groups, names, cols, assign, tag):
    """Устойчивость приписки: доля бутстрап-выборок, где своя субшкала — argmax.

    Не тест, а мера неопределённости: показывает, какие пункты держатся на
    объявленной субшкале случайно. Порога «прошло/не прошло» здесь нет —
    вопрос о ярлыке решается B1 и B5.
    """
    n, k = X.shape
    wins = np.zeros(k)
    for _ in range(N_BOOT):
        Cb = cov_of(X[RNG.integers(0, n, n)])
        Mb = item_subscale_matrix(Cb, groups, names, k)
        for i in range(k):
            if names[int(np.nanargmax(Mb[i]))] == assign[i]:
                wins[i] += 1
    frac = wins / N_BOOT
    weak = [i for i in np.argsort(frac) if frac[i] < 0.90]
    report(tag, None, f"устойчивость приписки (бутстрап, B={N_BOOT})",
           f"доля >= 0.90: {int(np.sum(frac >= 0.90))}/{k}; медиана: {np.median(frac):.3f}; "
           f"минимум: {frac.min():.3f}",
           *[f"  {cols[i]} ({assign[i]}): своя субшкала выигрывает в {frac[i]:.3f} выборок"
             for i in weak])
    return frac


def test_b3_permutation(C, names, assign, k, sizes, tag):
    """Объявленное разбиение против случайных разбиений тех же размеров.

    Проверяет не ярлыки, а сам факт: группировка пунктов несёт структуру, которой
    у случайной группировки нет. Провал означал бы, что субшкалы не соответствуют
    данным вообще; прохождение совместимо и с частично перепутанными ярлыками.
    """
    obs = partition_stat(C, assign, names, k)
    labels = np.concatenate([[s] * sizes[s] for s in names])
    null = np.array([partition_stat(C, RNG.permutation(labels), names, k) for _ in range(N_PERM)])
    p = float((np.sum(null >= obs) + 1) / (N_PERM + 1))
    ok = p < 0.05
    report(tag, ok, f"объявленное разбиение против случайных ({N_PERM} перестановок)",
           f"статистика: {obs:.4f}; медиана нулевого распределения: {np.median(null):.4f}; "
           f"перцентиль: {100 * np.mean(null < obs):.1f}",
           f"p = {p:.4f}")
    return ok


def test_b4_clerical(X, C, names, cols, assign, k, tag):
    """Объявленное разбиение против правдоподобных КАНЦЕЛЯРСКИХ ошибок.

    Полный перебор разбиений избыточен: реально возможны сдвиг границы блока
    (отсутствие Q17/Q25 делает сдвиг незаметным), обмен соседних пунктов на
    границе и перенос одного пункта в чужую субшкалу. Сплошное переименование
    субшкал в семейство не входит — статистика разбиения к именам групп
    инвариантна, и такая ошибка здесь неразличима по построению.

    Превосходство альтернативы само по себе ничего не значит при 26 пунктах:
    у каждой альтернативы разность с объявленным разбиением бутстрапится, и
    учитывается только устойчивое превосходство (>= 95 % выборок).
    """
    obs = partition_stat(C, assign, names, k)
    bounds = [i for i in range(1, k) if assign[i] != assign[i - 1]]
    alts = []
    for bi, b in enumerate(bounds):
        for d in (-2, -1, 1, 2):
            nb = sorted(set(bounds[:bi] + [b + d] + bounds[bi + 1:]))
            if len(nb) != len(bounds) or not (0 < nb[0] < nb[-1] < k):
                continue
            a, lab = [], 0
            for i in range(k):
                if lab < len(nb) and i >= nb[lab]:
                    lab += 1
                a.append(names[lab])
            alts.append((f"граница {bi + 1} сдвинута на {d:+d}", np.array(a)))
    for i in bounds:
        a = np.array(assign, dtype=object)
        a[i - 1], a[i] = assign[i], assign[i - 1]
        alts.append((f"обмен {cols[i - 1]}<->{cols[i]}", a))
    for i in range(k):
        for s in names:
            if s != assign[i]:
                a = np.array(assign, dtype=object)
                a[i] = s
                alts.append((f"{cols[i]} -> {s}", a))

    scored = sorted(((partition_stat(C, a, names, k), lbl, a) for lbl, a in alts),
                    key=lambda t: -t[0])
    better = [t for t in scored if t[0] > obs]

    n = X.shape[0]
    stable = []
    if better:
        idx_boot = [RNG.integers(0, n, n) for _ in range(500)]
        Cs = [cov_of(X[s]) for s in idx_boot]
        base = np.array([partition_stat(Cb, assign, names, k) for Cb in Cs])
        for val, lbl, a in better:
            wins = float(np.mean(np.array([partition_stat(Cb, a, names, k) for Cb in Cs]) > base))
            stable.append((lbl, val, wins))
    hard = [s for s in stable if s[2] >= 0.95]
    ok = not hard
    report(tag, ok, f"объявленное разбиение против {len(alts)} правдоподобных ошибок",
           f"объявленное: {obs:.4f}; альтернатив выше него: {len(better)}; "
           f"устойчиво выше (>= 95 % бутстрапа): {len(hard)}",
           *[f"  {lbl}: {val:.4f}, выигрывает в {w:.2f} бутстрап-выборок"
             + ("  УСТОЙЧИВО" if w >= 0.95 else "") for lbl, val, w in stable[:12]])
    return ok


def level_b(X, names, cols, assign, sizes, suffix):
    """Батарея B0-B4 на одной выборке; suffix различает выборки в тегах тестов."""
    k = X.shape[1]
    groups = {s: [i for i in range(k) if assign[i] == s] for s in names}
    powered = test_b0_power(X, groups, names, f"B0{suffix}")

    C = cov_of(X)
    M = item_subscale_matrix(C, groups, names, k)
    print("\n       Корреляция пункта с баллом каждой субшкалы")
    print("       (со своей — с поправкой на перекрытие, пункт исключён из балла)")
    print("       item  " + "  ".join(f"{s:>7}" for s in names) + "   объявлена  argmax")
    for i, item in enumerate(cols):
        am = names[int(np.nanargmax(M[i]))]
        print(f"       {item}  " + "  ".join(f"{M[i, j]:7.3f}" for j in range(len(names)))
              + f"   {assign[i]:>9}  {am}" + ("" if am == assign[i] else "  <== расхождение"))

    test_b1_discrimination(X, M, names, cols, assign, powered, f"B1{suffix}")
    test_b2_bootstrap(X, groups, names, cols, assign, f"B2{suffix}")
    test_b3_permutation(C, names, assign, k, sizes, f"B3{suffix}")
    test_b4_clerical(X, C, names, cols, assign, k, f"B4{suffix}")
    return powered


def middle_band(X):
    """Средний горб распределения Score_Total: полоса 9-23 балла.

    Распределение бимодально (analysis/bimodality.md): пол и потолок дают почти
    константные строки ответов, которые завышают все корреляции разом, включая
    межсубшкальные, и тем самым съедают различимость субшкал. В полосе смесь не
    работает, поэтому уровень B имеет там мощность, которой нет на полной выборке.
    """
    tot = X.sum(axis=1)
    return (tot >= 9) & (tot <= 23)


def test_b5_content_audit(header, cols, assign, names):
    """Содержательная сверка: стим пункта против объявленной субшкалы.

    Единственная проверка, различающая сплошное переименование субшкал, и
    единственная, способная приписку ПОДТВЕРДИТЬ. Автоматически не решается:
    таблица печатается для сверки со спецификацией теста экспертом.
    """
    print("\n[INFO] B5 — содержательная сверка (сверяется вручную со спецификацией теста)")
    for s in names:
        print(f"\n       {s}:")
        for i, c in enumerate(cols):
            if assign[i] != s:
                continue
            col = ITEM_TO_COLS[int(c[1:])][1]
            stem = re.sub(r"\s+", " ", header[col]).strip()
            print(f"         {c}  {stem[:96]}")
    RESULTS.append(("B5", None))


# ─────────────────────────────────────────────────────────────────────────────


def main():
    header, body = load_raw()
    answer_key = {r["item"]: (r["kz"], r["ru"]) for r in read_bom_csv(f"{ROOT}/input/answer_key.csv")}
    subs, absent, excluded = load_items()

    cols = [i for g in subs.values() for i in g]
    names = list(subs)
    sizes = {s: len(g) for s, g in subs.items()}
    assign = np.array([s for s, g in subs.items() for _ in g], dtype=object)

    X_all, langs = score_matrix(body, answer_key)
    idx_all = {k: i for i, k in enumerate(ITEM_KEYS)}
    X = X_all[:, [idx_all[c] for c in cols]]
    k = X.shape[1]
    groups = {s: [i for i in range(k) if assign[i] == s] for s in names}

    print("=" * 78)
    print("  ПРОВЕРКА ПРИПИСКИ ПУНКТ -> СУБШКАЛА")
    print("=" * 78)
    print(f"  N = {X.shape[0]} (kz={int(np.sum(langs == 'kz'))}, ru={int(np.sum(langs == 'ru'))}); "
          f"пунктов в анализе: {k}")
    print(f"  субшкалы: {sizes}; исключено: {excluded}; отсутствует в форме: {absent}")

    print("\n" + "-" * 78)
    print("  УРОВЕНЬ A — идентичность пункта (детерминированные проверки)")
    print("-" * 78)
    test_a1_structure(subs, absent, excluded)
    test_a2_pair_alignment(header)
    test_a3_key_in_column(body, answer_key)
    test_a4_shift_sensitivity(body, answer_key)
    test_a5_score_identity(X_all, subs)

    print("\n" + "-" * 78)
    print(f"  УРОВЕНЬ B — ярлык субшкалы | выборка: ВСЯ, N = {X.shape[0]}")
    print("-" * 78)
    powered_all = level_b(X, names, cols, assign, sizes, "a")

    band = middle_band(X)
    print("\n" + "-" * 78)
    print(f"  УРОВЕНЬ B — ярлык субшкалы | выборка: СРЕДНИЙ ГОРБ (9-23 балла), "
          f"n = {int(band.sum())}")
    print("  вне бимодальной смеси, завышающей корреляции на полной выборке")
    print("-" * 78)
    powered_band = level_b(X[band], names, cols, assign, sizes, "b")

    test_b5_content_audit(header, cols, assign, names)

    fails = [t for t, ok in RESULTS if ok is False]
    a_ok = all(ok for t, ok in RESULTS if t.startswith("A"))
    print("\n" + "=" * 78)
    print(f"  ИТОГ: провалов {len(fails)}" + (f" ({', '.join(fails)})" if fails else ""))
    print(f"  Идентичность пункта (A1-A5): {'доказана' if a_ok else 'НАРУШЕНА'}")
    print(f"  Мощность уровня B: вся выборка — {'есть' if powered_all else 'нет'}; "
          f"средний горб — {'есть' if powered_band else 'нет'}")
    if powered_band or powered_all:
        print("  Ярлык субшкалы: проверяем на выборке с мощностью, итог по её B1/B4.")
    else:
        print("  Ярлык субшкалы: НЕ ПРОВЕРЯЕМ — субшкалы эмпирически неразличимы на")
        print("  обеих выборках. Приписка держится на содержательной спецификации (B5).")
    print("  Сплошное переименование субшкал не проверяется ничем, кроме B5.")
    print("=" * 78)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
