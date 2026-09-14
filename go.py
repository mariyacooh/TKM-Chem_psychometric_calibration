#!/usr/bin/env python3
# =============================================================================
#  go.py — тонкий оркестратор пайплайна, единственная точка входа.
# =============================================================================
#  Логики пайплайна здесь нет: что чистить, какие пакеты грузить и какие скрипты
#  запускать решают сами R-скрипты (scripts/run_all.R, scripts/run_plots.R,
#  scripts/render_report_html.R, scripts/write_report_text.R). Своё здесь только
#  внешнее по отношению к R: UTF-8-локаль на POSIX, разведение потоков шага по
#  двум логам, удаление артефактов на хосте и вызов шага внутри уже запущенного
#  контейнера.
#
#  Каждая цель = последовательность вызовов Rscript (R_TARGETS). Шага установки
#  зависимостей здесь нет: на хосте это Rscript scripts/install_deps.R, в
#  контейнере пакеты запечены в образ (docker/Dockerfile).
#
#  Жизненный цикл контейнера — не задача раннера. Сборка, подъём и остановка
#  живут в container.py; здесь контейнер только проверяется на «запущен» и
#  используется по имени, поэтому про docker/compose.yaml этот файл не знает.
#
#  Зависимостей нет вообще: только стандартная библиотека, причём разбор
#  аргументов написан вручную — наличия каких-либо пакетов на хосте не
#  предполагается.
# =============================================================================

import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

# Корень проекта — каталог этого файла, поэтому запуск возможен из любого места.
ROOT = Path(__file__).resolve().parent

OUT_DIR = ROOT / "output"

# Вывод пайплайна идёт в файлы, а не на консоль: stdout -> last-out.log, stderr ->
# last-err.log (корень репозитория, покрыты `*.log` в .gitignore). Потоки разводятся
# файловыми дескрипторами дочернего процесса, поэтому остаются раздельными и на
# хосте, и в контейнере, а объём вывода перестаёт быть ограничением: пакет,
# печатающий прогресс или трассу итераций, пишет в файл, и глушить его на месте
# вызова не требуется. Логи усекаются один раз за запуск go.py, шаги дописывают.
OUT_LOG = ROOT / "last-out.log"
ERR_LOG = ROOT / "last-err.log"

# Рабочая директория внутри контейнера: репозиторий смонтирован туда томом
# (docker/compose.yaml), поэтому output/ и логи ложатся на хост.
CONTAINER_WORKDIR = "/proj"

# Единственная таблица соответствия цель -> R-скрипты. Порядок значим.
R_TARGETS = {
    "all": ["scripts/run_all.R", "scripts/render_report_html.R",
            "scripts/write_report_text.R"],
    "plots": ["scripts/run_plots.R", "scripts/render_report_html.R"],
    "text": ["scripts/write_report_text.R"],
}

# Цель без R: хостовая уборка, доступная на машине, где R может не быть вообще.
CLEAN_TARGET = "clean"

TARGETS = list(R_TARGETS) + [CLEAN_TARGET]

UTF8_RE = re.compile(r"utf-?8", re.IGNORECASE)


class PipelineError(Exception):
    """Падение шага или неготовность окружения.

    Хвост last-err.log печатается ТОЛЬКО при show_tail, то есть когда шаг успел
    запуститься и что-то туда написать. У проверок до первого шага (нет Rscript,
    нет контейнера) лог остался от прошлого прогона, и его хвост описывал бы чужое
    падение — печатать его хуже, чем не печатать ничего.
    """

    def __init__(self, message, show_tail=False):
        Exception.__init__(self, message)
        self.show_tail = show_tail


def print_help():
    print("""
go.py - thin orchestrator for the R pipeline (single entry point)

Usage:
  python go.py <target> [-docker CONTAINER]

Targets:
  all    - Full clean build: run_all.R wipes output/ and runs steps 0-10 + all
           plots in ONE R session, then render_report_html.R ->
           output/report.html and write_report_text.R -> output/report.{md,json}.
  plots  - Rebuild plots + HTML report ONLY, from the artifacts already in
           output/. Nothing is recomputed and output/ is not wiped: for edits to
           colors, palettes, labels, sizes in scripts/plots/. Requires a previous
           'all' (fails fast if output/cleaned_responses.csv is absent). Changed
           math in scripts/[0-9]*_*.R needs 'all'. The text report is not
           rebuilt: presentation-only edits change no numbers.
  text   - Rebuild output/report.{md,json} ONLY, from the artifacts already in
           output/: the same numbers as report.html without the embedded figures
           or per-respondent rows, for cross-checking a manuscript.
  clean  - Remove output/ (incl. report.html and report.{md,json}), render
           leftovers in scripts/ and both run logs. Host-side file removal;
           needs no R and rejects -docker.

Options:
  -docker CONTAINER  - Run the R steps inside an ALREADY RUNNING container of
                       that name (docker exec). The container must exist and be
                       running; this runner never builds, starts or stops it -
                       use container.py for that. Without the option the steps
                       run against host-native R.
  -h, --help         - Show this message (also shown when no target is given)

Run logs (all, plots, text):
  last-out.log  - stdout of the R steps
  last-err.log  - stderr of the R steps (step boundaries, timings, '!!' lines)
  Both are truncated at the start of a run and appended to by each step.
  The console keeps only the '--> <command>' line per step.

Container lifecycle (separate script):
  python container.py up     # build the image and start the container
  python container.py down   # stop and remove it

R packages (host-native R only, once):  Rscript scripts/install_deps.R
""".strip())


def usage_error(message):
    sys.stderr.write("go.py: %s\n\n" % message)
    sys.stderr.write("Run 'python go.py --help' for usage.\n")
    sys.exit(2)


def parse_args(argv):
    """Разбор командной строки вручную. Возврат (target, container).

    Приняты оба написания опции — `-docker NAME` и `--docker NAME`, а также форма
    с `=`. Цель без аргументов не подставляется: пустая командная строка это
    запрос справки, а не прогон.
    """
    target = None
    container = None
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg in ("-h", "--help"):
            print_help()
            sys.exit(0)
        elif arg in ("-docker", "--docker"):
            i += 1
            if i >= len(argv):
                usage_error("option %s requires a container name" % arg)
            container = argv[i]
        elif arg.startswith("-docker=") or arg.startswith("--docker="):
            container = arg.split("=", 1)[1]
            if not container:
                usage_error("option %s requires a container name" % arg.split("=", 1)[0])
        elif arg.startswith("-"):
            usage_error("unknown option: %s" % arg)
        elif target is None:
            target = arg
        else:
            usage_error("unexpected argument: %s" % arg)
        i += 1
    return target, container


def child_env():
    """Окружение шага. На POSIX-хостах — UTF-8-локаль, иначе R не разберёт
    кириллицу (RU/KZ) в исходниках и данных: под локалью C grepl() по
    кириллическим шаблонам падает («regular expression is invalid» / «unable to
    translate ... to a wide string»). В контейнере локаль задаёт ENV в
    docker/Dockerfile: docker exec хостовые LC_ALL/LANG не пробрасывает.
    """
    env = os.environ.copy()
    if os.name != "nt":
        if not UTF8_RE.search(env.get("LC_ALL", "")) and not UTF8_RE.search(env.get("LANG", "")):
            env["LC_ALL"] = "C.UTF-8"
            env["LANG"] = "C.UTF-8"
    return env


def reset_logs():
    for path in (OUT_LOG, ERR_LOG):
        try:
            path.unlink()
        except FileNotFoundError:
            pass


def tail(path, lines=20):
    """Последние строки файла. Читается только хвост: last-out.log вырастает до
    мегабайтов, и целиком он здесь не нужен.
    """
    if not path.exists():
        return []
    with open(path, "rb") as fh:
        fh.seek(0, os.SEEK_END)
        size = fh.tell()
        fh.seek(max(0, size - 65536))
        chunk = fh.read()
    return chunk.decode("utf-8", "replace").splitlines()[-lines:]


def require_docker():
    docker = shutil.which("docker")
    if docker is None:
        raise PipelineError("docker not found in PATH.")
    return docker


def require_running_container(name):
    """Контейнер обязан существовать и быть запущенным ДО первого шага.

    Проверка одна и ранняя: без неё несуществующее имя дало бы одинаковое падение
    docker exec на каждом шаге вместо одного внятного сообщения. Поднять контейнер
    отсюда нельзя намеренно — это ответственность container.py.
    """
    docker = require_docker()
    probe = subprocess.run(
        [docker, "inspect", "-f", "{{.State.Running}}", name],
        cwd=str(ROOT), stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if probe.returncode != 0:
        # Причин отказа две — нет такого объекта и нет самого демона, — и они
        # требуют разных действий, поэтому в сообщение попадает ответ docker.
        said = probe.stderr.strip().splitlines()
        detail = ("\n  docker: %s" % said[-1]) if said else ""
        raise PipelineError(
            "Container '%s' is not available.%s\n  Start it first: python container.py up"
            % (name, detail))
    if probe.stdout.strip() != "true":
        raise PipelineError(
            "Container '%s' exists but is not running.\n  Start it: python container.py up"
            % name)


def require_rscript():
    """Проверка «хостовый R вообще есть» ДО первого шага: иначе прогон падал бы
    сырым FileNotFoundError вместо указания, что ставить и чем заменить.
    """
    if shutil.which("Rscript") is None:
        raise PipelineError(
            "Rscript not found in PATH. Install R (see README.md)"
            "\n  or run the steps in a container: python go.py <target> -docker <name>")


def step_argv(script, container):
    """Команда одного шага списком argv: оболочка не запускается, поэтому
    экранировать нечего и перенаправление ставится дескрипторами, а не строкой.
    """
    if container is None:
        return ["Rscript", script]
    # -i без -t обязателен: с выделенным TTY docker слил бы stdout и stderr в один
    # поток, и разделить их по двум логам уже не вышло бы. Рабочая директория
    # задаётся явно, а не наследуется из образа.
    return ["docker", "exec", "-i", "-w", CONTAINER_WORKDIR, container, "Rscript", script]


def run_step(argv, env):
    print("--> %s" % " ".join(argv), flush=True)
    with open(OUT_LOG, "ab") as out_fh, open(ERR_LOG, "ab") as err_fh:
        code = subprocess.call(argv, cwd=str(ROOT), env=env, stdout=out_fh, stderr=err_fh)
    if code != 0:
        raise PipelineError(
            "Command failed with exit code %d: %s" % (code, " ".join(argv)), show_tail=True)


def run_pipeline(target, container):
    env = child_env()
    if container is None:
        require_rscript()
    else:
        require_running_container(container)
    reset_logs()
    for script in R_TARGETS[target]:
        run_step(step_argv(script, container), env)
    print("--> Output: %s (stdout), %s (stderr)." % (OUT_LOG.name, ERR_LOG.name))


def run_clean():
    """Хостовая уборка без R: сгенерированный output/ и остатки рендера в scripts/.

    Тот же набор остатков чистит scripts/render_report_html.R перед каждым
    рендером — там это часть корректности (устаревший кэш чанка), здесь просто
    уборка на машине, где R может не быть вообще.
    """
    print("--> Cleaning generated output and render leftovers...")
    if OUT_DIR.exists():
        shutil.rmtree(OUT_DIR)
        print("Removed %s directory." % OUT_DIR.name)
    scripts_dir = ROOT / "scripts"
    if scripts_dir.is_dir():
        patterns = ("report_files", "report_cache", "report*.html", "report.knit.md")
        for pattern in patterns:
            for path in scripts_dir.glob(pattern):
                if path.is_dir():
                    shutil.rmtree(path)
                else:
                    path.unlink()
        print("Removed render leftovers from scripts/ (%s)." % ", ".join(patterns))
    if OUT_LOG.exists() or ERR_LOG.exists():
        reset_logs()
        print("Removed %s and %s." % (OUT_LOG.name, ERR_LOG.name))
    print("Clean finished.")


def main(argv):
    target, container = parse_args(argv)
    if target is None:
        print_help()
        return 0
    if target not in TARGETS:
        usage_error("unknown target '%s' (expected one of: %s)" % (target, ", ".join(TARGETS)))
    if target == CLEAN_TARGET and container is not None:
        usage_error("'clean' is host-side file removal; -docker does not apply to it")

    try:
        if target == CLEAN_TARGET:
            run_clean()
        else:
            run_pipeline(target, container)
    except PipelineError as exc:
        print("Pipeline execution failed: %s" % exc)
        # Диагностика упавшего шага лежит в файле, поэтому хвост stderr печатается
        # здесь: иначе прогон падал бы без единой строки о причине.
        if exc.show_tail:
            lines = tail(ERR_LOG)
            if lines:
                print("--- tail %s ---" % ERR_LOG.name)
                for line in lines:
                    print(line)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
