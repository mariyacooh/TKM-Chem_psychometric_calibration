#!/usr/bin/env python3
# =============================================================================
#  container.py — жизненный цикл контейнера для прогона пайплайна.
# =============================================================================
#  Единственное место, знающее про docker/compose.yaml. Раннер (go.py) контейнер
#  только использует по имени и требует, чтобы тот уже был запущен; собирает,
#  поднимает и гасит его этот скрипт.
#
#  Контейнер эфемерный: постоянного вручную настроенного контейнера нет, образ
#  собирается декларативно из docker/Dockerfile (закреплённый R + запечённые
#  пакеты). Отдельной цели для установки зависимостей поэтому не существует.
#
#  Сборка отделена от запуска отпечатком входов: `up` сверяет sha256 по файлам
#  DEPS_INPUTS с меткой main253.deps на уже собранном образе и зовёт сборщик
#  только при расхождении. Набор пакетов меняется редко, а контейнер поднимается
#  каждый сеанс, поэтому безусловный `--build` платил бы за первое, чтобы
#  получить второе: с холодным кэшем это полная переустановка пакетов ради
#  побайтово того же образа. Сверка отпечатка заодно ОТЛИЧАЕТ устаревший образ от
#  актуального, чего прогон сборщика по кэшу не даёт: он собирает молча в обоих
#  случаях.
#
#  Вывод compose идёт на консоль как есть: это сборка образа, а не шаг пайплайна,
#  и в last-out.log / last-err.log ему не место.
#
#  Зависимостей нет: только стандартная библиотека, разбор аргументов ручной.
# =============================================================================

import hashlib
import os
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent

COMPOSE_FILE = "docker/compose.yaml"

# Имя закреплено в docker/compose.yaml (container_name), поэтому предсказуемо и
# годится как аргумент `go.py -docker`.
CONTAINER = "main253"

# Тег образа продублирован из docker/compose.yaml (`image:`): оттуда его читает
# сборщик, здесь — проверка отпечатка. Правка тега нужна в обоих местах.
IMAGE = "main253:4.6.0"

# Метка образа, несущая отпечаток; проставляется в docker/Dockerfile.
DEPS_LABEL = "main253.deps"

# Файлы, определяющие содержимое образа: сам Dockerfile и всё, что он COPY-ит
# внутрь до установки пакетов. Список обязан совпадать со строками COPY в
# docker/Dockerfile — файл, попавший в сборку, но не сюда, менялся бы, не вызывая
# пересборки. Прочие scripts/*.R в образ не копируются (при прогоне /proj
# перекрыт томом), поэтому на отпечаток не влияют.
#
# .R-version входит в список: пин читает _setup.R, и его смена обязана вызывать
# пересборку. Без него отпечаток остаётся прежним, `up` докладывает «up to date»,
# и контейнер поднимается на СТАРОМ R при новом пине — расхождение, которое ничто
# не ловит.
DEPS_INPUTS = (
    ".R-version",
    "docker/Dockerfile",
    "scripts/_setup.R",
    "scripts/_artifacts.R",
    "scripts/install_deps.R",
)

TARGETS = ("up", "down")


def print_help():
    print("""
container.py - lifecycle of the pipeline container (build / up / down)

Usage:
  python container.py <target> [--rebuild]

Targets:
  up     - Start the container in the background, building the image first only
           when it is missing or out of date (docker compose -f docker/compose.yaml
           up -d [--build]). The image bakes the pinned R and the R packages, so
           there is no separate deps step.
  down   - Stop and remove the container (docker compose -f docker/compose.yaml down).

Options:
  --rebuild  - With 'up': build the image even when the fingerprint matches.
               Needed when the image is damaged, or to pick up a moved base (the
               rocker tag is mutable, so an unchanged fingerprint means unchanged
               build inputs, not an identical image). Package versions do not move
               with it: they come from the PKG_SNAPSHOT date.

Whether a build is needed is decided by a sha256 over the build inputs
(%s), stored as the '%s' label on %s.

Once the container is up, run the pipeline against it:
  python go.py all -docker %s

This script never runs pipeline steps, and go.py never manages the container.
""".strip() % (", ".join(DEPS_INPUTS), DEPS_LABEL, IMAGE, CONTAINER))


def usage_error(message):
    sys.stderr.write("container.py: %s\n\n" % message)
    sys.stderr.write("Run 'python container.py --help' for usage.\n")
    sys.exit(2)


def parse_args(argv):
    """Разбор командной строки вручную. Возврат (цель, флаг пересборки); цель
    None — запрос справки."""
    target = None
    rebuild = False
    for arg in argv:
        if arg in ("-h", "--help"):
            print_help()
            sys.exit(0)
        elif arg in ("-rebuild", "--rebuild"):
            rebuild = True
        elif arg.startswith("-"):
            usage_error("unknown option: %s" % arg)
        elif target is None:
            target = arg
        else:
            usage_error("unexpected argument: %s" % arg)
    return target, rebuild


def docker_bin():
    """Путь до docker либо None с сообщением на stderr."""
    docker = shutil.which("docker")
    if docker is None:
        sys.stderr.write("container.py: docker not found in PATH.\n")
    return docker


def deps_fingerprint():
    """Короткий sha256 по байтам DEPS_INPUTS. Имена файлов входят в хэш наравне с
    содержимым, поэтому перенос кода между ними отпечаток меняет."""
    digest = hashlib.sha256()
    for rel in DEPS_INPUTS:
        path = ROOT / rel
        if not path.is_file():
            sys.stderr.write("container.py: build input is missing: %s\n" % rel)
            return None
        digest.update(rel.encode("utf-8"))
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()[:12]


def image_fingerprint(docker):
    """Отпечаток из метки собранного образа либо None, если образа нет или метки
    на нём нет (образ старше этой проверки, или собран в обход container.py)."""
    result = subprocess.run(
        [docker, "image", "inspect", "-f",
         '{{index .Config.Labels "%s"}}' % DEPS_LABEL, IMAGE],
        cwd=str(ROOT), stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
        universal_newlines=True)
    if result.returncode != 0:
        return None
    # Go-шаблон на отсутствующей метке не падает, а печатает <no value>.
    value = result.stdout.strip()
    return value if value and value != "<no value>" else None


def compose(docker, args, extra_env=None):
    argv = ["docker", "compose", "-f", COMPOSE_FILE] + args
    print("--> %s" % " ".join(argv), flush=True)
    env = dict(os.environ, **extra_env) if extra_env else None
    return subprocess.call([docker] + argv[1:], cwd=str(ROOT), env=env)


def build_reason(docker, rebuild, wanted):
    """Причина собрать образ либо None, если сборка не нужна."""
    if rebuild:
        return "--rebuild requested"
    current = image_fingerprint(docker)
    if current is None:
        return "no image %s with a '%s' label" % (IMAGE, DEPS_LABEL)
    if current != wanted:
        return "build inputs changed (%s -> %s)" % (current, wanted)
    return None


def up(docker, rebuild):
    wanted = deps_fingerprint()
    if wanted is None:
        return 1
    reason = build_reason(docker, rebuild, wanted)
    if reason is None:
        print("--> Image %s is up to date (deps %s); skipping the build."
              % (IMAGE, wanted), flush=True)
        code = compose(docker, ["up", "-d"])
    else:
        print("--> Building %s: %s" % (IMAGE, reason), flush=True)
        code = compose(docker, ["up", "-d", "--build"],
                       {"MAIN253_DEPS_FINGERPRINT": wanted})
    if code == 0:
        print("--> Container '%s' is up. Run: python go.py all -docker %s"
              % (CONTAINER, CONTAINER))
    return code


def main(argv):
    target, rebuild = parse_args(argv)
    if target is None:
        print_help()
        return 0
    if target not in TARGETS:
        usage_error("unknown target '%s' (expected one of: %s)" % (target, ", ".join(TARGETS)))
    if rebuild and target != "up":
        usage_error("--rebuild applies to 'up' only")

    docker = docker_bin()
    if docker is None:
        return 1
    if target == "up":
        return up(docker, rebuild)
    return compose(docker, ["down"])


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
