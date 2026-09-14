# syntax=docker/dockerfile:1
# Pinned R via the rocker tag (matches .R-version). Bakes the pinned R package
# set into the image so `docker compose up` starts ready. Declarative and
# reproducible: rebuilt from this file whenever deps change -- no hand-configured
# container or snapshot to maintain.
FROM rocker/tidyverse:4.6.0

# Locale -- match the host path exactly. The pipeline requires a UTF-8 locale to
# parse the Cyrillic (RU/KZ) sources, and go.py sets C.UTF-8 on the
# host. The rocker base defaults to en_US.UTF-8 and `docker exec` -- how go.py
# invokes a step -- does NOT forward the host's LC_ALL/LANG, so without this line
# the container would run a DIFFERENT (though still UTF-8) locale: string
# collation would diverge from the host and could reorder Cyrillic-keyed output
# (e.g. the distractor option letters that freq_order() hands out in
# 9_distractor.R). C.UTF-8 is built into glibc, so no locale-gen needed.
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# System libs -- the rocker/tidyverse base already ships what the host setup
# (README.md) lists: curl/ssl/xml2 for the tidyverse stack, fontconfig/harfbuzz/
# freetype for text rendering, and pandoc for the report render. Only glpk
# (linked by lavaan and other psychometric packages) is missing, so it is the
# single addition here -- not an incomplete copy of the host list.
RUN apt-get update -qq \
 && apt-get install -y -qq --no-install-recommends libglpk40 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /proj

# Install the pinned package set from binaries (PPM) via the repo's single source
# of truth. Only the files the install needs are copied, so editing analysis
# scripts does not bust this layer's build cache: install_deps.R sources _setup.R,
# which sources _artifacts.R and reads .R-version -- every module and data file
# _setup.R pulls in must be listed here or the build fails. At run time /proj is
# bind-mounted over this, but the installed packages live in the image's R
# library, not /proj.
#
# Copying .R-version is what makes the build itself verify the pin: _setup.R stops
# when getRversion() differs from it, so a FROM tag that drifts away from the pin
# fails the build here instead of yielding an image whose R silently disagrees
# with the repo.
COPY .R-version ./
COPY scripts/_setup.R scripts/_artifacts.R scripts/install_deps.R scripts/
RUN Rscript scripts/install_deps.R

# Отпечаток входов сборки (этот файл плюс четыре скопированных выше), проставляемый
# container.py: по нему `container.py up` отличает актуальный образ от
# устаревшего и вызывает сборку только при расхождении. ARG объявлен ПОСЛЕ
# установки пакетов, поэтому её слой не инвалидируется -- метка ложится
# отдельным завершающим слоем метаданных. Значение по умолчанию `unknown`
# оставляет отметку заведомо несовпадающей, если образ собран в обход
# container.py: следующий `up` пересоберёт (по кэшу, за секунды) и проставит
# настоящий отпечаток.
ARG DEPS_FINGERPRINT=unknown
LABEL main253.deps=$DEPS_FINGERPRINT
