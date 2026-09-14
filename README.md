# Psychometric calibration of a critical thinking test (chemistry education, N = 253)

Full data-processing and psychometric calibration pipeline for a critical thinking
test administered in Kazakh (KZ) and Russian (RU). The scope is calibration — item
parameters, dimensionality, reliability and differential item functioning. It is not
a validation study: no convergent evidence was collected and the construct behind the
general factor is not established (see the limitations below).

This document is the entry point for readers of the accompanying manuscript; the
maintenance documentation is [README.ru.md](README.ru.md) (Russian, more detailed on
packaging and tooling).

## A note on language

* **Data artifacts are English.** Every table the pipeline writes to `output/` uses
  English column names (`Item`, `Score_Total`, `Factor`, `Eigenvalue`, `Metric`, …),
  so the numbers behind the manuscript are readable without Russian.
* **Figures are English.** Titles, axes, legends and category labels in `output/plots/`
  are generated in English.
* **Code comments, console diagnostics and the narrative report are Russian.** So are
  `report.html` and `report.md`. They restate the same numbers in prose; nothing is
  computed there that is not already in the CSV artifacts.
* **Item and option texts are Kazakh and Russian** — they are the instrument itself.
  They appear in `input/` and in the distractor table.

## The instrument

Three a priori subscales: **S1 Interpretation**, **S2 Analysis**, **S3 Evaluation**.
26 items enter the analysis (S1: 8, S2: 9, S3: 9):

* `Q17` and `Q25` are absent from the Google Forms schema — never administered.
* `Q02` is excluded for extreme language DIF (ETS category C). It was still
  administered and scored: it remains a column in `cleaned_responses.csv` and takes
  part in the distractor analysis, but is out of `Score_Total` and of every
  psychometric model.

Item structure lives in one place — [input/items.csv](input/items.csv), one row per
item (`item,subscale,excluded,note`). Every preprocessing, analysis and plotting
script reads it through [scripts/config.R](scripts/config.R).

## Reproducing the run

R is pinned to the exact patch in [.R-version](.R-version) (**4.6.0**); package
versions are pinned by the dated Posit Package Manager snapshot `PKG_SNAPSHOT`
(**2026-07-28**) in [scripts/_setup.R](scripts/_setup.R). `scripts/_setup.R` aborts
the run if the interpreter does not match the pin.

```bash
Rscript scripts/install_deps.R   # once: binary packages from the pinned snapshot
python go.py all                 # wipes output/, runs steps 0-10 + all figures + reports
```

`go.py` is the only entry point; it holds no analysis logic. Other targets:
`plots` (figures and HTML report over existing artifacts), `text`
(`report.{md,json}` only), `clean`. A full run takes about two minutes.

### If R is not installed

Install the exact pinned patch, not "the latest 4.6.x" — reproducibility of a run is
tied to the patch, while PPM keys package binaries to the 4.6 minor version. On
Ubuntu 24.04 "noble":

```bash
# R, via rig
wget -qO- https://github.com/r-lib/rig/releases/download/latest/rig-linux-latest.tar.gz \
  | sudo tar xz -C /usr/local
rig add "$(cat .R-version)" && rig default "$(cat .R-version)"
# apt alternative when github.com is unreachable — PIN THE EXACT VERSION, since an
# unpinned r-base-core floats to the newer 4.6.x available in noble-cran40:
#   sudo apt-get install -y --no-install-recommends \
#     r-base-core=4.6.0-4.2404.0 r-base-dev=4.6.0-4.2404.0

# pandoc (report rendering) and the system libraries the R stack links against
sudo apt-get install -y --no-install-recommends pandoc \
  libglpk40 libglpk-dev libxml2-dev libcurl4-openssl-dev libssl-dev \
  libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev \
  libpng-dev libtiff5-dev libjpeg-dev libuv1-dev
```

Verify the pin, then install the R packages:

```bash
Rscript -e 'stopifnot(as.character(getRversion()) == readLines(".R-version")[1])'
Rscript scripts/install_deps.R
```

`install_deps.R` fetches precompiled binaries only, from the dated PPM snapshot, and
loads every package after installing — a name-only check would pass a broken binary
or a failed transitive dependency. Full notes on the snapshot mechanics are in
[README.ru.md](README.ru.md).

### Running in a container

Alternatively, in a container with the same R, locale and package set:

```bash
python container.py up
python go.py all -docker main253
python container.py down
```

Steps never call each other; they communicate only through files in `output/`. The
number in a filename is its position in the dependency graph, so
`scripts/[0-9]*_*.R` in numeric order is the execution order. A run stops at the
first failing step.

## Where the numbers are

All paths are relative to `output/`. `.txt` files are verbose step transcripts in
Russian; every number in them also exists in a CSV listed here.

### Sample and test structure
| Artifact | Contents |
| --- | --- |
| `preprocess_summary.csv` | Sample selection: rows in the export, dropped records, final N |
| `Descriptive/descriptive_stats_tables.csv` | Distribution by form language, course year, conference participation, region |

### Descriptive statistics
| Artifact | Contents |
| --- | --- |
| `Descriptive/descriptive_stats_detailed.csv` | Items and total scores: N, M, SD, quartiles, distribution shape |
| `Descriptive/descriptive_stats_by_course.csv` | Scores by course year |

### Sample bimodality
| Artifact | Contents |
| --- | --- |
| `Bimodality/extreme_patterns.csv` | Extreme response patterns, bimodality coefficient, dip test |
| `Bimodality/score_total_frequency.csv` | Frequency of every total-score value |
| `Bimodality/subsample_sensitivity.csv` | Reliability and CFA fit under trimming of extreme scores |
| `Bimodality/subsample_known_groups.csv` | Known-groups comparisons on the same subsamples |
| `Bimodality/cut_sensitivity.csv` | Metric gradient across cut points (three sweeps) |
| `Bimodality/cut_monotonicity.csv` | Monotonicity of metrics across sweeps, and sweep coverage |

### Distractor analysis (CTT)
| Artifact | Contents |
| --- | --- |
| `Distractor/distractor_pb_table.csv` | Selection rate and item-rest point-biserial per response option |

### Exploratory factor analysis
| Artifact | Contents |
| --- | --- |
| `EFA/efa_eigenvalues.csv` | Eigenvalues and the parallel-analysis line |
| `EFA/efa_parallel_analysis.csv` | Parallel analysis: number of factors (FA) and of components (PCA) |
| `EFA/efa_loadings.csv` | Factor loadings (ML, oblimin, 3 factors) |

### Confirmatory factor analysis
| Artifact | Contents |
| --- | --- |
| `Bifactor/bifactor_fit_table.csv` | Fit: 1F-CFA vs 3F-ICM-CFA vs bifactor CFA |
| `CFA/cfa_loadings.csv` | 3F-ICM-CFA loadings: B, SE, z, p, standardized Beta |
| `CFA/cfa_loadings_compact.csv` | The same loadings, compact: subscale, item, Beta |

### ESEM
| Artifact | Contents |
| --- | --- |
| `ESEM/esem_fit_table.csv` | Fit: 1F-CFA vs 3F-ICM-CFA vs 3F-ESEM (scaled) |
| `ESEM/esem_factor_correlations.csv` | ESEM factor correlations (geomin, oblique) |
| `ESEM/esem_loadings.csv` | ESEM loadings (standardized, geomin) |

### Reliability and the bifactor model
| Artifact | Contents |
| --- | --- |
| `2PL/2pl_bifactor_indices.csv` | Bifactor 2PL indices: omega_h, omega_total, ECV, PUC |
| `2PL/2pl_bifactor_subscale_omega.csv` | Subscale reliability: omega_s and omega_hs |
| `2PL/2pl_bifactor_loadings.csv` | Bifactor 2PL loadings (general G and specific S1-S3) |
| `CFA/cfa_bifactor_loadings.csv` | Bifactor CFA loadings: B, SE, z, p, standardized Beta |
| `Bifactor/omega_matrix_status.csv` | Tetrachoric matrix used for omega: min eigenvalue, smoothing status |

### Rasch model (1PL)
| Artifact | Contents |
| --- | --- |
| `Rasch/rasch_item_report.csv` | Item difficulty and Infit/Outfit MSQ |
| `Rasch/rasch_excluded_items.csv` | Items dropped for zero variance before calibration |

### 2PL IRT
| Artifact | Contents |
| --- | --- |
| `2PL/2pl_params.csv` | 2PL parameters: discrimination a, difficulty b, proportion correct |
| `2PL/2pl_excluded_items.csv` | Items dropped for zero variance before estimation |
| `2PL/1pl_2pl_comparison.csv` | 1PL vs 2PL: AIC, BIC, logLik, LRT |
| `2PL/2pl_tif_sem.csv` | TIF/SEM: information peak, values at theta = 0, range |
| `2PL/2pl_q3_summary.csv`, `2pl_q3_pairs.csv`, `2pl_q3_matrix.csv` | Local independence: Yen's Q3 summary, item pairs, full 26x26 matrix |
| `2PL/2pl_model_status.csv` | Convergence of the baseline 1PL and 2PL |

### DIF: language bias (KZ vs RU)
| Artifact | Contents |
| --- | --- |
| `DIF/dif_mh_table.csv` | Mantel-Haenszel: chi2, adjusted p, lnOR, ETS category, flag |
| `DIF/dif_excluded_items.csv` | Items dropped for zero variance before the DIF computation |

### Known-groups comparisons
| Artifact | Contents |
| --- | --- |
| `ANOVA/group_tests_significance.csv` | Significance summary of the omnibus group comparisons |
| `ANOVA/anova_results.xlsx` | Full ANOVA tables (all sheets) |

### Floor stratum: guessing, refit without the floor, hub cluster
| Artifact | Contents |
| --- | --- |
| `FloorStratum/floor_verdict.csv` | Verdict on the floor stratum and the structural selection boundary |
| `FloorStratum/floor_item_proportions.csv` | Per-item proportion correct inside the floor stratum against the guessing level |
| `FloorStratum/floor_group_tests.csv` | The same proportions by item group: hub cluster vs the rest |
| `FloorStratum/floor_contrasts.csv` | Within-stratum contrasts: Cochran's Q, paired hub/rest |
| `FloorStratum/nofloor_2pl_status.csv`, `nofloor_2pl_params.csv` | 2PL refit on the subsample without the floor stratum: convergence and per-item a, b |
| `FloorStratum/person_fit_summary.csv` | Person fit by stratum: Zh (lz) and Guttman errors |
| `FloorStratum/mixture_summary.csv`, `mixture_by_stratum.csv` | "Pure guessing + 2PL" mixture: random-response class share, overall and by stratum |
| `FloorStratum/hub_bifactor_status.csv`, `hub_bifactor_indices.csv`, `hub_bifactor_loadings.csv` | Bifactor 2PL with a hub-specific factor: convergence, indices, loadings |
| `FloorStratum/nohub_structure.csv`, `nohub_eigenvalues.csv` | Reliability, factor count and CFA fit on the 20 items without the hub; their eigenvalues |

Consolidated views of the same numbers: `report.html` (with figures) and
`report.md` / `report.json` (text and machine-readable). Both are in Russian and are
transcriptions — nothing is recomputed there.

## Analytic decisions and known limitations

Two working records document every choice and every open problem. They are in
Russian; the entries a reader of the manuscript will most likely want are:

**[DECISIONS.md](DECISIONS.md)** — resolved questions and their rationale, e.g. D4
(exclusion of `Q02` for extreme language DIF), D5 (data collection closed; N is not
hard-coded), D6 (smoothing of the tetrachoric matrix and its cost), D9
(non-parametric tests for the known-groups comparisons), D10 (version-sensitive values and
the package snapshot).

**[ISSUES.md](ISSUES.md)** — open problems and warnings, including:

* **1.4** — no statistical significance in the known-groups comparisons;
* **1.5** — the total score is bimodal, which inflates alpha and omega through
  sample spread;
* **1.7** — the key is the longest of the four options in 17 of 26 items (KZ) and 19
  of 26 (RU), so a strategy that never reads the stem scores above the sample mean.
  Whether any respondent used this cue is **not** established and cannot be tested
  directly on these data — the cue and correctness are collinear. Analysis:
  [analysis/item_cues.md](analysis/item_cues.md);
* **1.8** — four records share a byte-identical response vector;
* **1.9** — the floor of the scale is not homogeneous: a hub cluster is answered
  above the guessing level while the other 20 items sit at it.

Exploratory analyses outside the pipeline live in [analysis/](analysis/); `run_all.R`
does not call them and they change no number in `output/`.

## License

Code — [MIT](LICENSE). Data in `input/` — [CC BY 4.0](input/LICENSE). Artifacts in
`output/` are derived from the data and inherit CC BY 4.0. Attribution is required.
