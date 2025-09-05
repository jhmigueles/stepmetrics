---
editor_options: 
  markdown: 
    wrap: 72
---

# stepmetrics

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/stepmetrics)](https://CRAN.R-project.org/package=stepmetrics)
[![R-CMD-check_standard](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check_standard.yaml/badge.svg)](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check_standard.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jhmigueles/stepmetrics/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jhmigueles/stepmetrics?branch=master)
[![DOI](https://zenodo.org/badge/613043353.svg)](https://zenodo.org/badge/latestdoi/613043353)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/last-month/stepmetrics)](https://CRAN.R-project.org/package=stepmetrics)
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/stepmetrics)](https://CRAN.R-project.org/package=stepmetrics)


<!-- badges: end -->

## Overview

The **stepmetrics** package provides tools to calculate **step- and
cadence-based metrics** from wearable device data. It supports data
aggregated at **epochs of 1–60 seconds**, and automatically
re-aggregates sub-minute data to 60-second epochs before computing
metrics.

Currently, the package has been tested with data from:

-   **Fitbit** exports

-   **ActiGraph** CSV and AGD files

-   **GGIR** outputs (with externally computed step counts, e.g.,
    [Verisense
    algorithm](https://github.com/ShimmerEngineering/Verisense-Toolbox/tree/master/Verisense_step_algorithm?utm_source=chatgpt.com))

## Main functionalities

-   Summarizing **total steps per day**

-   Quantifying **time and steps across cadence bands**

-   Computing **cadence peaks** (e.g., 1-, 30-, 60-minute peaks)

-   Deriving **moderate, vigorous, and MVPA minutes and steps**

-   Producing **daily- and person-level summary datasets**

## Installation

The stable release of **stepmetrics** can be installed from CRAN:

``` r
# install.packages("devtools")
install.packages("stepmetrics")
```

You can install the development version of stepmetrics from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jhmigueles/stepmetrics")
```

## Core Workflow

The main function is `step.metrics()`, which processes raw step data and
exports day-level and person-level summaries.

``` r
library(stepmetrics)
step.metrics(datadir = "C:/mydata/",
             outputdir = "C:/myoutput/",
             idloc = "_",
             cadence_bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf),
             cadence_peaks = c(1, 30, 60),
             cadence_MOD = 100,
             cadence_VIG = 130,
             includedaycrit = 10,
             exclude_pk30_0 = TRUE,
             exclude_pk60_0 = TRUE,
             time_format = NULL)
```

This function does not return an object into the R session. Instead, it
generates:

-    **Day-level CSVs**: one file per participant in
    `outputdir/daySummary/`

-   **Person-level CSV**: aggregated averages in
    `outputdir/personSummary.csv`

## Working with GGIR output

If your step counts were generated in **GGIR** (e.g., with an external
algorithm such as Verisense):

-   Set `datadir` to the GGIR output directory (the folder whose name
    begins with `"output_"`).

-   The function will automatically detect and validate whether it’s a
    GGIR output folder using [`isGGIRoutput()`](reference).

**Important:** stepmetrics looks for a column with `"step"` in its name.
Valid examples:

-    `steps`

-   `step_column`

-   `step_count`

-   `step_per_epoch`

When computing steps externally for GGIR, ensure your chosen column name
follows this convention.

## Key Functions

-   **`readFile()`**: Reads and standardizes timestamp/step data from
    CSV, AGD, or GGIR `.RData` files. Handles ActiGraph headers,
    multiple files per participant, and aggregates sub-minute epochs to
    60 seconds.

-   **`define_day_indices()`**: Converts ISO-8601 timestamps to
    sequential day indices (useful for looping over days).

-   **`get_cadence_bands()`**: Calculates **minutes and steps** spent in
    predefined cadence bands (e.g., 0–19, 20–39 spm).

-   **`get_cadence_peaks()`**: Computes cadence peaks (mean of the top
    1, 30, 60 minutes of the day) and counts how many of those minutes
    had 0 steps.

-   **`isGGIRoutput()`**: Verifies whether a folder is a valid GGIR
    output directory suitable for stepmetrics (checks for part 2 data
    and step columns).

## Example Outputs

**Day-level output (`*_DaySum.csv`)** includes:

-   Participant ID, date, weekday

-   Recording duration, valid wear time, awake time (if GGIR available)

-   Total steps

-   Cadence peaks (values + number of zeroes)

-   Minutes and steps per cadence band

-   MPA, VPA, MVPA (minutes and steps)

**Person-level output (`personSummary.csv`)** includes:

-   Overall averages across valid days

-   Weighted weekday/weekend averages

-   Stratified weekday-only and weekend-only averages

## Citation

If you use **stepmetrics** in your research, please cite:

Migueles, JH. *stepmetrics: Calculate Step and Cadence Metrics from
Wearable Data*. Zenodo. DOI: 10.5281/zenodo.7858094
