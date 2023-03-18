
# stepmetrics

<!-- badges: start -->
[![R-CMD-check_standard](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check_standard.yaml/badge.svg)](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check_standard.yaml)
[![Codecov test coverage](https://codecov.io/gh/jhmigueles/stepmetrics/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jhmigueles/stepmetrics?branch=master)
[![DOI](https://zenodo.org/badge/613043353.svg)](https://zenodo.org/badge/latestdoi/613043353)
<!-- badges: end -->

The goal of stepmetrics is to calculate step and cadence metrics from aggregated 
step values per epoch. At the moment the package works with epochs from 1 to 60 seconds.
For epochs shorter than 60 seconds, the values are aggregated to 60 seconds prior
to calculating the step and cadence metrics. Therefore, for now the epoch length needs
to be divisible by 60 (i.e., 1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30 y 60 seconds).

This package has been tested with fitbit, actigraph files, and GGIR output at the moment.

## Installation

You can install the development version of stepmetrics from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jhmigueles/stepmetrics")
```

## Example

This is a basic example which shows you how to calculate the step and cadence metrics:

``` r
library(stepmetrics)
step.metrics(datadir = "C:/mydata/", outputdir = "C:/myoutput/",
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

This function does not return any object into the R session. It generates
day-level and person-level step and cadence metrics and store them in csv files
in the output directory (as defined with `outputdir`)

## Working with GGIR output?

If you calculated the step count in GGIR with an external function (for example the algorithm by [Verisense](https://github.com/ShimmerEngineering/Verisense-Toolbox/tree/master/Verisense_step_algorithm)), then the `datadir`should be defined as the 
path to the output directory generated in GGIR (the folder that starts with "output_"
in the name).

Note that stepmetrics will look for a column that includes "step" in the name, for
example: steps, step_column, step_count, step_per_epoch would be valid column names.
When using a external function in GGIR, you can decide on the column name for that
metric, thus, you need to make sure to meet this requirement.


