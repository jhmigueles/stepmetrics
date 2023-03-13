
# stepmetrics

<!-- badges: start -->
[![R-CMD-check](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jhmigueles/stepmetrics/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jhmigueles/stepmetrics/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jhmigueles/stepmetrics?branch=master)
<!-- badges: end -->

The goal of stepmetrics is to calculate step and cadence metrics from aggregated 
step values per epoch. At the moment the package works with epochs from 1 to 60 seconds.
For epochs shorter than 60 seconds, the values are aggregated to 60 seconds prior
to calculating the step and cadence metrics.

This package has been tested with fitbit and actigraph files at the moment.

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
step.metrics(datadir = "C:/mydata/", outputdir = "C:/myoutput/",idloc = "_",
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

