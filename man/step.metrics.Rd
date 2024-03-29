% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step.metrics.R
\name{step.metrics}
\alias{step.metrics}
\title{Function to calculate and export step metrics.}
\usage{
step.metrics(
  datadir,
  outputdir = "./",
  idloc = "_",
  cadence_bands = c(0, 1, 20, 40, 60, 80, 100, 120, Inf),
  cadence_peaks = c(1, 30, 60),
  cadence_MOD = 100,
  cadence_VIG = 130,
  includedaycrit = 10,
  includeawakecrit = NULL,
  includedaylengthcrit = 23,
  exclude_pk30_0 = TRUE,
  exclude_pk60_0 = TRUE,
  time_format = NULL,
  verbose = TRUE
)
}
\arguments{
\item{datadir}{Character (no default). Directory where the files containing the steps data are stored, e.g., "C:/mydata/". If you are trying to process GGIR output, then this should be the output directory from GGIR (name starts with "output_").}

\item{outputdir}{Character (no default). Directory where the output needs to be stored. Note that this function will attempt to create folders in this directory and uses those folder to keep output.}

\item{idloc}{Character (default = "_"). The ID is expected to be before the character(s) specified by \code{idloc} in the filename}

\item{cadence_bands}{Numeric (default = c(0, 1, 20, 40, 60, 80, 100, 120, Inf)). Cadence bands to calculate the time accumulated into.}

\item{cadence_peaks}{Numeric (default = c(1, 30, 60)). Cadence peaks to calculate.}

\item{cadence_MOD}{Numeric (default = 100)}

\item{cadence_VIG}{Numeric (default = 130)}

\item{includedaycrit}{Numeric (default = 10)}

\item{includeawakecrit}{Numeric (default = NULL). Only use this if part 5 reports are available.}

\item{includedaylengthcrit}{Numeric (default = 23). Only use this if part 5 reports are available.}

\item{exclude_pk30_0}{Logical (default = TRUE)}

\item{exclude_pk60_0}{Logical (default = TRUE)}

\item{time_format}{Character (default = NULL)}

\item{verbose}{logical (default = TRUE). Whether to print progress messages in the console.}
}
\value{
This function does not return any object. It stores csv files with the
         day-level and person-level data in the output directory.
}
\description{
This function loads the minute-based files containing the step counts,
               calculate the step and cadence metrics and store them in day-level and person-level csv files.
}
\examples{
\dontrun{
step.metrics(datadir = "C:/mydata/", outputdir = "D:/myoutput/")
}
}
