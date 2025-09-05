#' Check if a directory is a valid GGIR output folder for stepmetrics
#'
#' @description
#' Determines whether a given directory corresponds to a valid GGIR output
#' directory that can be used with \pkg{stepmetrics}. Several conditions
#' are checked in sequence:
#' \enumerate{
#'   \item Path exists and is a directory.
#'   \item Directory name begins with `"output_"`.
#'   \item Contains a `meta/` subfolder.
#'   \item Contains a `meta/ms2.out/` subfolder (GGIR part 2 milestone data).
#'   \item Contains at least one `*.RData` file in `ms2.out`.
#'   \item The loaded object \code{IMP$metashort} includes a step column
#'         (with `"step"` in its name).
#' }
#'
#' If any of these checks fail, the function returns \code{FALSE} and issues a
#' warning describing the missing requirement.
#'
#' @param path Character. Path to the directory to be tested.
#'
#' @return Logical scalar. Returns \code{TRUE} if the directory appears to be
#'   valid GGIR output suitable for stepmetrics, otherwise \code{FALSE}.
#'
#' @note
#' - A warning is issued if the directory looks like GGIR output but is missing
#'   required parts (e.g., part 2 milestone data or step counts in
#'   \code{IMP$metashort}).
#' - This function loads the first available RData file in
#'   \code{meta/ms2.out/} to verify the presence of step counts.
#'
#' @examples
#' \dontrun{
#' # Typical GGIR output folder
#' isGGIRoutput("C:/mystudy/output_GGIR/")
#'
#' # Non-GGIR directory
#' isGGIRoutput("C:/mystudy/rawdata/")
#' }
#'
#' @seealso [step.metrics()]
#' @export
isGGIRoutput = function(path) {

  # 1 - path is a directory
  check1 = dir.exists(path)

  if (check1 == TRUE) {

    # 2 - basename path starts with "output_"
    check2 = grepl("^output_", basename(path))

    if (check2 == TRUE) {
      # 3 - meta is a subfolder of path
      check3 = dir.exists(file.path(path, "meta"))

      if (check3 == TRUE) {
        # 4 - ms2.out is a folder within meta
        check4 = dir.exists(file.path(path, "meta/ms2.out"))

        if (check4 == TRUE) {
          # 5 - there are files available in ms2.out
          GGIRfiles = dir(file.path(path, "meta/ms2.out"), full.names = T, pattern = "*.RData")
          check5 = length(GGIRfiles) > 0

          if (check5 == TRUE) {
            # 6 - check step is available
            IMP = c()
            load(GGIRfiles[1])
            step_columns = which(grepl("step", colnames(IMP$metashort)))
            check6 = length(step_columns) == 1

            if (check6 == TRUE) {
              return(TRUE)

            } else {
              warning("The column names in your GGIR IMP$metashort dataset do not include any column with 'steps' or 'step_count' as name.")
              return(FALSE)
            }

          } else {
            warning("It seems you are trying to process GGIR output, but part 2 of GGIR has not been run yet (stepmetrics needs the part 2 milestone data from GGIR to work)")
            return(FALSE)
          }

        } else {
          warning("It seems you are trying to process GGIR output, but part 2 of GGIR has not been run yet (stepmetrics needs the part 2 milestone data from GGIR to work)")
          return(FALSE)
        }
      } else {
        warning("It seems you are trying to process GGIR output, but the directory specified does not contain a meta folder.")
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
