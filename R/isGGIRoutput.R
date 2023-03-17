#' Checks whether a directory is a GGIR output directory
#'
#' @param path Character (no default). Directory to test if it is a GGIR output directory
#'
#' @return Boolean: TRUE means it is GGIR output.
#' @export
#'
#' @examples
#' \dontrun{
#' isGGIRoutput("C:/mystudy/output_GGIR/")
#' }
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
