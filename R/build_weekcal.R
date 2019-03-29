
#' Build Week calendar
#'
#' @param start Starting year
#' @param end Ending year
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table data.table :=
#' @importFrom zoo na.locf
#'
#' @examples
#' 
#' build_weekcal()
#' 
build_weekcal <- function(start = 2018, end = 2020) {
  weekcal <- data.table(
    dates = seq(
      from = as.Date(paste0(start, "-01-01")) - 7,
      to = as.Date(paste0(end, "-12-31")) + 7,
      by = "days"
    )
  )
  weekcal[, saturdays := format(dates, format = "%u") %in% "6"]
  weekcal[, year := format(dates + 7, format = "%Y")]
  weekcal[saturdays == TRUE, week := sprintf("S%02d - %s", seq_along(saturdays), year), by = year]
  weekcal[, week := zoo::na.locf(week, na.rm = FALSE)]
  weekcal <- weekcal[!is.na(week)]
  
  weekcal <- weekcal[, list(
    week_start = min(dates),
    week_end = max(dates),
    n = .N
  ), by = week]
  weekcal <- weekcal[n == 7]
  weekcal[, n := NULL]
  weekcal[]
}
