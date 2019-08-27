
#' Mean of scenarii by weeks
#'
#' @param first_weekday The first day to use for starting a week, default to \code{1} (monday).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @return a \code{data.table}.
#' @export
#' 
#' @importFrom antaresRead readInputTS
#' @importFrom lubridate wday
#' @importFrom data.table first := 
#' @importFrom anytime anydate
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' setSimulationPath(path = "PATH/TO/STUDY")
#' 
#' }
mean_scenarii <- function(first_weekday = 1, opts = simOptions()) {
  ts <- readInputTS(thermalAvailabilities = "fr", opts = opts)
  ts[, date := anytime::anydate(time)]
  ts <- ts[, list(
    ThermalAvailabilities = mean(ThermalAvailabilities)
  ), by = list(area, cluster, date)]
  ts[, week := lubridate::wday(date, week_start = first_weekday)]
  ts[week > 1, week := 0]
  ts[, week := cumsum(week), by = list(area, cluster)]
  tsweek <- ts[, list(
    ThermalAvailabilities = mean(ThermalAvailabilities),
    date = first(date),
    n = .N
  ), by = list(area, cluster, week)]
  tsweek[n == 7][, n := NULL][]
}


