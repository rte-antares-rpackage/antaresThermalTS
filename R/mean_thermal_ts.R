
#' Mean of thermal timeseries by weeks
#'
#' @param first_weekday The first day to use for starting a week, default to \code{1} (monday).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @return a \code{data.table}.
#' @export
#' 
#' @importFrom antaresRead readInputTS readClusterDesc
#' @importFrom lubridate wday hour isoweek
#' @importFrom data.table first := 
#' @importFrom anytime anydate
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' setSimulationPath(path = "PATH/TO/STUDY")
#' 
#' mean_thermal_ts()
#' 
#' }
mean_thermal_ts <- function(first_weekday = 1, opts = simOptions()) {
  clusters <- readClusterDesc(opts = opts)
  ts <- readInputTS(thermalAvailabilities = "fr", opts = opts)
  ts[, WED19 := FALSE]
  ts[lubridate::wday(time, week_start = 1) == 3 & lubridate::hour(time) == 19, WED19 := TRUE]
  ts[, date := anytime::anydate(time)]
  # ts[, date := as.Date(format(time))]
  ts <- ts[, list(
    ThermalAvailabilities = mean(ThermalAvailabilities[WED19 == TRUE])
  ), by = list(area, cluster, date)]
  ts[, week := lubridate::wday(date, week_start = first_weekday)]
  ts[week > 1, week := 0]
  ts[, week := cumsum(week), by = list(area, cluster)]
  ts[lubridate::wday(date, week_start = 1) == 1, nweek := lubridate::isoweek(date)]
  ts[, nweek := nweek[!is.na(nweek)][1], by = week]
  tsweek <- ts[, list(
    ThermalAvailabilities = mean(ThermalAvailabilities, na.rm = TRUE),
    date = first(date),
    nweek = first(nweek),
    n = .N
  ), by = list(area, cluster, week)]
  tsweek <- tsweek[n == 7]
  tsweek[, n := NULL]
  tsweek <- merge(
    x = tsweek,
    y = clusters[, list(area, cluster, group)],
    by = c("area", "cluster")
  )
  tsweek[]
}


