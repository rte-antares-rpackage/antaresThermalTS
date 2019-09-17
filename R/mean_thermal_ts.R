
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
#' @importFrom antaresRead readClusterDesc
#' @importFrom lubridate wday hour isoweek
#' @importFrom data.table first := rbindlist
#' @importFrom anytime anydate
#' @importFrom progress progress_bar
#'
mean_thermal_ts2 <- function(first_weekday = 1, opts = simOptions()) {
  clusters <- readClusterDesc(opts = opts)
  clusters[, area := as.character(area)]
  clusters[, cluster := as.character(cluster)]
  clusters[, group := as.character(group)]
  clusters <- clusters[area == "fr"]
  pb <- progress_bar$new(
    format = "  Reading clusters time-series [:bar] :percent",
    total = nrow(clusters), clear = FALSE
  )
  ts_clus <- mapply(
    FUN = function(area, cluster) {
      pb$tick()
      path <- file.path(opts$inputPath, "thermal/series", area, cluster, "series.txt")
      if (file.size(path) == 0) {
        return(NULL)
      }
      ts <- fread(file = path)
      ts[, time := seq.POSIXt(from = opts$start, by = "hour", length.out = 8760)]
      ts[, date := anytime::anydate(time)]
      ts[, WED19 := FALSE]
      ts[lubridate::wday(time, week_start = 1) == 3 & lubridate::hour(time) == 19, WED19 := TRUE]
      ts <- melt(
        data = ts, 
        id.vars = c("time", "date", "WED19"), 
        variable.factor = FALSE, 
        variable.name = "tsId", 
        value.name = "ThermalAvailabilities"
      )
      ts <- ts[, list(
        ThermalAvailabilities = mean(ThermalAvailabilities[WED19 == TRUE])
      ), by = date]
      ts[, week := lubridate::wday(date, week_start = first_weekday)]
      ts[week > 1, week := 0]
      ts[, week := cumsum(week)]
      ts[lubridate::wday(date, week_start = 1) == 1, nweek := lubridate::isoweek(date)]
      ts[, nweek := nweek[!is.na(nweek)][1], by = week]
      tsweek <- ts[, list(
        ThermalAvailabilities = mean(ThermalAvailabilities, na.rm = TRUE),
        date = first(date),
        nweek = first(nweek),
        n = .N
      ), by = list(week)]
      tsweek <- tsweek[n == 7]
      tsweek[, n := NULL]
      tsweek[, area := area]
      tsweek[, cluster := cluster]
    },
    area = clusters$area,
    cluster = clusters$cluster, 
    SIMPLIFY = FALSE
  )
  ts_clus <- rbindlist(ts_clus)
  ts_clus <- merge(
    x = ts_clus,
    y = clusters[, list(area, cluster, group)],
    by = c("area", "cluster")
  )
  ts_clus[]
}
