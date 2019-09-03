
#' Get sum of Min Stable Power for running clusters by weeks
#'
#' @param first_weekday The first day to use for starting a week, default to \code{1} (monday).
#' @param area Area(s) to include in the computation.
#' @param remove_clusters Character vector. Cluster(s) to remove.
#' @param start_date Starting date of the study. If \code{NULL} (the default),
#'  the date of the antares study is used.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @return a \code{data.table}.
#' @export
#' 
#' @importFrom data.table fread rbindlist first
#' @importFrom lubridate wday hour
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' setSimulationPath(path = "PATH/TO/STUDY")
#' 
#' msp_run_clusters()
#' 
#' msp_run_clusters(first_weekday = 6) # saturday
#' 
#' }
msp_run_clusters <- function(first_weekday = 1, area = "fr", remove_clusters = NULL, start_date = NULL, opts = simOptions()) {
  if (is.null(start_date)) {
    start_date <- opts$start
  }
  area_ <- area
  clusters <- readClusterDesc(opts = opts)
  clusters <- clusters[area %in% area_]
  clusters <- clusters[!cluster %in% remove_clusters]
  clusters[, group := as.character(group)]
  dates_sd <- mapply(
    FUN = function(area, cluster) {
      data_mod <- fread(file = file.path(opts$inputPath, "thermal", "prepro", area, cluster, "modulation.txt"))
      data_mod[, time := seq(from = start_date, by = "hour", length.out = .N)]
      data_mod[, shutdown := 0]
      data_mod[lubridate::wday(time, week_start = 1) == 7 & lubridate::hour(time) == 5 & V3 == 0, shutdown := 1]
      data_mod <- data_mod[, lapply(.SD, sum), by = list(date = as.Date(format(time)))]
      data_mod[, week := lubridate::wday(date, week_start = first_weekday)]
      data_mod[week > 1, week := 0]
      data_mod[, week := cumsum(week)]
      data_mod <- data_mod[, list(
        shutdown = any(shutdown == 1), 
        date = first(date),
        n = .N
      ), by = week]
      data_mod[, area := area]
      data_mod[, cluster := cluster]
      data_mod[]
    },
    area = clusters$area,
    cluster = clusters$cluster, 
    SIMPLIFY = FALSE
  )
  sd_clus <- rbindlist(dates_sd)
  sd_clus <- merge(
    x = sd_clus, 
    y = clusters[group == "nuclear", list(area, cluster, min.stable.power, group)],
    by = c("area", "cluster")
  )
  sd_clus[n == 7, list(
    min.stable.power = sum(min.stable.power[shutdown == FALSE], na.rm = TRUE) / 1e3
  ), by = list(week, date, group)]
}

