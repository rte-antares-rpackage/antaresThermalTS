
#' Get sum of PCN for stopped clusters by weeks
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
#' @importFrom lubridate wday hour isoweek
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' setSimulationPath(path = "PATH/TO/STUDY")
#' 
#' pcn_stop_clusters()
#' 
#' pcn_stop_clusters(first_weekday = 6) # saturday
#' 
#' }
pcn_stop_clusters <- function(first_weekday = 1, area = "fr", remove_clusters = NULL, start_date = NULL, opts = simOptions()) {
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
      data_mod[lubridate::wday(time, week_start = 1) == 3 & lubridate::hour(time) == 19 & V3 == 0, shutdown := 1]
      data_mod <- data_mod[, list(shutdown = any(shutdown == 1) * 1), by = list(date = as.Date(format(time)))]
      data_mod[, week := lubridate::wday(date, week_start = first_weekday)]
      data_mod[week > 1, week := 0]
      data_mod[, week := cumsum(week)]
      data_mod[lubridate::wday(date, week_start = 1) == 1, nweek := lubridate::isoweek(date)]
      data_mod[, nweek := nweek[!is.na(nweek)][1], by = week]
      data_mod <- data_mod[, list(
        shutdown = any(shutdown == 1), 
        date = first(date),
        nweek = first(nweek),
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
    y = clusters[, list(area, cluster, nominalcapacity, group)],
    by = c("area", "cluster")
  )
  sd_clus[n == 7, list(
    nominalcapacity = sum(nominalcapacity[shutdown == TRUE], na.rm = TRUE) / 1e3,
    nominalcapacity.total = sum(nominalcapacity, na.rm = TRUE) / 1e3
  ), by = list(week, nweek, date, group)]
}

