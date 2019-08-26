
#' Get sum of Min Stable Power for running clusters by weeks
#'
#' @param start_date Starting date of the study.
#' @param first_weekday The first day to use for starting a week, default to \code{1} (monday).
#' @param stop_all_week Consider that a cluster is stopped if it is really stopped the 7 days
#'  of the weeks, otherwise the PCN of a clusters is counted for a week if it is stopped 
#'  at least one day in the week.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @return a \code{data.table}.
#' @export
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
msp_run_clusters <- function(start_date = NULL, first_weekday = 1, stop_all_week = TRUE, opts = simOptions()) {
  if (is.null(start_date)) {
    start_date <- opts$start
  }
  clusters <- readClusterDesc(opts = opts)
  clusters[, group := as.character(group)]
  dates_sd <- mapply(
    FUN = function(area, cluster) {
      data_mod <- fread(file = file.path(opts$inputPath, "thermal", "prepro", area, cluster, "modulation.txt"))
      data_mod[, time := seq(from = start_date, by = "hour", length.out = .N)]
      data_mod <- data_mod[, lapply(.SD, sum), by = list(date = as.Date(format(time)))]
      data_mod[, week := lubridate::wday(date, week_start = first_weekday)]
      data_mod[week > 1, week := 0]
      data_mod[, week := cumsum(week)]
      data_mod <- data_mod[, list(
        shutdown_any = any(V3 == 0), 
        shutdown_all = all(V3 == 0),
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
    y = clusters[, list(area, cluster, min.stable.power, group)],
    by = c("area", "cluster")
  )
  if (isTRUE(stop_all_week)) {
    sd_clus[n == 7, list(
      min.stable.power = sum(min.stable.power[shutdown_all == FALSE], na.rm = TRUE) / 1e4
    ), by = list(week, date, group)]
  } else {
    sd_clus[n == 7, list(
      min.stable.power = sum(min.stable.power[shutdown_any == FALSE], na.rm = TRUE) / 1e4
    ), by = list(week, date, group)]
  }
}

