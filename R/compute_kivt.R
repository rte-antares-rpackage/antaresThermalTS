
#' Compute Kivt coefficient
#'
#' @param calendar Calendar data read with \code{\link{read_calendar}}.
#' @param clusters_desc Clusters / groups description read with \code{\link{read_cluster_desc}}.
#' @param years Years to consider, if \code{NULL} (default), year range from \code{calendar} will be used.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table copy setorder := uniqueN 
#'
#' @examples
#' \dontrun{
#' # Calendar data
#' calendar <- read_calendar(path = "REF_Planning_5_ans_mars_2018.xlsx")
#' 
#' # Clusters description
#' clusters <- read_cluster_desc("HypothesesRTE_CHO-4145.xlsx")
#' 
#' 
#' # Kivt computation
#' kivt <- compute_kivt(calendar, clusters)
#' kivt
#' }
compute_kivt <- function(calendar, clusters_desc, years = NULL) {
  
  calendar <- copy(calendar)
  clusters_desc <- copy(clusters_desc)
  
  if (is.null(years)) {
    years <- range(calendar$date_debut, na.rm = TRUE)
    years <- format(years, format = "%Y")
  }
  
  weekcal <- build_weekcal(start = years[1], end = years[2])
  
  weekcal[, .id := 1]
  calendar[, .id := 1]
  
  week_groups <- weekcal[calendar[, list(
    .id,
    group = tranche,
    shutdown_start = as.Date(date_debut),
    shutdown_end = as.Date(date_de_fin_sans_prolongation) - 1
  )], on = ".id", allow.cartesian = TRUE]

  week_groups[, n_overlaps := n_overlaps(week_start, week_end, shutdown_start, shutdown_end)]
  
  clusters_desc <- clusters_desc[, list(group = corresp_groupes, type_groupe, pcn_mw)]
  clusters_desc[, pcn_mw := as.numeric(pcn_mw)]
  clusters_desc[, group_power := sum(pcn_mw), by = type_groupe]
  
  weekclus <- merge(
    x = week_groups, 
    y = clusters_desc,
    by = "group", all.x = TRUE, all.y = FALSE
  )
  weekclus <- weekclus[!is.na(type_groupe)]

  setorder(weekclus, week, group, -n_overlaps)
  weekclus <- unique(weekclus, by = c("week", "group"))
  
  coef_kivt <- weekclus[, list(
    kivt = sum(pcn_mw * n_overlaps / 7) / group_power * 100,
    n_days = sum(n_overlaps), 
    n = .N, n_group = uniqueN(group)
  ), by = list(week, week_start, week_end, type_groupe, group_power)]
  setorder(coef_kivt, week_start, type_groupe)
  coef_kivt[]
}
