
#' Create EDF clusters
#'
#' @param planning Calendar data read with \code{\link{read_calendar}}.
#' @param start_date Starting date of the study, if \code{NULL} (default),
#'  the date will be retrieve from the Antares study.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject createCluster
#' @importFrom lubridate hours days as_datetime
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_regex
create_clusters_edf <- function(planning, start_date = NULL, opts = simOptions()) {
  
  if (is.null(start_date))
    start_date <- format(opts$start, format = "%Y-%m-%d")
  
  planning <- copy(planning)
  planning <- planning[!is.na(code_gp)]

  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique(planning$code_gp), 
      nm = unique(planning$code_gp)
    ),
    FUN = function(cluster) {
      dat <- planning[code_gp == cluster & !is.na(date_debut)]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = 8760 * 3),
            rep(0, times = 8760 * 1)
          ),
          ncol = 4
        )
      } else {
        datetime_study <- seq(from = as.POSIXct(start_date, tz = "UTC"), length.out = 8760, by = "1 hour")
        datetime_study <- as.character(datetime_study)
        datetime_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_fin_arret[i] > dat$date_debut[i]) {
              res <- seq(
                from = as_datetime(dat$date_debut[i]), 
                to = dat$date_fin_arret[i] - hours(1), 
                by = "1 hour"
              )
              as.character(res)
            }
          }
        )
        
        datetime_prolongation <- unlist(datetime_prolongation)
        capacity_modulation <- (!datetime_study %in% datetime_prolongation) * 1
        matrix(
          data = c(
            rep(1, times = 8760 * 2),
            capacity_modulation,
            rep(0, times = 8760 * 1)
          ),
          ncol = 4
        )
      }
    }
  )
  
  
  for (cluster in unique(planning$code_gp)) {
    
    infos_clus <- planning[code_gp == cluster]
    infos_clus <- unique(infos_clus, by = "code_gp")
    
    cluster_infos <- descr_clusters(infos_clus$name_desc)
    
    opts <- createCluster(
      opts = opts,
      area = "area", 
      cluster_name = stri_replace_all_regex(string = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = FALSE,
      group = cluster_infos[["group"]],
      unitcount = 1L,
      nominalcapacity = floor(infos_clus$pcn_mw),
      `min-stable-power` = floor(infos_clus$pmin_mw),
      `must-run` = FALSE,
      # `min-down-time` = 1L,
      # `min-up-time` = 168L,
      
      `min-up-time` = cluster_infos[["min-up-time"]],
      `min-down-time` = cluster_infos[["min-down-time"]],
      spinning = cluster_infos[["spinning"]],
      `marginal-cost` = cluster_infos[["marginal-cost"]],
      `spread-cost` = cluster_infos[["spread-cost"]],
      `startup-cost` = cluster_infos[["startup-cost"]],
      `market-bid-cost` = cluster_infos[["market-bid-cost"]],
      co2 = cluster_infos[["co2"]],
      
      prepro_data = matrix(
        data = c(
          rep(7, times = 365 ),
          rep(1, times = 365),
          rep(1 - 0.5, times = 365 * 1),
          rep(0, times = 365 * 2),
          rep(1, times = 365 * 1)
        ),
        ncol = 6
      ),
      prepro_modulation = modulation_list[[cluster]]
    )
  }
  
  invisible(opts)
}


