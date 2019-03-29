
#' Create other clusters
#'
#' @param planning Calendar data read with \code{\link{read_calendar}}.
#' @param infos Info
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
create_clusters_other <- function(planning, infos, opts = simOptions()) {
  
  planning <- copy(planning)
  planning[is.na(code_gp), code_gp := nom_site]
  
  planning[nom_site == "PONT SUR SAMBRE", code_gp := "SAMBRT1"]
  planning[nom_site == "CROIX DE METZ", code_gp := "C.ME5T01"]

  
  infos[is.na(`for`), `for` := 1]
  
  infos[, pmax := as.numeric(pmax)]
  infos[is.na(pmax), pmax := 0]
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique(planning$code_gp), 
      nm = unique(planning$code_gp)
    ),
    FUN = function(cluster) {
      dat <- planning[code_gp == cluster & !is.na(dt_debut_arret)]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = 8760 * 3),
            rep(0, times = 8760 * 1)
          ),
          ncol = 4
        )
      } else {
        datetime_study <- seq(from = as.POSIXct("2018-07-01", tz = "UTC"), length.out = 8760, by = "1 hour")
        datetime_study <- as.character(datetime_study)
        datetime_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$dt_fin_arret[i] > dat$dt_debut_arret[i]) {
              res <- seq(
                from = as_datetime(dat$dt_debut_arret[i]), 
                to = dat$dt_fin_arret[i] - hours(1), 
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
    
    code_group <- corr_groupe_descr(cluster)
    cluster_infos <- descr_clusters(code_group)
    
    infos_clus <- infos[code_gp == cluster]
    
    opts <- createCluster(
      opts = opts,
      area = "area", 
      cluster_name = stri_replace_all_regex(string = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = FALSE,
      group = cluster_infos[["group"]],
      unitcount = 1L,
      nominalcapacity = floor(infos_clus$pmax),
      `min-stable-power` = floor(infos_clus$pmin),
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
          rep(1, times = 365 * 2),
          rep(1 - infos_clus[["for"]], times = 365 * 1),
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


