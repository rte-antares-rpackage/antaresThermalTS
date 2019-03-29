
#' Create nuclear clusters
#'
#' @param calendar Calendar data read with \code{\link{read_calendar}}.
#' @param clusters_desc Clusters / groups description read with \code{\link{read_cluster_desc}}.
#' @param kd_cho Kd coefficients read with \code{\link{read_kd_cho}}.
#' @param start_date Starting date of the study, if \code{NULL} (default),
#'  the date will be retrieve from the Antares study.
#' @param law_planned Law to use in Antares.
#' @param volatility_planned Volatility for the law.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{setSimulationPath} 
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom antaresEditObject createCluster
#' @importFrom lubridate hours days
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_regex
#' @importFrom progress progress_bar
create_clusters_nuclear <- function(calendar, clusters_desc, kd_cho, start_date = NULL, 
                                    law_planned = "geometric", volatility_planned = 1, opts = simOptions()) {
  
  if (is.null(start_date))
    start_date <- format(opts$start, format = "%Y-%m-%d")
  
  
  unique_tranches <- unique(calendar$tranche)
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique_tranches, 
      nm = unique_tranches
    ),
    FUN = function(cluster) {
      dat <- calendar[tranche == cluster]
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
            if (dat$date_de_fin_sans_prolongation[i] > dat$date_debut[i]) {
              res <- seq(
                from = dat$date_debut[i], 
                to = dat$date_de_fin_sans_prolongation[i] - hours(1), 
                by = "1 hour"
              )
              as.character(res)
            }
          }
        )
        
        coef_clus <- get_clusters_coef(cluster, clusters_desc, kd_cho, start_date)
        
        datetime_prolongation <- unlist(datetime_prolongation)
        capacity_modulation <- (!datetime_study %in% datetime_prolongation) * rep(coef_clus$abat_rso, each = 24)
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

  # Preprop data
  data_list <- lapply(
    X = setNames(
      object = unique_tranches, 
      nm = unique_tranches
    ),
    FUN = function(cluster) {
      dat <- calendar[tranche == cluster]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = 365 * 2),
            rep(0, times = 365 * 3),
            rep(1, times = 365 * 1)
          ),
          ncol = 6
        )
      } else {
        date_study <- seq(from = as.Date(start_date), length.out = 365, by = "1 day")
        date_reprise <- which(as.character(date_study) %in% as.character(dat$date_de_fin_sans_prolongation - days(1)))
        duree_prolongation_mean <- dat$duree_prolongation_mean[as.character(dat$date_de_fin_sans_prolongation) %in% as.character(date_study + days(1))]
        res <- matrix(
          data = c(
            rep(1, times = 365 * 2),
            rep(0, times = 365 * 3),
            rep(1, times = 365 * 1)
          ),
          ncol = 6
        )
        
        date_arret_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_de_fin_sans_prolongation[i] > dat$date_debut[i]) {
              res <- seq(
                from = as.Date(dat$date_debut[i]), 
                to = as.Date(dat$date_de_fin_avec_prolongation[i]) - days(1), 
                by = "1 day"
              )
              as.character(res)
            }
          }
        )
        
        coef_clus <- get_clusters_coef(cluster, clusters_desc, kd_cho, start_date)
        date_study <- as.character(date_study)
        date_arret_prolongation <- unlist(date_arret_prolongation)
        fo_rate <- (!date_study %in% date_arret_prolongation) * (1 - coef_clus$kidispo_hqe)
        
        res[, 3] <- fo_rate
        
        res[date_reprise, 2] <- duree_prolongation_mean
        res[date_reprise, 4] <- 1
        return(res)
      }
    }
  )
  
  pb <- progress_bar$new(
    format = "  Creating nuclear clusters [:bar] :percent",
    total = length(unique_tranches), clear = FALSE, width = 80
  )
  
  for (cluster in unique_tranches) {
    
    pb$tick()
    
    code_pal <- clusters_desc[corresp_groupes == cluster, c(code_palier)]
    cluster_infos <- descr_clusters(paste0("nuclear_", code_pal))
    
    opts <- createCluster(
      opts = opts,
      area = "area", 
      cluster_name = stri_replace_all_regex(str = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = FALSE,
      group = "nuclear",
      unitcount = 1L,
      nominalcapacity = clusters_desc[corresp_groupes == cluster, c(pcn_mw)],
      `min-stable-power` = clusters_desc[corresp_groupes == cluster, c(pmin_mw)],
      `must-run` = FALSE,
      # `min-down-time` = 1L,
      # `min-up-time` = 168L,
      `volatility.planned` = volatility_planned,
      `law.planned` = law_planned,
      
      `min-up-time` = cluster_infos[["min-up-time"]],
      `min-down-time` = cluster_infos[["min-down-time"]],
      spinning = cluster_infos[["spinning"]],
      `marginal-cost` = cluster_infos[["marginal-cost"]],
      `spread-cost` = cluster_infos[["spread-cost"]],
      `startup-cost` = cluster_infos[["startup-cost"]],
      `market-bid-cost` = cluster_infos[["market-bid-cost"]],
      
      prepro_data = data_list[[cluster]], 
      prepro_modulation = modulation_list[[cluster]]
    )
  }

  invisible(opts)
}


#' @importFrom lubridate years
get_clusters_coef <- function(name, clusters_desc, kd_cho, date_study) {
  code_pal <- clusters_desc[corresp_groupes == name, c(code_palier)]
  coefkd_week <- kd_cho[code_palier %in% code_pal, list(week = n_sem_annee, abat_rso, kidispo_hqe)]
  
  coefkd_week <- merge(x = coefkd_week, y = build_weekcal(), all.x = TRUE, all.y = FALSE)
  
  coefkd_week <- coefkd_week[rep(seq_len(.N), each = 7)]
  coefkd_week[, num_seq := seq_len(.N) - 1, by = week]
  coefkd_week[, week_start := week_start + num_seq]
  coefkd_week <- coefkd_week[, list(date = week_start, abat_rso, kidispo_hqe)]
  coefkd_week <- coefkd_week[date >= as.Date(date_study) & date < as.Date(date_study) + lubridate::years(1)]
  coefkd_week[]
}

