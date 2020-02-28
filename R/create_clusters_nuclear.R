
#' Create nuclear clusters
#'
#' @param calendar Calendar data read with \code{\link{read_calendar}}.
#' @param clusters_desc Clusters / groups description read with \code{\link{read_cluster_desc}}.
#' @param kd_cho Kd coefficients read with \code{\link{read_kd_cho}}.
#' @param start_date Starting date of the study, if \code{NULL} (default),
#'  the date will be retrieve from the Antares study.
#' @param area_name Name of the area where to create clusters.
#' @param law_planned Law to use in Antares.
#' @param volatility_planned Volatility for the law.
#' @param constraints Stretch/Zircaloy constraints read with
#'   \code{\link{read_constraints}}. Defaults to NULL.
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
create_clusters_nuclear <- function(calendar, clusters_desc, kd_cho, start_date = NULL, area_name = NULL, 
                                    law_planned = "geometric", volatility_planned = 1, constraints = NULL,
                                    opts = simOptions()) {
  
  if (is.null(start_date))
    start_date <- format(opts$start, format = "%Y-%m-%d")
  
  area_name <- get_area_name(area_name)
  
  unique_tranches <- unique(calendar$tranche)
  
  n_days <- if (is_leapyear(opts)) 366 else 365
  
  pb <- progress_bar$new(
    format = "  Preparing modulation data [:bar] :percent",
    total = length(unique_tranches), clear = FALSE
  )
  
  datetime_study <- seq(from = as.POSIXct(start_date, tz = "UTC"), length.out = 8760, by = "1 hour")
  datetime_study_chr <- as.character(datetime_study)
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique_tranches, 
      nm = unique_tranches
    ),
    FUN = function(cluster) {
      pb$tick()
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

        datetime_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_de_fin_sans_prolongation[i] > dat$date_debut[i]) {
              res <- seq(
                from = dat$date_debut[i], 
                to = dat$date_de_fin_sans_prolongation[i] + days(1) - hours(1), 
                by = "1 hour"
              )
              as.character(res)
            }
          }
        )
        
        coef_clus <- get_clusters_coef(cluster, clusters_desc, kd_cho, start_date)
        
        datetime_prolongation <- unlist(datetime_prolongation)
        capacity_modulation <- (!datetime_study_chr %in% datetime_prolongation) * rep(head(coef_clus$abat_rso, 365), each = 24)
        
        if (!is.null(constraints) && cluster %in% constraints$groupe) {
          date_debut <- constraints[groupe == cluster, date_debut]
          date_fin <- constraints[groupe == cluster, date_fin]
          min_gen_modulation <- ifelse(datetime_study >= date_debut & datetime_study < date_fin, 1, 0)
        } else {
          min_gen_modulation <- rep(0, times = 8760 * 1)
        }
        
        matrix(
          data = c(
            rep(1, times = 8760 * 2),
            capacity_modulation,
            min_gen_modulation
          ),
          ncol = 4
        )
      }
    }
  )
  
  pb <- progress_bar$new(
    format = "  Preparing TS data [:bar] :percent",
    total = length(unique_tranches), clear = FALSE
  )

  # Preprop data
  data_list <- lapply(
    X = setNames(
      object = unique_tranches, 
      nm = unique_tranches
    ),
    FUN = function(cluster) {
      pb$tick()
      dat <- calendar[tranche == cluster]
      if (nrow(dat) == 0) {
        matrix(
          data = c(
            rep(1, times = n_days * 2),
            rep(0, times = n_days * 3),
            rep(1, times = n_days * 1)
          ),
          ncol = 6
        )
      } else {
        date_study <- seq(from = as.Date(start_date), length.out = n_days, by = "1 day")
        date_reprise <- which(as.character(date_study) %in% as.character(dat$date_de_fin_sans_prolongation))
        duree_prolongation_mean <- dat$duree_prolongation_mean[as.character(dat$date_de_fin_sans_prolongation) %in% as.character(date_study)]
        res <- matrix(
          data = c(
            rep(7, times = n_days * 1),
            rep(1, times = n_days * 1),
            rep(0, times = n_days * 3),
            rep(1, times = n_days * 1)
          ),
          ncol = 6
        )
        
        date_arret_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_de_fin_sans_prolongation[i] > dat$date_debut[i]) {
              res <- seq(
                from = as.Date(dat$date_debut[i]), 
                # to = as.Date(dat$date_de_fin_avec_prolongation[i]) - days(1), 
                to = as.Date(dat$date_de_fin_sans_prolongation[i]) + days(dat$duree_prolongation_mean[i]) - days(1), 
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
        
        res[, 3] <- head(fo_rate, n = n_days)
        # browser()
        res[date_reprise, 2] <- duree_prolongation_mean
        res[date_reprise, 4] <- 1
        return(res)
      }
    }
  )
  
  pb <- progress_bar$new(
    format = "  Creating nuclear clusters [:bar] :percent",
    total = length(unique_tranches), clear = FALSE
  )
  
  for (cluster in unique_tranches) {
    
    pb$tick()
    
    code_pal <- clusters_desc[corresp_groupes == cluster, c(code_palier)]
    cluster_infos <- descr_clusters(paste0("nuclear_", code_pal))
    
    opts <- createCluster(
      opts = opts,
      area = area_name, 
      cluster_name = stri_replace_all_regex(str = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = TRUE,
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

#' @importFrom data.table setorder
#' @importFrom lubridate years year
get_clusters_coef <- function(name, clusters_desc, kd_cho, date_study) {
  code_pal <- clusters_desc[corresp_groupes == name, c(code_palier)]
  coefkd_week <- kd_cho[code_palier %in% code_pal, list(week = n_sem_annee, abat_rso, kidispo_hqe)]
  
  date_study <- as.Date(date_study)
  coefkd_week <- merge(
    x = coefkd_week,
    y = build_weekcal(start = year(date_study), end = year(date_study) + 2), 
    all.x = TRUE, all.y = FALSE
  )
  
  coefkd_week <- coefkd_week[rep(seq_len(.N), each = 7)]
  coefkd_week[, num_seq := seq_len(.N) - 1, by = week]
  coefkd_week[, week_start := week_start + num_seq]
  coefkd_week <- coefkd_week[, list(date = week_start, abat_rso, kidispo_hqe)]
  coefkd_week <- coefkd_week[date >= date_study & date < date_study + lubridate::years(1)]
  setorder(coefkd_week, date)
  coefkd_week[]
}

