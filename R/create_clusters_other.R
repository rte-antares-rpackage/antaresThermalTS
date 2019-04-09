
#' Create other clusters
#'
#' @param planning Calendar data read with \code{\link{read_calendar}}.
#' @param infos Info about clusters read with \code{\link{read_info}}.
#' @param hypothesis Kp coefficients read with \code{\link{read_kp_edf}}. If not \code{NULL}, used to compute FO rate.
#' @param start_date Starting date of the study, if \code{NULL} (default),
#'  the date will be retrieve from the Antares study.
#' @param area_name Name of the area where to create clusters.
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
#' @importFrom progress progress_bar
#' @importFrom utils head
create_clusters_other <- function(planning, infos, hypothesis = NULL, start_date = NULL, area_name = NULL, opts = simOptions()) {
  
  if (is.null(start_date))
    start_date <- format(opts$start, format = "%Y-%m-%d")
  
  area_name <- get_area_name(area_name)
  
  n_days <- if (is_leapyear(opts)) 366 else 365
  
  planning <- copy(planning)
  planning[is.na(code_gp), code_gp := nom_site]
  
  planning[nom_site == "PONT SUR SAMBRE", code_gp := "SAMBRT1"]
  planning[nom_site == "CROIX DE METZ", code_gp := "C.ME5T01"]

  
  infos[is.na(`for`), `for` := 1]
  
  infos[, pmax := as.numeric(pmax)]
  infos[is.na(pmax), pmax := 0]
  
  unique_code_gp <- unique(intersect(planning$code_gp, infos$code_gp))
  code_gp_rm <- setdiff(union(planning$code_gp, infos$code_gp), unique_code_gp)
  if (length(code_gp_rm) > 0) {
    warning(paste(
      "These clusters have been removed:", paste(code_gp_rm, collapse = ", "), "(not on both planning & info files)"
    ), call. = FALSE)
  }
  
  pb <- progress_bar$new(
    format = "  Preparing modulation data [:bar] :percent",
    total = length(unique_code_gp), clear = FALSE
  )
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique_code_gp, 
      nm = unique_code_gp
    ),
    FUN = function(cluster) {
      pb$tick()
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
        datetime_study <- seq(from = as.POSIXct(start_date, tz = "UTC"), length.out = 8760, by = "1 hour")
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
  
  
  pb <- progress_bar$new(
    format = "  Creating thermal clusters [:bar] :percent",
    total = length(unique_code_gp), clear = FALSE
  )
  
  
  if (!is.null(hypothesis)) {
    cols_kp <- grep("kp_\\d{4}.*", names(hypothesis), value = TRUE)
    kp <- hypothesis[, lapply(.SD, mean, na.rm = TRUE), by = list(code_gp = name_desc), .SDcols = cols_kp]
  }
  
  
  for (cluster in unique_code_gp) {
    
    pb$tick()
    
    infos_clus <- infos[code_gp == cluster]
    
    cluster_infos <- descr_clusters(infos_clus[["name_desc"]])
    
    if (!is.null(hypothesis) && isTRUE(infos_clus[["name_desc"]] %in% hypothesis$name_desc)) {
      fo_rate <- get_fo_rate_edf(edf = kp, code_groupe = infos_clus[["name_desc"]], date_study = start_date)
      fo_rate <- 1 - head(fo_rate$kp_value, n_days)
    } else {
      # fo_rate <- rep(1 - infos_clus[["for"]], times = 365)
      fo_rate <- rep(0.05, times = n_days)
    }
    
    opts <- createCluster(
      opts = opts,
      area = area_name, 
      cluster_name = stri_replace_all_regex(str = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = TRUE,
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
          rep(7, times = n_days ),
          rep(1, times = n_days),
          fo_rate,
          rep(0, times = n_days * 2),
          rep(1, times = n_days * 1)
        ),
        ncol = 6
      ),
      prepro_modulation = modulation_list[[cluster]]
    )
  }
  
  invisible(opts)
}


