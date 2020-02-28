
#' Create EDF clusters
#'
#' @param planning Calendar data read with \code{\link{read_calendar}}.
#' @param hypothesis Kp coefficients read with \code{\link{read_kp_edf}}.
#' @param start_date Starting date of the study, if \code{NULL} (default),
#'  the date will be retrieve from the Antares study.
#' @param area_name Name of the area where to create clusters.
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
#' @importFrom lubridate hours days as_datetime
#' @importFrom stats setNames
#' @importFrom stringi stri_replace_all_regex
#' @importFrom progress progress_bar
#' @importFrom utils head
create_clusters_edf <- function(planning, hypothesis, start_date = NULL,
                                area_name = NULL, constraints = NULL,
                                opts = simOptions()) {
  
  if (is.null(start_date))
    start_date <- format(opts$start, format = "%Y-%m-%d")
  
  area_name <- get_area_name(area_name)
  
  n_days <- if (is_leapyear(opts)) 366 else 365
  
  planning <- copy(planning)
  planning <- planning[!is.na(code_gp)]
  
  hypothesis <- copy(hypothesis)
  hypothesis <- hypothesis[!is.na(code_gp)]
  
  # remove AGP shutdown
  planning <- planning[type_darret != "AGP"]
  
  unique_code_gp <- unique(planning$code_gp)

  pb <- progress_bar$new(
    format = "  Preparing modulation data [:bar] :percent",
    total = length(unique_code_gp), clear = FALSE
  )
  
  datetime_study <- seq(from = as.POSIXct(start_date, tz = "UTC"), length.out = 8760, by = "1 hour")
  datetime_study_chr <- as.character(datetime_study)
  
  # Modulation data
  modulation_list <- lapply(
    X = setNames(
      object = unique_code_gp, 
      nm = unique_code_gp
    ),
    FUN = function(cluster) {
      pb$tick()
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
        datetime_prolongation <- lapply(
          X = seq_len(nrow(dat)), 
          FUN = function(i) {
            if (dat$date_fin_arret[i] > dat$date_debut[i]) {
              res <- seq(
                from = as_datetime(dat$date_debut[i]), 
                to = dat$date_fin_arret[i] + days(1) - hours(1), 
                by = "1 hour"
              )
              as.character(res)
            }
          }
        )
        
        datetime_prolongation <- unlist(datetime_prolongation)
        capacity_modulation <- (!datetime_study_chr %in% datetime_prolongation) * 1
        
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
    format = "  Preparing prepro data [:bar] :percent",
    total = length(unique_code_gp), clear = FALSE
  )
  
  
  data_list <- lapply(
    X = setNames(
      object = unique_code_gp, 
      nm = unique_code_gp
    ),
    FUN = function(cluster) {
      pb$tick()
      fo_rate <- get_fo_rate_edf(edf = hypothesis, code_groupe = cluster, date_study = start_date)
      fo_rate <- head(fo_rate$kp_value, n_days)
      matrix(
        data = c(
          rep(7, times = n_days),
          rep(1, times = n_days),
          1 - fo_rate,
          rep(0, times = n_days * 2),
          rep(1, times = n_days * 1)
        ),
        ncol = 6
      )
    }
  )
  
  
  pb <- progress_bar$new(
    format = "  Creating thermal clusters [:bar] :percent",
    total = length(unique_code_gp), clear = FALSE
  )
  
  
  for (cluster in unique_code_gp) {
    
    pb$tick()
    
    infos_clus <- planning[code_gp == cluster]
    infos_clus <- unique(infos_clus, by = "code_gp")
    
    cluster_infos <- descr_clusters(infos_clus$name_desc)
    
    opts <- createCluster(
      opts = opts,
      area = area_name, 
      cluster_name = stri_replace_all_regex(str = cluster, pattern = "[^[:alnum:]]", replacement = "_"), 
      add_prefix = TRUE,
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
      
      prepro_data = data_list[[cluster]],
      prepro_modulation = modulation_list[[cluster]]
    )
  }
  
  invisible(opts)
}






#' @importFrom data.table data.table rbindlist melt
#' @importFrom lubridate years
#' @importFrom stringi stri_extract
get_fo_rate_edf <- function(edf, code_groupe, date_study) {
  
  date_debut_etude <- year(date_study)
  
  kp <- c(paste0("kp_", date_debut_etude,"_hors_ete"), 
          paste0("kp_", date_debut_etude, "_ete"), 
          paste0("kp_",date_debut_etude + 1,"_hors_ete"),
          paste0("kp_", date_debut_etude + 1,"_ete"), 
          paste0("kp_", date_debut_etude + 2,"_hors_ete"), 
          paste0("kp_",date_debut_etude + 2,"_ete"))
  
  coresp_kp_week <- lapply(
    X = kp,
    FUN = function(x) {
      YEAR <- stri_extract(str = x, regex = "\\d{4}")
      YEAR <- as.numeric(YEAR)
      WEEKS <- 25:37
      if (grepl(pattern = "hors_ete", x = x)) {
        WEEKS <- (1:52)[-c(25:37)]
        YEAR <- rep(YEAR, length(WEEKS))
        YEAR[(38:52) - length(25:37)] <- YEAR[1] - 1
      } else {
        YEAR <- rep(YEAR, length(WEEKS))
      }
      data.table(week = sprintf("S%02d - %s", WEEKS, YEAR))
    }
  )
  names(coresp_kp_week) <- kp
  coresp_kp_week <- rbindlist(coresp_kp_week, idcol = "kp_period")
  
  
  edf_gp <- unique(edf[code_gp == code_groupe], by = "code_gp")
  
  kp <- c(paste0("kp_", date_debut_etude,"_hors_ete"), 
          paste0("kp_", date_debut_etude, "_ete"), 
          paste0("kp_",date_debut_etude + 1,"_hors_ete"),
          paste0("kp_", date_debut_etude + 1,"_ete"), 
          paste0("kp_", date_debut_etude + 2,"_hors_ete"), 
          paste0("kp_",date_debut_etude + 2,"_ete"))
  edf_gp <- melt(
    data = edf_gp, 
    id.vars = "code_gp",
    measure.vars = kp, 
    variable.factor = FALSE, 
    variable.name = "kp_period",
    value.name = "kp_value"
  )
  edf_gp <- merge(x = coresp_kp_week, y = edf_gp)
  edf_gp <- merge(x = edf_gp, y = build_weekcal(start = date_debut_etude, end = date_debut_etude + 2), by = "week")
  
  edf_gp <- edf_gp[rep(seq_len(.N), each = 7)]
  edf_gp[, num_seq := seq_len(.N) - 1, by = week]
  edf_gp[, date := week_start + num_seq]
  
  edf_gp <- edf_gp[date >= as.Date(date_study) & date < as.Date(date_study) + years(1)]
  
  edf_gp <- edf_gp[order(date), list(date, kp_value)]
  
  edf_gp[, kp_value := as.numeric(kp_value) / 100]
  
  edf_gp[]
}



