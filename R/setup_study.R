
#' Create an (empty) Antares study for Extended Shutdown Simulation
#'
#' @param path Path to an Antares study or to a directory for creating one.
#' @param start_date Starting date for the study.
#' @param study_name Name of the study.
#' @param area_name Name of the area to create or use.
#' @param keep_clusters For an existing study, a character vector of clusters to keep, all others will be removed.
#' @param nb_ts_thermal Number of thermal timeseries to set.
#'
#' @export
#'
#' @importFrom antaresEditObject createStudy updateGeneralSettings createArea removeCluster updateInputSettings
#' @importFrom antaresRead setSimulationPath getAreas readClusterDesc
#' @importFrom lubridate month wday year as_date leap_year
setup_study <- function(path, start_date = "2018-07-01", 
                        study_name = "prolongation-arrets", 
                        area_name = "fr",
                        keep_clusters = "fr_dsr_long",
                        nb_ts_thermal = 1000) {
  
  if (!dir.exists(path)) {
    createStudy(path = path, study_name = study_name)
  }
  
  suppressWarnings({
    opts <- setSimulationPath(path = path, simulation = "input")
  })
  
  start_date <- as_date(start_date)
  
  first.month.in.year <- tolower(month(start_date, label = TRUE, abbr = FALSE, locale = "English"))
  january.1st <- wday(as_date(paste0(year(start_date) + 1, "-01-01")), label = TRUE, abbr = FALSE, locale = "English")
  
  opts <- updateGeneralSettings(
    first.month.in.year = as.character(first.month.in.year),
    first.weekday = "Sunday",
    january.1st = as.character(january.1st),
    horizon = paste(year(start_date), year(start_date) + 1, sep = "-"), 
    leapyear = leap_year(year(start_date) + 1),
    opts = opts
  )
  
  options("antaresThermalTS.area_name" = area_name)
  if (!area_name %in% getAreas()) {
    opts <- createArea(name = area_name, opts = opts)
  }
  
  clus <- try(readClusterDesc(), silent = TRUE)
  if (!inherits(clus, "try-error") && length(clus) > 0) {
    clus <- clus[area == area_name, as.character(cluster)]
    clus <- setdiff(clus, keep_clusters)
    for (i in clus) {
      opts <- removeCluster(area = area_name, cluster_name = clus, add_prefix = FALSE, opts = opts)
    }
  }
  
  opts <- updateGeneralSettings(
    generate = "thermal",
    nbtimeseriesthermal = nb_ts_thermal,
    refreshtimeseries = "",
    readonly = FALSE
  )
  
  opts <- updateInputSettings(
    import = "thermal"
  )

  invisible(opts)
}







