
#' Create an (empty) Antares study for Extended Shutdown Simulation
#'
#' @param path Path to an Antares study or to a directory for creating one.
#' @param start_date Starting date for the study.
#' @param study_name Name of the study.
#' @param area_name Name of the area to create
#'
#' @export
#'
#' @importFrom antaresEditObject createStudy updateGeneralSettings createArea
#' @importFrom antaresRead setSimulationPath getAreas
#' @importFrom lubridate month wday year as_date
setup_study <- function(path, start_date = "2018-07-01", study_name = "prolongation-arrets", area_name = "area") {
  
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
    opts = opts
  )
  
  options("antaresThermalTS.area_name" = area_name)
  if (!area_name %in% getAreas()) {
    opts <- createArea(name = area_name, opts = opts)
  }

  invisible(opts)
}







