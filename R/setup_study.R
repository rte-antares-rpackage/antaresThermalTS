
#' Create an (empty) Antares study for Extended Shutdown Simulation
#'
#' @param path Path to a directory where to create study.
#' @param study_name Name of the study.
#' @param area_name Name of the area to create
#'
#' @export
#'
#' @importFrom antaresEditObject createStudy updateGeneralSettings createArea
#' @importFrom antaresRead setSimulationPath
setup_study <- function(path, study_name = "prolongation-arrets", area_name = "area") {
  createStudy(path = path, study_name = study_name)
  
  opts <- setSimulationPath(path = path, simulation = "input")
  
  opts <- updateGeneralSettings(
    first.month.in.year = "july",
    first.weekday = "Sunday",
    horizon = "2018-2019", 
    opts = opts
  )
  
  opts <- createArea(name = area_name, opts = opts)
  invisible(opts)
}







