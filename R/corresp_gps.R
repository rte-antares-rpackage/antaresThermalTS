
# Correspondance groupe

#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom data.table setDT setnames
corresp_gps <- function() {
  thermal <- system.file("thermaldata/fr_thermal_20190325.xlsx", package = "antaresThermalTS")
  thermal <- read_excel(path = thermal)
  thermal <- clean_names(thermal)
  setDT(thermal)
  setnames(thermal, "name", "groupe")
  setnames(thermal, "code_groupe", "code_gp")
  setnames(thermal, "cluster_bp", "name_desc")
  thermal[, list(groupe, code_gp, name_desc)][]
}
