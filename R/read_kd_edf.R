
#' Read Kd coefficents for EDF clusters
#'
#' @param path Path to Excel file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel anchored cell_limits
#' @importFrom janitor clean_names
#' @importFrom data.table setDT setnames
#'
read_kd_edf <- function(path) {
  
  infos_edf <- read_excel(path = path, sheet = "Planning EDF", range = anchored("A2", dim = c(NA, 9)))
  infos_edf <- clean_names(infos_edf)
  setDT(infos_edf)
  
  infos_edf <- merge(x = infos_edf, y = corresp_gps())
  infos_edf <- infos_edf[type_darret != "AGP"]
  
  kd_edf <- read_excel(path = path, sheet = "Kdispo", range = cell_limits(c(5, 1), c(NA, 12)))
  kd_edf <- clean_names(kd_edf)
  setDT(kd_edf)
  kd_edf <- kd_edf[!is.na(code)]
  setnames(kd_edf, "x1", "code_gp")
  
  merge(x = kd_edf, y = infos_edf, by = "code_gp", all = FALSE)
  
}
