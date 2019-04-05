
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






#' Read Kp coefficents for EDF clusters
#'
#' @param path Path to Excel file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel anchored cell_limits
#' @importFrom janitor clean_names
#' @importFrom data.table setDT setnames
#' @importFrom stringi stri_replace
#'
read_kp_edf <- function(path) {
  kp_edf <- read_excel(
    path = path,
    sheet = "Kp classique", range = anchored("C10", dim = c(NA, 7))
  )
  kp_edf <- clean_names(kp_edf)
  setDT(kp_edf)

  
  clusters_desc <- read_cluster_desc(path = path)
  
  kp_edf <- merge(x = kp_edf, y = clusters_desc[, list(nom, corresp_groupes, pcn_mw, pmin_mw)], by = "nom")
  
  setnames(kp_edf, "nom", "groupe")
  kp_edf[, groupe := stri_replace(str = groupe, regex = "(?<=[:alpha:])(\\d)", replacement = " $1")]
  corresp_gp <- corresp_gps()
  kp_edf <- merge(
    x = kp_edf,
    y = corresp_gp,
    by = "groupe"
  )
  kp_edf[, groupe := stri_replace_all_regex(groupe, "[:space:]", "")]
  
  kp_edf[]
}


