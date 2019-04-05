
#' Read Rte planning
#'
#' @param path Path to file.
#' @param clusters_desc Clusters description.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom readxl read_excel cell_limits
#' @importFrom janitor clean_names
#' @importFrom data.table setDT copy :=
#' @importFrom stringi stri_replace_all_regex stri_replace
read_planning_rte <- function(path, clusters_desc) {
  plan_rte <- read_excel(path = path, sheet = 1, range = cell_limits(c(8, 5), c(NA, NA)))
  plan_rte <- janitor::clean_names(plan_rte)
  setDT(plan_rte)
  plan_rte <- plan_rte[, list(groupe, date_debut, type_darret, date_fin_arret)]
  plan_rte[, groupe := stri_replace(str = groupe, regex = "(?<=[:alpha:])(\\d)", replacement = " $1")]
  corresp_gp <- corresp_gps()
  plan_rte <- merge(
    x = plan_rte,
    y = corresp_gp,
    by = "groupe"
  )
  plan_rte[, groupe := stri_replace_all_regex(groupe, "[:space:]", "")]
  
  clusters_desc <- copy(clusters_desc)
  clusters_desc[, nom := stri_replace_all_regex(nom, "[:space:]", "")]
  
  plan_rte <- merge(
    x = plan_rte, all.x = TRUE, all.y = FALSE, by = "groupe",
    y = clusters_desc[, list(groupe = nom, pcn_mw, pmin_mw)]
  )
  plan_rte[, `:=`(pcn_mw = as.numeric(pcn_mw), pmin_mw = as.numeric(pmin_mw))]
  plan_rte[]
}


