
#' Read Cluster Description
#'
#' @param path Path to Excel file
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom data.table setDT := setnames
#' @importFrom zoo na.locf
#'
#' @examples
#' \dontrun{
#' 
#' cluster_desc <- read_cluster_desc("HypothesesRTE_CHO-4145.xlsx")
#' 
#' }
#' 
read_cluster_desc <- function(path) {
  clusdesc <- read_excel(path = path, sheet = 2, skip = 5)
  clusdesc <- janitor::clean_names(clusdesc)
  setDT(clusdesc)
  
  # column with all missing values
  clusdesc[, x7 := NULL]
  # column with code groups
  setnames(clusdesc, "x8", "corresp_groupes")
  # rows with missing values
  clusdesc <- clusdesc[!is.na(nom)]
  
  # type of groups
  clusdesc[is.na(pcn_mw), type_groupe := nom]
  clusdesc[, type_groupe := zoo::na.locf(type_groupe)]
  
  # wrong corresp
  clusdesc[corresp_groupes == "SSEA2T1", corresp_groupes := "SSEA2T 1"]
  clusdesc[corresp_groupes == "SSEA2T2", corresp_groupes := "SSEA2T 2"]
  # clusdesc[corresp_groupes == "FLAMANVILLE 3", corresp_groupes := "FLAMAT 3"]
  clusdesc <- clusdesc[corresp_groupes != "FLAMANVILLE 3"]
  
  clusdesc <- clusdesc[!is.na(pcn_mw)]
  
  code_palier_dic <- data.table(
    type_groupe = c("Nucl\u00e9aire 1300", "Nucl\u00e9aire 900", "Nucl\u00e9aire N4"),
    code_palier = c("p4", "cp0_cp_cp2", "n4")
  )
  
  clusdesc <- merge(x = clusdesc, y = code_palier_dic, all.x = TRUE)
  
  clusdesc
}
