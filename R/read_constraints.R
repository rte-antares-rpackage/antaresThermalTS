
#' Read constraints
#'
#' @param path Path to Excel file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel anchored
#' @importFrom janitor clean_names
#' @importFrom data.table setDT
#'
read_constraints <- function(path) {
  
  constraints <- read_excel(path = path, sheet = "Stretch_Zircaloy", range = anchored("A6", dim = c(NA, 4)))
  constraints <- clean_names(constraints)
  setDT(constraints)
  
  constraints[]
}
