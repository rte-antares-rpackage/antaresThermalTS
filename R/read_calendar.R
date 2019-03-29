
#' Read calendar data
#'
#' @param path Path to a file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom data.table setDT := %chin%
#' @importFrom janitor clean_names
#' @importFrom readxl read_excel
#'
read_calendar <- function(path) {
  dat <- read_excel(path = path, skip = 3)
  dat <- clean_names(dat)
  setDT(dat)
  dat[is.na(duree_prolongation_semaine) & type_darret %chin% c("Essai TCG", "TCG"), duree_prolongation_semaine := 0]
  dat[format(date_debut, "%u") %chin% c("6", "7") & 
        format(date_de_fin_sans_prolongation, "%u") %chin% c("6", "7") & 
        abs(as.numeric(difftime(date_debut, date_de_fin_sans_prolongation, units = "day"))) <= 2, duree_prolongation_semaine := 0]
  dat[is.na(duree_prolongation_semaine), `:=`(duree_prolongation_semaine = 1, date_de_fin_avec_prolongation = date_de_fin_avec_prolongation + 7 * 86400)]
  dat[, duree_prolongation_jour := as.numeric(difftime(date_de_fin_avec_prolongation, date_de_fin_sans_prolongation, units = "day"))]
  dat[, duree_prolongation_mean := as.numeric(duree_prolongation_semaine) * 7 + 1]
  dat[]
}



