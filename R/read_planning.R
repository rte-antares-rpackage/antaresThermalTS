
#' Read a maintenance planning
#'
#' @param path Character. Path to the Excel file
#' @param sheet Character. Sheet in which the planning can be found. Defaults to 'Planning'.
#' @param start_col Character. Leftmost column of the planning (upper-case letter). Defaults to 'A'.
#' @param start_row Integer. Row number of the planning's header. Defaults to 16.
#'
#' @return A 12-column \code{data.table}: the same 12 columns as in the Excel sheet. Useless rows
#'   are dropped, and values from the first 4 columns are repeated in order to obtain a 'tidy'
#'   dataset.
#'
#' @export
#'
#' @importFrom readxl read_excel anchored
#' @importFrom data.table setDT := .SD
#' @importFrom zoo na.locf
#'
#' @examples
#' \dontrun{
#'
#' mydt <- read_planning("1805_Planning Mensuel GDF SUEZ_REF.xlsx")
#'
#' }
#'
read_planning <- function(path, sheet = "Planning", start_col = "A", start_row = 16) {

  data <- read_excel(
    path = path,
    sheet = sheet, na = c("", "NA"),
    range = anchored(anchor = paste0(start_col, start_row + 1), dim = c(NA, 12)),
    col_names = c("nom_site", "num_tranche", "code_gp", "pcn_mw",
                  "dt_debut_arret", "dt_fin_arret",
                  "dt_debut_corr", "dt_fin_corr", "num_corr",
                  'dt_debut_acc', "dt_fin_acc", "num_acc"),
    col_types = c("text", "numeric", "text", "numeric",
                  "date", "date",
                  "date", "date", "text",
                  "date", "date", "text")
  )
  setDT(data)

  # convert POSIXct columns to Date
  date_cols <- c("dt_debut_arret", "dt_fin_arret",
                 "dt_debut_corr", "dt_fin_corr",
                 "dt_debut_acc", "dt_fin_acc")
  data[, (date_cols) := lapply(X = .SD, FUN = as.Date), .SDcols = date_cols]

  # carry forward values in the first 4 columns
  first_cols <- c("nom_site", "num_tranche", "code_gp", "pcn_mw")
  data[, (first_cols) := lapply(X = .SD, FUN = na.locf, na.rm = FALSE), .SDcols = first_cols]

  # remove empty rows
  data2 <- data[, setdiff(names(data), first_cols), with = FALSE]
  data <- data[rowSums(is.na(data2)) != ncol(data2), ]
  
  # correctif gdf
  new_code_gp <- data.table(
    code_gp = c("GRACIT 1", "FOSCCT1" , "DK6 TAG1", "DK6 TAV1", "DK6 TAG2", "DK6 TAV2", "G.RIVT 1", "FOSCHT2" , "BILHOT"),
    new_code_gp = c("GRACIT 1", "FOSCCT 1" , "DK6 TG1", "DK6 TV1", "DK6 TG2", "DK6 TV2", "G.RIVT 1", "FOSCHT 2" , "BILHOT01")
  )
  
  data[new_code_gp, on = list(nom_site = code_gp), code_gp := new_code_gp]
  data[]
}
