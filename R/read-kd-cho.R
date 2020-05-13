
#' Read sheet "Kd CHO" from RTE hypothesis
#'
#' @param path Path to Excel file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @name read-kd-cho
#' 
#' @importFrom utils packageVersion
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom data.table setDT setnames melt := 
#' @importFrom stringi stri_replace_all_charclass
#'
read_kd_cho <- function(path) {
  kd_cho <- readxl::read_excel(path = path, sheet = "Kd CHO", skip = 8)
  kd_cho <- janitor::clean_names(kd_cho)
  setDT(kd_cho)
  setnames(x = kd_cho, old = c("x1", "x2", "x3"), new = c("n_days", "year", "n_week"))
  kd_cho[, (names(kd_cho)) := lapply(.SD, function(x) {
    if (all(is.na(x))) {
      return(NULL)
    } else {
      x
    }
  }), .SDcols = names(kd_cho)]
  kd_cho_long <- melt(
    data = kd_cho, 
    id = 1:4, 
    measure = patterns(kif = "^kif", kistretch = "^ki_stretch", kienv = "^kienv", kihiver = "^kihiver", kibouclage = "^ki_bouclage"), 
    variable.factor = FALSE, 
    variable.name = "palier"
  )
  kd_cho_long[palier == "1", `:=`(palier = "Palier 900 MW", code_palier = "cp0_cp_cp2")]
  kd_cho_long[palier == "2", `:=`(palier = "Palier 1300 MW", code_palier = "p4")]
  kd_cho_long[palier == "3", `:=`(palier = "Palier N4", code_palier = "n4")]
  kd_cho_long[, n_days := stringi::stri_replace_all_charclass(str = n_days, pattern = "[:space:]", replacement = "")]
  kd_cho_long[, date := as.Date(as.numeric(n_days), origin = "1960-01-01")]
  kd_cho_long
}


#' @rdname read-kd-cho
#' @export
read_kd_cho_macro <- function(path) {
  
  kd_cho <- read_excel(path = path, sheet = "Kd CHO", skip = 4, n_max = 105)
  
  kd_cho <- janitor::clean_names(kd_cho)
  setDT(kd_cho)
  kd_cho[, (names(kd_cho)) := lapply(.SD, function(x) {
    if (all(is.na(x))) {
      return(NULL)
    } else {
      x
    }
  }), .SDcols = names(kd_cho)]

  
  kd_cho_long <- melt(
    data = kd_cho, 
    id = 1, 
    measure = patterns(abat_rso = "^abat_rso", kidispo_hqe = "^k_dispo_hqe"
    ), 
    variable.factor = FALSE, 
    variable.name = "palier"
  )
  
  kd_cho_long[palier == "1", `:=`(palier = "Palier 900 MW", code_palier = "cp0_cp_cp2")]
  kd_cho_long[palier == "2", `:=`(palier = "Palier 1300 MW", code_palier = "p4")]
  kd_cho_long[palier == "3", `:=`(palier = "Palier N4", code_palier = "n4")]

  kd_cho_long[is.na(abat_rso), abat_rso := 1]
  kd_cho_long[abat_rso > 1 | abat_rso < 0, abat_rso := 1]
  
  kd_cho_long[]
}




