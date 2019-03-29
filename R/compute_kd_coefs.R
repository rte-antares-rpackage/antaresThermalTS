
#' Compute Kd coefficients
#'
#' @param kipr Kipr coefficient from \code{\link{compute_kipr}}.
#' @param kivt Kivt coefficient from \code{\link{compute_kivt}}.
#' @param kd_cho Kd coefficients read with \code{\link{read_kd_cho}}.
#'
#' @return a \code{data.table}
#' @export
#'
#' @importFrom data.table copy := setorder data.table
compute_kd_coefs <- function(kipr, kivt, kd_cho) {
  kipr <- copy(kipr)
  kivt <- copy(kivt)
  kd_cho <- copy(kd_cho)
  code_palier_dic <- data.table(
    type_groupe = c("Nucl\u00e9aire 1300", "Nucl\u00e9aire 900", "Nucl\u00e9aire N4"),
    code_palier = c("p4", "cp0_cp_cp2", "n4")
  )
  kipr <- merge(
    x = kipr[, .SD, .SDcols = c("week", "week_start", "week_end", "type_groupe", "kipr")],
    y = code_palier_dic, by = "type_groupe"
  )
  kipr_kivt <- merge(
    x = kipr,
    y = kivt[, .SD, .SDcols = c("week", "type_groupe", "kivt")],
    by = c("week", "type_groupe")
  )
  kipr_kivt[, type_groupe := NULL]
  
  kd_coefs <- merge(
    x = kipr_kivt,
    y = kd_cho[, list(week = n_sem_annee, kif, kistretch, kienv, kihiver, kibouclage, code_palier, palier)],
    by = c("week", "code_palier")
  )
  setorder(kd_coefs, week_start, code_palier)
  kd_coefs[]
}

