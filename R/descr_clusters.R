

#' Get cluster description
#'
#' @param name Type of cluster
#'
#' @noRd
descr_clusters <- function(name) {
  descr <- list(
    nuclear_n4 = list(
      `min-up-time` = 168L,
      `min-down-time` = 168L,
      spinning = 0,
      `marginal-cost` = 5.02,
      `spread-cost` = 0.4,
      `startup-cost` = 35599L,
      `market-bid-cost` = 5.02
    ),
    nuclear_p4 = list(
      `min-up-time` = 168L,
      `min-down-time` = 168L,
      spinning = 0,
      `marginal-cost` = 5.02,
      `spread-cost` = 0.4,
      `startup-cost` = 35599L,
      `market-bid-cost` = 5.02
    ),
    nuclear_cp0_cp_cp2 = list(
      `min-up-time` = 168L,
      `min-down-time` = 168L,
      spinning = 0,
      `marginal-cost` = 5.02,
      `spread-cost` = 0.4,
      `startup-cost` = 24435L,
      `market-bid-cost` = 5.02
    ),
    hard_coal_old_1 = list(
      group = "Hard coal",
      `min-up-time` = 8L,
      `min-down-time` = 8L,
      spinning = 0,
      co2 = 0.99,
      `marginal-cost` = 25.73,
      `spread-cost` = 0.4,
      `startup-cost` = 67390,
      `market-bid-cost` = 25.73
    ),
    gas_ccgt_new = list(
      group = "gas",
      `min-down-time` = 2L,
      spinning = 0,
      co2 = 0.35,
      `marginal-cost` = 25.9,
      `spread-cost` = 0.4,
      `startup-cost` = 25923.6,
      `market-bid-cost` = 25.9
    ),
    gas_ocgt_old = list(
      group = "gas",
      spinning = 0,
      co2 = 0.58,
      `marginal-cost` = 42.91,
      `spread-cost` = 0.4,
      `startup-cost` = 10774.9,
      `market-bid-cost` = 42.91
    ),
    gas_ocgt_new = list(
      group = "gas",
      spinning = 0,
      co2 = 0.49,
      `marginal-cost` = 35.76,
      `spread-cost` = 0.4,
      `startup-cost` = 3120.6,
      `market-bid-cost` = 35.76
    ),
    light_oil = list(
      group = "oil",
      `min-up-time` = 3L,
      `min-down-time` = 3L,
      spinning = 0, co2 = 0.78,
      `marginal-cost` = 90.17,
      `spread-cost` = 0.4,
      `startup-cost` = 5160.4,
      `market-bid-cost` = 90.17
    ),
    heavy_oil_old_1 = list(
      group = "oil",
      `min-up-time` = 3L,
      `min-down-time` = 3L,
      spinning = 0,
      co2 = 0.78,
      `marginal-cost` = 90.17,
      `spread-cost` = 0.4,
      `startup-cost` = 93845,
      `market-bid-cost` = 90.17
    ),
    gas_cogen = list(
      group = "gas",
      `min-up-time` = 24L,
      `min-down-time` = 24L,
      spinning = 0,
      co2 = 0.58,
      `marginal-cost` = 42.91,
      `spread-cost` = 0.4,
      `startup-cost` = 248007,
      `market-bid-cost` = 42.91
    )
  )
  
  if (!is.null(name) && name %in% names(descr)) {
    descr[[name]]
  } else {
    warning(paste0("No description found for: ", name), call. = FALSE)
    NULL
  }
}



#' Correspondence between groups and clusters
#'
#' @param code_groupe Code group
#'
#' @noRd
corr_groupe_descr <- function(code_groupe) {
  clus_name_ <- c("nuclear_p4", "nuclear_p4", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_n4", "nuclear_n4", "nuclear_n4", "nuclear_n4", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_p4", "nuclear_p4",
                  "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_p4",
                  "nuclear_p4", "nuclear_p4", "nuclear_p4", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2",
                  "nuclear_cp0_cp_cp2", "nuclear_cp0_cp_cp2", "hard_coal_old_1",
                  "hard_coal_old_1", "hard_coal_old_1", "hard_coal_old_1", "hard_coal_old_1",
                  "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new",
                  "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new",
                  "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new",
                  "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new", "gas_ccgt_new",
                  "gas_ocgt_old", "gas_cogen", "gas_cogen", "heavy_oil_old_1",
                  "heavy_oil_old_1", "heavy_oil_old_1", "heavy_oil_old_1", "heavy_oil_old_1",
                  "heavy_oil_old_1", "heavy_oil_old_1", "heavy_oil_old_1", "light_oil",
                  "light_oil", "gas_ocgt_new", "gas_ocgt_new", "light_oil", "light_oil",
                  "light_oil", "light_oil", "light_oil", "light_oil", "light_oil",
                  "light_oil", "light_oil", "light_oil", "gas_cogen", "gas_cogen",
                  "gas_cogen", "light_oil", "light_oil", "light_oil", "light_oil",
                  "light_oil", "light_oil", "gas_ccgt_new", "gas_ccgt_new")
  code_groupe_ <- c("BVIL7T 1", "BVIL7T 2", "BLAYAT 1", "BLAYAT 2", "BLAYAT 3",
                    "BLAYAT 4", "BUGEYT 2", "BUGEYT 3", "BUGEYT 4", "BUGEYT 5", "CATTET 1",
                    "CATTET 2", "CATTET 3", "CATTET 4", "CHIN2T 1", "CHIN2T 2", "CHIN2T 3",
                    "CHIN2T 4", "CHOO2T 1", "CHOO2T 2", "CIVAUT 1", "CIVAUT 2", "CRUA5T 1",
                    "CRUA5T 2", "CRUA5T 3", "CRUA5T 4", "D.BURT 1", "D.BURT 2", "D.BURT 3",
                    "D.BURT 4", "FESS5T 1", "FESS5T 2", "FLAMAT 1", "FLAMAT 2", "GOLF5T 1",
                    "GOLF5T 2", "GRAV5T 1", "GRAV5T 2", "GRAV5T 3", "GRAV5T 4", "GRAV5T 5",
                    "GRAV5T 6", "N.SE5T 1", "N.SE5T 2", "PALUET 1", "PALUET 2", "PALUET 3",
                    "PALUET 4", "PENLYT 1", "PENLYT 2", "SSAL7T 1", "SSAL7T 2", "SSEA2T 1",
                    "SSEA2T 2", "TRICAT 1", "TRICAT 2", "TRICAT 3", "TRICAT 4", "CORD5T 4",
                    "CORD5T 5", "HAVRET 4", "PROVET 5", "E.HUCT 6", "DK6 TAG1", "DK6 TAG2",
                    "BOUCHT 7", "M.PONT 5", "M.PONT 6", "", "", "G.RIVT 1", "FOSCCT1",
                    "BLENOT 5", "GRACIT 1", "E.HUCT 7", "E.HUCT 8", "C.ME5T01", "SAMBRT1",
                    "MORANT 1", "GENN3T 1", "AMFART14", "AMFART15", "CORD5T 2", "CORD5T 3",
                    "PORC2T 1", "PORC2T 2", "PORC2T 3", "PORC2T 4", "", "", "MTERFT 6",
                    "MTERFT 5", "MTERGT 6", "MTERGT 5", "VAIR6T 1", "VAIR6T 2", "VAIR6T 3",
                    "ARRI5T 1", "ARRI5T 2", "DIRINT 1", "DIRINT 2", "BRENNT 1", "BRENNT 2",
                    "BRENNT 3", "BILHOT", "FOSCHT2", "CALAIT1", "DFDC1T 1", "DFDC2T 1",
                    "DFDCOT 1", "DFELDT 1", "DFEGST 1", "DDBZHT 1", "DK6 TAV1", "DK6 TAV2"
  )
  
  if (!is.null(code_groupe) && code_groupe %in% code_groupe_) {
    clus_name_[which(code_groupe_ == code_groupe)]
  } else {
    warning(paste0("No correspondence found for: ", code_groupe), call. = FALSE)
    NULL
  }
}




