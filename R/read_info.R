
#' Read other clusters informations
#'
#' @param path Path to file.
#'
#' @return a \code{data.table}
#' @export
#' 
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom data.table setDT data.table rbindlist
#'
read_info <- function(path) {
  clus_infos <- read_excel(path = path, sheet = 2, skip = 6)
  clus_infos <- janitor::clean_names(clus_infos)
  setDT(clus_infos)
  clus_infos <- clus_infos[!is.na(edp_ed_prev)]
  
  corresp <- rbindlist(list(
    data.table(
      edp_ed_prev = c("GRACIT 1", "FOSCCT 1", "DKSOLT 1", "DKDUNT 1", "DKSOLT 2", "DKDUNT 2", "G.RIVT 1", "FOSCHT 2", "BILHOT01"),
      code_gp =     c("GRACIT 1", "FOSCCT1" , "DK6 TAG1", "DK6 TAV1", "DK6 TAG2", "DK6 TAV2", "G.RIVT 1", "FOSCHT2" , "BILHOT")
    ),
    data.table(
      edp_ed_prev = c("EMILE HUCHET 6", "EMILE HUCHET 7", "EMILE HUCHET 8", "PROVENCE 5"),
      code_gp =     c("E.HUCT 6",       "E.HUCT 7",       "E.HUCT 8",       "PROVET 5")
    ),
    data.table(
      edp_ed_prev = c("PONT SUR SAMBRE", "CROIX DE METZ"),
      code_gp =     c("SAMBRT1",         "C.ME5T01")
    ),
    data.table(
      edp_ed_prev = c("Morandes"),
      code_gp =     c("MORANT 1")
    )
  ))
  clus_infos <- merge(x = clus_infos, y = corresp, all.x = TRUE, all.y = FALSE)
  clus_infos <- unique(clus_infos, by = "edp_ed_prev")
  clus_infos <- clus_infos[edp_ed_prev != "EDP"]
  clus_infos[code_gp == "MORANT 1" & is.na(pmin), pmin := 190]
  
  clus_infos <- clus_infos[!is.na(code_gp)]
  
  # correctif gdf
  new_code_gp <- data.table(
    code_gp = c("GRACIT 1", "FOSCCT1" , "DK6 TAG1", "DK6 TAV1", "DK6 TAG2", "DK6 TAV2", "G.RIVT 1", "FOSCHT2" , "BILHOT"),
    new_code_gp = c("GRACIT 1", "FOSCCT 1" , "DK6 TG1", "DK6 TV1", "DK6 TG2", "DK6 TV2", "G.RIVT 1", "FOSCHT 2" , "BILHOT01")
  )
  
  clus_infos[new_code_gp, on = list(code_gp = code_gp), code_gp := new_code_gp]
  
  clus_infos <- merge(x = clus_infos, y = corresp_gps(), by = "code_gp", all.x = TRUE)
  
  clus_infos[]
}
