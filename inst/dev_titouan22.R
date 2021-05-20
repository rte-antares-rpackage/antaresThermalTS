library(data.table)
library(readxl)
path = "C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/packages/data/JeuDonneesPDE_2020_30032020/Macro_import_NUC_mars2020_PP.xlsm"
path_input = "C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/packages/data/JeuDonneesPDE_2020_30032020/"
clus_infos2 <- data.table(read_excel(path = path, sheet = "Caractéristiques", skip = 6))
clus_infos2 <- clus_infos2[!is.na(...1)]
clus_infos2$cut <- ""
out_filliale <-''
for(i in 1:nrow(clus_infos2)){
  if(is.na(clus_infos2[i]$pcn)){
    out_filliale <- clus_infos2[i]$...1
  }
  clus_infos2$cut[i] <- out_filliale
}
clus_infos2 <- clus_infos2[!is.na(pcn)]

name_hypotheses <- "HypothesesRTE_Mars.xlsx"
info_gdf <- read_info(path = file.path(path_input, name_plan_gdf))

clus_infos <- read_excel(path = file.path(path_input, name_plan_gdf), sheet = 2, skip = 6)
clus_infos <- janitor::clean_names(clus_infos)
setDT(clus_infos)

clus_infos <- clus_infos[!is.na(edp_ed_prev)]


setnames(clus_infos2, "...1", "edp_ed_prev")
setnames(clus_infos2, "pcn", "pmax")
setnames(clus_infos2, "...8", "code_gp")


clus_infos <- clus_infos2


# corresp <- rbindlist(list(
#   data.table(
#     edp_ed_prev = c("GRACIT 1", "FOSCCT 1", "DKSOLT 1", "DKDUNT 1", "DKSOLT 2", "DKDUNT 2", "G.RIVT 1", "FOSCHT 2", "BILHOT01"),
#     code_gp =     c("GRACIT 1", "FOSCCT 1" , "DK6 TG1", "DK6 TV1", "DK6 TG2", "DK6 TV2", "G.RIVT 1", "FOSCHT 2" , "BILHOT01")
#   ),
#   data.table(
#     edp_ed_prev = c("EMILE HUCHET 6", "EMILE HUCHET 7", "EMILE HUCHET 8", "PROVENCE 5"),
#     code_gp =     c("E.HUCT 6",       "E.HUCT 7",       "E.HUCT 8",       "PROVET 5")
#   ),
#   data.table(
#     edp_ed_prev = c("PONT SUR SAMBRE", "CROIX DE METZ"),
#     code_gp =     c("SAMBRT1",         "C.ME5T01")
#   ),
#   data.table(
#     edp_ed_prev = c("Morandes"),
#     code_gp =     c("MORANT 1")
#   ),
#   data.table(
#     edp_ed_prev = c("AMFARD 14", "AMFARD 15", "PROVENCE 4B"),
#     code_gp =     c("AMFART14", "AMFART15", "PROVET 4B")
#   )
# ))
# clus_infos <- merge(x = clus_infos, y = corresp, all.x = TRUE, all.y = FALSE)
clus_infos <- unique(clus_infos, by = "edp_ed_prev")
clus_infos <- clus_infos[edp_ed_prev != "EDP"]
clus_infos[, pmin := as.numeric(pmin)]
clus_infos[code_gp == "MORANT 1" & is.na(pmin), pmin := corresp_gps()[code_gp == "MORANT 1", c(pmin)]]

clus_infos <- clus_infos[!is.na(code_gp)]

# # correctif gdf
# new_code_gp <- data.table(
#   code_gp = c("GRACIT 1", "FOSCCT1" , "DK6 TAG1", "DK6 TAV1", "DK6 TAG2", "DK6 TAV2", "G.RIVT 1", "FOSCHT2" , "BILHOT"),
#   new_code_gp = c("GRACIT 1", "FOSCCT 1" , "DK6 TG1", "DK6 TV1", "DK6 TG2", "DK6 TV2", "G.RIVT 1", "FOSCHT 2" , "BILHOT01")
# )
#
# clus_infos[new_code_gp, on = list(code_gp = code_gp), code_gp := new_code_gp]

clus_infos <- merge(
  x = clus_infos, y = corresp_gps()[, list(groupe, code_gp, name_desc)],
  by = "code_gp", all.x = TRUE
)

if(nrow(clus_infos[is.na(name_desc)])>0){
  warning(paste0("filtred following group : ", paste0(clus_infos[is.na(name_desc)]$code_gp, collapse = ","), " please update fr_thermal_20190411.xlsx file inside package"))
}
clus_infos


fwrite(clus_infos, "clus_infos.csv")
