## Ce script permet de cr?er les time series des groupes nucl?aires et thermiques dans Antares ##

rm(list=ls())
#.libPaths("E:/libraryR3.5")

library(data.table)
library(antaresThermalTS)
library(lubridate)
library(antaresEditObject)
library(readxl)
library(janitor)
library(zoo)
library(stringr)
library(stringi)
library(Jmisc)
library(progress)

# sourceAll("R/")

# Lecture donnees ---------------------------------------------------------

# Chemin pour acceder aux fichiers
path_input <- "C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/packages/data/JeuDonneesPDE_2020_30032020"

#Fichiers hypotheses
name_hypotheses <- "HypothesesRTE_Mars.xlsx"

#Fichiers groupes nucleaires
name_kd_cho <- "Macro__NUC_new.xlsm"
name_planning_nuc <- "REF_Planning_5_ans_mars_2020.xlsx"
name_contraintes <- "PDE_Contraintes_Groupes.xlsx"

#Fichiers groupes THF
name_plan_edf <- "PLANNING_RTE_032020.xlsx"
name_plan_gdf <- "2004_Planning Mensuel GDF SUEZ.xlsx"
name_plan_uniper <- "Planning_UFRP_0520_REF.xlsx"
name_plan_pss <- "PONT_SUR_SAMBRE_POWER_2020 04.XLSX"
name_plan_de  <- "GPMENSUEL_DIRECT ENERGIE_04_2020.xlsx"
name_plan_total  <- "GPMENSUELTOTAL 2020 05_REF.XLSX"
correspondance_filiere_cluster_path  <- "correspondance_filiere_cluster.xlsx"
start_date <- "2020-04-01"
#POUR LE PDH :
# 1er samedi de la semaine 27 pour le BP = 1er Juillet
# 1er samedi de la semaine 27 pour les plannings = 29 Juin
# autrement dit, les donnees BP commencent un samedi et le vrai 29 juin 2019 est un samedi (pour coherence avec plannings)


# Data --------------------------------------------------------------------

#Lecture de caracteristiques techniques de groupes EDF
clusters_desc <- read_cluster_desc(file.path(path_input, name_hypotheses))


#Lecture du planning d'arret des groupes nucleaires
calendar <- read_calendar(path = file.path(path_input, name_planning_nuc))
calendar <- calendar[tranche != "FLAMAT 3"]

#Lecture des coefficients de groupes nucleaires
kd_cho <- read_kd_cho_macro(file.path(path_input, name_kd_cho))


#Lecture de planning d'arret des groupes THF :

#EDF :
plan_edf <- read_planning_rte(path = file.path(path_input, name_plan_edf), clusters_desc = clusters_desc)
#Lecture des coefficients de groupes THF EDF
hypothesis <- read_kp_edf(path = file.path(path_input, name_hypotheses))
constraints <- read_constraints(path = file.path(path_input, name_contraintes))

#ENGIE :
plan_gdf <- read_planning(path = file.path(path_input, name_plan_gdf ))
info_gdf <- read_info(path = file.path(path_input, name_plan_gdf))

#UNIPER :
plan_uniper <- read_planning(path = file.path(path_input, name_plan_uniper))
info_uniper <- read_info(path = file.path(path_input, name_plan_uniper))

#PSS :
plan_pss <- read_planning(path = file.path(path_input, name_plan_pss))
info_pss <- read_info(path = file.path(path_input, name_plan_pss))


#DIRECT ENERGIE :
plan_de <- read_planning(path = file.path(path_input, name_plan_de))
info_de <- read_info(path = file.path(path_input, name_plan_de))


#TOTAL :
plan_total <- read_planning(path = file.path(path_input, name_plan_total))
info_total <- read_info(path = file.path(path_input, name_plan_total))

#Recopilation d'infos de groupes THF (hors EDF)
plans <- rbindlist(list(plan_gdf, plan_uniper, plan_pss, plan_de, plan_total))
infos <- rbindlist(list(info_gdf, info_uniper, info_pss, info_de, info_total))



# Preparation de l'etude --------------------------------------------------

## Il faut cr?er une ?tude Antares vide pour g?n?rer les TS
antaresEditObject::createStudy("C:/Users/TitouanRobert/Desktop/", "empty_study")
setup_study(path = "C:/Users/TitouanRobert/Desktop/empty_study", area_name = "FR", start_date = start_date, keep_clusters = "")



opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/empty_study")
# Creation clusters ---------------------------------------------------
optsBP <- setSimulationPath("C:/Users/TitouanRobert/Desktop/antaresStd - Copie/")
correspondance_filiere_cluster <- data.table(read_excel(file.path(path_input, correspondance_filiere_cluster_path)))


# Creation clusters nucleaires ---------------------------------------------------
create_clusters_nuclear(
  calendar = calendar,
  clusters_desc = clusters_desc,
  correspondance_filiere_cluster = correspondance_filiere_cluster,
  kd_cho = kd_cho,
  area_name = "FR",
  start_date = start_date,
  constraints = constraints,
  opts_bp = optsBP,
  opts = opts
)


# calendar = calendar
# clusters_desc = clusters_desc
# kd_cho = kd_cho
# start_date = start_date
# area_name = "FR"
# law_planned = "geometric"
# volatility_planned = 1
# constraints = constraints
# opts = simOptions()
# correspondance_filiere_cluster = correspondance_filiere_cluster

# Creation clusters THF EDF ---------------------------------------------------

create_clusters_edf(
  planning = plan_edf,
  hypothesis = hypothesis,
  start_date = start_date,
  area_name = "FR",
  constraints = constraints,
  correspondance_filiere_cluster = correspondance_filiere_cluster,
  opts_bp = optsBP,
  opts = opts
)


# Creation clusters Autres Producteurs ---------------------------------------------------

create_clusters_other(
  planning = plans,
  infos = infos,
  hypothesis = hypothesis,
  start_date = start_date,
  area_name = "FR",
  constraints = constraints,
  correspondance_filiere_cluster = correspondance_filiere_cluster,
  opts_bp = optsBP,
  opts = opts
)

