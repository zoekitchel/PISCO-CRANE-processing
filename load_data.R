# AUTHOR: kitchel@oxy.edu

# PURPOSE: Load PISCO/CRANE data from dataone portal ####
#(https://opc.dataone.org/view/doi%3A10.25494%2FP6%2FMLPA_kelpforest.9)
#Downloaded on January 7, 2025 by Kitchel
#Placed 3 files (fish.6, swath.7, and upc.6) in GitIgnore because of large file size

#############################
#Setup ####
#############################
#Load packages
library(data.table)
library(stringdist)
library(vegan)
library(dplyr)

##Load PISCO data from dataone portal ####

#Sites
MLPA_kelpforest_site_table <- fread(file.path("data","raw","resourceMap_MLPA_kelpforest_9","data","MLPA_kelpforest_site_table.6.csv"))

#Taxon
MLPA_kelpforest_taxon_table <- fread(file.path("data","raw","resourceMap_MLPA_kelpforest_9","data","MLPA_kelpforest_taxon_table.7.csv"))
PISCO_taxa_key <- unique(MLPA_kelpforest_taxon_table[,.(classcode, species_definition)])

#Fish
MLPA_kelpforest_fish <- fread(file.path("data","raw","resourceMap_MLPA_kelpforest_9","data","MLPA_kelpforest_fish.6.csv"))
  #Link with full taxonomy
  MLPA_kelpforest_fish <- PISCO_taxa_key[MLPA_kelpforest_fish, on = "classcode"]  
  
#Swath survey sampling
MLPA_kelpforest_swath <- fread(file.path("data","raw","resourceMap_MLPA_kelpforest_9","data","MLPA_kelpforest_swath.7.csv"))
  #Link with full taxonomy
  MLPA_kelpforest_swath <- PISCO_taxa_key[MLPA_kelpforest_swath, on = "classcode"]  
  
#UPC survey sampling (UPC = uniform point count)
MLPA_kelpforest_upc <- fread(file.path("data","raw","resourceMap_MLPA_kelpforest_9","data","MLPA_kelpforest_upc.6.csv"))
  #Link with full taxonomy
  MLPA_kelpforest_upc <- PISCO_taxa_key[MLPA_kelpforest_upc, on = "classcode"]

##Delete observations in PISCO where event is from VRG observations ####
        
PISCO_event_data_only <- MLPA_kelpforest_site_table[grepl("VRG",method) == F,]
PISCO_fish_data_only <- MLPA_kelpforest_fish[grepl("VRG",method) == F,]
PISCO_swath_data_only <- MLPA_kelpforest_swath[grepl("VRG",method) == F,]
PISCO_upc_data_only <- MLPA_kelpforest_upc[grepl("VRG",method) == F,]  

#Reformat to CRANE format so it can go through our cleaning scripts? (maybe here)

  
#Change PISCO UPC substrate classifications from text to numbers
  #FLATREL 0-.1m, HIREL >2m, MODREL 1-2m, SLTREL .1-1m, BEDDRK bedrock, BOULD boulder, COB cobble, SAND sand
  #also adjust superlayers


####### FOR NOW #####

#For now, Just merge CCA, gorgonian, date, site, relief, substrate, lat/lon data for Maggie S. ####

#Lat/Lon site
PISCO_lat_lon_site <- unique(PISCO_event_data_only[,.(site, latitude, longitude)])

#Export as CSV
fwrite(PISCO_lat_lon_site, file = file.path("data","keys","PISCO_lat_lon_site.csv"))

#From UPC, only extract CCA (Crustose Corraline Algae) percent cover from 2016 onward
PISCO_CCA_UPC <- PISCO_upc_data_only[classcode == "CRUCOR",]

#Limit to during and after 2016
PISCO_CCA_UPC_16_23 <- PISCO_CCA_UPC[survey_year >= 2016]

#Export as CSV
fwrite(PISCO_CCA_UPC_16_23, file = file.path("data","processed","PISCO_CCA_UPC_16_23.csv"))

#From UPC, only relief and substrate from 2016 onward
PISCO_relief_substrate_UPC <- PISCO_upc_data_only[classcode %in% c("FLATREL", "HIREL", "MODREL", "SLTREL","BEDRK","BOULD","COB","SAND"),]
PISCO_relief_substrate_UPC_16_23 <- PISCO_relief_substrate_UPC[survey_year >= 2016,]

#Export as CSV
fwrite(PISCO_relief_substrate_UPC_16_23, file = file.path("data","processed","PISCO_relief_substrate_UPC_16_23.csv"))

#From Swath, only gorgonian density
gorgonian_spp <- unique(MLPA_kelpforest_taxon_table[grepl("Gorgonian",common_name) == T,classcode])

PISCO_gorgonian_swath <- PISCO_swath_data_only[classcode %in% gorgonian_spp] #Only gorgonians

#Limit to during and after 2016
PISCO_gorgonian_swath_16_23 <- PISCO_gorgonian_swath[survey_year >= 2016]

#From Swath, only macrocystis
PISCO_macrocystis_swath <- PISCO_swath_data_only[classcode %in% c("MACPYRHF","MACPYRAD")]

#Limit to during and after 2016
PISCO_macrocystis_swath_16_23 <- PISCO_swath_data_only[survey_year >= 2016]

#Export macrocystis data as csv
fwrite(PISCO_macrocystis_swath_16_23, file = file.path("data","processed","PISCO_macrocystis_swath_16_23.csv"))




#Matching site names (if needed)

#VRG site names
dat_event[,site_match := tolower(Site)][,site_match := gsub("- ","",site_match)][,site_match := gsub("'","",site_match)]

#PISCO
MLPA_kelpforest_site_table_6[,site_match := gsub("_", " ", tolower(site))][,
                                                                           site_match := gsub(" e$", " east", site_match)][,
                                                                                                                           site_match := gsub(" w$", " west", site_match)][,
                                                                                                                                                                           site_match := gsub(" s$", " south", site_match)][,
                                                                                                                                                                                                                            site_match := gsub(" n$", " north", site_match)][,
                                                                                                                                                                                                                                                                             site_match := gsub(" cen$", " central", site_match)]
