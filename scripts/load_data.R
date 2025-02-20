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


####### FOR NOW ####


#For now, Just merge CCA, gorgonian, date, site, relief, substrate, lat/lon, and depth data for Maggie S. ####

#Lat/Lon site
PISCO_lat_lon_site_16_23 <- unique(PISCO_event_data_only[survey_year >= 2016,.(site, latitude, longitude)])

#Only latitudes in SCB
PISCO_lat_lon_site_16_23_SCB <- PISCO_lat_lon_site_16_23[latitude < 34.5099177] #South of Jalama Beach County Park, just north of Pt. Conception

#Export as CSV
fwrite(PISCO_lat_lon_site_16_23_SCB, file = file.path("data","keys","PISCO_lat_lon_site_16_23_SCB.csv"))

#Add 0s (For each year, month, dat, site, zone, transect, if no value, set as 0)
#This means that if there's only CCA observed in 1 year, it doesn't get a biased high value for percent cover
#All identifying columns
ID_col <- colnames(PISCO_upc_data_only[,c(3:11)])

#Unique transects
Unique_UPC_transects <- unique(PISCO_upc_data_only[,..ID_col])

#From UPC, only extract CCA (Crustose Corraline Algae) percent cover from 2016 onward ####
PISCO_CCA_UPC <- PISCO_upc_data_only[classcode == "CRUCOR",]

#Unique transects with CCA identified
Unique_UPC_transects_wCCA <- unique(PISCO_CCA_UPC[,..ID_col])

#Only keep rows that do not have CCA
rows_no_CCA <- data.table::fsetdiff(Unique_UPC_transects, Unique_UPC_transects_wCCA)

#Add CCA count and pct_cov of 0
rows_no_CCA[,count := 0][,pct_cov := 0][,classcode := "CRUCOR"][,species_definition := "Coralline Algae -Crustose"][,category := "COVER"]

#Reorder new columns
rows_to_add <- rows_no_CCA[,.(classcode,species_definition, campus, method, survey_year, year, month, day,site, zone, transect, category, count, pct_cov)]

#Rbind
PISCO_CCA_UPC_0s <- rbind(PISCO_CCA_UPC,rows_to_add, fill = T)

#Limit to during and after 2016
PISCO_CCA_UPC_16_23 <- PISCO_CCA_UPC_0s[survey_year >= 2016]

#Export as CSV including year and zones
fwrite(PISCO_CCA_UPC_16_23, file = file.path("data","processed","PISCO_CCA_UPC_16_23.csv"))

#Take site x zone averages from 2016-2023
PISCO_CCA_UPC_sitezone_averages <- PISCO_CCA_UPC_16_23[,.(avg_pct_cov = mean(pct_cov)),.(classcode, species_definition, campus, method, site, zone)]

#Export summarized file 
#Export as CSV including year and zones
fwrite(PISCO_CCA_UPC_sitezone_averages, file = file.path("data","processed","PISCO_CCA_UPC_sitezone_averages.csv"))

#Relief and substrate ####
#From UPC, only relief and substrate from 2016 onward
PISCO_relief_substrate_UPC <- PISCO_upc_data_only[classcode %in% c("FLATREL", "HIREL", "MODREL", "SLTREL","BEDRK","BOULD","COB","SAND"),]
PISCO_relief_substrate_UPC_16_23 <- PISCO_relief_substrate_UPC[survey_year >= 2016,]

#Export as CSV
fwrite(PISCO_relief_substrate_UPC_16_23, file = file.path("data","processed","PISCO_relief_substrate_UPC_16_23.csv"))

#Swath ####
#Add 0s (For each year, month, dat, site, zone, transect, if no value, set as 0)
#This means that if there's only CCA observed in 1 year, it doesn't get a biased high value for percent cover
#All identifying columns
ID_col_swath <- colnames(PISCO_swath_data_only[,c(3:11)])

#Unique transects
Unique_Swath_transects <- unique(PISCO_swath_data_only[,..ID_col_swath])

#From Swath, only gorgonian density ####
gorgonian_spp <- unique(MLPA_kelpforest_taxon_table[grepl("Gorgonian",common_name) == T,classcode])
gorgonian_spp_def <- unique(MLPA_kelpforest_taxon_table[grepl("Gorgonian",common_name) == T, species_definition])

PISCO_gorgonian_swath <- PISCO_swath_data_only[classcode %in% gorgonian_spp] #Only gorgonians

#Identify 0 count rows that need to be added for all 5 gorgonian classcodes
rows_to_add <- data.table()
for(i in 1:length(gorgonian_spp)){
  #Unique transects with gorgonian classcode
  Unique_Swath_transects_wgorgonian <- unique(PISCO_gorgonian_swath[classcode == gorgonian_spp[i],..ID_col_swath])
  
  #Only keep rows that do not have specific gorgonian species
  rows_no_gorgonians <- data.table::fsetdiff(Unique_Swath_transects, Unique_Swath_transects_wgorgonian)
  
  #Add count of 0
  rows_no_gorgonians[,count := 0][,classcode := gorgonian_spp[i]][,species_definition := gorgonian_spp_def[i]]
  
  #Reorder new columns
  rows_to_add_single <- rows_no_gorgonians[,.(classcode,species_definition, campus, method, survey_year, year, month, day,site, zone, transect,count)]
  
  #Rbind with empty dt
  rows_to_add <- rbind(rows_to_add, rows_to_add_single)
}


#Rbind
PISCO_gorgonian_swath_0s <- rbind(PISCO_gorgonian_swath,rows_to_add, fill = T)

#Limit to during and after 2016
PISCO_gorgonian_swath_16_23 <- PISCO_gorgonian_swath_0s[survey_year >= 2016]


#Export gorgonian data as csv
fwrite(PISCO_gorgonian_swath_16_23, file = file.path("data","processed","PISCO_gorgonian_swath_16_23.csv"))

#From Swath, only macrocystis ####
PISCO_macrocystis_swath <- PISCO_swath_data_only[classcode %in% c("MACPYRHF","MACPYRAD")] #Note, PISCO only has MACPYRAD (no data on holdfasts in swath)

#Identify 0 count rows that need to be added for MACPYRAD macrocystis classcode

  #Unique transects with macrocystis classcode
  Unique_Swath_transects_wmacrocystis <- unique(PISCO_macrocystis_swath[classcode == "MACPYRAD",..ID_col_swath])
  
  #Only keep rows that do not have specific gorgonian species
  rows_no_macro <- data.table::fsetdiff(Unique_Swath_transects, Unique_Swath_transects_wmacrocystis)
  
  #Add count of 0
  rows_no_macro[,count := 0][,size := 0][,classcode := "MACPYRAD"][,species_definition := "Macrocystis pyrifera"]
  
  #Reorder new columns
  rows_to_add <- rows_no_macro[,.(classcode,species_definition, campus, method, survey_year, year, month, day,site, zone, transect,count, size)]

#Rbind
PISCO_macrocystis_swath_0s <- rbind(PISCO_macrocystis_swath,rows_to_add, fill = T)

#Limit to during and after 2016
PISCO_macrocystis_swath_16_23 <- PISCO_macrocystis_swath_0s[survey_year >= 2016]

#Export macrocystis data as csv
fwrite(PISCO_macrocystis_swath_16_23, file = file.path("data","processed","PISCO_macrocystis_swath_16_23.csv"))


#From swath AND upc data, extract site depths
PISCO_upc_site_zone_depth_date <- unique(PISCO_upc_data_only[survey_year >= 2016,.(survey_year, site, zone, depth)])
PISCO_swath_site_zone_depth_date <- unique(PISCO_swath_data_only[survey_year >= 2016,.(survey_year, site, zone, depth)])

#Just in case either upc or swath recorded and the other did not
PISCO_site_zone_depth_date <- unique(rbind(PISCO_upc_site_zone_depth_date, PISCO_swath_site_zone_depth_date))

#Summarize depth by site and zone
PISCO_site_zone_depth <- PISCO_site_zone_depth_date[,.(depth = round(mean(depth, na.rm = T),1)),.(site, zone)]

fwrite(PISCO_site_zone_depth, file = file.path("data","processed","PISCO_site_zone_depth.csv"))

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
