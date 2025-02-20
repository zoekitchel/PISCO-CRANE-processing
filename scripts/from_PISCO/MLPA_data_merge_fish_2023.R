### THIS CODE WILL BRING IN FISH DATA FROM HSU, UCSC, UCSB, AND VRG DATA IN 'FINAL FORM', MERGE THEM, AND CREATE DERIVED DATASETS WITH SITE MEAN DENSITY, BIOMASS 
### ORIGNAL CODE CREATED:  MARCH 2020, KATIE DAVIS KOEHN
### CODE MAJOR UPDATES: FEBRUARY 2021, AVREY PARSONS-FIELD
### CODE MINOR UPDATES: June 2021 to output biomass in kg/transect (per 60m2)
### Update Oct 2021- decision was made to remove "not looked for" spp from fish dataset before output of MLPA_kelpforest_fish base dataset which is the version shared publicly.
### CODE MINOR UPDATES: MARCH 2024, KATELIN SEETO 


#Run this code each year and save a new copy with the current year in the filename in order to maintain archive of all previous code

#Notes on the location of data:
#PISCO data (UCSC and UCSB) are located in G:\Shared drives\PISCO Subtidal UCSB and UCSC\PISCO subtidal data clearinghouse
#This is so that datasets are not duplicated across multiple locations- i.e. all analyses for any PISCO project that query the data do so from this same location
#UCSC and UCSB data that are already combined are referred to as PISCO
#HSU and VRG data are located in G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Final data by sampling group

#ACTION- Each new year, update the year throughout this file using 'find and replace'

#clear global environment at the start of every session
rm(list=ls())

#load packages
library(plyr)
library(dplyr)
library(googlesheets4)
library(reshape2)
library(tidyr)
library(janitor)

####################################################################################
#Bring in PISCO data
####################################################################################
#set working directory for bringing in PISCO data 
#for windows
setwd("G:/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")
#for mac
#setwd("/Volumes/GoogleDrive/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")

#bring in UCSC/UCSB combined dataset
PISCO_fish <- read.csv(file="PISCO_FISH.csv")
#importing sets 'transect' to character, so convert it back to factor
PISCO_fish$transect <- as.factor(PISCO_fish$transect)
#Rename year to survey_year
PISCO_fish$survey_year <- PISCO_fish$year
#format method to character and rename "SBTL_FISH" to "SBTL_FISH_PISCO"
PISCO_fish$method <- as.character(PISCO_fish$method)
PISCO_fish$method <- if_else(PISCO_fish$method == "SBTL_FISH", "SBTL_FISH_PISCO", PISCO_fish$method)
PISCO_fish$method <- as.factor(PISCO_fish$method)

##Remove Oregon data.
PISCO_fish <- subset(PISCO_fish, method != "SBTL_FISH_OREGON")

##Remove UCSB lobster data 
PISCO_fish <- subset(PISCO_fish, classcode != "PANITNT")


####################################################################################
#Bring in HSU and VRG data
####################################################################################
#set working directory for bringing in HSU and VRG data 
#for windows
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Final data by sampling group")
#for mac
#setwd("/Volumes/GoogleDrive/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")

#bring in HSU dataset
HSU_fish <- read.csv(file="HSU_FISH_2014-2023.csv")
#reset transect from character to factor
HSU_fish$transect <- as.factor(HSU_fish$transect)

#bring in VRG dataset
VRG_fish <- read.csv(file="VRG_FISH_2004-2023.csv")

#reset transect from character to factor
VRG_fish$transect <- as.factor(VRG_fish$transect)


####################################################################################
#Run checks, summaries and make conversions
####################################################################################

#generate list of sites for each group, merge and check for inconsistencies in site naming
PISCO_sites <- as.data.frame(unique(PISCO_fish$site))
names(PISCO_sites)[names(PISCO_sites) == "unique(PISCO_fish$site)"] <- "site" 
PISCO_sites$group <- "PISCO"
HSU_sites <- as.data.frame(unique(HSU_fish$site))
names(HSU_sites)[names(HSU_sites) == "unique(HSU_fish$site)"] <- "site"
HSU_sites$group <- "HSU"
VRG_sites <- as.data.frame(unique(VRG_fish$site))
names(VRG_sites)[names(VRG_sites) == "unique(VRG_fish$site)"] <- "site"
VRG_sites$group <- "VRG"

all_sites <- bind_rows(PISCO_sites, HSU_sites, VRG_sites)
all_sites <- all_sites[order(all_sites$site),]

##ACTION - visually inspect 'all_sites". review for site name inconsistencies or redundancies
#On 5-27-22, 'all_sites' has 398 rows
#On 3-27-24, 'all_sites' has 408 rows


#Make sure that 'transect' did not revert to character
HSU_fish$transect <- as.factor(HSU_fish$transect)
VRG_fish$transect <- as.factor(VRG_fish$transect)

#merge datasets
MLPA_fish <- bind_rows(PISCO_fish, HSU_fish, VRG_fish)

#previous step converts factor variables to characters and produces warnings. Convert back to factor variables.
MLPA_fish <-MLPA_fish %>% mutate_if(is.character,as.factor)

#crane fish from SBI in 2004 are duplicated across VRG and UCSB data, need to drop from VRG dataset (should be 390 rows of data removed)
MLPA_fish.2 <- filter(MLPA_fish, !(grepl('SBI', site) & grepl('VRG', campus) & grepl(2004, year)))

#repcheck 
MLPA_fish.grouped <- MLPA_fish.2 %>% group_by(campus, method, survey_year, site, zone, level)
fish_tx_summary <- summarize(MLPA_fish.grouped, n.tx = n_distinct(transect))
fish_tx_summary.transposed <- spread(fish_tx_summary, level, n.tx)
fish_repcheck <- fish_tx_summary.transposed[c("campus", "method", "survey_year", "site", "zone", "BOT", "MID", "CAN", "CNMD")]

##ACTION - visually inspect 'fish_repcheck". This is a full list of replication by site (which is site/side for PISCO)
#In 2021, 'fish_repcheck' has 9064 rows
#On 5-27-22 'fish_repcheck' has 9542 rows
#On 3-27-24 'fish_repcheck' has 11093 rows


#identify sites-years where bottom transects were not equal to 3 or 4. There are many deviations from the standard 3-4 transect/zone site set-up. 
#Transect reps vary from 1 to 14.
fish_repcheck_tx_not_3or4 <- subset(fish_repcheck, BOT < 3 | BOT > 4)

##ACTION - visually inspect 'fish_repcheck_tx_not_3or4"
#In 2021, 'fish_repcheck_tx_not_3or4" has 470 rows
#On 5-27-22, 'fish_repcheck_tx_not_3or4" has 470 rows
#On 3-27-2024 'fish_repcheck_tx_not_3or4" has 471 rows with most recent obs from 2020


#TBD to add- for fish rep check, maybe just confirm that all data past a certain year has correct fish replication. Or does it matter?


##############################
#Convert site names  ###Special note- in future years we plan to move this code to "DataIn" so that all data has consistent site names from the beginning of the process

#Load in the 'master_site_table' in order to convert sitenames to the uniform standard. This also changes some site names that are needed to match up sites with different names with different groups.
#For example UCSB site names in Malibu and at Santa Barbara Island need to be converted to VRG site names because they currently survey those sites.
#We will use VRG site names for all SBI sites, even those that aren't in current monitoring plan.
#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
site.name.conversions <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8/edit?usp=sharing")
#re-name the 'site' column to 'site_name_current', this will be the name we convert to

#SPECIAL NOTE As of July 2021 the decision was made that the official site names in all of the merged datasets will be in PISCO format which is all capitols and underscores

site.name.conversions <- rename(site.name.conversions, site_name_current = site)
#copy the 'site_name_old' column and call it 'site' to match up with the fish data
site.name.conversions$site <- site.name.conversions$site_name_old
#simplify to just the site name variables needed for conversions
site.name.conversions <- site.name.conversions[c("site", "site_name_current", "site_name_old")]


#merge site conversion table into data
MLPA_fish.3 <- left_join(MLPA_fish.2, site.name.conversions)

#for site names that do not need to be converted, in column 'site_name_current' replace the 'NA' with the site name
MLPA_fish.3$site_name_current[is.na(MLPA_fish.3$site_name_current)] <- MLPA_fish.3$site[is.na(MLPA_fish.3$site_name_current)]

#check that the update worked correctly
MLPA_fish_sitescheck <- unique(MLPA_fish.3[c("site", "site_name_current", "site_name_old")])

#ACTION- confirm that 'MLPA_fish_sitecheck' has 404 obs as of 2023 (prior to 2023 this was 396 obs but VRG gave us extra data with 8 new sites to add)

#Site name check
#run a query for site names in the data that do not match the master site table to fix typos or novel site names
#select non-matches, classcodes that appear in dataset that are not in data missing from species list
missing.sites <- MLPA_fish.3[is.na(MLPA_fish.3$site_name_current),]
#list problem site codes
prob.site.codes <- as.data.frame(unique(missing.sites$site))

#to finish off the conversions, drop variable 'site' and rename 'site_name_current' as the 'site'
MLPA_fish.3$site <- NULL
names(MLPA_fish.3)[names(MLPA_fish.3) == "site_name_current"] <- "site"

#set count to 0 when code is NO_ORG
MLPA_fish.3$count <- if_else(MLPA_fish.3$classcode == "NO_ORG", as.integer(0), MLPA_fish.3$count)

#select columns for export of merged dataset
MLPA_kelp_forest_fish <- MLPA_fish.3[c("campus", "method", "survey_year", "year", "month", 
                                       "day", "site", "zone", "level", "transect", "classcode", 
                                       "count", "fish_tl", "min_tl", "max_tl", "sex", "observer", 
                                       "depth", "vis", "temp", "surge", "pctcnpy", "notes", "site_name_old")]


#replace blanks with NA for particular variables with blanks
MLPA_kelp_forest_fish$sex[MLPA_kelp_forest_fish$sex==""] <- NA
MLPA_kelp_forest_fish$observer[MLPA_kelp_forest_fish$observer==""] <- NA
MLPA_kelp_forest_fish$notes[MLPA_kelp_forest_fish$notes==""] <- NA
MLPA_kelp_forest_fish$surge[MLPA_kelp_forest_fish$surge==""] <- NA


#Note that the Master Species table must be updated each year with a new "LOOKED" column for each new year, prior to running this code

#bring in master species table from Google to incorporate 'looked for' info and filter out species not counted consistently or accurately
#You will need to follow steps to authenticate your Google account (make sure to use the same account you have linked with MLPA L-T Kelp Forest Monitoring Project - Admin) and enter an authentification code
#note, this will take a minute to complete
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
fish.species.table <- subset(master.species.table, sample_type == "FISH" & sample_subtype == "FISH")

#Keep "LOOKED" columns, campus, and pisco_classcode. Drop all others to not create duplicate lines during the join. 
fish.species.table.2 <- fish.species.table |> select(campus, pisco_classcode, starts_with("LOOKED"))

#Filter for distinct rows so each code is represented once per campus
#Check that all codes only occur once per campus. If more than one look for differences in LOOKED for columns
code.dupes <- fish.species.table.2 |> 
  distinct() |> 
  get_dupes(c(campus, pisco_classcode))
  
#code.dupes should be 0. If more than 0 check for differences in LOOKED for columns

#Once it is confirmed there are no code dupes and all classcodes have the same Looked for years, condense to one like per campus-classcode combination
fish.species.table.2 <-  fish.species.table.2 |> distinct()

#transpose years
##ACTION - each year you need to add the variable "LOOKEDYYYY" to the next line of code
fish.species.looked <- melt(fish.species.table.2, id.vars = c("campus", "pisco_classcode"), measure.vars = c("LOOKED1999", "LOOKED2000", "LOOKED2001", 
                                                                                                             "LOOKED2002", "LOOKED2003", "LOOKED2004", 
                                                                                                             "LOOKED2005", "LOOKED2006", "LOOKED2007", 
                                                                                                             "LOOKED2008", "LOOKED2009", "LOOKED2010", 
                                                                                                             "LOOKED2011", "LOOKED2012", "LOOKED2013", 
                                                                                                             "LOOKED2014", "LOOKED2015", "LOOKED2016", 
                                                                                                             "LOOKED2017", "LOOKED2018", "LOOKED2019", 
                                                                                                             "LOOKED2020", "LOOKED2021", "LOOKED2022", 
                                                                                                             "LOOKED2023"))
#revalue 'X' in looked for columns to yes and blank values to no
fish.species.looked$value <- revalue(fish.species.looked$value, c("X"="yes"))
fish.species.looked$value[is.na(fish.species.looked$value)] <- "no"

#removed 'LOOKED' so the variable is just the year
fish.species.looked$year <- as.factor(gsub("LOOKED", "", fish.species.looked$variable))
names(fish.species.looked)[names(fish.species.looked) == "value"] <- "LOOKEDFOR"
fish.species.looked$variable <- NULL

#join fish dataset with 'looked for' table to filter out species that were not effectively or consistently looked for in a given year by a given program
#first rename year to survey_year in the looked for table to match with the correct year in the fish table, and pisco_classcode to classcode to match data
names(fish.species.looked)[names(fish.species.looked) == "year"] <- "survey_year"
names(fish.species.looked)[names(fish.species.looked) == "pisco_classcode"] <- "classcode"
#format campus and classcode as factors in looked for table
fish.species.looked$campus <- as.factor(fish.species.looked$campus)
fish.species.looked$classcode <- as.factor(fish.species.looked$classcode)
#format survey_year as factor in fish data prior to merging
MLPA_kelp_forest_fish$survey_year <- as.factor(MLPA_kelp_forest_fish$survey_year)
#join tables
MLPA_kelp_forest_fish.2 <- left_join(MLPA_kelp_forest_fish, fish.species.looked, by=c("survey_year", "campus", "classcode"))
#reformat variables as factors in joined dataset
MLPA_kelp_forest_fish.2$classcode <- as.factor(MLPA_kelp_forest_fish.2$classcode)
MLPA_kelp_forest_fish.2$LOOKEDFOR <- as.factor(MLPA_kelp_forest_fish.2$LOOKEDFOR)


#Check that the 'looked for' column filled correctly
summary(MLPA_kelp_forest_fish.2$LOOKEDFOR)

##ACTION - There should only be 'no' and yes' values, If there are NA's in 'LOOKEDFOR', investigate and fix those until there are zero NA's
lookedforNAs <- MLPA_kelp_forest_fish.2[is.na(MLPA_kelp_forest_fish.2$LOOKEDFOR),]
lookedforNAspp <- as.data.frame(unique(lookedforNAs$classcode))

#Check that the 'looked for' column has NO only for the appropriate cryptic species
not.looked.for <- subset(MLPA_kelp_forest_fish.2, LOOKEDFOR == "no")
not.looked.for.spp <- as.data.frame(unique(not.looked.for$classcode))


##ACTION - review 'not.looked.for.spp' to confirm that list only includes the appropriate cryptic species
#As of 2020 there are 24 species: COTT, CLIN, SYRI, ACOR, OTRI, KSEI, JZON, BLEN, GIBB,GOBI, LIPA, CNUG, LLEP, , CANA, CPUG, NUNI, RNIC, , AHOL, LDAL, LMUC, NSTE, AHAR, GELE, LZEB, 
#As of 5-27-22, there are 12 species: RNIC, COTT, JZON, AHAR, ACOR, CNUG, AHOL, GELE, GOBI, OTRI, LDAL, LZEB
#As of 3-27-24, there are 12 species: RNIC, COTT, JZON, AHAR, ACOR, CNUG, AHOL, GELE, GOBI, OTRI, LDAL, LZEB

################
#Remove cryptic species

#remove the not-looked-for species
MLPA_kelp_forest_fish.3 <- subset(MLPA_kelp_forest_fish.2, LOOKEDFOR == "yes")

####################################################################################
#Export merged "raw" dataset
#contains observation level data for all groups (i.e. not summed to transect or with biomass etc.)

#View a summary of the merged dataset
summary(MLPA_kelp_forest_fish.3)

summary(MLPA_kelp_forest_fish.3$observer)

#KS check the 1 NA in count. Remove this?
countNAs <- MLPA_kelp_forest_fish.3[is.na(MLPA_kelp_forest_fish.3$count),]
TLNAs <- MLPA_kelp_forest_fish.3[is.na(MLPA_kelp_forest_fish.3$fish_tl),]

######################################
#EXPORT- merged dataset

#set working directory for export 
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/MLPA merged datasets")

##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_kelp_forest_fish.3, file="MLPA_kelpforest_fish.csv", row.names=FALSE)
######################################



####################################################################################
#Generate dataset with site mean biomass, density and targeted vs non-targeted biomass
####################################################################################

###Use this if you have run the code above:
#clean up global environment, keeping just the current working dataset (so you can see what you're doing)
rm(list= ls()[!(ls() == 'MLPA_kelp_forest_fish.3')])

###Use this if you are going to start with the file generated in line 193 (i.e. re-creating the dataset with site mean biomass, density and targeted vs non-targeted biomass
#set working directory
#for windows
#setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#for mac
#setwd("/Volumes/GoogleDrive/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#MLPA_kelp_forest_fish.3 <- read.csv(file="MLPA merged datasets/MLPA_kelpforest_fish.csv")


################
#Remove canopy transects etc.

#remove canopy transect data
#Reasoning is that these are not completed on all transects, 
#canopy transects are included in the original version created above "MLPA-kelpforest_fish"
MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.3, level != "CAN")
#remove the site SCI_PELICAN_FAR_WEST, which was a one-off site
MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.4, site != "SCI_PELICAN_FAR_WEST")
#remove half-blind gobies (LCON), 
#decision was made to remove them as UCSC (and UCSB to a lesser extent) sometimes encounters groups in the thousands, and they will inflate density estimates
MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.4, classcode != "LCON")
#remove unidentified fishes from data
MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.4, classcode != "UNID")
#remove black-eyed gobies (not looked for consistently by all groups, only some sizes looked for by UCSC)
MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.4, classcode != "RNIC")


#identify records with missing size and populate with the average size for a species
missing.sizes <- subset(MLPA_kelp_forest_fish.4, is.na(fish_tl) & !classcode %in% c("NO_ORG", "HFRAEGG", "CVENEGG"))
#Should be 99 observations
#export missing sizes to excel- APF will investigate some of these and attempt to fix UCSB ones at least
#un-comment the next line as needed to export the missing sizes for further investigation
#write.csv(missing.sizes, file="Data processing/QAQC temporary files/fish_missing_sizes_to investigate.csv", row.names=FALSE)

#ACTION - investigate excel file with size errors. as of 4-2-21 there are 99 rows
#as of 2-22-21 APF is skipping those fixes, therefore those lines will be deleted in the next step
#HOWEVER, when there is time, DM and APF should investigate those errors and fix them. For example, there are EMOR with count of 75- that is obviously the size entered in count



#review the list of species with missing sizes which will therefore have NA values moving forward
missing.sizes.spp <- as.data.frame(unique(missing.sizes$classcode))

#calculate mean size by species for all species from the entire dataset
MLPA_fish_size_group <- MLPA_kelp_forest_fish.4 %>% group_by(classcode)
MLPA_fish_size_mean <- summarize(MLPA_fish_size_group, mean_tl = mean(fish_tl, na.rm = TRUE))
#round size mean
MLPA_fish_size_mean$mean_tl <- round(MLPA_fish_size_mean$mean_tl, 0)

#populate missing sizes with mean length from MLPA_fish_size_mean
#join mean length into data by classcode
MLPA_kelp_forest_fish.4.1 <- left_join(MLPA_kelp_forest_fish.4, MLPA_fish_size_mean, by="classcode")
#replace missing sizes with mean sizes
MLPA_kelp_forest_fish.4.1$fish_tl <- if_else(is.na(MLPA_kelp_forest_fish.4.1$fish_tl), MLPA_kelp_forest_fish.4.1$mean_tl, MLPA_kelp_forest_fish.4.1$fish_tl)
#convert NaN in fish_tl back to NA
MLPA_kelp_forest_fish.4.1$fish_tl[is.nan(MLPA_kelp_forest_fish.4.1$fish_tl)] <- NA

#check again for missing sizes
missing.sizes.2 <- subset(MLPA_kelp_forest_fish.4.1, is.na(fish_tl) & !classcode %in% c("NO_ORG", "HFRAEGG", "CVENEGG"))

##ACTION - confirm 'missing.sizes.2' has 0 obs

#if all worked correctly, rename dataset 4.1 to 4 to go back into current workflow
MLPA_kelp_forest_fish.4 <- MLPA_kelp_forest_fish.4.1
#remove mean_tl
MLPA_kelp_forest_fish.4$mean_tl <- NULL
#count number of obs that should have no sizes (NO_ORG and shark eggs only)
nonsized_obs <- MLPA_kelp_forest_fish.4 |> 
  filter(classcode %in% c("NO_ORG", "HFRAEGG", "CVENEGG")) |> 
  nrow()
#double check fish_tl for structure (will still be some NAs for NO_ORG transects and shark eggs)
summary(MLPA_kelp_forest_fish.4$fish_tl)

##ACTION - review summary of variable 'fish_tl' The NA's should be the total count of nonsized_obs


#check for records with missing counts - in 2008 there is an observation from UCSC of a group of spiny dogfish with no count information
missing.counts <- subset(MLPA_kelp_forest_fish.4, is.na(count))

##ACTION - review 'missing.counts' and fix 
#APF 2-22-21. the only record with missing count is a group of dogfish from UCSC 2008. No way to fix this, so that line will be deleted below


#No way to fix missing count for 2008 spiny dogfish record, therefore remove from the dataset
#MLPA_kelp_forest_fish.4 <- subset(MLPA_kelp_forest_fish.4, !is.na(count))

#in 2021 we decided to rename the life history table to "spp_attribute_table

#bring in spp_attribut_table"
MLPA_life_history_table_fishes <- read_sheet("https://docs.google.com/spreadsheets/d/1h-gAvQWT3sw1PjEau4dI0r31a1w7qUnAx6PaqC_Yk4Q/edit?usp=sharing")
#This file found in G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables


#This is the original life history table which is no longer in use
#MLPA_life_history_table_fishes <- read_sheet("https://docs.google.com/spreadsheets/d/1OsGt4m4n247TTvRzAa1TQB0FrMsBR9Vaa7TdtW_vXQs/edit#gid=258316809")
#This file found in G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables


#remove fecundity variables for now - add back later when they are up-to-date
MLPA_life_history_table_fishes$F_Type_toupdate <- NULL
MLPA_life_history_table_fishes$F_a_toupdate <- NULL
MLPA_life_history_table_fishes$F_b_toupdate <- NULL
MLPA_life_history_table_fishes$F_L_units_toupdate <- NULL
MLPA_life_history_table_fishes$F_input_length_toupdate <- NULL
MLPA_life_history_table_fishes$L_conversion_toupdate <- NULL
MLPA_life_history_table_fishes$Description_toupdate <- NULL
MLPA_life_history_table_fishes$Output.units_toupdate <- NULL
MLPA_life_history_table_fishes$F_Citation_toupdate <- NULL
MLPA_life_history_table_fishes$source_fecundity_toupdate <- NULL
MLPA_life_history_table_fishes$fecundity_notes_toupdate <- NULL

#rename classcode as pisco_classcode in fish table for joining with life history table
names(MLPA_kelp_forest_fish.4)[names(MLPA_kelp_forest_fish.4) == "classcode"] <- "pisco_classcode"

#join fish data with life history table
MLPA_kelp_forest_fish.5 <- left_join(MLPA_kelp_forest_fish.4, MLPA_life_history_table_fishes, by="pisco_classcode")

#make sure all classcodes matched
fish_LH_mismatch <- subset(MLPA_kelp_forest_fish.5, is.na(common_name))
missing.codes <- as.data.frame(unique(fish_LH_mismatch$pisco_classcode))


##ACTION - review 'missing.codes'. Any novel species will need to be added to the spreadsheet "MLPA_kelpforest_fishes_life_history_table"
#The only missing code should be NO_ORG, HFRAEGG, and CVENEGG
#that table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables

#Only novel species found 2-22-21 APF was HSPI brown irish lord. APF added to the life history table
#However not all information was researched (not enough time) populated biomass info with SGUT info

#convert characters to factors
MLPA_kelp_forest_fish.5 <-MLPA_kelp_forest_fish.5 %>% mutate_if(is.character, as.factor)

################
#calculate fish weight from length-weight and length-length functions with unit conversions when necessary

##functions from Jeremy Claisse's biomass conversion code, see script: WLC_complete_2_Oct_2013_Claisse.r for full code
### create weight length relationship function
WLC<- function (a,length,b) {a*length^b}

### create Length-Length relationship functions
#if conversions$LC_type_for_WL  ==  "TYPICAL"
LC_TYPICAL<-function (a,TL,b) {(a*(TL))+b} #### J. Claisse: changed from original to +b !!!!!!   ### this XL=aTL+b  # XL is for whatever is required for WL equation
#if conversions$LC_type_for_WL  ==  "REVERSE"
LC_REVERSE<-function (a,TL,b) {(TL-b)/a}

#perform length-length conversions using functions defined above by conversion type
MLPA_kelp_forest_fish.5$fish_length_converted <- if_else(MLPA_kelp_forest_fish.5$WL_input_length %in% c("FL", "SL", "SVL") & MLPA_kelp_forest_fish.5$LC_type_for_WL  ==  "TYPICAL", 
                                                        LC_TYPICAL(MLPA_kelp_forest_fish.5$LC.a._for_WL, MLPA_kelp_forest_fish.5$fish_tl, MLPA_kelp_forest_fish.5$LC.b._for_WL), MLPA_kelp_forest_fish.5$fish_tl)
MLPA_kelp_forest_fish.5$fish_length_converted <- if_else(MLPA_kelp_forest_fish.5$WL_input_length %in% c("FL", "SL", "SVL") & MLPA_kelp_forest_fish.5$LC_type_for_WL  ==  "REVERSE", 
                                                        LC_REVERSE(MLPA_kelp_forest_fish.5$LC.a._for_WL, MLPA_kelp_forest_fish.5$fish_tl, MLPA_kelp_forest_fish.5$LC.b._for_WL), MLPA_kelp_forest_fish.5$fish_length_converted)

#convert cm to mm where input units are mm
MLPA_kelp_forest_fish.5$fish_length_converted <- if_else(MLPA_kelp_forest_fish.5$WL_L_units == "mm", MLPA_kelp_forest_fish.5$fish_length_converted*10, MLPA_kelp_forest_fish.5$fish_length_converted)

#calculate fish weight using the lengh conversions just calculated (NOTE:  this is the individual weight of a fish of a given classcode-size. Need to multiply by the count to get the total mass for each row)
MLPA_kelp_forest_fish.5$fish_weight_individual <- WLC(MLPA_kelp_forest_fish.5$WL_a, MLPA_kelp_forest_fish.5$fish_length_converted, MLPA_kelp_forest_fish.5$WL_b)

#convert weights that were output in kg to g
MLPA_kelp_forest_fish.5$fish_weight_individual <- if_else(MLPA_kelp_forest_fish.5$WL_W_units == "kg", MLPA_kelp_forest_fish.5$fish_weight_individual*1000, MLPA_kelp_forest_fish.5$fish_weight_individual)

#assign zeros for fish_weight to transects with no organisms observed
MLPA_kelp_forest_fish.5$fish_weight_individual <- if_else(MLPA_kelp_forest_fish.5$pisco_classcode == "NO_ORG", 0, MLPA_kelp_forest_fish.5$fish_weight_individual)

#find NA values in fish_weight to make sure all observations were computed
missing.weight <- subset(MLPA_kelp_forest_fish.5, is.na(fish_weight_individual))

#ACTION - check variable 'missing.weight'. 9-25-21APF no observations in missing_weight


#plot length (original TL in cm) against fish weight to visualize the curves
plot(MLPA_kelp_forest_fish.5$fish_tl, MLPA_kelp_forest_fish.5$fish_weight_individual)

#ACTION -review plot. TBD need to add notes on what to look for, out-liers etc.



#rename dataset
MLPA_fish_biomass <- MLPA_kelp_forest_fish.5

################
#summarize by transect

#clean up global environment, keeping just the current working dataset (so you can see what you're doing)
rm(list= ls()[!(ls() == 'MLPA_fish_biomass')])

#convert classcodes to characters to convert yoy codes with if_else (problems combining factors with different levels)
MLPA_fish_biomass$pisco_classcode <- as.character(MLPA_fish_biomass$pisco_classcode)
MLPA_fish_biomass$yoy_code <- as.character(MLPA_fish_biomass$yoy_code)

#assign yoy codes to select species using size cut-offs defined in life history table
MLPA_fish_biomass$pisco_classcode.2 <- if_else(!is.na(MLPA_fish_biomass$yoy_code) & MLPA_fish_biomass$yoy_size > MLPA_fish_biomass$fish_tl, MLPA_fish_biomass$yoy_code, "")
#last line was assigning NAs to classcodes without sizes so remove these NAs
MLPA_fish_biomass$pisco_classcode.2[is.na(MLPA_fish_biomass$pisco_classcode.2)] <- ""
#replace blanks in new classcode column with original classcodes, leaving the new yoy codes
MLPA_fish_biomass$pisco_classcode.2 <- if_else(MLPA_fish_biomass$pisco_classcode.2 == "", MLPA_fish_biomass$pisco_classcode, MLPA_fish_biomass$pisco_classcode.2)

#rename pisco_classcode.2 to classcode
names(MLPA_fish_biomass)[names(MLPA_fish_biomass) == "pisco_classcode.2"] <- "classcode"

#convert classcode back to factor
MLPA_fish_biomass$classcode <- as.factor(MLPA_fish_biomass$classcode)

#have a look and make sure no 'NA's remained
summary(MLPA_fish_biomass$classcode)

#ACTION - Confirm from the summary that zero NA's

#have a look at the years of data
summary(MLPA_fish_biomass$survey_year)

#ACTION- confirm that each survey years is represented


#calculate biomass for each row by multiplying counts by fish_weight_individual
MLPA_fish_biomass$row_biomass <- MLPA_fish_biomass$count*MLPA_fish_biomass$fish_weight_individual


###########
# This is where biomass is converted to kg/transect i.e. kg/60m2

#convert biomass from g/transect to kg/transect
MLPA_fish_biomass$row_biomass.kgt <- MLPA_fish_biomass$row_biomass*0.001


#assign legal size when applicable
MLPA_fish_biomass$legal <- if_else(!is.na(MLPA_fish_biomass$legal_size_in) & MLPA_fish_biomass$fish_tl >= MLPA_fish_biomass$legal_size_in*2.54, "1", "")
MLPA_fish_biomass$legal <- if_else(!is.na(MLPA_fish_biomass$legal_size_in) & MLPA_fish_biomass$fish_tl <= MLPA_fish_biomass$legal_size_in*2.54, "0", MLPA_fish_biomass$legal)
MLPA_fish_biomass$legal <- as.factor(MLPA_fish_biomass$legal)
#include NA where not applicable
MLPA_fish_biomass$legal[MLPA_fish_biomass$legal==""] <- NA

#reduce dataset to select columns for summing counts and biomass, transposing, and zero populating
MLPA_fish_biomass_reduced <- MLPA_fish_biomass[c("campus", "method", "survey_year", "year", "month", 
                                       "day", "site", "zone", "level", "transect", "classcode", 
                                       "count", "fish_tl", "observer", "depth", "vis", "surge", "pctcnpy", 
                                       "site_name_old", "genus", "species", "Family", "BroadTrophic", 
                                       "Targeted", "biomass_buster", "row_biomass.kgt")]

#Note: in 2024 KS found duplicate NO_ORG transects for UCSC in 2012 since we drop notes. May want to remove these at a later date? 
#Other dupes were created when dropping size and max/min makes the obs identical to another for that tx
#MLPA_fish_biomass_dupes <- MLPA_fish_biomass_reduced |> get_dupes()
# 1364 obs 4-25-24

######################################
# Export at this stage for a working transect and species level dataset
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/MLPA merged datasets")
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_fish_biomass_reduced, file="MLPA_fish_biomass_density_transect_raw.csv", row.names = FALSE)

######################################



#sum count and biomass for each site by classcode
MLPA_fish_biomass_grouped <- MLPA_fish_biomass_reduced %>% group_by(campus, method, survey_year, site, classcode)
MLPA_fish_biomass_summed <- summarize(MLPA_fish_biomass_grouped, sum.count = sum(count, na.rm=TRUE), sum.biomass = sum(row_biomass.kgt, na.rm=TRUE))

#calculate the number of transects surveyed to divide the summed counts and biomass to get means
#calculate the number of (bottom) transects done for each campus method survey_year site combo
#subset to bottom transects
MLPA_fish_tx_bot <- subset(MLPA_fish_biomass_reduced, level == "BOT")
#count up distinct transects by zone
MLPA_fish_tx_bot_grouped <- MLPA_fish_tx_bot %>% group_by(campus, method, survey_year, site, zone)
bot_tx_zone_summary <- summarize(MLPA_fish_tx_bot_grouped, n.tx = n_distinct(transect))

#next sum transects across zones at the site level
MLPA_fish_tx_bot_grouped.2 <- bot_tx_zone_summary %>% group_by(campus, method, survey_year, site)
MLPA_fish_tx_bot_sum <- summarize(MLPA_fish_tx_bot_grouped.2, n.fish.tx = sum(n.tx))

#join transect count to summarized dataset
MLPA_fish_biomass_summed.2 <- left_join(MLPA_fish_biomass_summed, MLPA_fish_tx_bot_sum)

#divide sum.count and sum.biomass by the number of transects done to get mean biomass per transect
MLPA_fish_biomass_summed.2$mean.count <- MLPA_fish_biomass_summed.2$sum.count/MLPA_fish_biomass_summed.2$n.fish.tx
MLPA_fish_biomass_summed.2$mean.biomass <- MLPA_fish_biomass_summed.2$sum.biomass/MLPA_fish_biomass_summed.2$n.fish.tx

#add density prefix to classcode for transposing density
MLPA_fish_biomass_summed.2$den_classcode <- paste("den", MLPA_fish_biomass_summed.2$classcode, sep = "_")
#add biomass prefix to classcode for transposing biomass
MLPA_fish_biomass_summed.2$bm_classcode <- paste("bm", MLPA_fish_biomass_summed.2$classcode, sep = "_")

#HERE IS WHERE WE TRANSPOSE THE DATA TO GENERATE ZEROS FOR SPECIES NOT OBSERVED AT A SITE/YEAR - THIS ASSUMES THAT ALL SPECIES
#"LOOKED FOR" WERE LOOKED FOR IN EVERY YEAR

#transpose classcodes for zero populating - density and biomass separately
#transpose density
MLPA_fish_density_transpose <- dcast(MLPA_fish_biomass_summed.2, campus + method + survey_year + site ~ den_classcode, value.var = "mean.count")
#zero populate (fill in NAs with zeros)
MLPA_fish_density_transpose[is.na(MLPA_fish_density_transpose)] <- 0

#transpose biomass
MLPA_fish_biomass_transpose <- dcast(MLPA_fish_biomass_summed.2, campus + method + survey_year + site ~ bm_classcode, value.var = "mean.biomass")
#zero populate (fill in NAs with zeros)
MLPA_fish_biomass_transpose[is.na(MLPA_fish_biomass_transpose)] <- 0

#combine density and biomass into one dataset
MLPA_fish_den_bm_means <- join(MLPA_fish_density_transpose, MLPA_fish_biomass_transpose)

#drop NO_ORG from density and biomass columns
MLPA_fish_den_bm_means$den_NO_ORG <- NULL
MLPA_fish_den_bm_means$bm_NO_ORG <- NULL


################
#next summarize mean density and biomass by targeted status, excluding biomass busters

#See MLPA_kelpforest-fishes_life_history_table for which species are classified as "biomass busters"

#DROP BIOMASS BUSTERS, and must keep transects with NO_ORG to include zeros in summaries
MLPA_fish_biomass_nobusters <- subset(MLPA_fish_biomass_reduced, biomass_buster != 1 | classcode == "NO_ORG")

#format targeted as character
MLPA_fish_biomass_nobusters$Targeted <- as.character(MLPA_fish_biomass_nobusters$Targeted)
#assign targeted status to NO_ORG for proper summing (values will be zero but must be assigned)
MLPA_fish_biomass_nobusters$Targeted <- if_else(MLPA_fish_biomass_nobusters$classcode == "NO_ORG", "Targeted", MLPA_fish_biomass_nobusters$Targeted)
#format targeted as factor
MLPA_fish_biomass_nobusters$Targeted <- as.factor(MLPA_fish_biomass_nobusters$Targeted)

#sum up count and biomass for each site by targeted
MLPA_fish_targeted_grouped <- MLPA_fish_biomass_nobusters %>% group_by(campus, method, survey_year, site, Targeted)
MLPA_fish_targeted_summed <- summarize(MLPA_fish_targeted_grouped, sum.count = sum(count, na.rm=TRUE), sum.biomass = sum(row_biomass.kgt, na.rm=TRUE))

#join transect count to summarized dataset
MLPA_fish_targeted_summed.2 <- left_join(MLPA_fish_targeted_summed, MLPA_fish_tx_bot_sum)

#divide sum.count and sum.biomass by the number of transects done to get mean per transect
MLPA_fish_targeted_summed.2$mean.count <- MLPA_fish_targeted_summed.2$sum.count/MLPA_fish_targeted_summed.2$n.fish.tx
MLPA_fish_targeted_summed.2$mean.biomass <- MLPA_fish_targeted_summed.2$sum.biomass/MLPA_fish_targeted_summed.2$n.fish.tx

#transpose targeted/non-targeted for zero populating - density and biomass separately
#transpose density
MLPA_targeted_density_transpose <- dcast(MLPA_fish_targeted_summed.2, campus + method + survey_year + site + n.fish.tx ~ Targeted, value.var = "mean.count")
#zero populate (fill in NAs with zeros)
MLPA_targeted_density_transpose[is.na(MLPA_targeted_density_transpose)] <- 0
#add density prefix to transposed columns
names(MLPA_targeted_density_transpose)[names(MLPA_targeted_density_transpose) == "Targeted"] <- "den_targeted"
names(MLPA_targeted_density_transpose)[names(MLPA_targeted_density_transpose) == "Nontargeted"] <- "den_nontargeted"

#transpose biomass
MLPA_targeted_biomass_transpose <- dcast(MLPA_fish_targeted_summed.2, campus + method + survey_year + site + n.fish.tx ~ Targeted, value.var = "mean.biomass")
#zero populate (fill in NAs with zeros)
MLPA_targeted_biomass_transpose[is.na(MLPA_targeted_biomass_transpose)] <- 0
#add density prefix to transposed columns
names(MLPA_targeted_biomass_transpose)[names(MLPA_targeted_biomass_transpose) == "Targeted"] <- "bm_targeted"
names(MLPA_targeted_biomass_transpose)[names(MLPA_targeted_biomass_transpose) == "Nontargeted"] <- "bm_nontargeted"

#combine density and biomass into one dataset
MLPA_fish_targeted_combined <- join(MLPA_targeted_density_transpose, MLPA_targeted_biomass_transpose)

#join targeted data with species data
MLPA_fish_den_bm_means.2 <- left_join(MLPA_fish_den_bm_means, MLPA_fish_targeted_combined)

##########################################
#Bring in information from the master_site_table

#Load in the 'master_site_table' 
#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
fish_time_series <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8")
#reduce to the variables we need
fish_time_series <- fish_time_series[c("campus","site","latitude", "longitude", "mpa_group", "site_status", "mlpa_region", "time_series_category")]   

#merge the site table info into the fish data (including the timeseries category to be used for filtering out the good timeseries data)
#merge by campus and site to prevent duplication of lines of data when there are two lines for a site in master_site_table (i.e. DEL-MAR_REFERENCE_2)
MLPA_fish_den_bm_means.3 <- join(MLPA_fish_den_bm_means.2, fish_time_series, by=c("campus", "site"))

#reorder columns so that site info moves to the beginning (COLUMN NAMES MAY CHANGE IN FUTURE, NOTE HERE)
MLPA_fish_den_bm_means.4 <- MLPA_fish_den_bm_means.3 %>% select(campus, method, survey_year, site, latitude, longitude, mpa_group, site_status, mlpa_region, time_series_category, n.fish.tx, den_nontargeted, den_targeted, bm_nontargeted, bm_targeted, everything())

#need to flatten the data frame because something came in as a list
MLPA_fish_den_bm_means.5 <- data.frame(lapply(MLPA_fish_den_bm_means.4, as.character), stringsAsFactors=FALSE)


####################################################################################
#Export site means FISH dataset
####################################################################################
#contains site mean level data summed to transect with biomass and targeted designations
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/MLPA merged datasets")
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_fish_den_bm_means.5, file="MLPA_fish_site_means_with_targeted.csv", row.names = FALSE)







