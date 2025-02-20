# Overriding Note: In 2024 VANTUNA RESEARCH GROUP  (VRG) provided their entire complete dataset including data collected on other projects.
# Note on YEARS: This code is for building complete VRG datasets with data for 2004, 2007-2023
# APF decided to replace all existing VRG MLPA data with this complete dataset so that any and all edits/corrections that VRG has made over the years will be incorporated
# Therefore, this code brings in the complete dataset, runs check and preps the data to be merged to statewide data set

###ORIGNAL CODE CREATED:  FEBRUARY-MARCH 2020, KATIE DAVIS KOEHN
###CODE UPDATED: February 2024, AVREY PARSONS-FIELD & KATELIN SEETO

#This process mirrors PISCO DataIn procedures and should run all of the same checks/procedures/conversions

#Formatting is designed to match format of 'FINAL' data files
#The process is run separately for each data type (fish, swath, upc, sizefreq) as seen below in the different sections
#This code must be run BEFORE merging data from all campuses with the data merging code 

#See archived versions of this file for previous code
#Each year save as this file with updated year so that previous year's code is archived


#############################################################################################
###   Initial tasks to do prior to running this code- i.e. getting the data files ready   ###
#############################################################################################
# raw data files as sent by Jonathan Williams are found in G:\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Raw data\VRG\original data sent by VRG_2004-2023 complete data set
# DO NOT edit the original files- they are to serve as an archive of exactly what was sent by VRG
# Next copy the files into G:\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Raw data\VRG 
# edit the files as listed below to be used

####################################################
###   Manual Edits to be made to the CSV files   ###
####################################################
# makes these manual changes to the excel csv files
# Location: MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Raw data\VRG

### Event Data ###   (aka SITELOG)  (they use the term 'dive event')
#move "FieldYear' to column A
#rename the file to --> 'VRG_dive_event_2004-2023_raw'

### FISH ###
#move "FieldYear' to column A
#rename the file to --> 'VRG_fish_2004-2023_raw'

### SWATH ###
#rename the file to --> 'VRG_swath_2004-2023_raw'

### UPC ###
# Note that VRG sent two different UPC files- one file for 2004 which is summed to transect, and one file for 2007-2023 which is in VRG point by point (31 points) format
# 2004 UPC data
#rename the file from 'All UPC_PISCO data' to --> 'VRG_upc_2004_raw'
#2007-2023 UPC data
# MIGHT NOT NEED TO DO, file sent 3-18-24 appears to have the correct date format:  # convert SampleDate to be in dd-month-YY format to match DiveEvent (i.e 16-Jun-08)
#rename the file from 'All UPC_VRG data' to --> 'VRG_upc_2007-2023_raw'

### ISC data aka SIZE FREQUENCY ###
#rename the file to  --> 'VRG_sizefreq_YEAR_raw'


########################
###   Load Packages  ###
########################

#ACTION- Each new year, update the year throughout this file using 'find and replace'

#load packages (make sure all packages are installed prior to loading if running for the first time on a specific machine)

library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(googlesheets4)
library(tidyr)
library(janitor)


#clear global environment
rm(list=ls())


################################
##########   SITELOG  ##########
################################

####################################################
###   CHECK- FIND DUPLICATTED LINES IN SITELOG   ###
####################################################
#Need to run this first because sitelog is used in all data types

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Raw data/VRG")
#bring in VRG sitelog (they call it "dive event")  
VRG_dive_event <- read.csv("VRG_dive_event_2004-2023_raw.csv")
#pull out lines where there are duplicates for the variables we use to join by for the different sampling method's data
dive_event_dupes <- VRG_dive_event %>% 
  dplyr::count(FieldYear, Site, SampleDate, SamplingOrganization, DiveReplicate, DepthZone, TransectType) %>% 
  filter(n > 1)


#If there are many lines, uncomment the line below and export the list
#write.csv(Dive_event_dupes, file="QAQC temp files/VRG_diveevent_duplicate_lines.csv")


################################
##########    FISH    ##########
################################

#clear global environment
rm(list=ls())

################################################################
###   Explanation of checks we do not run on VRG fish data   ###
################################################################

# Cannot run "CHECK - size entry where min is larger than max" because VRG fish data only has a single size per line (i.e. no size bins)
# Cannot run "CHECK - Size bins that are too large" because VRG fish data only has a single size per line (i.e. no size bins)
# Cannot run "CHECK - Transect Replication" because we don't necessarily know how many VRG should have

##########################################################################
###   LOAD - FISH, SITELOG, MASTER SPECIES TABLE & MASTER SITE TABLE   ###
##########################################################################

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Raw data/VRG")

#bring in VRG fish data 
VRG_fish <- read.csv("VRG_fish_2004-2023_raw.csv")

#bring in VRG sitelog (they call it "dive event")  
VRG_dive_event <- read.csv("VRG_dive_event_2004-2023_raw.csv")
#make sure that anything that came in as character is changed to factor
VRG_dive_event <- VRG_dive_event %>% mutate_if(is.character,as.factor)
#replace "NR" (not recorded) values with NA for fish and sitelog
VRG_fish[VRG_fish == "NR"] <- NA
VRG_dive_event[VRG_dive_event == "NR"] <- NA

#bring in master species table (file location G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables\master_spp_table.gsheet)
#Use this if you need to reset the google authentication token- it will open a browser window to rerun the google login
#googlesheets4::gs4_auth(email = FALSE)
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
#reduce species table to VRG only
vrg.species.table <- subset(master.species.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.species.table <- vrg.species.table[c("sample_type", "sample_subtype", "pisco_classcode", "orig_classcode", "max_total_length")] 

#bring in master site table (file locationG:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables\master_site_table.gsheet)
master.site.table <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8/edit?usp=sharing")
#reduce site table to VRG only
vrg.site.table <- subset(master.site.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.site.table <- vrg.site.table[c("site", "campus", "site_name_old")] 
#"site_name_old" are the original VRG site names, so rename the column so that it matches up when merging later
vrg.site.table$Site <- vrg.site.table$site_name_old 


#############################
###   CHECK- Site Names   ###
#############################
# run a query for site names in the data that do not match the master site table to fix typos or novel site names
# NOTE this does not convert the site names- it only checks for anomalous site names. The site name conversions happens later
#NOTE: this is the only time that dive_event site names are checked, so be sure to run through this before running swath, UPC & SizeFreq

#Check dive_event site names with master site table
#join site table with dive event, joins by "Site"
site.check_dive_event <- join(VRG_dive_event, vrg.site.table, type = "left")
#select non-matches, site names that appear in dive event that are not listed on the master site table
missing.sites_dive_event <- site.check_dive_event[is.na(site.check_dive_event$site),]
#list problem  codes
prob.site.codes_dive_event <- as.data.frame(unique(missing.sites_dive_event$Site))
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.site.codes, file="QAQC temp files/VRG_dive_event_problemsitecodes.csv")

#Check fish data site names with master site table
#join site table with fish to "import PISCO classcodes"site_name_old"
site.check_fish <- join(VRG_fish, vrg.site.table, type="left")
#select non-matches, classcodes that appear in dataset that are not in data missing from site list
missing.sites_fish <- site.check_fish[is.na(site.check_fish$site),]
#list problem  codes
prob.site.codes_fish <- as.data.frame(unique(missing.sites_fish$Site))
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.site.codes, file="QAQC temp files/VRG_fish_problemsitecodes.csv")


#############################################
###   CHECK- Fish - Sitelog Cross Check   ###
#############################################

#VRG has variable SamplingOrganization, which are subgroups within VRG. We treat them like Observer Name. 
#There are a few crosscheck errors due to this variable but on 2-16-24 JW told APF that they don't care about errors with that, so delete that variable in this check
VRG_fish_temp <- VRG_fish %>% 
  select(!c(SamplingOrganization)) 

#Check for fish data that has no matching dive event record. Pulls out Unmatched records where there is fish data but no corresponding dive event record
fisheventcheck <- anti_join(VRG_fish_temp, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
#summarize mismatches at the transect level
fisheventcheck <- unique(subset(fisheventcheck, select = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType")))

#Check for records in dive event that have no associated fish transects
eventfishcheck <- anti_join(VRG_dive_event, VRG_fish_temp, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
#reduce to only fish data (remove benthic dive events)
eventfishcheck <- subset(eventfishcheck, TransectType %in% c("Canopy", 'Midwater','Bottom'))

#Join the fisheventcheck and eventfishcheck for comparison. 
fishcrosscheck <- join(fisheventcheck, eventfishcheck, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"), type = "full", match = "all")

#ACTION- visually inspect 'fishcrosscheck'. If zero observations then continue. Otherwise correct the errors found and re-run the code above until there are zero 
#Lines with missing dive event info (all NAs) had fish data but no dive event. Lines with dive event data had dive events with no fish data
#use the following line if you want to export to excel for fixing errors
#write.csv(fishcrosscheck, file="QAQC temp files/VRG_fisheventcrosscheck.csv")


##################################################
###   FORMAT - variable names to match PISCO   ###
##################################################

# join fish data with sitelog data (aka dive event) to bring in metadata variables like depth and heading
#'VRG_fish.2' should have the same number of observations as 'VRG_fish'
VRG_fish.2 <- left_join(VRG_fish, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType"))

#format date from character string
VRG_fish.2$Date <- as.Date(VRG_fish.2$SampleDate, format = "%d-%b-%y")
#create year column
VRG_fish.2$year <- year(VRG_fish.2$Date)
#create month column
VRG_fish.2$month <- month(VRG_fish.2$Date)
#create day column
VRG_fish.2$day <- day(VRG_fish.2$Date)
#create campus column
VRG_fish.2$campus <- "VRG"
#create method column
VRG_fish.2$method <- "SBTL_FISH_VRG"
VRG_fish.2$method <- as.factor(VRG_fish.2$method)
#add minimum and maximum size columns and populate with NAs because VRG data doesn't have size bins in the dataset provided
VRG_fish.2$min_tl <- NA
VRG_fish.2$max_tl <- NA

#edit variable names
names(VRG_fish.2)[names(VRG_fish.2) == 'Species'] <- 'orig_classcode'
names(VRG_fish.2)[names(VRG_fish.2) == 'EstimatedLength'] <- 'fish_tl'
names(VRG_fish.2)[names(VRG_fish.2) == 'Abundance'] <- 'count'
names(VRG_fish.2)[names(VRG_fish.2) == 'SamplingOrganization'] <- 'observer'
names(VRG_fish.2)[names(VRG_fish.2) == 'sheephead.sex'] <- 'sex'
names(VRG_fish.2)[names(VRG_fish.2) == 'DiveReplicate'] <- 'transect'
names(VRG_fish.2)[names(VRG_fish.2) == 'Site'] <- 'site'
names(VRG_fish.2)[names(VRG_fish.2) == 'Temperature'] <- 'temp'
names(VRG_fish.2)[names(VRG_fish.2) == 'Visibility'] <- 'vis'
names(VRG_fish.2)[names(VRG_fish.2) == 'SurveyDepth'] <- 'depth'
names(VRG_fish.2)[names(VRG_fish.2) == 'DiveEventComments'] <- 'notes'
names(VRG_fish.2)[names(VRG_fish.2) == 'FieldYear'] <- 'survey_year'

#convert VRG surge values to LIGHT, MODERATE and HIGH. Note that VRG surge is variable Surge, we add variable surge
VRG_fish.2$Surge <- as.factor(VRG_fish.2$Surge)
VRG_fish.2$surge <- revalue(VRG_fish.2$Surge, c("0"="LIGHT", "0.5"="LIGHT", "1"="LIGHT", "1.5"="MODERATE", "2"="MODERATE", "3"="HIGH"))
#convert 'TransectType' to level and convert to BOT, MID, CAN
VRG_fish.2$level <- as.factor(revalue(VRG_fish.2$TransectType, c("Bottom"="BOT", "Midwater"="MID", "Canopy"="CAN")))
#rename zones to match PISCO format 
VRG_fish.2$zone <- revalue(VRG_fish.2$DepthZone, c("Deep"="DEEP", "Inner"="INNER", "Middle"="MID", "Outer Middle"="OUTER MIDDLE", "Outer"="OUTER"))
#rename sex to match PISCO format
VRG_fish.2$sex <- revalue(VRG_fish.2$sex, c("F"="FEMALE", "M"="MALE", "U"= NA))
#change notes to all caps 
VRG_fish.2$notes <- toupper(VRG_fish.2$notes)
#add columns 'sample_type' and 'sample_subtype' to match with species table to be imported next
VRG_fish.2$sample_type <- "FISH"
VRG_fish.2$sample_subtype <- "FISH"

#join species table with fish dataset to import PISCO classcodes
VRG_fish.3 <- join(VRG_fish.2, vrg.species.table, type="left")


#################################
###   CHECK - Species codes   ###
#################################
# Find species code typos or novel species codes

#select NA values resulting from import of species table- these are non-matches, thus are not in the master species list
missing.fish.codes <- VRG_fish.3[is.na(VRG_fish.3$pisco_classcode),]
#list problem fish codes
prob.fish.codes <- as.data.frame(unique(missing.fish.codes$orig_classcode))

#ACTION - check variable 'prob.fish.codes'. if zero observations, continue. If problem code is found, then fix, or contact Avrey about additions to the Master Species Table
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.fish.codes, file="QAQC temp files/VRG_probfishcodes.csv")

####################################################
###   CHECK - REVIEW Larger than Average Sizes   ###
####################################################

#look for large size fish using max total length from the species table (maximum recorded lengths from fishbase)
oversizedfish <- subset(VRG_fish.3, fish_tl > max_total_length)
#export csv to review 
#write.csv(oversizedfish, file=""QAQC temp files/VRG_oversizedfish.csv", row.names = FALSE)

#ACTION- review the large sizes in "oversizedfish". Investigate all lines for entry errors. If size estimates are within reason, leave as is. 
#Confirm with the observer for all observations above the max reported length

#####################################
###   FORMAT - classcode, CNMD,   ###
#####################################

#rename classcode - this converts VRG classcodes to pisco classcodes
names(VRG_fish.3)[names(VRG_fish.3) == 'pisco_classcode'] <- 'classcode'

#create level CNMD for sites where only BOT&MID or BOT&CAN were surveyed within a given year/site on inners transects.
#CNMD can be considered CAN or MID depending on analysis
# VRG_fish.grouped <- VRG_fish.3 %>% group_by(campus, method, survey_year, site, zone, level)
# VRG_tx_sum <- summarize(VRG_fish.grouped, n.tx = n_distinct(transect))
VRG_tx_sum<- VRG_fish.3 %>%
  group_by(campus, method, survey_year, site, zone, level) %>%
  dplyr::summarize(n.tx = n_distinct(transect))
VRG_tx_sum.transposed <- spread(VRG_tx_sum, level, n.tx)
VRG_tx_sum.inners <- subset(VRG_tx_sum.transposed, zone == "INNER")
VRG_in_cnmd <- subset(VRG_tx_sum.inners, is.na(MID) | is.na(CAN))
VRG_in_cnmd$newlevel <- "CNMD"

#join new level CNMD into dataset and replace old level
VRG_fish.4 <- left_join(VRG_fish.3, VRG_in_cnmd)
VRG_fish.4$level <- as.character(VRG_fish.4$level)
VRG_fish.4$newlevel <- as.character(VRG_fish.4$newlevel)
VRG_fish.4$newlevel <- ifelse(VRG_fish.4$level == "BOT", "BOT", VRG_fish.4$newlevel)
VRG_fish.4$newlevel[is.na(VRG_fish.4$newlevel)] <- VRG_fish.4$level[is.na(VRG_fish.4$newlevel)]
VRG_fish.4$newlevel <- as.factor(VRG_fish.4$newlevel)

#remove old level and rename newlevel as level
VRG_fish.4$level <- NULL
names(VRG_fish.4)[names(VRG_fish.4) == 'newlevel'] <- 'level'

#reorder and select columns to match pisco formats
VRG_fish.5 <- VRG_fish.4[c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "level", "transect", 
                           "classcode", "count", "fish_tl", "min_tl", "max_tl", "sex", "observer", "depth", "vis", "temp", "surge", "notes")]


###########################################
###   CHECK - Duplicate lines of data   ###
###########################################

#find duplicate rows of a classcode on each transect, by size

#add dupes variable
VRG_fish.5$dupes <- duplicated(VRG_fish.5[c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "level", "transect", 
                                            "classcode", "count", "fish_tl", "min_tl", "max_tl", "sex", "observer", "depth", "vis", "temp", "surge", "notes")])
#subset out dupes
fish.dupes <- subset(VRG_fish.5, dupes == "TRUE")

#remove dupes variable 
VRG_fish.5$dupes <- NULL

# review fish.dupes and correct errors
#check that all duplicates are real and consolidate them into single lines of data, then re-run until fish.dupes has zero observations
#if lots of errors then export to excel
#write.csv(fish.dupes, file="QAQC temp files/VRG_fishdupes.csv", row.names = FALSE)


############################################################
###   CHECK - FIND TRANSECTS SAMPLED ON DIFFERENT DATES  ###
############################################################

#Pulls out sites that have been sampled more than one time per year
#In 2023 JW changed survey year to be different than SampleDate year so there would be no errors for transects sampled more than one time in a year

VRG_fish.obs <- VRG_fish.5 %>% 
  group_by(survey_year, year, site, zone, level, transect) %>% 
  dplyr::summarize(month.obs = n_distinct(month), day.obs = n_distinct(day), 
                   observer.obs = n_distinct(observer), depth_obs = n_distinct(depth), 
                   vis_obs = n_distinct(vis), temp_obs = n_distinct(temp), 
                   surge_obs = n_distinct(surge)) 

#stack data to search for occurrences with multiple values per transect
VRG_fish.obs.2 <- melt(VRG_fish.obs,  id.vars = c("survey_year", "year", "site", "zone", "transect", "level"), value.name = "n.obs")

#wherever there is more than one value per variable per transect, output observation problems
VRG_fish.obs.probs <- subset(VRG_fish.obs.2, n.obs != 1)

#Pull out which years and sites have duplicate dates sampled 
VRG_fish.obs.probs_byyearsite <- VRG_fish.obs.probs %>% 
  distinct(survey_year, year, site)

#write.csv(VRG_fish.obs.probs_byyearsite, file="QAQC temp files/VRG_fish.obs.probs_byyearsite.csv", row.names = FALSE) 




#ACTION- review 'fish.obs.probs' to find and correct errors 
#Then run code above again until 'fish.obs.probs' has zero observations
#if lots of errors then export to excel
#write.csv(VRG_fish.obs.probs, file="QAQC temp files/VRG_fishobservationproblems.csv", row.names = FALSE)


#########################################
###   CHECK - UNIQUE VARIABLE CHECK   ###
#########################################


#This allows for a visual check of all variables in the dataset. 
#This is the same as visually checking the filters in excel which should have been done prior to creating the final raw file
#list unique values for each variable and inspect
#ACTION- Visually inspect the outputs in the Console to confirm no irregularities
print(sort(unique(VRG_fish.5$survey_year), na.last = FALSE))
print(sort(unique(VRG_fish.5$year), na.last = FALSE))
print(sort(unique(VRG_fish.5$month), na.last = FALSE))
print(sort(unique(VRG_fish.5$day), na.last = FALSE))
print(sort(unique(VRG_fish.5$site), na.last = FALSE))
print(sort(unique(VRG_fish.5$zone), na.last = FALSE))
print(sort(unique(VRG_fish.5$level), na.last = FALSE))
print(sort(unique(VRG_fish.5$transect), na.last = FALSE))
print(sort(unique(VRG_fish.5$classcode), na.last = FALSE))
print(sort(unique(VRG_fish.5$count), na.last = FALSE))
print(sort(unique(VRG_fish.5$fish_tl), na.last = FALSE))
print(sort(unique(VRG_fish.5$min_tl), na.last = FALSE))
print(sort(unique(VRG_fish.5$max_tl), na.last = FALSE))
print(sort(unique(VRG_fish.5$sex), na.last = FALSE))
print(sort(unique(VRG_fish.5$method), na.last = FALSE))
print(sort(unique(VRG_fish.5$observer), na.last = FALSE))
print(sort(unique(VRG_fish.5$depth), na.last = FALSE))
print(sort(unique(VRG_fish.5$vis), na.last = FALSE))
print(sort(unique(VRG_fish.5$surge), na.last = FALSE))
print(sort(unique(VRG_fish.5$notes), na.last = FALSE))

summary(VRG_fish.5)

#################################
###   Export Final data set   ###
#################################
# write to csv without data row numbers
# WARNING Be sure to update the year in the filename below before running!!!
# Reminder to move the previous dataset to the archive folder called "Archive previous data here each time a year is added"

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Final data by sampling group")

# REMINDER- comment out the line below after use!
#write.csv(VRG_fish.5, file="VRG_FISH_2004-2023.csv", row.names=FALSE)


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#################################
##########    SWATH    ##########
#################################

#clear global environment
rm(list=ls())

###############################################################################
###   LOAD VRG SWATH, SITELOG, MASTER SPECIES TABLE and MASTER SITE TABLE   ###
###############################################################################

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Raw data/VRG")

#bring in VRG swath data
VRG_swath <- read.csv("VRG_swath_2004-2023_raw.csv")

#bring in VRG sitelog (they call it "dive event")  
VRG_dive_event <- read.csv("VRG_dive_event_2004-2023_raw.csv")
#IF you get a warning, might need this code to Convert back to factor variables.
#VRG_dive_event <- VRG_dive_event %>% mutate_if(is.character,as.factor)
#replace "NR" (not recorded) values with NA for fish and sitelog
VRG_swath[VRG_swath == "NR"] <- NA
VRG_dive_event[VRG_dive_event == "NR"] <- NA

#bring in master species table (file location G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables\master_spp_table.gsheet)
#Use this if you need to reset the google authentication token- it will open a browser window to rerun the google login
#googlesheets4::gs4_auth(email = FALSE)
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
#reduce species table to VRG only
vrg.species.table <- subset(master.species.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.species.table <- vrg.species.table[c("sample_type", "sample_subtype", "pisco_classcode", "orig_classcode")] 

#bring in master site table (file locationG:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables\master_site_table.gsheet)
master.site.table <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8/edit?usp=sharing")
#reduce site table to VRG only
vrg.site.table <- subset(master.site.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.site.table <- vrg.site.table[c("site", "campus", "site_name_old")] 
vrg.site.table$Site <- vrg.site.table$site_name_old 


#################################################################
###   Explanation of checks we do NOT run on VRG SWATH data   ###
#################################################################

# We want to add as many checks as possible but...
# Do not run "CHECK - Find when metadata is not uniform across each transect" because metadata is listed in DiveEvent, not SWATH entry and we join the two files

###################################
###   CHECK- novel site names   ###
###################################
# run a query for site names in the data that do not match the master site table to fix typos or novel site names
# NOTE this does not convert the site names- it only checks for anomalous site names. The site name conversions happens later
# Note that dive_event site names were checked in the fish section above

#join site table with  dataset to "import PISCO classcodes"site_name_old"
site.check <- join(VRG_swath, vrg.site.table, type="left")
#select non-matches, classcodes that appear in dataset that are not in data missing from species list
missing.sites <- site.check[is.na(site.check$site),]
#list problem site codes
prob.site.codes <- as.data.frame(unique(missing.sites$Site))
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.site.codes, file="QAQC temp files/VRG_problemsitecodes.csv")


###################################
###   CHECK- SWATH vs sitelog   ###
###################################

#VRG has variable SamplingOrganization, which are subgroups within VRG. We treat them like Observer Name. 
#There are a few crosscheck errors due to this variable but on 2-16-24 JW told APF that they don't care about errors with that, so delete that variable before proceeding
VRG_swath_temp <- VRG_swath %>% 
  select(!c(SamplingOrganization)) 

#Check swath data against dive event. Unmatched records where there is swath data but no corresponding dive event record will be identified in 'swatheventcheck'. Make corrections in CLEAN files until this has zero observations.
swatheventcheck <- anti_join(VRG_swath, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
swatheventcheck2 <- unique(subset(swatheventcheck, select = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType")))

#Reverse check for records in event file that have no associated swath transects. Second step restricts the table to swath transects.
eventswathcheck <- anti_join(VRG_dive_event, VRG_swath, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
eventswathcheck2 <- subset(eventswathcheck, TransectType == 'Swath')

#This joins the two checks for comparison. Lines with missing dive event info had swath data but no dive event. Lines with dive event data had dive events with no swath records. Write file to csv to cross check records and fix errors.
swathcrosscheck <- join(swatheventcheck2, eventswathcheck2, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"), type = "full", match = "all")

#ACTION- check the variable 'swathcrosscheck'. If zero observations then continue. Otherwise correct the errors found and re-run the code above until there are zero 
#use the following line if you want to export to excel for fixing errors
#write.csv(swathcrosscheck, file="QAQC temp files/VRG_swatheventcrosscheck.csv")


#################################
###   FORMAT to match PISCO   ###
#################################

#join swath data with dive event. Joined dataframe 'VRG_swath.2' should have the same number of observations as 'the original swath dataframe'VRG_swath'
VRG_swath.2 <- left_join(VRG_swath, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType"))

##Reformat variables to match with PISCO formats
#format date from character string
VRG_swath.2$Date <- as.Date(VRG_swath.2$SampleDate, format = "%d-%b-%y")
#create year column
VRG_swath.2$year <- year(VRG_swath.2$Date)
#create month column
VRG_swath.2$month <- month(VRG_swath.2$Date)
#create day column
VRG_swath.2$day <- day(VRG_swath.2$Date)
#create campus column
VRG_swath.2$campus <- "VRG"
#create method column
VRG_swath.2$method <- "SBTL_SWATH_VRG"

#edit variable names
names(VRG_swath.2)[names(VRG_swath.2) == 'BenthicReefSpecies'] <- 'orig_classcode'
names(VRG_swath.2)[names(VRG_swath.2) == 'Abundance'] <- 'count'
names(VRG_swath.2)[names(VRG_swath.2) == 'SamplingOrganization'] <- 'observer'
names(VRG_swath.2)[names(VRG_swath.2) == 'NumberOfStipes'] <- 'size'
names(VRG_swath.2)[names(VRG_swath.2) == 'DiveReplicate'] <- 'transect'
names(VRG_swath.2)[names(VRG_swath.2) == 'Site'] <- 'site'
names(VRG_swath.2)[names(VRG_swath.2) == 'SurveyDepth'] <- 'depth'
names(VRG_swath.2)[names(VRG_swath.2) == 'DiveEventComments'] <- 'notes'
names(VRG_swath.2)[names(VRG_swath.2) == 'FieldYear'] <- 'survey_year'

#rename zones to match PISCO format
VRG_swath.2$zone <- revalue(VRG_swath.2$DepthZone, c("Deep"="DEEP", "Inner"="INNER", "Middle"="MID", "Outer"="OUTER", "Outer Middle"="OUTER MIDDLE"))

#add columns to swath data to match species table
VRG_swath.2$sample_type <- "SWATH"

#remove some species from swath data that should not be in swath dataset (per communication Katie Davis with Jonathan Williams, Feb 2021)
#In 2024 APF moved this deletion step to use the master species list instead. These non-PISCO sampled species are listed as pisco_classcode = DELETE now so that we have a record of which ones are deleted in the master species table
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Mastocarpus papillatus", "Codium fragile", "Chondracanthus sp")), ]
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Hermissenda opalescens", "Saccharina latissima", "Okenia rosacea")), ]
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Limacia cockerelli", "Desmarestia foliacea", "Crossata ventricosa")), ]
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Desmarestia ligulata var. firma", "Pteria sterna", "Tetilla sp A")), ]
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Terebratalia transversa", "Stylaster californicus", "Alaria marginata")), ]
# VRG_swath.3 = VRG_swath.2[ !(VRG_swath.2$orig_classcode %in% c("Desmarestia dudresnayi tabacoides")), ]


#################################
###   CHECK - Species codes   ###
#################################
#Pull out problem or novel species codes

#join species table with swath dataset to import PISCO classcodes
VRG_swath.3 <- left_join(VRG_swath.2, vrg.species.table)
#select all non-matches, classcodes in data missing from species list
missing.swath.codes <- VRG_swath.3[is.na(VRG_swath.3$pisco_classcode),]
#condense to list of problem swath codes
prob.swath.codes <- as.data.frame(unique(missing.swath.codes$orig_classcode))

#ACTION - check variable 'prob.swath.codes'. if zero observations, continue. If problem code is found, then fix, or contact Dan Malone about additions to the Master Species Table
#if lots of bad codes then export problem SWATH codes to excel_output
#write.csv(prob.swath.codes, file="QAQC temp files/problemswathcodes.csv", row.names = FALSE)
#after fixing problem codes, run code above again until 'prob.swath.codes' has zero observations.


################################################
###   CHECK - FIND DUPLICATE LINES OF DATA   ###
################################################

#change pisco_classcode to 'classcode'
names(VRG_swath.3)[names(VRG_swath.3) == 'pisco_classcode'] <- 'classcode'
#find duplicate rows of classcode on each transect, by size
#VRG_swath.4$dupes <- duplicated(VRG_swath.4[c("year", "month", "day", "site", "zone", "transect", "observer", "Segment", "classcode", "count", "size")])
# #subset out dupes, excluding Macrocystis (stipes counts are often duplicated in data entry for Macrocystis and can be summed later)
# swath.dupes <- subset(VRG_swath.4, dupes == "TRUE" & classcode != "MACPYRAD")

#### Ave check this- I think this may explain why we have dupes showing up that do not appear in the raw data. Maybe an error during the joining with DiveEvent?
VRG_dupes_test <- VRG_swath.3 %>% 
  get_dupes(year, month, day, site, zone, transect, observer, Segment, classcode, count, size)


############################################################
###   FORMAT - sum segments to transect, select columns  ###
############################################################

#group by transect and sum count by classcode (sum across segments)
VRG_swath.4 <- VRG_swath.3 %>% 
  group_by(campus, method, survey_year, year, month, day, site, zone, transect, observer, depth, notes, classcode, size) %>% 
  dplyr::summarize(count = sum(count)) %>% 
  ungroup()
#remove lines with classcode = DELETE
VRG_swath.4 <- VRG_swath.4 %>% 
  filter(classcode != "DELETE")

#reorder and select columns to match pisco formats
VRG_swath.5 <- VRG_swath.4[c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect", "classcode", "count", "size", "depth", "observer", "notes")]

# KS- check this TBD this line may not be needed, but in previous versions we had to convert transect to character
#VRG_swath.5$transect <- as.character(VRG_swath.5$transect)


#########################################
###   CHECK - UNIQUE VARIABLE CHECK   ###
#########################################

#This allows for a visual check of all variables in the dataset. 
#This is the same as visually checking the filters in excel which should have been done prior to creating the SASCleanExcel file
#list unique values for each variable and inspect
#ACTION- Visually inspect the outputs in the Console to confirm no irregularities
print(unique(VRG_swath.5$campus), na.last = FALSE)
print(unique(VRG_swath.5$method), na.last = FALSE)
print(unique(VRG_swath.5$survey_year), na.last = FALSE)
print(sort(unique(VRG_swath.5$year), na.last = FALSE))
print(sort(unique(VRG_swath.5$month), na.last = FALSE))
print(sort(unique(VRG_swath.5$day), na.last = FALSE))
print(sort(unique(VRG_swath.5$site), na.last = FALSE))
print(sort(unique(VRG_swath.5$zone), na.last = FALSE))
print(sort(unique(VRG_swath.5$transect), na.last = FALSE))
print(sort(unique(VRG_swath.5$classcode), na.last = FALSE))
print(sort(unique(VRG_swath.5$count), na.last = FALSE))
print(sort(unique(VRG_swath.5$size), na.last = FALSE))
print(sort(unique(VRG_swath.5$depth), na.last = FALSE))
print(sort(unique(VRG_swath.5$observer), na.last = FALSE))
print(sort(unique(VRG_swath.5$notes), na.last = FALSE))

summary(VRG_swath)

#################################
###   Export Final data set   ###
#################################
# write to csv without data row numbers
# WARNING Be sure to update the year in the filename below before running!!!
# Reminder to move the previous dataset to the archive folder called "Archive previous data here each time a year is added"

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Final data by sampling group")

#REMINDER- comment out the line below after use!
#write.csv(VRG_swath.5, file="VRG_SWATH_2004-2023.csv", row.names=FALSE)



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

################
###   UPC   ####
################

# Important Notes on how to proceed during processing February 2024:
# VRG UPC data for 2007-2023 is in a different format than PISCO- they list every meter point separately and have 31 points. 
# VRG UPC data for 2004 is in PISCO format (remember that VRG complete data set has years 2004, 2007-2023)
# So we will need to bring in the 2007-2023 data, convert it to PISCO style, and then append in the 2004 data.

#clear global environment
rm(list=ls())

###############################################################################
###   LOAD VRG UPC, SITELOG, MASTER SPECIES TABLE and MASTER SITE TABLE   ###
###############################################################################

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Raw data/VRG")

#bring in VRG UPC data (remember there are two files!)
VRG_upc <- read.csv("VRG_upc_2007-2023_raw.csv")  
VRG_upc.2004 <- read.csv("VRG_upc_2004_raw.csv")

#Convert DiveReplicate from an integer to a factor to match with sitelog
VRG_upc$DiveReplicate <- as.factor(VRG_upc$DiveReplicate)
VRG_upc.2004$DiveReplicate <- as.factor(VRG_upc.2004$DiveReplicate)

#bring in VRG sitelog (they call it "dive event")  
VRG_dive_event <- read.csv("VRG_dive_event_2004-2023_raw.csv") 
#IF you get a warning, might need this code to Convert back to factor variables.
#VRG_dive_event <- VRG_dive_event %>% mutate_if(is.character,as.factor)

#replace "NR" (not recorded) values with NA for fish and sitelog
VRG_upc[VRG_upc == "NR"] <- NA
VRG_dive_event[VRG_dive_event == "NR"] <- NA

#bring in master species table (file location G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables\master_spp_table.gsheet)
#Use this if you need to reset the google authentication token- it will open a browser window to rerun the google login
#googlesheets4::gs4_auth(email = FALSE)
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
#reduce species table to VRG only
vrg.species.table <- subset(master.species.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.species.table <- vrg.species.table[c("sample_type", "sample_subtype", "pisco_classcode", "orig_classcode")] 

#bring in master site table (file locationG:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables\master_site_table.gsheet)
master.site.table <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8/edit?usp=sharing")
#reduce site table to VRG only
vrg.site.table <- subset(master.site.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.site.table <- vrg.site.table[c("site", "campus", "site_name_old")] 
vrg.site.table$Site <- vrg.site.table$site_name_old 


###   SPECIAL - temporarily join the two UPC files to run the metadata related checks. Will need to format the files to match each other later on
#TEMPORARILY join the two UPC files
upc_temp <- join(VRG_upc, VRG_upc.2004, type="full")



########################################################################
###   CHECK - Explanation of checks we do NOT run on VRG UPC data   ###
########################################################################

# We want to add as many checks as possible but...
# Do not run "CHECK - Find when metadata is not uniform across each transect" because metadata is listed in DiveEvent, not UPC entry 


###################################
###   CHECK- novel site names   ###
###################################
# run a query for site names in the data that do not match the master site table to fix typos or novel site names
# NOTE this does not convert the site names- it only checks for anomalous site names. The site name conversions happens later
# Note that dive_event site names were checked in the fish section above

#join site table with  data to import PISCO site names 
site.check <- join(upc_temp, vrg.site.table, type="left")
#select non-matches, site names that appear in dataset that are not in data missing from site list
missing.sites <- site.check[is.na(site.check$site),]
#list problem site codes
prob.site.codes <- as.data.frame(unique(missing.sites$Site))
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.site.codes, file="QAQC temp files/VRG_problemsitecodes.csv")


######################################
###   CHECK - UPC versus sitelog   ###
######################################

#VRG has variable SamplingOrganization, which are subgroups within VRG. We treat them like Observer Name. 
#There are a few crosscheck errors due to this variable but on 2-16-24 JW told APF that they don't care about errors with that, so delete that variable before proceeding
upc_temp <- upc_temp %>% 
  select(!c(SamplingOrganization)) 

#Check upc data against dive event. 
upceventcheck <- anti_join(upc_temp, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
#simplify to one line per transect
upceventcheck2 <- unique(subset(upceventcheck, select = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType")))
#Reverse check for records in event file that have no associated upc transects. Second step restricts the table to upc transects.
eventupccheck <- anti_join(VRG_dive_event, upc_temp, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
eventupccheck2 <- subset(eventupccheck, TransectType == 'UPC')
#This joins the two checks for comparison. Lines with missing dive event info had upc data but no dive event. Lines with dive event data had dive events with no upc records. Write file to csv to cross check records and fix errors.
upccrosscheck <- join(upceventcheck2, eventupccheck2, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"), type = "full", match = "all")

#ACTION- check the variable 'upcrosscheck'. If zero observations then continue. Otherwise correct the errors found and re-run the code above until there are zero 
#use the following line if you want to export to excel for fixing errors
#write.csv(upccrosscheck, file="QAQC temp files/VRG_upceventcrosscheck.csv")


#####################################################################################
###   FORMAT- 2007-2023 UPC data from point by point to BenthicReefSpecies sums   ###
#####################################################################################
#Note that VRG original data is listed by meter point, so cover, substrate, relief and superlayer each have their own column

#drop meter (because we will sum at the transect level)
VRG_upc$Meter <- NULL
#drop 'the 'SpeciesGroupF' variable (an extra variable with higher taxonomic groupings that we do not use)
VRG_upc$SpeciesGroupF <- NULL
#stack cover, substrate, relief, and superlayer into one column
VRG_upc <- melt(VRG_upc, id=c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType"), variable.name = "Category", value.name = "BenthicReefSpecies")
#remove rows with NAs from superlayer
VRG_upc <- subset(VRG_upc, BenthicReefSpecies !="NA")
#add count variable and populate with 1, we will use this to sum over each transect
VRG_upc$Count <- 1
#group by transect and sum count by category and BenthicReefSpecies
VRG_upc <- VRG_upc %>% 
  group_by(FieldYear, Site, SampleDate, SamplingOrganization, DiveReplicate, DepthZone, TransectType, Category, BenthicReefSpecies) %>% 
  dplyr::summarise(Count = sum(Count))


#####################################################
###   CHECK - FIND TOTAL COUNTS NOT EQUAL TO 31   ###
#####################################################
# Find errors where total points in Category (BenthicReefSpecies, Relief, Substrate) do not equal 31 per transect
#Applies to VRG 2007-2023 data, not to the 2004 data which had 30 points

#group by only category and pull out any totals that do not equal 31
prob.upc.counts <- VRG_upc %>% 
  group_by(FieldYear, Site, SampleDate, SamplingOrganization, DiveReplicate, DepthZone, TransectType, Category) %>% 
  dplyr::summarise(Count = sum(Count)) %>% 
  subset(Count != 31)
#exclude superlayer totals because they do not have to equal 31
prob.upc.counts <- subset(prob.upc.counts, Category != "Superlayer")

#ACTION- review 'prob.upc.counts' to fix entry errors where counts did not sum to 31
#Then run code above again until 'prob.upc.counts' has zero observations
#if lots of errors then export to excel
#write.csv(prob.upc.counts, file="QAQC temp files/probUPCcounts_2007-2023.csv")
#2024-prob.upc.counts = 39obs, JW reviewed all problem counts and confirmed that the lines left are true missing points


#####################################################
###   CHECK - FIND TOTAL COUNTS NOT EQUAL TO 30   ###
#####################################################
#for 2004 UPC data, find errors where total points in Category (BenthicReefSpecies, Relief, Substrate) do not equal 30 per transect

#group by transect and sum count by category
prob.upc.counts.2004 <- VRG_upc.2004 %>% 
  group_by(FieldYear, Site, SampleDate, SamplingOrganization, DiveReplicate, DepthZone, TransectType, Category) %>% 
  dplyr::summarize(Count = sum(Count))
#pull out any totals that do not equal 30
prob.upc.counts.2004 <- subset(prob.upc.counts.2004, Count != 30)
#note that there are no superlayers in the 2004 VRG UPC data so we do not need to exclude superlayer 

#ACTION- review 'prob.upc.counts.2004' to fix entry errors where counts did not sum to 30
##2024-prob.upc.counts.2004 = 16obs, JW reviewed all problem counts and confirmed that the 2004 data is as is- data sheets are not clear enough to warrant any data changes.



######################################################
###   MERGE - Append 2004 data to 2007-2023 data   ###
######################################################
# until this point the 2004 data and the 2007-2023 data are separate. Now that their formatting matches we will join them

#drop 'the 'SpeciesGroupF' variable in 2004 data (an extra variable with higher taxonomic groupings that we do not use)
#it was already dropped in 2007-2023 above
VRG_upc.2004$SpeciesGroupF <- NULL
#join 2004 and 2007-2023 data
VRG_upc_complete <- rbind(VRG_upc.2004, VRG_upc)



#######################################
###   CHECK- FIND DUPLICATED LINES  ###
########################################
#find duplicate rows of classcode on each transect, by size
#first run this on 2004 UPC data
VRG_upc_dupes <- VRG_upc_complete %>% 
  group_by(FieldYear, Site, SampleDate, SamplingOrganization, DiveReplicate, DepthZone, TransectType, Category, BenthicReefSpecies) %>% 
  dplyr::summarise(Count = sum(Count))

VRG_upc_dupes$dupes <- duplicated(VRG_upc_dupes[c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType", "Category", "BenthicReefSpecies")])
#subset out dupes
VRG_upc_dupes <- subset(VRG_upc_dupes, dupes == "TRUE")

#ACTION - un-comment out as needed if there are many obs in 'VRG_upc_dupes'. Be sure to COMMENT-OUT when finished
#export upc duplicates to excel_output
#write.csv(VRG_upc_dupes, file="upcdupes.csv", row.names = FALSE)
#ACTION- use "upcdupes" to consolidate duplicated lines, or fix entry errors as needed
#Then run code above again until 'upc.dupes' has zero observations


###############################
###   CHECK- Species Codes  ###
###############################
#find classcodes in the data that are missing from species list (i.e. due to a typo or a new species added this year *see notes below)

#add and rename columns to upc data to match species table
missing.upc.codes <- dplyr::rename(VRG_upc_complete, orig_classcode = BenthicReefSpecies,
                            sample_type = TransectType)
#join species table with upc dataset to import current PISCO classcodes
missing.upc.codes <- left_join(missing.upc.codes, vrg.species.table)
#find where classcode has no match in master species table
missing.upc.codes <- missing.upc.codes[is.na(missing.upc.codes$pisco_classcode),]
#create list of unique classcodes that do not match classcodes in the master species list
prob.upc.codes <- as.data.frame(unique(missing.upc.codes$orig_classcode))

#ACTION- review 'prob.upc.codes' to find typos and/or novel codes- incorrect codes need to be fixed in data, 
#also note that these typo errors should not occur if the validations are working in the entry file

#ACTION- New or altered codes need to be added to or edited in the master species table which should only be done by Avrey or Jonathan
#to add any new species, all upc observers must have been trained to include them for the entire dive season
#after fixing problem codes, run code above again until 'prob.upc.codes' has zero observations.



###############################
###   FORMAT - match PISCO  ###
###############################

#rename category  "BenthicReefSpecies" to COVER
VRG_upc_complete$Category <- revalue(VRG_upc_complete$Category, c("BenthicReefSpecies"="Cover"))

#join upc data with dive event to add "depth" because VRG UPC data does not have depth. Joined dataframe should have the same number of observations as the original upc dataframe.
VRG_upc_complete <- left_join(VRG_upc_complete, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType"))

#format date 
VRG_upc_complete$Date <- as.Date(VRG_upc_complete$SampleDate, format = "%d-%b-%y")
#create year column
VRG_upc_complete$year <- year(VRG_upc_complete$Date)
#create month column
VRG_upc_complete$month <- month(VRG_upc_complete$Date)
#create day column
VRG_upc_complete$day <- day(VRG_upc_complete$Date)
#create campus column
VRG_upc_complete$campus <- "VRG"
#create method column
VRG_upc_complete$method <- "SBTL_UPC_VRG"
#change the values in category to all caps
VRG_upc_complete$Category <- revalue(VRG_upc_complete$Category, c("Cover"="COVER", "Substrate"="SUBSTRATE", "Relief"="RELIEF", "Superlayer"="SUPERLAYER"))

#Edit variable names
names(VRG_upc_complete)[names(VRG_upc_complete) == 'BenthicReefSpecies'] <- 'orig_classcode'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'Count'] <- 'count'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'SamplingOrganization'] <- 'observer'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'Category'] <- 'category'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'DiveReplicate'] <- 'transect'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'Site'] <- 'site'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'SurveyDepth'] <- 'depth'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'DiveEventComments'] <- 'notes'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'FieldYear'] <- 'survey_year'
names(VRG_upc_complete)[names(VRG_upc_complete) == 'TransectType'] <- 'sample_type'

#rename zones to match PISCO format
VRG_upc_complete$zone <- revalue(VRG_upc_complete$DepthZone, c("Deep"="DEEP", "Inner"="INNER", "Middle"="MID", "Outer"="OUTER", "Outer Middle"="OUTER MIDDLE"))

#change notes to all caps 
VRG_upc_complete$notes <- toupper(VRG_upc_complete$notes)

#select columns to keep
VRG_upc_complete <- VRG_upc_complete[c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect", "category", "orig_classcode", "count", "depth", "observer", "notes")]


####################################################################
###   CHECK- find mismatched UPC category-species entry errors   ###
####################################################################
# Note- this check has to be done after all formatting conversions are complete so that we can check the species codes and check by sample_subtype = category
# Goal is to find entry errors where sample_subtype is mismatched to the classcode (i.e. superlayer = bedrock)

# We already have vrg.species.table with the four columns needed: "sample_type", "sample_subtype", "pisco_classcode", "orig_classcode"
#add sample_type column to upc data to match species table
VRG_upc_complete.1 <- VRG_upc_complete %>% 
  mutate(sample_type = "UPC")
#rename sample_subtype in the species table to match CATEGORY in the UPC data
names(vrg.species.table)[names(vrg.species.table) == 'sample_subtype'] <- 'category'
#join species table with upc dataset to import current PISCO classcodes- this should have the same number of obs as VRG_upc_complete.1
VRG_upc_complete.2 <- left_join(VRG_upc_complete.1, vrg.species.table)
#find entry errors from mismatched classcodes (i.e. superlayer = bedrock)
missing.upc.codes <- VRG_upc_complete.2[is.na(VRG_upc_complete.2$pisco_classcode),]

#ACTION- review 'missing.upc.codes' to fix mismatched entry errors 
#Then run code above again until missing.upc.codes has zero observations.


############################################################
###   CHECK - FIND TRANSECTS SAMPLED ON DIFFERENT DATES  ###
############################################################

#Pulls out sites that have been sampled more than one time per year
#In 2023 JW changed survey year to be different than SampleDate year so there would be no errors for transects sampled more than one time in a year

VRG_upc.obs <- VRG_upc_complete.2 %>% 
  group_by(survey_year, year, site, zone, transect) %>% 
  dplyr::summarize(month.obs = n_distinct(month), day.obs = n_distinct(day), 
                   observer.obs = n_distinct(observer), depth_obs = n_distinct(depth)) 

#stack data to search for occurrences with multiple values per transect
VRG_upc.obs.2 <- melt(VRG_upc.obs,  id.vars = c("survey_year", "year", "site", "zone", "transect"), value.name = "n.obs")

#wherever there is more than one value per variable per transect, output observation problems
VRG_upc.obs.probs <- subset(VRG_upc.obs.2, n.obs != 1)

#Pull out which years and sites have duplicate dates sampled 
VRG_upc.obs.probs_byyearsite <- VRG_upc.obs.probs %>% 
  distinct(survey_year, year, site)

#write.csv(VRG_upc.obs.probs_byyearsite, file="QAQC temp files/VRG_upc.obs.probs_byyearsite.csv", row.names = FALSE) 




#ACTION- review 'fish.obs.probs' to find and correct errors 
#Then run code above again until 'fish.obs.probs' has zero observations
#if lots of errors then export to excel
#write.csv(VRG_upc.obs.probs, file="QAQC temp files/VRG_upcobservationproblems.csv", row.names = FALSE)


###############################################################################
###   FORMAT - convert classcode, calculate percent cover, select columns   ###
###############################################################################

#rename pisco_classcode to classcode so that is the one we keep
names(VRG_upc_complete.2)[names(VRG_upc_complete.2) == 'pisco_classcode'] <- 'classcode'

#percent cover calculation
#calculate the total number of points sampled in each category for calculating percent cover
upc_points_summed <- VRG_upc_complete.2 %>% group_by(year, month, day, site, zone, transect, category) %>% 
  dplyr::summarize(tot_points = sum(count))

#transpose by category to merge in with data for dividing
upc_points_summed.2 <- spread(upc_points_summed, category, tot_points)

#merge into dataset
VRG_upc_complete.3 <- left_join(VRG_upc_complete.2, upc_points_summed.2)

#Add a column for percent cover and divide point totals by classcode by the total number of points per transect in each category (divide superlayer by cover point total)
VRG_upc_complete.3$pct_cov <- if_else(VRG_upc_complete.3$category == "COVER", 100*VRG_upc_complete.3$count/VRG_upc_complete.3$COVER, 0)
VRG_upc_complete.3$pct_cov <- if_else(VRG_upc_complete.3$category == "RELIEF", 100*VRG_upc_complete.3$count/VRG_upc_complete.3$RELIEF, VRG_upc_complete.3$pct_cov)
VRG_upc_complete.3$pct_cov <- if_else(VRG_upc_complete.3$category == "SUBSTRATE", 100*VRG_upc_complete.3$count/VRG_upc_complete.3$SUBSTRATE, VRG_upc_complete.3$pct_cov)
VRG_upc_complete.3$pct_cov <- if_else(VRG_upc_complete.3$category == "SUPERLAYER", 100*VRG_upc_complete.3$count/VRG_upc_complete.3$COVER, VRG_upc_complete.3$pct_cov)

#sum again by classcode to merge species that were combined when converting to pisco_classcode into a single count
#group by transect and sum count by classcode
VRG_upc_complete.4 <- VRG_upc_complete.3 %>% group_by(campus, method, survey_year, year, month, day, site, zone, transect, category, classcode, depth, observer, notes) %>% 
  dplyr::summarize(count = sum(count), pct_cov = sum(pct_cov)) %>% 
  ungroup()

#select columns to keep
VRG_upc_complete.5 <- VRG_upc_complete.4[c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect", 
                                           "category", "classcode", "count", "pct_cov", "depth", "observer", "notes")]


########################################
###   CHECK- unique variable check   ###
########################################
#list unique values for each variable and inspect

#ACTION- Visually inspect the outputs in the Console to confirm no irregularities
print(unique(VRG_upc_complete.5$campus), na.last = FALSE)
print(unique(VRG_upc_complete.5$method), na.last = FALSE)
print(sort(unique(VRG_upc_complete.5$survey_year), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$year), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$month), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$day), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$site), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$zone), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$transect), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$category), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$classcode), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$count), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$pct_cov), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$depth), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$observer), na.last = FALSE))
print(sort(unique(VRG_upc_complete.5$notes), na.last = FALSE))

summary(VRG_upc_complete.5)

#################################
###   Export Final data set   ###
#################################
# write to csv without data row numbers
# WARNING Be sure to update the year in the filename below before running!!!
# Reminder to move the previous dataset to the archive folder called "Archive previous data here each time a year is added"

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Final data by sampling group")

#REMINDER- comment out the line below after use!
#write.csv(VRG_upc_complete.5, file="VRG_UPC_2004-2023.csv", row.names=FALSE)





########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


#########################
###   SIZEFREQUENCY   ###  
#########################

#clear global environment
rm(list = ls())

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Raw data/VRG")

#bring in VRG size frequency data for the current year
VRG_sizefreq <- read.csv("VRG_sizefreq_2004-2023_raw.csv")

#bring in VRG site log (DiveEvent)
VRG_dive_event <- read.csv("VRG_dive_event_2004-2023_raw.csv")
#replace "NR" (not recorded) values with NA for fish and sitelog
VRG_sizefreq[VRG_sizefreq == "NR"] <- NA
VRG_dive_event[VRG_dive_event == "NR"] <- NA

#bring in master species table (file location G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Life history tables\master_spp_table.gsheet)
#Use this if you need to reset the google authentication token- it will open a browser window to rerun the google login
#googlesheets4::gs4_auth(email = FALSE)
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
#reduce species table to VRG only
vrg.species.table <- subset(master.species.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.species.table <- vrg.species.table[c("sample_type", "sample_subtype", "pisco_classcode", "orig_classcode")] 

#bring in master site table (file locationG:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables\master_site_table.gsheet)
master.site.table <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8/edit?usp=sharing")
#reduce site table to VRG only
vrg.site.table <- subset(master.site.table, campus == "VRG")
#reduce table to subset of columns for merging with data to convert classcodes
vrg.site.table <- vrg.site.table[c("site", "campus", "site_name_old")] 
vrg.site.table$Site <- vrg.site.table$site_name_old 


###################################
###   CHECK- Novel Site Names   ###
###################################
# run a query for site names in the data that do not match the master site table to fix typos or novel site names
# NOTE this does not convert the site names- it only checks for anomalous site names. The site name conversions happens later
# Note that dive_event site names were checked in the fish section above

#join site table with  data to import PISCO site names 
site.check <- join(VRG_sizefreq, vrg.site.table, type="left")
#select non-matches, site names that appear in dataset that are not in data missing from site list
missing.sites <- site.check[is.na(site.check$site),]
#list problem site codes
prob.site.codes <- as.data.frame(unique(missing.sites$Site))
#use the following line if you want to export to excel for fixing errors
#write.csv(prob.site.codes, file="QAQC temp files/VRG_problemsitecodes.csv")


############################################
###  CHECK - Size Frequency vs. Sitelog  ###
############################################

#VRG has variable SamplingOrganization, which are subgroups within VRG. We treat them like Observer Name. 
#There are a few crosscheck errors due to this variable but on 2-16-24 JW told APF that they don't care about errors with that, so delete that variable before proceeding
VRG_sizefreq_temp <- VRG_sizefreq %>% 
  select(!c(SamplingOrganization)) 

#Check size frequency data against dive event.
sizefreqeventcheck <- anti_join(VRG_sizefreq_temp, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
sizefreqeventcheck2 <- unique(subset(sizefreqeventcheck, select = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType")))

#Reverse check for records in event file that have no associated sizefreq transects. Second step restricts the table to sizefreq transects.
eventsizefreqcheck <- anti_join(VRG_dive_event, VRG_sizefreq_temp, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"))
eventsizefreqcheck2 <- subset(eventsizefreqcheck, TransectType == 'ISC')

#This joins the two checks for comparison. Lines with missing dive event info had sizefreq data but no dive event. Lines with dive event data had dive events with no sizefreq records. Write file to csv to cross check records and fix errors.
sizefreqcrosscheck <- join(sizefreqeventcheck2, eventsizefreqcheck2, by = c("FieldYear", "Site", "SampleDate", "DiveReplicate", "DepthZone", "TransectType"), type = "full", match = "all")

#ACTION- check the variable 'sizefreqcrosscheck'. If zero observations then continue. Otherwise correct the errors found and re-run the code above until there are zero 
#use the following line if you want to export to excel for fixing errors
#write.csv(sizefreqcrosscheck, file="QAQC temp files/VRG_sizefreqcrosscheck.csv")




######################################
###  SIZE FREQUENCY - Conversions  ###
######################################

#Once sizefreqcrosscheck has zero observations, join data with dive event. 
#Joined dataframe should have the same number of observations as the original sizefreq dataframe.
VRG_sizefreq.2 <- left_join(VRG_sizefreq, VRG_dive_event, by = c("FieldYear", "Site", "SampleDate", "SamplingOrganization", "DiveReplicate", "DepthZone", "TransectType"))

#format date from character string
VRG_sizefreq.2$Date <- as.Date(VRG_sizefreq.2$SampleDate, format = "%d-%b-%y")
#create year column
VRG_sizefreq.2$year <- year(VRG_sizefreq.2$Date)
#create month column
VRG_sizefreq.2$month <- month(VRG_sizefreq.2$Date)
#create day column
VRG_sizefreq.2$day <- day(VRG_sizefreq.2$Date)
#create campus column
VRG_sizefreq.2$campus <- "VRG"
#create method column
VRG_sizefreq.2$method <- "SBTL_SIZEFREQ_VRG"
#create disease column
VRG_sizefreq.2$disease <- ""
#create location column (transect vs. random)
VRG_sizefreq.2$location <- "RANDOM"

#rename zones to match PISCO format
VRG_sizefreq.2$zone <- revalue(VRG_sizefreq.2$DepthZone, c("Deep"="DEEP", "Inner"="INNER", "Middle"="MID", "Outer"="OUTER", "Outer Middle"="OUTER MIDDLE"))

#convert size data from mm to cm
VRG_sizefreq.2$size <- VRG_sizefreq.2$SizeClass / 10

#Edit variable names
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'BenthicReefSpecies'] <- 'orig_classcode'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'Abundance'] <- 'count'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'SamplingOrganization'] <- 'observer'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'DiveReplicate'] <- 'transect'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'Site'] <- 'site'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'SurveyDepth'] <- 'depth'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'DiveEventComments'] <- 'notes'
names(VRG_sizefreq.2)[names(VRG_sizefreq.2) == 'FieldYear'] <- 'survey_year'

#convert to PISCO species codes
#add columns to sizefreq data to match species table
#VRG_sizefreq.2$sample_type <- "SIZEFREQ"
VRG_sizefreq.2$sample_type <- revalue(VRG_sizefreq.2$TransectType, c("ISC"="SIZEFREQ", "Swath" = "SWATH"))
#join species table with sizefreq dataset to import PISCO classcodes
VRG_sizefreq.3 <- left_join(VRG_sizefreq.2, vrg.species.table)



###############################
###   CHECK- Species Codes  ###
###############################


#select non-matches, classcodes in data missing from species list
missing.sizefreq.codes <- VRG_sizefreq.3[is.na(VRG_sizefreq.3$pisco_classcode),]
prob.sizefreq.codes <- as.data.frame(unique(missing.sizefreq.codes$orig_classcode))


##SPECIAL NOTE- MESFRA for 2020 data was not imported correctly and ended up as NA- This was fixed manually after the fact
#when running this code in 2022, try to fix this in the code, not time to do it right now APF 9-23-21
#ACTION- check the variable 'prob.sizefreq.codes'. If zero observations then continue. Otherwise correct the errors found and re-run the code above until there are zero 
#use the following line if you want to export to excel for fixing errors
#write.csv(missing.sizefreq.codes, file="QAQC temp files/VRG_missing_sizefreq_codes.csv")

###########################################################
###   FORMAT - Final formatting changes to match PISCO  ###
###########################################################


#rename classcode
names(VRG_sizefreq.3)[names(VRG_sizefreq.3) == 'pisco_classcode'] <- 'classcode'

#group by size and classcode and sum count
VRG_sizefreq.4 <- VRG_sizefreq.3 %>% group_by(campus, method, survey_year, year, month, day, site, location, zone, transect, classcode, size, disease, depth, observer, notes) %>% 
  dplyr::summarise(count = sum(count)) 



## Ave- Check this, but I think we can delete this step. We already select the columns we want in the summarise() grouping from line #1173-1175  
#select columns to keep
VRG_sizefreq.5 <- VRG_sizefreq.4[c("campus", "method", "survey_year", "year", "month", "day", "site", "location", "zone", "transect", "classcode", "size", "disease", "depth", "observer", "count", "notes")]


####################################
###   CHECK - REVIEW VARIABLES   ###
####################################

#list unique values for each variable and inspect each
#ACTION- Visually inspect the outputs in the Console as you progress looking for irregularities

print(unique(VRG_sizefreq.5$campus), na.last = FALSE)
print(unique(VRG_sizefreq.5$method), na.last = FALSE)
print(sort(unique(VRG_sizefreq.5$survey_year), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$year), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$month), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$day), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$site), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$location), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$zone), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$transect), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$classcode), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$size), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$disease), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$depth), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$observer), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$count), na.last = FALSE))
print(sort(unique(VRG_sizefreq.5$notes), na.last = FALSE))

summary(VRG_sizefreq)

# Check for observations that should not have NA values (campus:location, classcode, size, count)
# Uncomment and run the line corresponding with where anomalous NA's are present 

# NA_classcode <- VRG_sizefreq.5 |> filter(is.na(classcode))
# NA_size <- VRG_sizefreq.5 |> filter(is.na(size))
# NA_count <- VRG_sizefreq.5 |> filter(is.na(count))

#use the following line if you want to export to excel for fixing errors
#write.csv(NA_size, file="QAQC temp files/VRG_sizefreq_NA_size.csv")

############################################################
###   SIZE FREQUENCY -merge new data with previous data  ###
############################################################

# NOTE: This section did not get run in 2024 since VRG sent complete sizing dataset that included all years

#bring in VRG 'Final data' file from the previous years called "past" data
#Be sure to edit the year in the next line
VRG_sizefreq_past <- read.csv("Final data by sampling group/VRG_SIZEFREQ_2004-2021.csv")

##reformat transect as a factor to match the other datasets for merging (2004 data have non-numeric replicate names)
VRG_sizefreq.5$transect <- as.factor(VRG_sizefreq.6$transect)

#combine new data with past data
VRG_SIZEFREQ <- bind_rows(VRG_sizefreq_past, VRG_sizefreq.5)
#if the previous step converts factor variables to characters and produces warnings, then use the following line to convert back to factor variables.
#VRG_UPC <- VRG_UPC %>% mutate_if(is.character,as.factor)

##Note- APF changed the way the final data files are saved by adding the year range of the data to the filename. This way you cannot overwrite the dataset created the year before.
#Additionally, within the google drive archive, we now have an archival record of what the dataset was for a given timeframe if we ever need to look back
#Additionally this protects against accidentally overwriting by over-running this code because once the previous years file is moved to the archive folder this code cannot be run

#write to csv without data row numbers
#WARNING Be sure to update the year in the filename below before running!!!
#write.csv(VRG_SIZEFREQ, file="Final data by sampling group/VRG_SIZEFREQ_2004-2022.csv", row.names=FALSE)

#ACTION - View the new file in google drive (G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Final data by sampling group)
#Open the file in excel and inspect it. 
#Then move the previous dataset (i.e. 2004-2020) to the archive folder called "Archive previous data here each time a year is added"





#################################
###   Export Final data set   ###
#################################
# write to csv without data row numbers
# Reminder to move the previous dataset to the archive folder called "Archive previous data here each time a year is added"

#set working directory
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Final data by sampling group")

#REMINDER- comment out the line below after use!
#WARNING Be sure to update the year in the filename below before running!!!
#write.csv(VRG_sizefreq.6, file="VRG_SIZEFREQ_2004-2023.csv", row.names=FALSE)



###########################################################################################################################

#END OF CODE

