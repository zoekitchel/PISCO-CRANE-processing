##THIS CODE WILL BRING IN BENTHIC SWATH, UPC and SIZEFREQ DATA FROM HSU, UCSC, UCSB, AND VRG DATA IN 'FINAL FORM', MERGE THEM, AND CREATE DERIVED DATASETS WITH SITE MEANS
### ORIGNAL CODE CREATED:  MARCH 2020, KATIE DAVIS KOEHN
### CODE UPDATED: MARCH 2024, KATELIN SEETO
### UPDATED: May 2021 to include site name formatting edits
### UPDATED: September 2021, just size frequency portion
### UPDATED: Feb 2022, Dan Malone - remove observations of spp not looked for from raw swath dataset

#Run this code each year and save a new copy with the current year in the filename in order to maintain archive of all previous code

#Notes on the location of data:
#PISCO data (UCSC and UCSB) are located in G:\Shared drives\PISCO Subtidal UCSB and UCSC\PISCO subtidal data clearinghouse
#This is so that datasets are not duplicated across multiple locations- i.e. all analyses for any PISCO project that query the data do so from this same location
#UCSC and UCSB data that are already combined are refered to as PISCO
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


###############################
##########   SWATH   ##########
###############################

#Bring in PISCO data
#set working directory for bringing in PISCO data (i.e. UCSC and UCSB data that has already been merged)
setwd("G:/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")
#setwd("/Volumes/GoogleDrive/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")

PISCO_swath <- read.csv(file="PISCO_SWATH.csv")
#convert transect to factor (always imports as character)
PISCO_swath$transect <- as.factor(PISCO_swath$transect)
#rename year to survey_year
PISCO_swath$survey_year <- PISCO_swath$year
#format method and rename "SBTL_SWATH" to "SBTL_SWATH_PISCO"
PISCO_swath$method <- as.character(PISCO_swath$method)
PISCO_swath$method <- if_else(PISCO_swath$method == "SBTL_SWATH", "SBTL_SWATH_PISCO", PISCO_swath$method)
PISCO_swath$method <- as.factor(PISCO_swath$method)

#drop Oregon data - denoted by method="SBTL_SWATH_OREGON"
PISCO_swath <- subset(PISCO_swath, method != "SBTL_SWATH_OREGON")

#Bring in HSU and VRG data
#set working directory for bringing in HSU and VRG data
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#setwd("/Volumes/GoogleDrive/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")

#bring in HSU dataset
HSU_swath <- read.csv(file="Final data by sampling group/HSU_SWATH_2014-2023.csv")
#convert transect to factor (always imports as character)
HSU_swath$transect <- as.factor(HSU_swath$transect)

#bring in VRG dataset
VRG_swath <- read.csv(file="Final data by sampling group/VRG_SWATH_2004-2023.csv")
#convert transect to factor (always imports as character)
VRG_swath$transect <- as.factor(VRG_swath$transect)


###########################
###   Site name check   ###
###########################
#SPECIAL NOTE - the plan is that in the future we will move this step to Data In - all site names should be converted and checked prior to merging so that each groups dataset has correct names from the start
#OR this needs to happen at the end stage of each data type?

#generate list of sites for each group, merge and check for inconsistencies in site naming
PISCO_sites <- as.data.frame(unique(PISCO_swath$site))
names(PISCO_sites)[names(PISCO_sites) == "unique(PISCO_swath$site)"] <- "site" 
PISCO_sites$group <- "PISCO"
HSU_sites <- as.data.frame(unique(HSU_swath$site))
names(HSU_sites)[names(HSU_sites) == "unique(HSU_swath$site)"] <- "site"
HSU_sites$group <- "HSU"
VRG_sites <- as.data.frame(unique(VRG_swath$site))
names(VRG_sites)[names(VRG_sites) == "unique(VRG_swath$site)"] <- "site"
VRG_sites$group <- "VRG"

all_sites <- bind_rows(PISCO_sites, HSU_sites, VRG_sites)
all_sites <- all_sites[order(all_sites$site),]

##ACTION - inspect 'all_sites' - these are all unique site names from all data
#as of 5-24-22 'all_sites" has 364 lines - after removing SBTL_SWATH_OREGON -
#4-26-2024 has 375 lines


#############################################################
###   Merge campus datasets and run a replication check   ###
#############################################################

#merge datasets
MLPA_swath <- bind_rows(PISCO_swath, HSU_swath, VRG_swath)
#previous step converts factor variables to characters and produces warnings. Convert back to factor variables.
MLPA_swath <-MLPA_swath %>% mutate_if(is.character,as.factor)

#repcheck - these steps take a minute to complete
MLPA_swath.grouped <- MLPA_swath %>% group_by(campus, method, survey_year, site, zone)
swath_tx_summary <- summarize(MLPA_swath.grouped, n.tx = n_distinct(transect))
swath_tx_summary.transposed <- spread(swath_tx_summary, zone, n.tx)

# Filter for zones that have more than 2 transects completed for a given year 
MLPA_swath_reps <- swath_tx_summary.transposed |> filter(DEEP > 2 | INNER >2 | MID > 2 | OUTER > 2)

#write repcheck to csv to inspect in excel
#un-comment to run as needed
#write.csv(swath_tx_summary.transposed, file = "Data processing/QAQC temporary files/swath_repcheck.csv")

##ACTION - inspect the excel file 'swath_repcheck' for replication for each site and year sampled


#############################
###   Convert site names  ###
#############################

#Special note- in future years we plan to move this code to "DataIn" so that all data has consistent site names from the beginning of the process

#Load in the 'master_site_table' in order to convert sitenames to the uniform standard. This also changes some site names that are needed to match up sites with different names with different groups.
#For example UCSB site names in Malibu and at Santa Barbara Island need to be converted to VRG site names because they currently survey those sites.
#We will use VRG site names for all SBI sites, even those that aren't in current monitoring plan.
#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
site.name.conversions <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8")
#re-name the 'site' column to 'site_name_current', this will be the name we convert to

#SPECIAL NOTE As of July 2021 the decision was made that the official site names in all of the merged datasets will be in PISCO format which is all capitols and underscores
#DM I had to add "dplyr::" before rename function otherwise it wouldn't work
site.name.conversions <- dplyr::rename(site.name.conversions, site_name_current = site) 

#copy the 'site_name_old' column and call it 'site' to match up with the swath data
site.name.conversions$site <- site.name.conversions$site_name_old
#simplify to just the site name variables needed for conversions
site.name.conversions <- site.name.conversions[c("site", "site_name_current", "site_name_old")]

#merge site conversion table into data
MLPA_swath.2 <- left_join(MLPA_swath, site.name.conversions)

#for site names that do not need to be converted (i.e. do not have a value for "site_name_old"), in column 'site_name_current' replace the 'NA' with the site name
MLPA_swath.2$site_name_current[is.na(MLPA_swath.2$site_name_current)] <- MLPA_swath.2$site[is.na(MLPA_swath.2$site_name_current)]

#check that the update worked correctly
MLPA_swath_sitescheck <- unique(MLPA_swath[c("site")])
MLPA_swath.2_sitescheck <- unique(MLPA_swath.2[c("site")])
#check that all sites names are unique between campuses
MLPA_swath.2_sitescheck <- unique(MLPA_swath.2[c("campus" , "site", "site_name_current", "site_name_old")])
MLPA_swath.2_sitescheck$dupes <- duplicated(MLPA_swath.2_sitescheck[,1:2])
MLPA_swath.2_sitescheck.dupes <- subset(MLPA_swath.2_sitescheck, dupes == "TRUE")

##ACTION- inspect 'MLPA_swath.2_sitescheck'
#As of 3-5-21 has 362 observations
#As of 5-24-22 has 368 observations (includes sites observed by UCSB & DUNES VANDENBERG, etc)
#As of 4-18-24 has 379 observations

##ACTION- inspect 'MLPA_swath.2_sitescheck.dupes'
#Should have 0 observations 


##############################################
###   Check for data missing a site name   ###
##############################################

missing_sites<-subset(MLPA_swath_sitescheck, !(site %in% all_sites$site))
all_sites2<-unique(all_sites[c("site")])

##ACTION- inspect 'missing_sites'
#Should have 0 observations


############################
###   Final formatting   ###
############################

#to finish off the conversions, drop variable 'site' and rename 'site_name_current' as the 'site'
MLPA_swath.2$site <- NULL
names(MLPA_swath.2)[names(MLPA_swath.2) == "site_name_current"] <- "site"

#assign count of zero to NO_ORG records that don't already have it
MLPA_swath.2$count <- if_else(MLPA_swath.2$classcode == "NO_ORG", as.integer(0), MLPA_swath.2$count)

#find transects where NO-ORG was entered, but there are also other records organisms on the transect
#this can happen during merging of segments if one or more segments was NO_ORG, but should be removed at the transect level)
MLPA_swath_NO_ORG <- subset(MLPA_swath.2, classcode == "NO_ORG")
MLPA_swath_NO_ORG$classcode <- NULL
MLPA_swath_NO_ORG$count <- NULL
MLPA_swath_NO_ORG.2 <- left_join(MLPA_swath_NO_ORG, MLPA_swath.2)
n.classcodes <- MLPA_swath_NO_ORG.2 %>% group_by(campus, method, year, month, day, site, zone, transect) %>% summarize(n.classcodes = n_distinct(classcode))
#remove records where there is only one classcode (legit NO_ORG transects)
n.classcodes <- subset(n.classcodes, n.classcodes != 1)

n.classcodes$drop <- "drop"
n.classcodes$classcode <- "NO_ORG"

#merge into swath data and drop records using drop column
MLPA_swath.3 <- left_join(MLPA_swath.2, n.classcodes)
MLPA_swath.3 <- subset(MLPA_swath.3, is.na(drop))

#drop extra columns
MLPA_swath.3$drop <- NULL
MLPA_swath.3$n.classcodes <- NULL

#find other records with zeros for count
count.zeros <- subset(MLPA_swath.3, count == 0)

#find records with missing counts 
#There were several records where counts were not recorded by HSU. When creating site means will manually remove red and purple urchins at Caspar 3 Ten Mile 2 in 2015 where a substantial number of counts are missing/done improperly)
missing.count <- subset(MLPA_swath.3, is.na(count))

#drop zero count observations (data were not consistently zero populated so remove zero counts to avoid confusion by users), missing counts will also be dropped
MLPA_swath.4 <- subset(MLPA_swath.3, is.na(count) | count != 0 | classcode == "NO_ORG")

#format columns for export of 'raw' dataset at the transect level
MLPA_swath.5 <- MLPA_swath.4[c("campus", "method", "survey_year", "year", "month", 
                                       "day", "site", "zone", "transect", "classcode", 
                                       "count", "size", "disease", "depth", "observer", "notes", "site_name_old")]

#show and review summary of file so far
summary(MLPA_swath.5)
summary(MLPA_swath.5$observer)
##ACTION - inspect the summary stats

#assign missing values (NA) to blank cells
MLPA_swath.5$disease[MLPA_swath.5$disease==""] <- NA
MLPA_swath.5$observer[MLPA_swath.5$observer==""] <- NA
MLPA_swath.5$notes[MLPA_swath.5$notes==""] <- NA


##################################################################
#DM adding a filter to remove species observations that were not "looked for" in particular years, this is done again below
#with the zero-populated data prior to calculating site means, it needs to be again to remove zero's from years when species aren't looked for

#bring in master species table from Google to incorporate 'looked for' info and filter out species not counted consistantly or accurately
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)

#reduce species table to just swath information
swath.species.table <- subset(master.species.table, sample_type == "SWATH")

#Keep "LOOKED" columns, campus, and pisco_classcode. Drop all others to not create duplicate lines during the join. 
swath.species.table.2 <- swath.species.table |> select(campus, pisco_classcode, starts_with("LOOKED"))

#Filter for distinct rows so each code is represented once per campus
#Check that all codes only occur once per campus. If more than one look for differences in LOOKED for columns
swath.code.dupes <- swath.species.table.2 |> 
  distinct() |> 
  get_dupes(c(campus, pisco_classcode))

##ACTION - swath.code.dupes should be 0. If more than 0 check for differences in LOOKED for columns

#Once it is confirmed there are no code dupes and all classcodes have the same Looked for years, condense to one like per campus-classcode combination
swath.species.table.2 <-  swath.species.table.2 |> distinct()

#transpose years
##ACTION - each year you need to add the variable "LOOKEDYYYY" to the next line of code
swath.species.looked <- melt(swath.species.table.2, id.vars = c("campus", "pisco_classcode"), measure.vars = c("LOOKED1999", "LOOKED2000", "LOOKED2001", 
                                                                                                               "LOOKED2002", "LOOKED2003", "LOOKED2004", 
                                                                                                               "LOOKED2005", "LOOKED2006", "LOOKED2007", 
                                                                                                               "LOOKED2008", "LOOKED2009", "LOOKED2010", 
                                                                                                               "LOOKED2011", "LOOKED2012", "LOOKED2013", 
                                                                                                               "LOOKED2014", "LOOKED2015", "LOOKED2016", 
                                                                                                               "LOOKED2017", "LOOKED2018", "LOOKED2019", 
                                                                                                               "LOOKED2020", "LOOKED2021", "LOOKED2022",
                                                                                                               "LOOKED2023"))

#revalue 'X' in looked for columns to yes and blank values to no
swath.species.looked$value <- revalue(swath.species.looked$value, c("X"="yes"))
swath.species.looked$value[is.na(swath.species.looked$value)] <- "no"

#removed 'LOOKED' so the variable is just the year
swath.species.looked$survey_year <- as.factor(gsub("LOOKED", "", swath.species.looked$variable))
names(swath.species.looked)[names(swath.species.looked) == "value"] <- "LOOKEDFOR"
swath.species.looked$variable <- NULL
names(swath.species.looked)[names(swath.species.looked) == "pisco_classcode"] <- "classcode"
#format campus and classcode as factors in looked for table
swath.species.looked$campus <- as.factor(swath.species.looked$campus)
swath.species.looked$classcode <- as.factor(swath.species.looked$classcode)
#format survey_year as factor in swath data prior to merging
MLPA_swath.5$survey_year <- as.factor(MLPA_swath.5$survey_year)

#join swath dataset with 'looked for' table to filter out species that were not effectively or consistently looked for in a given year by a given program
#join tables
MLPA_swath.6 <- left_join(MLPA_swath.5, swath.species.looked, by=c("survey_year", "campus", "classcode"))
#reformat variables as factors in joined dataset
MLPA_swath.6$classcode <- as.factor(MLPA_swath.6$classcode)
MLPA_swath.6$LOOKEDFOR <- as.factor(MLPA_swath.6$LOOKEDFOR)

#LOOKEDFOR will include NAs (species that aren't in the master species table for a specific campus), check the NAs by campus and classcode
lookedfor.nas <- subset(MLPA_swath.6, is.na(LOOKEDFOR))
lookedfor.nas.2 <- unique(lookedfor.nas[c("campus", "classcode")])

#check the not looked for records to make sure they only include the appropriate species
not.looked.for.swath <- subset(MLPA_swath.6, LOOKEDFOR == "no")
not.looked.for.swath.spp <- unique(not.looked.for.swath[c("campus", "survey_year", "classcode")])
not.looked.for.swath.spp.filtered <- unique(not.looked.for.swath.spp[c("classcode")])

##ACTION review 'not.looked.for.swath.spp.filtered"- this is the list of all species codes removed on any line.
#as of 2-11-22 26 observations
##ACTION review 'not.looked.for.swath.spp' which lists by campus and by year- some species are removed only in particular years for a particular campus
#as of 5-24-22 the only removed observations are from HSU and are correct - PODMAC and GERRUB


#check for large numbers of obs being discarded
not.looked.for.swath.spp2 <- not.looked.for.swath %>% group_by(campus, survey_year, classcode) %>% summarize(sum_count = sum(count))
#sort by descending value to review largest counts of removed data
not.looked.for.swath.spp2 <-not.looked.for.swath.spp2[order(-not.looked.for.swath.spp2$sum_count),]

##ACTION review 'not.looked.for.swath.spp.2' lists by total count removed
#as of 5-24-22 this is correct- removing GERRUB and PODMAC from HSU

#filter out not looked for observations 
MLPA_swath.7 <- subset(MLPA_swath.6, LOOKEDFOR == "yes")

#drop LOOKEDFOR variable
MLPA_swath.7$LOOKEDFOR <- NULL

#convert characters to factors
MLPA_swath.7 <-MLPA_swath.7 %>% mutate_if(is.character,as.factor)
#view summary
summary(MLPA_swath.7)

#########################################
####   Export merged "raw" dataset   ###
########################################

#contains observation level data for all groups (i.e. MACPYR, HALSPP and  not summed to transect or site means)
#APF 2-7-24 the previous statement may not be true- I think this is the transect level dataset

#export merged raw dataset
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_swath.7, file="MLPA merged datasets/MLPA_kelpforest_swath.csv", row.names=FALSE)
####################################################################################


##########################################
####   Create Transect level dataset   ###
##########################################

# If you have run the code above use this:
#clean up global environment, keeping just the current working dataset (so you can see what you're doing)
rm(list= ls()[!(ls() == 'MLPA_swath.7')])

### If you have not run the code above and have the dataset loaded, then use this to load it in:
#set working directory
#setwd("g:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#MLPA_swath.7 <- read.csv(file="MLPA merged datasets/MLPA_kelpforest_swath.csv")


#drop 'deep' transects done by VRG (not done by other groups)
MLPA_swath.8 <- subset(MLPA_swath.7, zone != "DEEP")

#convert raw MACSTIPE counts to total stipe count per transect

#subset out macrocystis to multiply stipes by count and append in as a new classcode
MLPA_swath.macstipes <- subset(MLPA_swath.8, classcode == "MACPYRAD")

#multiply count by size to get total macstipes
MLPA_swath.macstipes$MACSTIPES <-MLPA_swath.macstipes$count*MLPA_swath.macstipes$size
#check which observations have missing counts for MACSTIPES
MLPA_swath.macstipes.NA <- MLPA_swath.macstipes |> filter(is.na(count) | is.na(size))
#a handful (only 11 total) of records had missing counts, drop these
#on 4-18-24 there were 16 records of missing sizes and therefor MACSTIPES
MLPA_swath.macstipes <- subset(MLPA_swath.macstipes, !is.na(MACSTIPES))
#drop count and classcode
MLPA_swath.macstipes$count <- NULL
MLPA_swath.macstipes$classcode <- NULL

#assign new classcode as MACSTIPES
MLPA_swath.macstipes$classcode <- "MACSTIPES"

#rename MACSTIPES column as count
names(MLPA_swath.macstipes)[names(MLPA_swath.macstipes) == "MACSTIPES"] <- "count"

#append to data
MLPA_swath.8 <- rbind(MLPA_swath.8, MLPA_swath.macstipes)

#sum up species by transect now that macstipes are calculated
MLPA_swath.9 <- MLPA_swath.8 %>% group_by(campus, method, survey_year, year, month, day, site, zone, transect, classcode) %>% summarize(count = sum(count))





#transpose classcodes to zero populate
MLPA_swath_transposed <- dcast(MLPA_swath.9, campus + method + survey_year + year + month + day + site + zone + transect ~ classcode, value.var = "count")








#zero populate
MLPA_swath_transposed[is.na(MLPA_swath_transposed)] <- 0

#back transpose to merge with species table and filter out not-looked for species
MLPA_swath_zeros <- melt(MLPA_swath_transposed, id.vars = c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect"), value.name = "count", variable.name = "classcode")

#bring in master species table from Google to incorporate 'looked for' info and filter out species not counted consistently or accurately
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)

#reduce species table to just swath information
swath.species.table <- subset(master.species.table, sample_type == "SWATH")

#Keep "LOOKED" columns, campus, and pisco_classcode. Drop all others to not create duplicate lines during the join. 
swath.species.table.2 <- swath.species.table |> select(campus, pisco_classcode, starts_with("LOOKED"))

#Filter for distinct rows so each code is represented once per campus
#Check that all codes only occur once per campus. If more than one look for differences in LOOKED for columns
swath.code.dupes <- swath.species.table.2 |> 
  distinct() |> 
  get_dupes(c(campus, pisco_classcode))
##ACTION - swath.code.dupes should be 0. If more than 0 check for differences in LOOKED for columns

#Once it is confirmed there are no code dupes and all classcodes have the same Looked for years, condense to one like per campus-classcode combination
swath.species.table.2 <-  swath.species.table.2 |> distinct()


#transpose years
##ACTION - each year you need to add the variable "LOOKEDYYYY" to the next line of code
swath.species.looked <- melt(swath.species.table.2, id.vars = c("campus", "pisco_classcode"), measure.vars = c("LOOKED1999", "LOOKED2000", "LOOKED2001", 
                                                                                                               "LOOKED2002", "LOOKED2003", "LOOKED2004", 
                                                                                                               "LOOKED2005", "LOOKED2006", "LOOKED2007", 
                                                                                                               "LOOKED2008", "LOOKED2009", "LOOKED2010", 
                                                                                                               "LOOKED2011", "LOOKED2012", "LOOKED2013", 
                                                                                                               "LOOKED2014", "LOOKED2015", "LOOKED2016", 
                                                                                                               "LOOKED2017", "LOOKED2018", "LOOKED2019", 
                                                                                                               "LOOKED2020", "LOOKED2021", "LOOKED2022",
                                                                                                               "LOOKED2023"))
#revalue 'X' in looked for columns to yes and blank values to no
swath.species.looked$value <- revalue(swath.species.looked$value, c("X"="yes"))
swath.species.looked$value[is.na(swath.species.looked$value)] <- "no"

#removed 'LOOKED' so the variable is just the year
swath.species.looked$survey_year <- as.factor(gsub("LOOKED", "", swath.species.looked$variable))
names(swath.species.looked)[names(swath.species.looked) == "value"] <- "LOOKEDFOR"
swath.species.looked$variable <- NULL
names(swath.species.looked)[names(swath.species.looked) == "pisco_classcode"] <- "classcode"
#format campus and classcode as factors in looked for table
swath.species.looked$campus <- as.factor(swath.species.looked$campus)
swath.species.looked$classcode <- as.factor(swath.species.looked$classcode)
#format survey_year as factor in swath data prior to merging
MLPA_swath_zeros$survey_year <- as.factor(MLPA_swath_zeros$survey_year)

#join swath dataset with 'looked for' table to filter out species that were not effectively or consistently looked for in a given year by a given program
#join tables
MLPA_swath.10 <- left_join(MLPA_swath_zeros, swath.species.looked, by=c("survey_year", "campus", "classcode"))
#recode MACSTIPES as looked for (the classcode 'MACSTIPES is not in species table so need to do this manually)
MLPA_swath.10$LOOKEDFOR <- if_else(MLPA_swath.10$classcode == "MACSTIPES", "yes", as.character(MLPA_swath.10$LOOKEDFOR))
#reformat variables as factors in joined dataset
MLPA_swath.10$classcode <- as.factor(MLPA_swath.10$classcode)
MLPA_swath.10$LOOKEDFOR <- as.factor(MLPA_swath.10$LOOKEDFOR)

#LOOKEDFOR will include NAs (species that aren't in the master species table for a specific campus), check the NAs by campus and classcode
lookedfor.nas <- subset(MLPA_swath.10, is.na(LOOKEDFOR))
lookedfor.nas.2 <- unique(lookedfor.nas[c("campus", "classcode")])

#check the not looked for records to make sure they only include the appropriate species
not.looked.for.swath <- subset(MLPA_swath.10, LOOKEDFOR == "no")
not.looked.for.swath.spp <- unique(not.looked.for.swath[c("campus", "survey_year", "classcode")])
not.looked.for.swath.spp.filtered <- unique(not.looked.for.swath.spp[c("classcode")])

##ACTION review 'not.looked.for.swath.spp.filtered"- this is the list of all species codes removed on any line.
#as of 7-15-21 123 observations
#as of 2-11-22  observations
#as of 4-18-24 133 observation
##ACTION review 'not.looked.for.swath.spp' which lists by campus and by year- some species are removed only in particular years for a particular campus

#check for large numbers of obs being discarded
not.looked.for.swath.spp2 <- not.looked.for.swath %>% group_by(campus, survey_year, classcode) %>% summarize(sum_count = sum(count))
#sort by descending value to review largest counts of removed data
not.looked.for.swath.spp2 <-not.looked.for.swath.spp2[order(-not.looked.for.swath.spp2$sum_count),]

##ACTION review 'not.looked.for.swath.spp.2' lists by total count removed
#5-23-22 confirmed that the only removed species are PODMAC and GERRUB from HSU

#filter out not looked for observations 
MLPA_swath.11 <- subset(MLPA_swath.10, LOOKEDFOR == "yes")

#filter out urchin data from HSU in 2015 when counts weren't done properly
MLPA_swath.12 <- filter(MLPA_swath.11, !(grepl('CASPAR_3', site) & grepl('STRPURAD', classcode) & grepl("2015", survey_year)))
MLPA_swath.13 <- filter(MLPA_swath.12, !(grepl('CASPAR_3', site) & grepl('MESFRAAD', classcode) & grepl("2015", survey_year)))
MLPA_swath.14 <- filter(MLPA_swath.13, !(grepl('TEN_MILE_2', site) & grepl('STRPURAD', classcode) & grepl("2015", survey_year)))
MLPA_swath.15 <- filter(MLPA_swath.14, !(grepl('TEN_MILE_2', site) & grepl('MESFRAAD', classcode) & grepl("2015", survey_year)))

########################################
###  Export transect level dataset   ###
########################################

##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
write.csv(MLPA_swath.15, file="MLPA merged datasets/MLPA_swath_CLEAN.csv", row.names = FALSE)

####################################
###   Create site mean dataset   ###
###################################

# If you have run the code above use this:
#clean up global environment, keeping just the current working dataset (so you can see what you're doing)
rm(list= ls()[!(ls() %in% c('MLPA_swath.15','MLPA_swath.9'))])


#if you need to pull in the transect level dataset use this:
#setwd("H:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#MLPA_swath.15 <- read.csv(file="MLPA merged datasets/MLPA_swath_CLEAN.csv")

#calculate site mean densities now that zeros are added and not looked for species are removed
MLPA_swath.means <- MLPA_swath.15 %>% 
  group_by(campus, method, survey_year, site, classcode) %>% 
  summarize(mean_count = mean(count))

#add density prefix to classcode
MLPA_swath.means$den_classcode <- paste("den", MLPA_swath.means$classcode, sep = "_")

#transpose back to wide form. Now zeros are real zeros and blanks are not looked for
MLPA_swath_site_means <- dcast(MLPA_swath.means, campus + method + survey_year + site  ~ den_classcode, value.var = "mean_count")

#calculate number of transects completed per site
MLPA_swath_tx_by_zone <- MLPA_swath.15 %>% group_by(campus, method, survey_year, site, zone) %>% summarize(n.tx = n_distinct(transect))




#next sum transects across zones at the site level
MLPA_swath_tx_sum <- MLPA_swath_tx_by_zone %>% group_by(campus, method, survey_year, site) %>% summarize(n.swath.tx = sum(n.tx))

#make sure survey_year is factor
MLPA_swath_tx_sum$survey_year <- as.factor(MLPA_swath_tx_sum$survey_year)
MLPA_swath_site_means$survey_year <- as.factor(MLPA_swath_site_means$survey_year)

#join transect count to main dataset
MLPA_swath_site_means.2 <- left_join(MLPA_swath_tx_sum, MLPA_swath_site_means)

###################################
###   Add timeseries category   ### 
###################################

#ACTION - before continuing, each year review "master_site_table"- review the 'time series category' to be certain we have assigned them correctly.
# this assignment allows us to filter out only data with good (2) or very good (1) timeseries

#################
#Load in the 'master_site_table' in order to assign data as good or very good

#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
swath_time_series <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8")
#reduce to the variables we need
swath_time_series <- swath_time_series[c("campus","site","latitude", "longitude", "mpa_group", "site_status", "mlpa_region", "time_series_category")]   

#merge the site table info into the swath data (including the timeseries category to be used for filtering out the good timeseries data)
#merge by campus and site to prevent duplication of lines of data when there are two lines for a site in master_site_table (i.e. DEL-MAR_REFERENCE_2)
MLPA_swath_site_means.3 <- join(MLPA_swath_site_means.2, swath_time_series, by=c("campus", "site"))


#merge the site table info into the swath data
# MLPA_swath_site_means.3 <- left_join(MLPA_swath_site_means.2, swath_time_series)

###TESTING -DM
mean(subset(MLPA_swath_site_means.3$den_STRPURAD, MLPA_swath_site_means.3$site=="HOPKINS_UC"))
MLPA_swath.OUTPUTED <- read.csv(file="MLPA merged datasets/MLPA_swath_site_means.csv")
mean(subset(MLPA_swath.OUTPUTED$den_STRPURAD, MLPA_swath.OUTPUTED$site=="HOPKINS_UC"))
MLPA_swath_site_means.5$den_STRPURAD <- as.numeric(MLPA_swath_site_means.5$den_STRPURAD)
mean(subset(MLPA_swath_site_means.5$den_STRPURAD, MLPA_swath_site_means.5$site=="HOPKINS_UC"))


#reorder columns so that site info moves to the beginning (COLUMN NAMES MAY CHANGE IN FUTURE, NOTE HERE)
MLPA_swath_site_means.4 <- MLPA_swath_site_means.3 %>% 
  select(campus, method, survey_year, site, latitude, longitude, mpa_group, site_status, mlpa_region, time_series_category, n.swath.tx, everything())

#need to flatten the data frame because something came in as a list
MLPA_swath_site_means.5 <- data.frame(lapply(MLPA_swath_site_means.4, as.character), stringsAsFactors=FALSE)

#####################################
###   Export site means dataset   ###
#####################################

#write to csv in MLPA merged dataset folder
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_swath_site_means.5, file="MLPA merged datasets/MLPA_swath_site_means.csv", row.names = FALSE, na = "")


########################################################################################################################################################################


############################
##########   UPC  ##########
############################


#clear environment
rm(list=ls())

###############################
###   Bring in PISCO data   ###
###############################

#set working directory for bringing in PISCO data (i.e. UCSC and UCSB data that has already been merged)
#for windows
setwd("G:/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")
#setwd("/Volumes/GoogleDrive/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")

PISCO_upc <- read.csv(file="PISCO_UPC.csv")
PISCO_upc$transect <- as.factor(PISCO_upc$transect)
PISCO_upc$survey_year <- PISCO_upc$year
#format method and rename "SBTL_UPC" to "SBTL_UPC_PISCO"
PISCO_upc$method <- as.character(PISCO_upc$method)
PISCO_upc$method <- if_else(PISCO_upc$method == "SBTL_UPC", "SBTL_UPC_PISCO", PISCO_upc$method)
PISCO_upc$method <- as.factor(PISCO_upc$method)
#drop Oregon data
PISCO_upc <- subset(PISCO_upc, method != "SBTL_UPC_OREGON")
#drop a couple of one-off sites without site info
#PISCO_upc <- subset(PISCO_upc, site != "PISMO_W")
#PISCO_upc <- subset(PISCO_upc, site != "SAL_E")


#####################################
###   Bring in HSU and VRG data   ###
#####################################

#set working directory for bringing in HSU and VRG data
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
#setwd("/Volumes/GoogleDrive/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")

#bring in HSU dataset
HSU_upc <- read.csv(file="Final data by sampling group/HSU_UPC_2014-2023.csv")
HSU_upc$transect <- as.factor(HSU_upc$transect)

#bring in VRG dataset
VRG_upc <- read.csv(file="Final data by sampling group/VRG_UPC_2004-2023.csv")
VRG_upc$transect <- as.factor(VRG_upc$transect)

#merge datasets
MLPA_upc <- bind_rows(PISCO_upc, HSU_upc, VRG_upc)
#previous step converts factor variables to characters and produces warnings. Convert back to factor variables.
MLPA_upc <-MLPA_upc %>% mutate_if(is.character,as.factor)

#generate list of classcodes to check whether some should be combined so that there is better continuity across groups and years (e.g. erect fleshy red algae)
upc_classcodes <- as.data.frame(sort(unique(MLPA_upc$classcode)))


##############################
#Convert site names  ###Special note- in future years we plan to move this code to "DataIn" so that all data has consistent site names from the beginning of the process

#Load in the 'master_site_table' in order to convert sitenames to the uniform standard. This also changes some site names that are needed to match up sites with different names with different groups.
#For example UCSB site names in Malibu and at Santa Barbara Island need to be converted to VRG site names because they currently survey those sites.
#We will use VRG site names for all SBI sites, even those that aren't in current monitoring plan.
#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
site.name.conversions <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8")
#re-name the 'site' column to 'site_name_current', this will be the name we convert to

#SPECIAL NOTE: As of July 2021 the decision was made that the official site names in all of the merged datasets will be in PISCO format which is all capitols and underscores

site.name.conversions <- dplyr::rename(site.name.conversions, site_name_current = site)
#copy the 'site_name_old' column and call it 'site' to match up with the swath data
site.name.conversions$site <- site.name.conversions$site_name_old
#simplify to just the site name variables needed for conversions
site.name.conversions <- site.name.conversions[c("site", "site_name_current", "site_name_old")]

#merge site conversion table into data
MLPA_upc.2 <- left_join(MLPA_upc, site.name.conversions)

#for site names that do not need to be converted (i.e. do not have a value for "site_name_old"), in column 'site_name_current' replace the 'NA' with the site name
MLPA_upc.2$site_name_current[is.na(MLPA_upc.2$site_name_current)] <- MLPA_upc.2$site[is.na(MLPA_upc.2$site_name_current)]

#check that the update worked correctly
MLPA_upc_sitescheck <- unique(MLPA_upc.2[c("site", "site_name_current", "site_name_old")])

##ACTION - review 'MLPA_upc_sitecheck' manually. this should list all site names that were converted


#drop variable 'site' and rename 'site_name_current' as the new 'site'
MLPA_upc.2$site <- NULL
names(MLPA_upc.2)[names(MLPA_upc.2) == "site_name_current"] <- "site"

#order columns for export
MLPA_upc.3 <- MLPA_upc.2[c("campus", "method", "survey_year", "year", "month", 
                               "day", "site", "zone", "transect", "category", "classcode", 
                               "count", "pct_cov", "depth", "observer", "notes", "site_name_old")]

MLPA_upc.4 <-MLPA_upc.3 %>% mutate_if(is.character,as.factor)
#insert NA for blank observer names
MLPA_upc.4$observer[MLPA_upc.4$observer==""] <- NA
#insert NA for blank notes
MLPA_upc.4$notes[MLPA_upc.4$notes==""] <- NA

#copy dataset to be rounded and exported as the "raw" dataset. The next section will use the non-rounded data for site means etc.
MLPA_upc.raw <- MLPA_upc.4

#round percent cover to one decimal (this is the correct precision)
MLPA_upc.raw$pct_cov <- round(MLPA_upc.raw$pct_cov, digits = 1)

summary(MLPA_upc.raw)

##ACTION - review the summary visually

##################################
###   Export raw UPC dataset   ###
##################################
#This dataset has the transect level values

#export merged raw dataset
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_upc.raw, file="MLPA merged datasets/MLPA_kelpforest_upc.csv", row.names=FALSE)
####################################################################################


#####################################################
###   Create UPC  transect level CLEAN data set   ###
#####################################################
#CLEAN refers to the data filtered by LOOKEDFOR, a handful of codes being lumped, and being zero populated
#This has to be performed before site means can be calculated

#combine all categories of fleshy erect red algae into one 'erectred' category to consolidate across years and groups
#This decision to group was made early on and can be re-addressed in the future if red-algea specific questions come to bare
MLPA_upc.4$classcode <- revalue(MLPA_upc.4$classcode, c("BRANCH" = "ERECTRED", "BUSHY" = "ERECTRED", "BUSHY00" = "ERECTRED", 
                                                        "LACY" = "ERECTRED", "LEAFY" = "ERECTRED", "SB00RED" = "ERECTRED", 
                                                        "SC00RED" = "ERECTRED", "SCSB99RED" = "ERECTRED"))

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.4 |> get_dupes()

#sum at the classcode level to aggregate red algae counts, just include pct_cov b/c this is what we will be summarized at the site level
MLPA_upc.5 <- MLPA_upc.4 %>% 
  group_by(campus, method, survey_year, year, month, day, site, zone, transect, category, classcode) %>% 
  summarize(pct_cov = sum(pct_cov))

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.5 |> get_dupes() # 0

#drop 'deep' transects done by VRG (not done by other groups)
MLPA_upc.6 <- subset(MLPA_upc.5, zone != "DEEP")

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.6 |> get_dupes() #0

#a small number of transects have missing data from one category (e.g. substrate and cover but no relief), need to remove these categories b/c they mess up averages
MLPA_upc_category_transpose <- dcast(MLPA_upc.6, campus + method + survey_year + year + month + day + site + zone + transect ~ category, value.var = "pct_cov", fun.aggregate = sum)

MLPA_upc_category_transpose$COVER <- as.character(MLPA_upc_category_transpose$COVER)
MLPA_upc_category_transpose$COVER <- if_else(MLPA_upc_category_transpose$COVER=="0", "drop", "keep")
MLPA_upc_category_transpose$SUBSTRATE <- as.character(MLPA_upc_category_transpose$SUBSTRATE)
MLPA_upc_category_transpose$SUBSTRATE <- if_else(MLPA_upc_category_transpose$SUBSTRATE=="0", "drop", "keep")
MLPA_upc_category_transpose$RELIEF <- as.character(MLPA_upc_category_transpose$RELIEF)
MLPA_upc_category_transpose$RELIEF <- if_else(MLPA_upc_category_transpose$RELIEF=="0", "drop", "keep")
MLPA_upc_category_transpose$SUPERLAYER <- "keep"

#recast to long form to merge back in later and drop missing categories
MLPA_upc_category_drop <- melt(MLPA_upc_category_transpose, id.vars = c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect"), value.name = "drop", variable.name = "sample_subtype")

#transpose classcode to zero populate
MLPA_upc_transposed <- dcast(MLPA_upc.6, campus + method + survey_year + year + month + day + site + zone + transect ~ classcode, value.var = "pct_cov")

#zero populate
MLPA_upc_transposed[is.na(MLPA_upc_transposed)] <- 0

#recast to long form to merge with looked for info from species table
MLPA_upc_zeros <- melt(MLPA_upc_transposed, id.vars = c("campus", "method", "survey_year", "year", "month", "day", "site", "zone", "transect"), value.name = "pct_cov", variable.name = "classcode")
 
# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc_zeros |> get_dupes() #0 

#bring in master species table from Google to incorporate 'looked for' info and filter out species not counted consistantly or accurately
master.species.table <- read_sheet("https://docs.google.com/spreadsheets/d/1AihNG0z8wlBRz_XQaeU3HfH7QSbZ-kkOSrYkN8AFlS0/edit#gid=0", sheet = "SPP_TABLE", col_names = TRUE)
upc.species.table <- subset(master.species.table, sample_type == "UPC")


#Keep "LOOKED" columns, campus, and pisco_classcode. Drop all others to not create duplicate lines during the join. 
upc.species.table.2 <- upc.species.table |> select(campus, pisco_classcode, sample_subtype, starts_with("LOOKED"))

#drop where code = delete
upc.species.table.2 <- subset(upc.species.table.2, pisco_classcode != "DELETE")

#Filter for distinct rows so each code is represented once per campus
#Check that all codes only occur once per campus. 
code.dupes <- upc.species.table.2 |> 
  distinct() |> 
  get_dupes(c(campus, pisco_classcode, sample_subtype))

#code.dupes should be 0. If more than 0 check for differences in LOOKED for columns

#Once it is confirmed there are no code dupes and all classcodes have the same Looked for years, condense to one like per campus-classcode combination
upc.species.table.2 <-  upc.species.table.2 |> distinct()


#transpose years
##ACTION - each year you need to add the variable "LOOKEDYYYY" to the next line of code
upc.species.looked <- melt(upc.species.table.2, id.vars = c("campus", "pisco_classcode", "sample_subtype"), measure.vars = c("LOOKED1999", "LOOKED2000", "LOOKED2001", 
                                                                                                                             "LOOKED2002", "LOOKED2003", "LOOKED2004", 
                                                                                                                             "LOOKED2005", "LOOKED2006", "LOOKED2007", 
                                                                                                                             "LOOKED2008", "LOOKED2009", "LOOKED2010", 
                                                                                                                             "LOOKED2011", "LOOKED2012", "LOOKED2013", 
                                                                                                                             "LOOKED2014", "LOOKED2015", "LOOKED2016", 
                                                                                                                             "LOOKED2017", "LOOKED2018", "LOOKED2019", 
                                                                                                                             "LOOKED2020", "LOOKED2021", "LOOKED2022",
                                                                                                                             "LOOKED2023"))
#revalue 'X' in looked for columns to yes and blank values to no
upc.species.looked$value <- revalue(upc.species.looked$value, c("X"="yes"))
upc.species.looked$value[is.na(upc.species.looked$value)] <- "no"

#removed 'LOOKED' so the variable is just the year
upc.species.looked$survey_year <- as.factor(gsub("LOOKED", "", upc.species.looked$variable))
names(upc.species.looked)[names(upc.species.looked) == "value"] <- "LOOKEDFOR"
upc.species.looked$variable <- NULL

#join upc dataset with 'looked for' table to filter out species that were not effectively or consistently looked for in a given year by a given program
names(upc.species.looked)[names(upc.species.looked) == "pisco_classcode"] <- "classcode"
#format campus and classcode as factors in looked for table
upc.species.looked$campus <- as.factor(upc.species.looked$campus)
upc.species.looked$classcode <- as.factor(upc.species.looked$classcode)
#format survey_year as factor in upc data prior to merging
MLPA_upc_zeros$survey_year <- as.factor(MLPA_upc_zeros$survey_year)
#join tables
MLPA_upc.7 <- left_join(MLPA_upc_zeros, upc.species.looked, by=c("survey_year", "campus", "classcode"))

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.7 |> get_dupes() # 40512 > changed years to match for DODPAC > 28464!
unique_test <- unique(test[c('campus', 'classcode')])
#setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data/Data processing/QAQC temporary files")
#write.csv(unique_test, file="master_spp_table_errors.csv", row.names = FALSE)

#recode ERECTRED as looked for (consolidated classcode needs to be assigned manually)
MLPA_upc.7$LOOKEDFOR <- if_else(MLPA_upc.7$classcode == "ERECTRED", "yes", as.character(MLPA_upc.7$LOOKEDFOR))
MLPA_upc.7$sample_subtype <- if_else(MLPA_upc.7$classcode == "ERECTRED", "COVER", as.character(MLPA_upc.7$sample_subtype))

#reformat variables as factors in joined dataset
MLPA_upc.7$LOOKEDFOR <- as.factor(MLPA_upc.7$LOOKEDFOR)

#LOOKEDFOR will include NAs (species that aren't in the master species table for a specific campus), check the NAs by campus and classcode
lookedfor.nas <- subset(MLPA_upc.7, is.na(LOOKEDFOR))
lookedfor.nas.2 <- unique(lookedfor.nas[c("campus", "classcode")])

#check the not looked for records to make sure they only include the appropriate species
not.looked.for.upc <- subset(MLPA_upc.7, LOOKEDFOR == "no")
#filter out the zeros and look at the not looked for species that had non-zero counts
not.looked.for.upc.2 <- subset(not.looked.for.upc, pct_cov != 0 )
#look at the species by year by campus (may need to be adjusted in the looked for columns of the master species table)
not.looked.for.upc.spp <- unique(not.looked.for.upc.2[c("campus", "survey_year", "classcode")])

##ACTION review 'not.looked.for.upc.spp' as of 5-25-22 0 observations

#filter out not looked for observations (this will remove no and NA)
MLPA_upc.8 <- subset(MLPA_upc.7, LOOKEDFOR == "yes")

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.8 |> get_dupes() # 18766

#merge in the drop column created earlier to drop out missing categories from transects before taking the mean
#first format variables
MLPA_upc_category_drop$survey_year <- as.factor(MLPA_upc_category_drop$survey_year)
MLPA_upc.9 <- left_join(MLPA_upc.8, MLPA_upc_category_drop)

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.9 |> get_dupes() # 18766

#drop the missing categories
MLPA_upc.10 <- subset(MLPA_upc.9, drop=="keep")
MLPA_upc.10$category_classcode <- paste(MLPA_upc.10$sample_subtype, MLPA_upc.10$classcode, sep = "_")

# KS REMOVE AFTER DUPES RESOLVED 
test <- MLPA_upc.10 |> get_dupes() # 18766

#check that the appropriate categories and classcodes are matched
cat_class <- as.data.frame(unique(MLPA_upc.10$category_classcode))

################################################
###   Export CLEAN transect level data set   ###
################################################

##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_upc.10, file="MLPA merged datasets/MLPA_upc_CLEAN.csv", row.names = FALSE)


##########################################
###   Create site means UPC data set   ###
##########################################

#calculate site mean densities now that zeros are added and not looked for species and missing categories from transects are removed
MLPA_upc.means <- MLPA_upc.10 %>% group_by(campus, method, survey_year, site, category_classcode) %>% 
  summarize(mean_pctcov = mean(pct_cov)) |> 
  ungroup()

#transpose back to wide form. Now zeros are real zeros and blanks are not looked for
MLPA_upc_site_means <- dcast(MLPA_upc.means, campus + method + survey_year + site  ~ category_classcode, value.var = "mean_pctcov")
MLPA_upc_site_means[is.na(MLPA_upc_site_means)] <- ""


##########################################
###   Export UPC site means data set   ###
##########################################

##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_upc_site_means, file="MLPA merged datasets/MLPA_upc_site_means.csv", row.names = FALSE, na = "")
#note that all site means data sets have to be "CLEAN" because you have to use the CLEAN transect level dataset to make them



############################################################################################################################################################################################################################################################



###################################################
##########   SWATH & UPC = 'BENTHIC'   ############
###################################################

##Combine Swath and UPC data to create a "BENTHIC" Dataset with transect level means

#clean up global environment
rm(list=ls())


#bring in upc site means by reading the file exported above
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")
MLPA_upc_site_means <- read.csv(file="MLPA merged datasets/MLPA_upc_site_means.csv")
MLPA_upc_site_means$survey_year <- as.factor(MLPA_upc_site_means$survey_year)


#bring in swath site means by reading the file exported above
MLPA_swath_site_means <- read.csv(file="MLPA merged datasets/MLPA_swath_site_means.csv")
MLPA_swath_site_means$survey_year <- as.factor(MLPA_swath_site_means$survey_year)


#drop method from both swath and upc for joining
MLPA_swath_site_means$method <- NULL
MLPA_upc_site_means$method <- NULL

#Join UPC and Swath
MLPA_benthic_site_means <- full_join(MLPA_swath_site_means, MLPA_upc_site_means)

#in 2012, site WESTON_DC has upc data but no swath data. Fill in site info manually

MLPA_benthic_site_means$latitude <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", 36.51035, MLPA_benthic_site_means$latitude)
MLPA_benthic_site_means$longitude <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", -121.9458, MLPA_benthic_site_means$longitude)
MLPA_benthic_site_means$mpa_group <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", "Point Lobos SMR", MLPA_benthic_site_means$mpa_group)
MLPA_benthic_site_means$mlpa_region <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", "CENTRAL", MLPA_benthic_site_means$mlpa_region)
MLPA_benthic_site_means$time_series_category <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", "1", MLPA_benthic_site_means$time_series_category)
MLPA_benthic_site_means$n.swath.tx <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", as.integer(0), MLPA_benthic_site_means$n.swath.tx)

#For 2023 data KS did not run these 2 lines because there was no match for the columns in the dataframe and the columns are not present in last years dataset 
#MLPA_benthic_site_means$tier <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", "I", MLPA_benthic_site_means$tier)
#MLPA_benthic_site_means$mpa_status <- if_else(MLPA_benthic_site_means$site == "WESTON_DC" & MLPA_benthic_site_means$survey_year == "2012", "mpa", MLPA_benthic_site_means$mpa_status)

#####################################
###   Export "Benthic" dataset   ###
####################################

##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_benthic_site_means, file="MLPA merged datasets/MLPA_benthic_site_means.csv", row.names = FALSE, na = "")

####################################################################################


#######################################
##########   SIZEFREQUENCY   ##########  
#######################################

rm(list=ls())


##Avreys notes for fixes to be made 1-8-24
#method for all data should be SBTL_SIZEFREQ_PISCO, but there are location = TRANSECT (sizes collected on a swath transect), SIZE_TRANSECT (HSU only, special transects laid for doing sizes only), RANDOM (roving diver)
#in Jan 2024 APF confirmed VRG size methods with Jonathan. All sizes collected on swath transects are entered to their ISC data file. regardless of transect # they are entered as transect = 1.
#Additional roving diver sizes are entered to ISC as well, therefore we CANNOT separate sizes collected on transect versus off transect.
#APF recommendation to move forward = do not include "location". always Pull all sizes from swath - including MACPYR
#for UCSB this means PANINT sizes from fish simply get transferred to sizefreq **however TBD- lobster abundance needs its own file?
#Basic level for this data should be either zone or site- all VRG sizes are at zone level, UCSB has some sizes from 'side' (site) level.



###############################
###   Bring in PISCO data   ###
###############################

#set working directory for bringing in PISCO data (i.e. UCSC and UCSB data that has already been merged)
setwd("G:/Shared drives/PISCO Subtidal UCSB and UCSC/PISCO subtidal data clearinghouse")


PISCO_sizefreq <- read.csv(file="PISCO_ROVINGDIVERSIZEFREQ.csv")
PISCO_sizefreq$transect <- as.factor(PISCO_sizefreq$transect)
#add survey_year, which is same as year
PISCO_sizefreq$survey_year <- PISCO_sizefreq$year

#format and edit "method"  *The end product will combine roving diver sizes and sizes from swath surveys, so method will just be SBTL_SIZEFREQ_PISCO from here on out
PISCO_sizefreq$method <- factor(PISCO_sizefreq$method)
summary(PISCO_sizefreq$method)

#generate list of methods
method_check <- as.data.frame(unique(PISCO_sizefreq$method))
#Rename SBTL_ROVINGDIVERSIZEFREQ to SBTL_SIZEFREQ_PISCO
PISCO_sizefreq$method <- if_else(PISCO_sizefreq$method == "SBTL_ROVINGDIVERSIZEFREQ", "SBTL_SIZEFREQ_PISCO", PISCO_sizefreq$method)

PISCO_sizefreq$method <- as.factor(PISCO_sizefreq$method)
summary(PISCO_sizefreq$method)


#drop Oregon data
PISCO_sizefreq <- subset(PISCO_sizefreq, method != "SBTL_SIZEFREQ_OREGON")


#####################################
###   Add sizes from swath data   ###
#####################################

#Only need to do for PISCO data. All VRG sizes (except MACPYR) are in the ISC file

PISCO_swath <- read.csv(file="PISCO_SWATH.csv")

#survey year was manually added by APF. Only one january survey (in 2014) was manually changed to 2013

#add location variable and populate with TRANSECT
PISCO_swath$location <- "TRANSECT"

#subset to observations with size information (excluding Macrocystis)
PISCO_swath.2 <- subset(PISCO_swath, !is.na(size) & classcode != "MACPYRAD")
#drop METERS_sampled and segment from swath and rename notes to match
names(PISCO_swath.2)[names(PISCO_swath.2) == 'Notes'] <- 'NOTES'
PISCO_swath.2$METERS_sampled <- NULL
PISCO_swath.2$SEGMENT <- NULL
#convert transect from integer to factor
PISCO_swath.2$transect <- as.factor(PISCO_swath.2$transect)

#COMBINE SIZING from SWATH with SIZEFREQ
PISCO_sizefreq.2 <- bind_rows(PISCO_sizefreq, PISCO_swath.2)



#At this stage PISCO data includes sizes from roving diver and pulled from swath

#############################
###   Bring in VRG data   ###
#############################

#set working directory for bringing in VRG data
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")

#bring in VRG dataset
VRG_sizefreq <- read.csv(file="Final data by sampling group/VRG_SIZEFREQ_2004-2023.csv")
VRG_sizefreq$transect <- as.factor(VRG_sizefreq$transect)


###APF 9-23-21 I looked at the raw swath data from VRG and they DO NOT have sizes for anything other than MACPYR
###KS also checked on 4-18-24 and found the same thing
#therefore DO NOT add sizes from swath

##############################
###   Bring in HSU data   ###
#############################

#set working directory for bringing in HSU  data
setwd("G:/Shared drives/MLPA L-T Kelp Forest Monitoring Project - Admin/Monitoring Data")

#bring in HSU dataset
HSU_sizefreq <- read.csv(file="Final data by sampling group/HSU_SIZEFREQ_2014-2023.csv")
#make sure "transect" is factor
HSU_sizefreq$transect <- as.factor(HSU_sizefreq$transect)


#HSU file already includes all sizes that exist. There are only a few sizes from swath data (only in 2017) and those are already entered into the size file
#Therefore DO NOT add sizes from swath for HSU

###########################
###   Merge data sets   ###
###########################

MLPA_sizefreq <- bind_rows(PISCO_sizefreq.2, HSU_sizefreq, VRG_sizefreq)
#previous step converts factor variables to characters and produces warnings. Convert back to factor variables.
MLPA_sizefreq <-MLPA_sizefreq %>% mutate_if(is.character,as.factor)


##############################
###   Convert site names   ###
##############################

#Special note- in future years we plan to move this code to "DataIn" so that all data has consistent site names from the beginning of the process

#Load in the 'master_site_table' in order to convert sitenames to the uniform standard. This also changes some site names that are needed to match up sites with different names with different groups.
#For example UCSB site names in Malibu and at Santa Barbara Island need to be converted to VRG site names because they currently survey those sites.
#We will use VRG site names for all SBI sites, even those that aren't in current monitoring plan.
#This table is located G:\Shared drives\MLPA L-T Kelp Forest Monitoring Project - Admin\Monitoring Data\Site tables
#filename "master_site_table"
site.name.conversions <- read_sheet("https://docs.google.com/spreadsheets/d/13pGR8NGGleVXS2hfFvqChPVxOROu2lJb8YTINT2V5e8")
#re-name the 'site' column to 'site_name_current', this will be the name we convert to

#**SPECIAL NOTE** As of July 2021 the decision was made that the official site names in all of the merged datasets will be in PISCO format which is all capitols and underscores

site.name.conversions <- dplyr::rename(site.name.conversions, site_name_current = site)
#copy the 'site_name_old' column and call it 'site' to match up with the swath data
site.name.conversions$site <- site.name.conversions$site_name_old
#simplify to just the site name variables needed for conversions
site.name.conversions <- site.name.conversions[c("site", "site_name_current", "site_name_old")]



#merge site conversion table into data
MLPA_sizefreq.2 <- left_join(MLPA_sizefreq, site.name.conversions)

#update current site name with site names that didn't change
MLPA_sizefreq.2$site_name_current[is.na(MLPA_sizefreq.2$site_name_current)] <- MLPA_sizefreq.2$site[is.na(MLPA_sizefreq.2$site_name_current)]
#check that the update worked correctly
MLPA_sizefreq_sitescheck <- unique(MLPA_sizefreq.2[c("site", "site_name_current", "site_name_old")])

#drop variable 'site' and rename 'site_name_current' as the new 'site'
MLPA_sizefreq.2$site <- NULL
names(MLPA_sizefreq.2)[names(MLPA_sizefreq.2) == "site_name_current"] <- "site"

#order columns for export
MLPA_sizefreq.3 <- MLPA_sizefreq.2[c("campus", "method", "survey_year", "year", "month", 
                           "day", "site", "location", "zone", "transect", "classcode", "count", "size", "disease", "depth", "observer", "notes", "site_name_old")]

#drop records where classcode is NO_ORG                           
MLPA_sizefreq.4 <- subset(MLPA_sizefreq.3, classcode != "NO_ORG")



#Special edits specific to UCSC because of how they collect sizes of urchins
#For years 2010 and on, STRPUR and MESFRA sizes are collected and entered into swath data. recruits are separated from adults
#therefore combine classcode STRPURAD with STRPURREC, and also combine MEFRAAD with MESFRAREC
#therefore re-code STRPURAD and STRPURREC as STPUR
#get rid of 'AD' and 'REC' suffixes for urchins since sizes are listed
MLPA_sizefreq.4$classcode <- revalue(MLPA_sizefreq.4$classcode, c("STRPURAD"="STRPUR", "STRPURREC"="STRPUR", "MESFRAAD"="MESFRA", "MESFRAREC"="MESFRA"))

#manual check of classcodes
#generate list of classcodes
classcode_check <- as.data.frame(unique(MLPA_sizefreq.4$classcode))


#ACTION- manually check "classcode_check" to check for issues 


summary(MLPA_sizefreq.4)

#populate missing values with NA
MLPA_sizefreq.4$disease[MLPA_sizefreq.4$disease==""] <- NA
MLPA_sizefreq.4$observer[MLPA_sizefreq.4$observer==""] <- NA
MLPA_sizefreq.4$notes[MLPA_sizefreq.4$notes==""] <- NA



#########################################
###   Export Size Frequency dataset   ###
#########################################

#NOTE that the filename is long in order to make it clear that this file includes all sizes we have ever taken!
#write to csv in MLPA merged dataset folder
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_sizefreq.4, file="MLPA merged datasets/MLPA_kelpforest_rovingdiver_and_transect_sizefreq.csv", row.names=FALSE)

##ACTION - there are manual edits that have to be done##


#Manual edit for UCSB- COCHE to COCHE_POINT
#Special MANUAL edits specific to UCSC
#There are a few lines of data from UCSC 2005 of urchins with sizes, however this is an anomaly because sizes were not recorded on swath at UCSC until 2010
#therefore APF is manually deleting these lines in excel after exporting. I need to code this part when i have time
#deleted two lines of data of NERLUE with sizes (were imported from swath)

####################################################################################
# as of 9-30-21 need to revisit this idea- need to clarify with D Malone what we are putting on DataOne
#Now we need to make MLPA_rovingdiver_sizefreq - this is the file that can be added to DataOne
# It needs to only have roving diver sizes, and NOT have any sizes from swath
#MLPA_sizefreq_rovingonly <- subset(MLPA_sizefreq.4, location !="TRANSECT")


####################################################################################
#Export Size Frequency rovingonly dataset
#NOTE that the filename is long in order to make it clear that this file includes all sizes we have ever taken!
#write to csv in MLPA merged dataset folder
##ACTION - need to un-comment line below to run. PLEASE comment it out again to guard against over-writing!!!
#write.csv(MLPA_sizefreq_rovingonly, file="MLPA merged datasets/MLPA_kelpforest_rovingonly_sizefreq.csv", row.names=FALSE)


