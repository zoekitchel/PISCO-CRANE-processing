# AUTHOR: kitchel@oxy.edu

# PURPOSE: Pull environmental data for CRANE sites (both in situ and external)

#############################
##Setup
#############################
library(ggplot2)
library(data.table)
library(dplyr)
library(sf)
library(raster)
library(sdmpredictors)
library(stringr)
library(rvest)
library(viridis)
library(marmap)
library(purrr)

#pull in functions
source("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/main/UPC_in_situ_habitat.R")
source("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/main/scripts/bathy_join_function.R")


#############################
#Pull in lat lon values ####
#############################

#Import CSV of PISCO site latitudes and longitudes
PISCO_lat_lon_site_16_23_SCB <- fread(file = file.path("data","keys","PISCO_lat_lon_site_16_23_SCB.csv"))

#Convert PISCO points to spatial points
PISCO_lat_lon_site.sf <- st_as_sf(PISCO_lat_lon_site_16_23_SCB,
                            coords = c("longitude","latitude"),
                            crs = 4326)

#############
#Prep in situ habitat characteristics
#############

#Pull in PISCO UPC data for substrate and relief calculations
PISCO_relief_substrate_UPC_16_23 <- fread(file = file.path("data","processed","PISCO_relief_substrate_UPC_16_23.csv"))

#Pull in PISCO swath data for macrocystis hold fast and stipe densities
PISCO_macrocystis_swath_16_23 <- fread(file = file.path("data","processed","PISCO_macrocystis_swath_16_23.csv"))

#######Calculate average macrocystis plant (count) and stipe (size) density per site

#Reminder: TAKE DEEPS OUT of site averages for VRG FOR COMPARISON w/ PISCO

#plants
PISCO_plant_density_bysitezone <- PISCO_macrocystis_swath_16_23[classcode == "MACPYRAD",.(survey_year,year, month, day, campus, site, zone, transect, count)]
PISCO_plant_density_bysitezone[, giantkelp_plant_density_m2_annual := sum(count),.(survey_year,year, month, day, campus, site, zone)] #Sum count across 2 transects
PISCO_plant_density_bysitezone[, giantkelp_plant_density_m2_annual := giantkelp_plant_density_m2_annual/120] #Divide by area of 2 transects (120 m^2)

#Take mean values across full time period
PISCO_plant_density_bysitezone[, giantkelp_plant_density_m2 := mean(giantkelp_plant_density_m2_annual), .(campus, site, zone)]

#Unique site values
PISCO_plant_density_bysitezone.r <- unique(PISCO_plant_density_bysitezone[,.(site, zone, giantkelp_plant_density_m2)])

colnames(PISCO_plant_density_bysitezone.r) <- c("Site","DepthZone","giantkelp_plant_density_m2")

#stipes
PISCO_stipe_density_bysitezone <- PISCO_macrocystis_swath_16_23[classcode == "MACPYRAD",.(survey_year,year, month, day, campus, site, zone, transect, count, size)]
PISCO_stipe_density_bysitezone[,stipe_count := count*size] #multiply number of plants by size to get total stipe count for each size class
PISCO_stipe_density_bysitezone[, giantkelp_stipe_density_m2_annual := sum(stipe_count),.(survey_year,year, month, day, campus, site, zone)] #Sum count across 2 transects
PISCO_stipe_density_bysitezone[, giantkelp_stipe_density_m2_annual := giantkelp_stipe_density_m2_annual/120] #Divide by area of 2 transects (120 m^2)

#Take mean values for each zone and site across full time period
PISCO_stipe_density_bysitezone[, giantkelp_stipe_density_m2 := mean(giantkelp_stipe_density_m2_annual), .(campus, site, zone)]

#Unique site values
PISCO_stipe_density_bysitezone.r <- unique(PISCO_stipe_density_bysitezone[,.(site, zone, giantkelp_stipe_density_m2)])

colnames(PISCO_stipe_density_bysitezone.r) <- c("Site","DepthZone","giantkelp_stipe_density_m2")


##Relief PISCO ####
PISCO_relief <- get_relief(PISCO_relief_substrate_UPC_16_23, source = "PISCO_dataone")

#average to get single value per Site and Depth Zone
PISCO_relief <- PISCO_relief[,.(site, zone, Relief_index, Relief_SD, Relief_simpson)] #limit to columns we need

PISCO_relief_bysitezone <-  PISCO_relief[ , lapply(.SD, mean) , by=c("site","zone")] #single value for each site x zone

setnames(PISCO_relief_bysitezone, c("site","zone"),c("Site","DepthZone"))

##Substrate PISCO ####
PISCO_substrate <- get_substrate(PISCO_relief_substrate_UPC_16_23, source = "PISCO_dataone")

#average to get single value per Site and Depth Zone
PISCO_substrate <- PISCO_substrate[,.(site, zone, Substrate_index, Substrate_SD, Substrate_simpson)] #limit to columns we need

PISCO_substrate_bysitezone <-  PISCO_substrate[ , lapply(.SD, mean) , by=c("site","zone")] #single value for each site x zone

setnames(PISCO_substrate_bysitezone, c("site","zone"),c("Site","DepthZone"))


##################################################
#Prep large scale habitat characteristics
##################################################
#event data
source("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/main/scripts/bathy_join_function.R")

#######TEMP

BO_sst <- load_layers(layercodes = c("BO_sstmax", "BO_sstmean", "BO_sstmin", "BO_sstrange") ,
                      equalarea=FALSE, rasterstack=TRUE) 



#reduce to PISCO sites in CA only
CA_sst <- crop(BO_sst, extent(min(PISCO_lat_lon_site_16_23_SCB$longitude, na.rm = T)-0.1, max(PISCO_lat_lon_site_16_23_SCB$longitude, na.rm = T)+0.1,
                              min(PISCO_lat_lon_site_16_23_SCB$latitude, na.rm = T)-0.1, max(PISCO_lat_lon_site_16_23_SCB$latitude, na.rm = T)+0.1))

rm(BO_sst)

#create multifocal function to apply focal function to each layer of the raster brick

multiFocal <- function(x, w=matrix(1, nr=3, nc=3), ...) {
  
  if(is.character(x)) {
    x <- brick(x)
  }
  # The function to be applied to each individual layer
  fun <- function(ind, x, w, ...){
    focal(x[[ind]], w=w, ...)
  }
  
  n <- seq(nlayers(x))
  list <- lapply(X=n, FUN=fun, x=x, w=w, ...)
  
  out <- stack(list)
  return(out)
}


#fill in missing cells with neighborhood averages
#important to note that this does also add erroneous cells with values on the edge, but this does not impact our extractions because these are points on land and no sampling points are on land
CA_sst.t <- multiFocal(CA_sst, w = matrix(1, 3, 3), fun = "mean", NAonly = TRUE, na.rm = T)

#transform to crs of raster stack
PISCO_lat_lon_site.t <- st_transform(PISCO_lat_lon_site.sf, crs = st_crs(CA_sst))

#add new columns with extracted temperature data
#maximum
PISCO_lat_lon_site_16_23_SCB[,BO_sstmax := raster::extract(CA_sst.t[[1]], PISCO_lat_lon_site.t)]

#mean
PISCO_lat_lon_site_16_23_SCB[,BO_sstmean := raster::extract(CA_sst.t[[2]], PISCO_lat_lon_site.t)]

#minimum
PISCO_lat_lon_site_16_23_SCB[,BO_sstmin := raster::extract(CA_sst.t[[3]], PISCO_lat_lon_site.t)]

#range
PISCO_lat_lon_site_16_23_SCB[,BO_sstseas := raster::extract(CA_sst.t[[4]], PISCO_lat_lon_site.t)]

#######TEMP from san diego 1km satellite
#Link: http://spg-satprojects.ucsd.edu
#Link: https://spg-satdata.ucsd.edu
#Metadata for HDF files

#Scrape links to monthly temp and chlorophyll values from UCSD website

#first, navigate to year page
folder_urls <- readLines("https://raw.githubusercontent.com/zoekitchel/socal_reef_community_structure/1f3d4dfe45d10af9b8cc82cc4060ab2218a87ef5/data/enviro_predictors/ucsd_data_download_1km_chl_sst_2016_2023.txt")

#empty data.table
full_link.dt <- data.table()

#make list of all links to download
for(i in 1:length(folder_urls)){
  url <- folder_urls[i]
  
  variable <- ifelse(str_detect(url,"chl"),"chl","sst")
  year <- as.numeric(substr(url,30,33))
  
  #individual month links using rvest
  monthly_link_list <- url %>%
    read_html() %>%
    html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>%
    html_attr("href") #identify all links on page
  
  monthly_link_list.r <- monthly_link_list |>
    keep(~ str_detect(.x, "comp.hdf"))
  
  #add full url to all links
  monthly_link_list.full <- paste0(url,monthly_link_list.r)
  
  #make datatable
  subset.dt <- data.table(variable = variable, year = year, month = seq(1,12),
                          file_name = monthly_link_list.r, link_long = monthly_link_list.full)
  
  full_link.dt <- rbind(full_link.dt, subset.dt)
  
}

#empty data table to fill with site, and month specific data
PISCO_lat_lon_site_variable_full <- data.table()

#load up hdf key with lat lon (can only be done on zoe's computer)
hdf_key <- stack(file.path("~","Dropbox","Repositories","socal_reef_community_structure","data","enviro_predictors","cal_aco_3840_Latitude_Longitude.hdf")) #TOO BIG TO PULL FROM GITHUB

#trim to study area
crop_ext <- extent(1617,2069,1727,2204)

hdf_key.c <- crop(hdf_key, crop_ext)

#convert into data table
hdf_key.xyz <- data.table(rasterToPoints(hdf_key.c))

colnames(hdf_key.xyz) <- c("x","y","latitude","longitude")

#download and process each kmz file and populate data table with chlorophyll and temp data
for(i in 1:nrow(full_link.dt)){
  temp <- tempdir()
  download.file(full_link.dt[i,link_long], file.path(temp, "temp.hdf"))
  hdf <- raster(file.path(temp, "temp.hdf"))
  
  #trim to study area
  crop_ext <- extent(1617,2069,1727,2204) #Visually assessed
  
  hdf.c <- crop(hdf, crop_ext)
  
  #change 255 values to NA ()
  hdf.c.re <- reclassify(hdf.c, cbind(255, NA)) #"values of 0 and 255 are considered invalid (from source)"
  
  #convert into data table
  hdf.xyz <- data.table(rasterToPoints(hdf.c.re))
  
  colnames(hdf.xyz) <- c("x","y","value")
  
  #merge with key
  hdf_merge <- hdf.xyz[hdf_key.xyz, on = c("x","y")]
  
  #set up empty raster
  e <- extent(min(hdf_merge$longitude), max(hdf_merge$longitude), min(hdf_merge$latitude),max(hdf_merge$latitude))
  
  r <- raster(e,ncol = 452, nrow = 477)
  
  #then, rasterize xyz
  new_raster <- rasterize(x = hdf_merge[,.(longitude,latitude)], y = r, field = hdf_merge[,.(value)], fun = mean, na.rm = T)
  
  #set crs
  crs(new_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  #match crs of lat lon to crs of raster
  lat_lon_site.t <- st_transform(PISCO_lat_lon_site.sf, crs(new_raster))
  
  value <- raster::extract(new_raster,lat_lon_site.t) #21 lat lon from VRG are missing! check these some time!
  
  #copy input lat lon 
  lat_lon_site_variable <- copy(PISCO_lat_lon_site_16_23_SCB)
  
  #fill new columns
  lat_lon_site_variable[,year:=full_link.dt[i,year]][,month:=full_link.dt[i,month]][,variable:= full_link.dt[i,variable]][,value:=  value]
  
  #add to full data table
  PISCO_lat_lon_site_variable_full <- rbind(PISCO_lat_lon_site_variable_full,   lat_lon_site_variable)
  
  print(paste0(i," out of ",nrow(full_link.dt)))
  
}

#Note that values use 1 byte per pixel with standard scaling. Linear scaling is used for SST and logarithmic scaling for Chl: https://spg-satdata.ucsd.edu/Readme.htm
#so, conversions
#SST (deg C) = 0.15 * PV - 3.0
PISCO_lat_lon_site_variable_full[variable == "sst",value_adj := 0.15*value-3.0]

#Chl (mg m-3) = 10^(0.015 * PV - 2.0), i.e. 10 to the power of 0.015 * PV - 2.0
PISCO_lat_lon_site_variable_full[variable == "chl",value_adj :=10^(0.015*value-2.0)]


#save as csv
#fwrite(PISCO_lat_lon_site_variable_full, file.path("data","raw","enviro_predictors","PISCO_lat_lon_site_variable_full.csv"))
PISCO_lat_lon_site_variable_full <- fread(file.path("data","raw","enviro_predictors","PISCO_lat_lon_site_variable_full.csv"))

#mean_value per site across all months in time series
PISCO_lat_lon_site_variable_full[, mean_value_adj := mean(value_adj, na.rm = T),.(site, variable)]

#min and max value per site across all months in time series
#mean annual minimum
PISCO_lat_lon_site_variable_full[, min_annual_value_adj := min(value_adj, na.rm = T),.(site, variable, year)][,min_annual_value_adj := ifelse(is.infinite(min_annual_value_adj), NA, min_annual_value_adj)]
PISCO_lat_lon_site_variable_full[, min_value_adj := mean(min_annual_value_adj, na.rm = T),.(site, variable)]
#mean annual maximum
PISCO_lat_lon_site_variable_full[, max_annual_value_adj := max(value_adj, na.rm = T),.(site, variable, year)][,max_annual_value_adj := ifelse(is.infinite(max_annual_value_adj), NA, max_annual_value_adj)]
PISCO_lat_lon_site_variable_full[, max_value_adj := mean(max_annual_value_adj, na.rm = T),.(site, variable)]

#reduce to one row per site
PISCO_lat_lon_site_variable_full.r <- unique(PISCO_lat_lon_site_variable_full[,.(site, variable, mean_value_adj, min_value_adj, max_value_adj)])

#change name from site to Site
setnames(PISCO_lat_lon_site_variable_full.r,"site","Site")

#split into temperature and chlorophyll
PISCO_sst_bysite <- PISCO_lat_lon_site_variable_full.r[variable == "sst"][,mean_sst_C := mean_value_adj][,max_sst_C := max_value_adj][,min_sst_C := min_value_adj][,.(Site, mean_sst_C, max_sst_C, min_sst_C)]
PISCO_chl_bysite <- PISCO_lat_lon_site_variable_full.r[variable == "chl"][,mean_chl_mg_m3 := mean_value_adj][,max_chl_mg_m3 := max_value_adj][,min_chl_mg_m3 := min_value_adj][,.(Site, mean_chl_mg_m3, max_chl_mg_m3, min_chl_mg_m3)]

#######Distance to 200 m isobath

PISCO_distance_200mbathy_bysite <- add_depth_columns(PISCO_lat_lon_site_16_23_SCB, ETOPO = T, USGS_socal=F, ETOPO_dist_200m = T) #Try a second time if it doesn't work

#Uppercase S in site
setnames(PISCO_distance_200mbathy_bysite,"site","Site")

#remove lat lon columns
PISCO_distance_200mbathy_bysite <- unique(PISCO_distance_200mbathy_bysite[,.(Site, dist_200m_bath)])

###############################################
#merge all site level environmental variables
###############################################
#insitu environmental metrics
PISCO_substrate_bysitezone
PISCO_all_env_lat_lon <- PISCO_substrate_bysitezone[PISCO_lat_lon_site_16_23_SCB, on = c("Site"="site")]
PISCO_relief_bysitezone
PISCO_all_env_lat_lon <- PISCO_relief_bysitezone[PISCO_all_env_lat_lon, on = c("Site","DepthZone")]
PISCO_plant_density_bysitezone.r
PISCO_all_env_lat_lon <- PISCO_plant_density_bysitezone.r[PISCO_all_env_lat_lon, on = c("Site","DepthZone")]
PISCO_stipe_density_bysitezone.r
PISCO_all_env_lat_lon <- PISCO_stipe_density_bysitezone.r[PISCO_all_env_lat_lon, on = c("Site","DepthZone")]


#large scale environmental metrics
PISCO_distance_200mbathy_bysite
PISCO_all_env_lat_lon <- PISCO_distance_200mbathy_bysite[PISCO_all_env_lat_lon, on = c("Site")]
PISCO_sst_bysite
PISCO_all_env_lat_lon <- PISCO_sst_bysite[PISCO_all_env_lat_lon, on = c("Site")]
PISCO_chl_bysite
PISCO_all_env_lat_lon <- PISCO_chl_bysite[PISCO_all_env_lat_lon, on = c("Site")]

#FYI, only fish surveyed at ANACAPA_BLACK_SEA_BASS
PISCO_all_env_lat_lon <- PISCO_all_env_lat_lon[Site != "ANACAPA_BLACK_SEA_BASS"]

#save without depths
fwrite(PISCO_all_env_lat_lon,file.path("data","processed","PISCO_all_env_lat_lon.csv"))

PISCO_all_env_lat_lon <- fread(file.path("data","processed","PISCO_all_env_lat_lon.csv"))

#Finally, merge in averaged depth data
PISCO_site_zone_depth <- fread(file = file.path("data","processed","PISCO_site_zone_depth.csv"))

setnames(PISCO_site_zone_depth, c("site","zone"),c("Site","DepthZone"))

PISCO_all_env_lat_lon_wsurveydepth <- PISCO_site_zone_depth[PISCO_all_env_lat_lon, on = c("DepthZone","Site")]


#save

fwrite(PISCO_all_env_lat_lon_wsurveydepth,file.path("data","processed","PISCO_all_env_lat_lon_wsurveydepth.csv"))


