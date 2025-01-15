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


#############################
#Pull in lat lon values ####
#############################

#Import CSV of PISCO site latitudes and longitudes
PISCO_lat_lon_site <- fread(file = file.path("data","keys","PISCO_lat_lon_site.csv"))

#Convert PISCO points to spatial points
PISCO_lat_lon_site.sf <- st_as_sf(PISCO_lat_lon_site,
                            coords = c("longitude","latitude"),
                            crs = 4326)

#############
#Prep in situ habitat characteristics
#############

#Pull in PISCO UPC data for substrate and relief calculations
PISCO_relief_substrate_UPC_16_23 <- fread(file = file.path("data","processed","PISCO_relief_substrate_UPC_16_23.csv"))

#Pull in PISCO swath data for macrocystis hold fast and stipe densities
PISCO_macrocystis_swath_16_23 <- fread(file = file.path("data","processed","PISCO_macrocystis_swath_16_23.csv"))

#######Calculate average macrocystis stipe density and plant density per site

#TAKE DEEPS OUT of site averages FOR COMPARISON

#stipes
PISCO_stipe_density_bysite <- PISCO_macrocystis_swath_16_23[classcode == "MACPYRAD",.(survey_year,year, month, day, campus, site, zone, transect, count)]
PISCO_stipe_density_bysite[, giantkelp_stipe_density_m2_annual := sum(count)/120,.(survey_year,year, month, day, campus, site, zone)]

#Mean across years #FOR THIS TO WORK, I NEED TO ADD IN ZEROS, HAVEN'T DONE YET
PISCO_stipe_density_bysite[, giantkelp_stipe_density_m2 := mean(giantkelp_stipe_density_m2_annual), .(campus, site, zone)]

#Unique site values
PISCO_stipe_density_bysite.r <- unique(PISCO_stipe_density_bysite[,.(site, zone, giantkelp_stipe_density_m2)])

colnames(PISCO_stipe_density_bysite.r) <- c("Site","DepthZone","giantkelp_stipe_density_m2")

#kelp plants are NOT recorded by PISCO

###########relief
PISCO_relief <- get_relief(PISCO_relief_substrate_UPC_16_23, source = "PISCO_dataone")

#average to get single value per Site and Depth Zone
PISCO_relief <- PISCO_relief[,.(site,zone, Relief_index, Relief_SD, Relief_simpson)] #limit to columns we need

PISCO_relief_bysite <-  PISCO_relief[ , lapply(.SD, mean) , by=c("site","zone")] #single value for each site

###########substrate
PISCO_substrate <- get_substrate(PISCO_relief_substrate_UPC_16_23, source = "PISCO_dataone")

#average to get single value per Site and Depth Zone
PISCO_substrate <- PISCO_substrate[,.(site, zone, Substrate_index, Substrate_SD, Substrate_simpson)] #limit to columns we need

PISCO_substrate_bysite <-  PISCO_substrate[ , lapply(.SD, mean) , by=c("site", "zone")] #single value for each site


##################################################
#Prep large scale habitat characteristics
##################################################
#event data
source("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/main/scripts/bathy_join_function.R")

#######TEMP

BO_sst <- load_layers(layercodes = c("BO_sstmax", "BO_sstmean", "BO_sstmin", "BO_sstrange") ,
                      equalarea=FALSE, rasterstack=TRUE) 



#reduce to PISCO sites in CA only
CA_sst <- crop(BO_sst, extent(min(PISCO_lat_lon_site$longitude, na.rm = T)-0.1, max(PISCO_lat_lon_site$longitude, na.rm = T)+0.1,
                              min(PISCO_lat_lon_site$latitude, na.rm = T)-0.1, max(PISCO_lat_lon_site$latitude, na.rm = T)+0.1))

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


#fill in two missing cells with neighborhood averages
#important to note that this does also add erroneous cells with values on the edge, but this does not impact our extractions because these are points on land and no sampling points are on land
CA_sst.t <- multiFocal(CA_sst, w = matrix(1, 3, 3), fun = "mean", NAonly = TRUE, na.rm = T)

#transform to crs of raster stack
PISCO_lat_lon_site.t <- st_transform(PISCO_lat_lon_site.sf, crs = st_crs(CA_sst))

#add new columns with extracted temperature data
#maximum
PISCO_lat_lon_site[,BO_sstmax := raster::extract(CA_sst.t[[1]], PISCO_lat_lon_site.t)]

#mean
PISCO_lat_lon_site[,BO_sstmean := raster::extract(CA_sst.t[[2]], PISCO_lat_lon_site.t)]

#minimum
PISCO_lat_lon_site[,BO_sstmin := raster::extract(CA_sst.t[[3]], PISCO_lat_lon_site.t)]

#range
PISCO_lat_lon_site[,BO_sstseas := raster::extract(CA_sst.t[[4]], PISCO_lat_lon_site.t)]

#######TEMP san diego 1km satellite
#Link: http://spg-satprojects.ucsd.edu
#Link: https://spg-satdata.ucsd.edu
#Metadata for HDF files

#Scrape links to monthly temp and chlorophyll values from UCSD website

#START HERE (pull from github instead)

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
lat_lon_site_variable_full <- data.table()

#load up hdf key with lat lon
hdf_key <- stack(file.path("data","enviro_predictors","cal_aco_3840_Latitude_Longitude.hdf")) #TOO BIG TO PULL FROM GITHUB

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
  crop_ext <- extent(1617,2069,1727,2204)
  
  hdf.c <- crop(hdf, crop_ext)
  
  #change 255 values to NA ()
  hdf.c.re <- reclassify(hdf.c, cbind(255, NA)) #"values of 0 and 255 are considered invalid"
  
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
  lat_lon_site.t <- st_transform(lat_lon_site.sf, crs(new_raster))
  
  value <- raster::extract(new_raster,lat_lon_site.t) #21 lat lon from VRG are missing! check these some time!
  
  #copy input lat lon 
  lat_lon_site_variable <- copy(lat_lon_site_fix)
  
  #fill new columns
  lat_lon_site_variable[,year:=full_link.dt[i,year]][,month:=full_link.dt[i,month]][,variable:= full_link.dt[i,variable]][,value:=  value]
  
  #add to full data table
  lat_lon_site_variable_full <- rbind(lat_lon_site_variable_full,   lat_lon_site_variable)
  
  print(paste0(i," out of ",nrow(full_link.dt)))
  
}

#Note that values use 1 byte per pixel with standard scaling. Linear scaling is used for SST and logarithmic scaling for Chl: https://spg-satdata.ucsd.edu/Readme.htm
#so, conversions
#SST (deg C) = 0.15 * PV - 3.0
lat_lon_site_variable_full[variable == "sst",value_adj := 0.15*value-3.0]

#Chl (mg m-3) = 10^(0.015 * PV - 2.0), i.e. 10 to the power of 0.015 * PV - 2.0
lat_lon_site_variable_full[variable == "chl",value_adj :=10^(0.015*value-2.0)]

#save as csv
#fwrite(lat_lon_site_variable_full, file.path("data","enviro_predictors","lat_lon_site_variable_full.csv"))
lat_lon_site_variable_full <- fread(file.path("data","enviro_predictors","lat_lon_site_variable_full.csv"))

#mean_value per site across all months in time series, also across DepthZones, because the resolution is 1km, so we wouldn't expect depth zones to be different
lat_lon_site_variable_full[, mean_value_adj := mean(value_adj, na.rm = T),.(Site, variable)]

#min and max value per site across all months in time series, also across DepthZones. Will first take min of each year, and then mean of minimums. Same for max
#mean annual minimum
lat_lon_site_variable_full[, min_annual_value_adj := min(value_adj, na.rm = T),.(Site, variable, year)][,min_annual_value_adj := ifelse(is.infinite(min_annual_value_adj), NA, min_annual_value_adj)]
lat_lon_site_variable_full[, min_value_adj := mean(min_annual_value_adj, na.rm = T),.(Site, variable)]
#mean annual maximum
lat_lon_site_variable_full[, max_annual_value_adj := max(value_adj, na.rm = T),.(Site, variable, year)][,max_annual_value_adj := ifelse(is.infinite(max_annual_value_adj), NA, max_annual_value_adj)]
lat_lon_site_variable_full[, max_value_adj := mean(max_annual_value_adj, na.rm = T),.(Site, variable)]

#reduce to one row per site
lat_lon_site_variable_full.r <- unique(lat_lon_site_variable_full[,.(Site, variable, mean_value_adj, min_value_adj, max_value_adj)])

#split into temperature and chlorophyll
sst_bysite <- lat_lon_site_variable_full.r[variable == "sst"][,mean_sst_C := mean_value_adj][,max_sst_C := max_value_adj][,min_sst_C := min_value_adj][,.(Site, mean_sst_C, max_sst_C, min_sst_C)]
chl_bysite <- lat_lon_site_variable_full.r[variable == "chl"][,mean_chl_mg_m3 := mean_value_adj][,max_chl_mg_m3 := max_value_adj][,min_chl_mg_m3 := min_value_adj][,.(Site, mean_chl_mg_m3, max_chl_mg_m3, min_chl_mg_m3)]

#######Distance to 200 m isobath

distance_200mbathy_bysite <- add_depth_columns(lat_lon_site_fix, ETOPO = F, CDFW = F, USGS_socal=F, dist_200m = T)


#remove lat lon columns
distance_200mbathy_bysite <- unique(distance_200mbathy_bysite[,.(Site, DepthZone, dist_200m_bath)])

###############################################
#merge all site level environmental variables
###############################################
#insitu
substrate_bysite
all_env_lat_lon <- substrate_bysite[lat_lon_site_fix, on = c("Site","DepthZone")]
relief_bysite
all_env_lat_lon <- relief_bysite[all_env_lat_lon, on = c("Site","DepthZone")]
macro_density_bysite
all_env_lat_lon <- macro_density_bysite[all_env_lat_lon, on = c("Site","DepthZone")]
stipe_density_bysite
all_env_lat_lon <- stipe_density_bysite[all_env_lat_lon, on = c("Site","DepthZone")]


#large scale
distance_200mbathy_bysite
all_env_lat_lon <- distance_200mbathy_bysite[all_env_lat_lon, on = c("Site","DepthZone")]
sst_bysite
all_env_lat_lon <- sst_bysite[all_env_lat_lon, on = c("Site")]
chl_bysite
all_env_lat_lon <- chl_bysite[all_env_lat_lon, on = c("Site")]

#save
fwrite(all_env_lat_lon,file.path("data","enviro_predictors","all_env_lat_lon.csv"))

all_env_lat_lon <- fread(file.path("data","enviro_predictors","all_env_lat_lon.csv"))