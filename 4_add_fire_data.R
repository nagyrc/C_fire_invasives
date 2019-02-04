#Script for adding fire data to meta-analysis
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox", "sp")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")
getwd()

#set crs for all data layers: epsg projection 4326 - wgs 84
crs1 <- 4326

#Download US shapefile
us_shp <- file.path('states_shp', "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  # The location where the data is housed
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  # Where you want to data to be downloaded to
  dest <- paste0(file.path('states_shp', ".zip"))
  # Download the data
  download.file(loc, dest)
  # Unzip the data file and move to permenant location
  unzip(dest, exdir = file.path('states_shp'))
  # Delete zip file
  unlink(dest)
  # Check to make sure it worked
  assert_that(file.exists(us_shp))
}


#bring in shapefile of US states; select 48 contiguous; tranform to match crs of other layers; remove extra fields
usa_shp <- st_read(file.path('states_shp'), layer = 'cb_2016_us_state_20m') %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  dplyr::select(STATEFP, STUSPS) %>%
  setNames(tolower(names(.))) %>% 
  st_set_crs(4269) %>% 
  st_transform(.,crs1)

class(usa_shp)

crs(usa_shp)
###########################
#MTBS
#Download the MTBS fire polygons
mtbs_shp <- file.path('mtbs', 'mtbs_perims_DD.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0('mtbs', ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = 'mtbs')
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
}


#bring in MTBS data
mtbs_fire <- st_read(dsn = 'mtbs',
                     layer = "mtbs_perims_DD", quiet = TRUE) %>%
  mutate(MTBS_ID = Fire_ID,
         MTBS_DISCOVERY_YEAR = Year) %>%
  dplyr::select(MTBS_ID, MTBS_DISCOVERY_YEAR) %>%
  st_transform(., crs1)


#bring in dataframe and convert dataframe to sf object with epsg projection 4326 - wgs 84
studyid = read_csv("studyid.csv")
studyid_sf  <-  st_as_sf(studyid, coords = c('long', 'lat'), crs = crs1)

#make sure they all match
crs(studyid_sf)
crs(usa_shp)
crs(mtbs_fire)
#all are NAs...why?

#extract discovery year for points in studyid
mtbs_test <- mtbs_fire  %>%
  sf::st_intersection(., studyid) %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) %>%
  dplyr::select(-mtbs_keep) %>%
  group_by(id) %>%
  summarise(last_burn_year = max(MTBS_DISCOVERY_YEAR))

#adding MTBS last year burn to studyid_df
mtbs_clean <- studyid_sf %>%
  left_join(., as.data.frame(mtbs_test) %>% dplyr::select(-geometry), by = 'id')


###########################
#MODIS
#bring in MODIS data
dir <- 'modis_events'

layer_reclass <- function(dir) {
  
  files <- list.files(file.path(dir), pattern = 'BurnDate') # list all files from a given driver folder
  # Assuming TIF images, List files from wdata folder
  ## Change below if using any other format
  ltif <- grep(".tif$", files, ignore.case = TRUE, value = TRUE)
  stkl <- stack()
  
  for(i in 1:length(ltif)){
    
    file_split <- ltif[i] %>%
      strsplit(split = "_") %>%
      unlist
    
    if(!file.exists(paste0(dir, '/modis_', file_split[3], '.tif'))) {
      
      x <- raster(file.path(dir, ltif[i]),
                  package = "raster", varname = fname)
      
      rcl_matrix <- matrix(c(1, Inf, as.numeric(file_split[3])),
                           ncol=3, byrow=TRUE)
      
      stkl <- reclassify(x, rcl_matrix)
      writeRaster(stkl, filename = paste0(dir, '/modis_', file_split[3], '.tif'))
    }
  }
  files <- list.files(file.path(dir), pattern = 'modis_', full.names = TRUE) # list all files from a given driver folder
  
  stk <- stack(files)
  return(stk)
}

yearly_modis <- layer_reclass(dir = dir)




###option #1
#extract modis at points in studyid
modis_df <- velox(yearly_modis)$extract_points(sp = studyid_sf) %>%
  as_tibble()
colnames(modis_df) <- names(yearly_modis)

#create last year burned at points in studyid
modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(studyid_sf)$id) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)
###

###option #2
modis_df <- velox(yearly_modis)$extract(sp = studyid_sf, fun = function(x) max(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
  as_tibble()
colnames(modis_df) <- c('ID_sp', names(yearly_modis))

modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(studyid_sf)$id) %>%
  dplyr::select(-ID_sp) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)
###


###########################
#BAECV
#bring in BAECV last year burned from Adam
baecvlyb <- raster("baecv/lyb_usa_baecv_1984_2015.tif")

str(studyid_sf)

#convert to shapefile

#then studyid$baecvlyb <- raster::extract (baecvlyb, studyid)

#find points where yr_samp < lyb, then need time -1
#give Adam a shapefile of these points 
