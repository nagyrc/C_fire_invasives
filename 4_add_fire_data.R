#Script for adding fire data to meta-analysis
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")
getwd()

#set crs for all data layers: Albers equal area
crs <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

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


#bring in shapefile of US states; select 48 contiguous; tranform to match crs of other layers
usa_shp <- st_read(file.path('states_shp'), layer = 'cb_2016_us_state_20m') %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  st_transform(st_crs(crs)) %>%  # set to Albers equal area
  dplyr::select(STATEFP, STUSPS) %>%
  setNames(tolower(names(.)))


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
  st_transform(st_crs(crs)) %>%
  mutate(MTBS_ID = Fire_ID,
         MTBS_DISCOVERY_YEAR = Year) %>%
  dplyr::select(MTBS_ID, MTBS_DISCOVERY_YEAR)


#bring in dataframe and convert dataframe to sf object with Albers equal area projection
studyid = read_csv("studyid.csv")
studyid_sf  <-  st_as_sf(studyid, coords = c('long', 'lat'), crs = crs)

#make sure they all match
crs(studyid_sf)
crs(usa_shp)
crs(mtbs_fire)
#all are NAs...why?

#extract discovery year for points in studyid
mtbs_test <- mtbs_fire  %>%
  sf::st_intersection(., studyid_sf) %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) %>%
  dplyr::select(-mtbs_keep) %>%
  group_by(id) %>%
  summarise(last_burn_year = max(MTBS_DISCOVERY_YEAR))

#adding MTBS last year burn to clean_study
mtbs_clean <- clean_study %>%
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

library(velox)

#transform dataframe into equal area projection
clean_study_laea <- clean_study %>%
  st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')

###option #1
#extract modis at points in clean_study
modis_df <- velox(yearly_modis)$extract_points(sp = clean_study_laea) %>%
  as_tibble()
colnames(modis_df) <- names(yearly_modis)

#create last year burned at points in clean_study
modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(clean_study)$id) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)
###

###option #2
modis_df <- velox(yearly_modis)$extract(sp = clean_study_laea, fun = function(x) max(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
  as_tibble()
colnames(modis_df) <- c('ID_sp', names(yearly_modis))

modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(clean_study)$id) %>%
  dplyr::select(-ID_sp) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)
###


###########################
#BAECV
#bring in BAECV last year burned from Adam
library(raster)

baecvlyb <- raster("baecv/lyb_usa_baecv_1984_2015.tif")

str(clean_study)

#convert to shapefile

#then clean_study$baecvlyb <- raster::extract (baecvlyb, clean_study)

#find points where yr_samp < lyb, then need time -1
#give Adam a shapefile of these points 
