#Script for adding fire data to meta-analysis
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox", "sp", "tidyverse","rgdal")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")
getwd()

#set crs for all data layers: Albers Equal Area
crs1 <- 'ESRI:102003'
crs1b <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'


#bring in dataframe and convert dataframe to sf object with ESRI projection 102003
studyid = read_csv("studyid.csv")
studyid_sf  <-  st_as_sf(studyid, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b)

st_crs(studyid_sf)

###########################
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
  st_transform(.,crs1b)
#not sure if the transform statment worked here

st_crs(usa_shp)




###########################

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
  st_transform(., crs1b)

st_crs(mtbs_fire)
#yes, this was correctly transformed

#extract discovery year for points in studyid and add a field to show which fires occurred before/after sampling date
mtbs_int <- sf::st_intersection(studyid_sf, mtbs_fire)
#524 observations; so some points didn't burn

unique(mtbs_int$X1)
#where did this field come from???

unique(mtbs_fire$MTBS_DISCOVERY_YEAR)
unique(mtbs_int$MTBS_DISCOVERY_YEAR)
#these look good


#keep those where fire date is before sampling date
mtbs_keep <- mtbs_int %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) 
#466 had fires before the sampling date

mtbs_keep <- mtbs_keep %>%
  group_by(X1) %>%
  dplyr::mutate(max_yr = max(MTBS_DISCOVERY_YEAR)) %>%
  filter(MTBS_DISCOVERY_YEAR == max_yr) %>%
  dplyr::select(-max_yr) %>%
  ungroup


#adding MTBS last year burn to studyid_df
mtbs_add <- studyid_sf %>%
  left_join(as.data.frame(mtbs_keep) %>% 
  dplyr::select(-geometry)) %>%
  dplyr::select(-mtbs_keep)




###########################




###########################
#BAECV
#bring in BAECV last year burned from Adam
#will need to bring in his code on how he created last year burn eventually
baecvlyb <- raster("baecv/lyb_usa_baecv_1984_2015.tif")
crs(baecvlyb)
crs(studyid_sf)


###
#intersection of BAECV lyb with studyid_sf

#reproject raster to match CRS of dataframe
baecvlyb_trans <- projectRaster(baecvlyb, crs = crs1b)
crs(baecvlyb_trans)


#extract lyb from BAECV to the points in studyid_sf
lll <- raster::extract(baecvlyb, studyid_sf, sp = TRUE)

unique(lll$lyb_usa_baecv_1984_2015)
#there are many years when I used the original version (baecvlyb) which is what I would expect
#when I used the reprojected version (baecvlyb_trans), only 0 and NA
#something weird is happening here


#create sf object from extracted values
baecvtest_sf  <-  st_as_sf(lll, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b)

###
baecv_keep <- baecvtest_sf %>%
  filter(lyb_usa_baecv_1984_2015 <= yr_samp)
#1186 observations
#these ones are keepers

baecv_no <- baecvtest_sf  %>%
  filter(lyb_usa_baecv_1984_2015 > yr_samp) %>%
  select(-topdepth_cm, -bottomdepth_cm, -thick, -veg, - Article_ID, -pool, -pool_value, -Study_ID, -site, -BD_estimated, -study)
#127 observations
#these are burn date after sample collection
#these are the ones I need Adam to recalculate (time - 1); give him a shapefile of these

#join these no points back to studyid_sf for Adam
Xno <- unique(baecv_no$X1)
baecv_no_Adamll <- studyid %>%
  filter(X1 %in% Xno)
write.csv(baecv_no_Adamll, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/baecv_no_ll.csv")
#go to Adams' script for dealing with these points; then come back here


baecv_add <- mtbs_add %>%
  left_join(as.data.frame(baecv_keep) %>% 
  dplyr::select(-geometry, -yr_samp, -site, -topdepth_cm, -bottomdepth_cm, -BD_estimated, -veg, -study, -thick, -Article_ID, -pool, -pool_value, -Study_ID)) 
#MTBS and BAECV look good; still need to fix MODIS

#add lat/long back in for plotting, quick id, etc.
ll <- studyid %>%
  select(lat, long, X1)

baecv_add_ll <- baecv_add %>%
  left_join(as.data.frame(ll))

write.csv(baecv_add_ll, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/studyid_with_fire_almost.csv")


#adding in BAECV info (second to last year burned) from Adam's script
#open data
baecv_gpkg <- readOGR("baecv/lyb_forthoseplots.gpkg", "lyb_forthoseplots")

#turn this into a dataframe
as.data.frame(baecv_gpkg)

#add these points from Adam by replacing values
baecv_rep <- baecv_add_ll %>%
  mutate(baecvlyb = ifelse(Study_ID == 154, 2001,lyb_usa_baecv_1984_2015)) %>%
  mutate(baecv_lyb = ifelse(Study_ID == 226, 1986, baecvlyb)) %>%
  select(-lyb_usa_baecv_1984_2015, -baecvlyb)

write.csv(baecv_rep, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/studyid_with_fire.csv")

yrbn = read_csv("yrbn.csv")

baecv_rep %>%
  mutate(lyb = ifelse(MTBS_DISCOVERY_YEAR > baecv_lyb, MTBS_DISCOVERY_YEAR, baecv_lyb)) %>%
  filter(study %in% yrbn) %>%
  mutate(lyb = last_year_burned)

###########################
#MODIS
#bring in MODIS data; create stack of rasters
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

crs(yearly_modis)
#+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs 
#does not match above; need to reproject

###

#reproject raster to match CRS of dataframe
yearly_modis_trans <- projectRaster(yearly_modis, crs = crs1b)

#did this actually reproject?
crs(yearly_modis_trans)
#CRS arguments:
#+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
#+datum=NAD83 +units=m +no_defs +towgs84=0,0,0


projection(yearly_modis_trans)
projection(as(studyid_sf, 'Spatial'))

extent(yearly_modis_trans)
extent(as(studyid_sf, 'Spatial'))
#x's of studyid_sf are all -; x's of modis are some - and some +
#y's of studyid_sf are all +; y's of modis are some - and some +



plot(yearly_modis_trans[[1]])

#can use this to decrease run time; cropped version of modis data
modis_study_area <- crop(yearly_modis_trans, as(studyid_sf, 'Spatial'))

# explore the raster
modis_study_area


#extract modis values at points and add to studyid_sf
modistest <- raster::extract(modis_study_area, studyid_sf, sp = TRUE)
#df = TRUE
###

unique(modistest$modis_2004)
#I'm not sure these numbers are correct because in 2016, 2010, there are values = 2016.0000 and 2010.0000
#this seems a bit weird


###Find last year burned for modis that occurred before sampling
#I need the column name of last year that has value other than 0 or NA prior to yr_samp

#create sf object from extracted values
modistest_sf  <-  st_as_sf(modistest, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b)


#transform modis data into long format, keeping X1 so I can group by this later
modisselect <- modistest_sf %>%
  dplyr::select(X1, yr_samp, modis_2001:modis_2017) %>%
  gather("year", "burn", -X1, -yr_samp, -geometry) 


#create subset of data where burn happens before sampling
#any burns that happened after sampling date, we don't care about
keep <- modisselect %>% 
  separate(year, c("first", "almost"), sep = "_") %>%
  mutate(modis_yr = as.numeric(almost)) %>%
  filter(modis_yr <= yr_samp)
#this removes quite a few rows...are these all burns after sampling?  or am I removing NAs?
#removing NAs is ok, that means no burn

#need to return the modis_yr where burn is maximized
#for ties, choose the last year
keepmodlyb <- keep %>%
  dplyr::group_by(X1) %>%
  mutate(the_rank  = rank(-burn, ties.method = "last")) %>%
  filter(the_rank == 1) %>% dplyr::select(-the_rank)
#this appears to be working; just need to make sure MODIS data extracted properly


#adding MODIS last year burn to mtbs_add
modis_add <- baecv_rep %>%
  left_join(as.data.frame(keepmodlyb) %>% 
              dplyr::select(-geometry, -almost, -first, -burn, -yr_samp)) 







###########################
