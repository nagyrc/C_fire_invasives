#Script for adding fire data to meta-analysis
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox", "sp")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")
getwd()

#set crs for all data layers: Albers Equal Area
crs1 <- 'ESRI:102003'
crs1b <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'


#bring in dataframe and convert dataframe to sf object with epsg projection 4326 - wgs 84
studyid = read_csv("studyid.csv")
studyid_sf  <-  st_as_sf(studyid, coords = c('long', 'lat'), crs = crs1b)

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
#not sure if the transform statment worked here

st_crs(mtbs_fire)

#extract discovery year for points in studyid and add a field to show which fires occurred before/after sampling date
mtbs_int <- mtbs_fire  %>%
  sf::st_intersection(., studyid_sf)
#so 524/1313 points have had fire and the rest have not
#does this have any points with multiple fires???

unique(mtbs_int$X1)
#461...where did this field come from???


#keep those where fire date is before sampling date
mtbs_keep <- mtbs_int %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) 
#466 obs of the 524 had fires before the sampling date, which is what we care about

unique(mtbs_keep$MTBS_ID)
#31

unique(mtbs_keep$geometry)
#36

unique(mtbs_keep$MTBS_DISCOVERY_YEAR)
#17

mtbs_keep <- mtbs_keep %>%
  mutate(MTBS_lyb = MTBS_DISCOVERY_YEAR)
#note: may need to group by MTBS_ID?

#adding MTBS last year burn to studyid_df
mtbs_add <- studyid_sf %>%
  left_join(., as.data.frame(mtbs_keep) %>% dplyr::select(-geometry), by = c('pool_value','Study_ID','Article_ID','site','yr_samp','pool','thick','study','topdepth_cm','bottomdepth_cm','BD_estimated','veg')) %>%
  dplyr::select(-MTBS_ID, -MTBS_DISCOVERY_YEAR, -mtbs_keep, -X1.x, -X1.y)


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

########################

#reproject raster to match CRS of dataframe
#this runs forever then throws this warning message
yearly_modis_trans <- projectRaster(yearly_modis, crs = crs1b)
#Warning message:
#In .Internal(gc(verbose, reset)) :
  #closing unused connection 3 (/var/folders/b5/w0ns98qx6qj57p36w47z6cv40000gn/T//RtmpcpHtIu/raster///r_tmp_2019-02-06_184307_1718_19366.gri)

#did this actually reproject?
crs(yearly_modis_trans)
#CRS arguments:
#+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
#+datum=NAD83 +units=m +no_defs +towgs84=0,0,0
#so, I think it did reproject

###Extract option #1
#extract modis values at points
modis_df <- velox(yearly_modis)$extract_points(sp = studyid_sf) %>%
  as_tibble()
colnames(modis_df) <- names(yearly_modis)

is.numeric(modis_df$modis_2001)
###

###Extract option #2
#this also works to extract
#extract modis values at points and add to studyid_sf
modistest <- raster::extract(yearly_modis, studyid_sf, sp = TRUE)
#df = TRUE
###

###Get max value option #1
#get max value of columns within a row
modis_max <- modis_df %>% mutate(mak = do.call(pmax, (.)))
is.numeric(modis_max$mak)

#checking numbers to make sure they are reasonable.
unique(modis_max$mak)
summary(modis_max$mak)
#hmmm...these are all zeros. so no, this didn't work



#get column name for max value (indicates year)
year <- colnames(modis_df)[max.col(modis_df,ties.method  = "last")]

#combine max value and name of column where max value is found to get last year burned
lastyr <- as.data.frame(cbind(modis_max$mak, year))
unique(lastyr$year)

lastyr$mak <- as.numeric(lastyr$mak)
is.numeric(lastyr$mak)

modyr <- ifelse(lastyr$mak == 0, NA, lastyr$year)
###


###Get max value option #2
#alternate method for finding the max value across columms
#modis_df %>% 
  #mutate(mak = do.call(pmax, (.))) %>%
  #dplyr::select(mak) %>% 
  #cbind(modis_df)

#need to get the year that corresponds to this max value
modis_max <- modis_df %>% 
  rownames_to_column('id') %>%
  left_join(
    modis_max %>% 
      #rownames_to_column('id') %>%
      gather(max_year, max_cnt, modis_2001:modis_2017) %>% 
      group_by(id) %>% 
      slice(which.max(max_cnt)), 
    by = 'id'
  )
#just need to check and see how this is dealing with ties

unique(modis_max$max_cnt)
#still all zeros

########################
#old text here
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
modis_df <- velox(yearly_modis)$extract_points(sp = studyid_sf, fun = function(x) max(x, na.rm=TRUE), small = TRUE, df = TRUE) %>%
  as_tibble()
colnames(modis_df) <- c('ID_sp', names(yearly_modis))

modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(studyid_sf)$id) %>%
  dplyr::select(-ID_sp) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)
###
#end old text
###########################


###########################
#BAECV
#bring in BAECV last year burned from Adam
baecvlyb <- raster("baecv/lyb_usa_baecv_1984_2015.tif")
crs(baecvlyb)
str(studyid_sf)

str(baecvlyb)

###
#look at the data and make sure these values make sense
#this runs forever
hist(baecvlyb)

plot(baecvlyb)

test2 <- baecvlyb@data
head(test2)
###


###
#intersection of BAECV lyb with studyid_sf
#this crashed R repeatedly
#baecv_int <- velox(baecvlyb)$extract_points(sp = studyid_sf) %>%
  #as_tibble()

#copy of dataframe
lll <- studyid_sf

#extract lyb from BAECV to the points in studyid_sf
lll$baecvlyb <- raster::extract(baecvlyb, lll)

unique(lll$baecvlyb)
#only 2010...why is there only 1 year???

summary(lll$baecvlyb)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2010    2010    2010    2010    2010    2010 
#only 2010...why is there only 1 year???

###
baecv_lll <- lll %>%
  mutate(baecv_lll = ifelse(baecvlyb <= yr_samp, 1, 0)) %>%
  filter(baecv_lll != 0)
#243 observations
#these ones are keepers

baecv_no <- lll %>%
  mutate(baecv_no = ifelse(baecvlyb <= yr_samp, 1, 0)) %>%
  filter(baecv_no == 0)
#1070 observations
#these are either burn date after sample collection or ????
#these are the ones I need Adam to recalculate (time - 1); give him a shapefile of these


