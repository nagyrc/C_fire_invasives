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

studyidplot <- studyid[!is.na(studyid$lat),]

#check to see if X1 came through...if not, add it here
studyidplot$X1 <- as.factor(1:nrow(studyidplot))

#transform to sf object
studyid_sf  <-  st_as_sf(studyidplot, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b) 
#rm(studyid)
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
#723 observations; so some points didn't burn

unique(mtbs_int$X1)

unique(mtbs_fire$MTBS_DISCOVERY_YEAR)
unique(mtbs_int$MTBS_DISCOVERY_YEAR)
#these look good

is.numeric(mtbs_int$MTBS_DISCOVERY_YEAR)
is.numeric(mtbs_int$yr_samp)
as.numeric(mtbs_int$yr_samp)
#keep those where fire date is before sampling date
mtbs_keep <- mtbs_int %>%
  mutate(yr_samp = as.numeric(yr_samp)) %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) 
#617 had fires before the sampling date

mtbs_keep <- mtbs_keep %>%
  group_by(X1) %>%
  dplyr::mutate(max_yr = max(MTBS_DISCOVERY_YEAR)) %>%
  filter(MTBS_DISCOVERY_YEAR == max_yr) %>%
  dplyr::select(-max_yr) %>%
  ungroup
#583 observations once less recent burns are removed

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

#STEP 1: reproject raster to match points or points to match raster
#option 1: reproject raster to match CRS of dataframe
#baecvlyb_trans <- projectRaster(baecvlyb, crs = crs1b)
#crs(baecvlyb_trans)

#option 2: or reproject points to match raster
studyidsfrep <- st_as_sf(studyidplot, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs = st_crs(baecvlyb))


#STEP 2: intersection of BAECV lyb with studyid_sf
#option 1: extract lyb from BAECV to the points in studyid_sf
#lll <- raster::extract(baecvlyb, studyid_sf, sp = TRUE)
#lll <- raster::extract(baecvlyb_trans, studyid_sf, sp = TRUE)

#unique(lll$lyb_usa_baecv_1984_2015)
#there are many years when I used the original version (baecvlyb) which is what I would expect
#when I used the reprojected version (baecvlyb_trans), only 0 and NA
#something weird is happening here

#option 2: do the extract with the reprojected points
lllb <- raster::extract(baecvlyb, studyidsfrep, sp = TRUE)

unique(lllb$lyb_usa_baecv_1984_2015)
#there are many years here; these look like reasonable numbers for last year burn

#STEP 3
#option 1: create sf object from extracted values
#baecvtest_sf  <-  st_as_sf(lll, coords = c('long', 'lat'), crs = 4326) %>%
  #st_transform(crs1b)

#option 2: create sf object from extracted values
baecvtest_sfb  <-  st_as_sf(lllb, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b)




###
#after choosing option #1 or #2, go on here
baecv_keep <- baecvtest_sfb %>%
  filter(lyb_usa_baecv_1984_2015 <= yr_samp)
#1870 observations
#these ones are keepers

baecv_no <- baecvtest_sfb  %>%
  filter(lyb_usa_baecv_1984_2015 > yr_samp) %>%
  dplyr::select(-topdepth_cm, -bottomdepth_cm, -thick, -veg, - Article_ID, -pool, -pool_value, -Study_ID, -site, -BD_estimated, -study)
#151 observations
#these are burn date after sample collection
#these are the ones I need Adam to recalculate (time - 1); give him a shapefile of these

#join these no points back to studyid_sf for Adam
Xno <- unique(baecv_no$X1)
baecv_no_Adamll <- studyidplot %>%
  filter(X1 %in% Xno)
write.csv(baecv_no_Adamll, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/baecv_no_ll.csv")
#go to Adams' script for dealing with these points; then come back here

stdids <- unique(baecv_no_Adamll$Study_ID)

baecv_add <- mtbs_add %>%
  left_join(as.data.frame(baecv_keep) %>% 
  dplyr::select(-geometry, -yr_samp, -site, -topdepth_cm, -bottomdepth_cm, -BD_estimated, -veg, -study, -thick, -Article_ID, -pool, -pool_value, -Study_ID)) 
#MTBS and BAECV look good

#add lat/long back in for plotting, quick id, etc.
ll <- studyidplot %>%
  dplyr::select(lat, long, X1)

baecv_add_ll <- baecv_add %>%
  left_join(as.data.frame(ll))

write.csv(baecv_add_ll, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/studyid_with_fire_almost.csv")


#adding in BAECV info (second to last year burned) from Adam's script
#open data
baecv_gpkg <- rgdal::readOGR("baecv/lyb_preliminary.gpkg", "lyb_preliminary")

#turn this into a dataframe
baecv_from_Adam <- as.data.frame(baecv_gpkg)

#add these points from Adam by replacing values
baecv_rep <- baecv_add_ll %>%
  mutate(baecvlyb = ifelse(Study_ID == 249, 2001,lyb_usa_baecv_1984_2015)) %>%
  mutate(baecv_lyb = ifelse(Study_ID == 321, 1986, baecvlyb)) %>%
  dplyr::select(-lyb_usa_baecv_1984_2015, -baecvlyb)

#write.csv(baecv_rep, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/studyid_with_fire.csv")

yrbn <- read_csv("last_year_burn_overwrite.csv")

#the Mahood ones are using satellite data, so I removed those
yrbn <- yrbn %>%
  filter(!study == "Mahood et al. unpub1") 

last_year_burned <- yrbn %>%
  dplyr::select(X1, last_year_burned)

baecv_rep %>%
  mutate(as.factor(baecv_rep$X1))

is.factor(baecv_rep$X1)
is.factor(last_year_burned$X1)

#take the max of the satellite data
baecv_rep <- baecv_rep %>%
  mutate(maxsat = ifelse(MTBS_DISCOVERY_YEAR > baecv_lyb, MTBS_DISCOVERY_YEAR, baecv_lyb)) %>%
  left_join(last_year_burned)

#need to replace the 6 values here with those in yrbn
siwf <- baecv_rep %>%
  mutate(masterlyb = case_when(last_year_burned > 1900 ~ last_year_burned, maxsat > 1900 ~ maxsat))

write.csv(siwf, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/siwf.csv", row.names = FALSE)
###

siwf$Study_ID <- as.factor(siwf$Study_ID)
is.factor(siwf$Study_ID)

is.numeric(siwf$masterlyb)
siwf$timesincefire <- siwf$yr_samp - siwf$masterlyb




###
#not sure if I need this below other than as a check
siwf$masterlyb <- as.factor(siwf$masterlyb)
is.factor(siwf$masterlyb)

bbb <- unique(siwf[c("Study_ID", "masterlyb", "study")])

bbb <- siwf %>%
  group_by(Study_ID, masterlyb, study) %>%
  dplyr::select(Study_ID, masterlyb, study)

write.csv(bbb, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/bbb.csv", row.names = FALSE)

