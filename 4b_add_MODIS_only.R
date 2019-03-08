#Script for adding MODIS fire data to meta-analysis (if needed)
#Dr. R. Chelsea Nagy
#created March 8, 2019

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


###########################
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

#STEP 1 reproject raster to match dataframe or reproject dataframe to match raster
#option 1:reproject raster to match CRS of dataframe
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


#Q: non-matching extents...is this ok?
#A: It is fine - the extents do not need to be identical for raster::extract to work. 
#A: The main thing to check would be that the extent of studyid_sf is bounded by the extent of the MODIS data.



plot(yearly_modis_trans[[1]])

#can use this to decrease run time; cropped version of modis data
modis_study_area <- crop(yearly_modis_trans, as(studyid_sf, 'Spatial'))

# explore the raster
modis_study_area


#option 2:reproject dataframe to match CRS of raster
studyidsfrep2 <- st_as_sf(studyid, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs = st_crs(yearly_modis))

#STEP 2: extract modis values at points and add to studyid_sf

#option 1: using cropped and reprojected MODIS data
modistest <- raster::extract(modis_study_area, studyid_sf, sp = TRUE)
#df = TRUE
###

unique(modistest$modis_2004)
#I'm not sure these numbers are correct because in 2016, 2010, there are values = 2016.0000 and 2010.0000
#this seems a bit weird


#option 2: using reprojected dataframe
modistest <- raster::extract(yearly_modis, studyidsfrep2, sp = TRUE)



#STEP 3: create sf object from extracted values
modistest_sf  <-  st_as_sf(modistest, coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs1b)

###
#once I choose an option above, go on with code
###Find last year burned for modis that occurred before sampling
#I need the column name of last year that has value other than 0 or NA prior to yr_samp

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