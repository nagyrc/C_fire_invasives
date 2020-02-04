# Get last year burned at time of sample for different points that were sampled in different years
# Author: Adam Mahood
#
# note: this is designed for running in s3 since the baecv files are so huge.

# setup =======================================================================>
library(tidyverse)
library(sf)
library(raster)
library(foreach)
library(doParallel)
csv_file <- "baecv_no_ll.csv"
s3_path <- "s3://earthlab-amahood/C_fire_invasives/"
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
baecv_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 "
corz <- detectCores() -1

# data import =================================================================>
system(paste("aws s3 cp", paste0(s3_path, csv_file), csv_file))

dd <-read_csv(csv_file)%>%
  mutate(duped = duplicated(lat)) %>%
  filter(duped==F)%>%
  dplyr::select(-duped,-topdepth_cm, -bottomdepth_cm, -BD_estimated,-veg,-thick,
                -Article_ID, -pool,-pool_value) %>%
  st_as_sf(coords=c("long","lat"), crs = latlong) %>%
  st_transform(st_crs(baecv_crs))


# do the stuff ================================================================>

y_max <- max(dd$yr_samp)

local_scrap<- "scrap/"
dir.create(local_scrap)
registerDoParallel(corz)
results <- list()
years <- 1984:y_max

# just sync the folders first
system(paste("aws s3 sync","s3://earthlab-ls-fire/v1.1/bc/", local_scrap))



for(yy in 1:length(years)){
  # defining filenames
  target_file<-paste0("BAECV_bc_",years[yy],"_v1.1_20170908.tif")
  bc_file <- paste0(local_scrap, target_file)
  
  bc<- raster(bc_file)
  ddd <- dd %>% 
    mutate(burned = raster::extract(bc,.),
           burn_year = years[yy])
  results[[yy]] <- ddd
  print(yy)
  # system(paste0("rm ", bc_file))
  
}

final <- do.call("rbind", results) %>%
  dplyr::filter(burn_year < yr_samp) %>%
  dplyr::filter(burned > 0) %>%
  mutate(Study_ID = as.factor(Study_ID)) %>%
  group_by(Study_ID) %>%
  summarise(lyb = max(burn_year)) %>%
  dplyr::select(-Study_ID)%>%
  st_join(x=dd, y=.) 

st_write(final, "lyb_forthoseplots.gpkg", delete_dsn = T)
system("aws s3 cp lyb_forthoseplots.gpkg s3://earthlab-amahood/C_fire_invasives/lyb_forthoseplots.gpkg")
