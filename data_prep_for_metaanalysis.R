#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(doBy)
library(reshape)
library(velox)

# Read in alldatall.csv
alldatall = read_csv("data/alldatall.csv")

###
#automate the studyid csv that we did manually
#create Article ID
studyid <- alldatall
studyid$Article_IDs <- str_sub(studyid$study,1,4)
unique(studyid$Article_IDs)
studyid$Article_IDe <- str_sub(studyid$study,-4,-1)
unique(studyid$Article_IDe)
studyid$Article_ID <- paste(toupper(studyid$Article_IDs),studyid$Article_IDe)
studyid$Article_ID <- gsub(" ", "", studyid$Article_ID) 
studyid$Article_ID <- gsub("MAHOpub1", "MAHO2018a", studyid$Article_ID) 
studyid$Article_ID <- gsub("MAHOpub2", "MAHO2018b", studyid$Article_ID) 

unique(studyid$Article_ID)
#these look great

head(studyid)

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

# Use this code to download any data file
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
  # Check t make sure it worked
  assert_that(file.exists(us_shp))
}

#use this to bring in Short data (if desired)
fpa_gdb <- file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb")
if (!file.exists(fpa_gdb)) {
  pg <- read_html("https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/")
  fils <- html_nodes(pg, xpath=".//dd[@class='product']//li/a[contains(., 'zip') and contains(., 'GDB')]")
  dest <- paste0(fpa_prefix, ".zip")
  walk2(html_attr(fils, 'href'),  html_text(fils),
        ~GET(sprintf("https:%s", .x), write_disk(dest), progress()))
  unzip(dest, exdir = fpa_prefix)
  unlink(dest)
  assert_that(file.exists(fpa_gdb))
  system(paste0("aws s3 sync ",
                raw_prefix, " ",
                s3_raw_prefix))
}

#Download the MTBS fire polygons
mtbs_shp <- file.path('mtbs', 'mtbs_perimeter_data_v2', 'dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0('mtbs', ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = 'mtbs')
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
}

#bring in shapefile of US states
usa_shp <- st_read(file.path('states_shp'), layer = 'cb_2016_us_state_20m') %>%
    filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
    st_transform(4326) %>%  # e.g. US National Atlas Equal Area
    dplyr::select(STATEFP, STUSPS) %>%
    setNames(tolower(names(.)))

#bring in MTBS data
mtbs_fire <- st_read(dsn = 'mtbs',
                     layer = "mtbs_perims_DD", quiet = TRUE) %>%
  st_transform(st_crs(usa_shp)) %>%
  mutate(MTBS_ID = Fire_ID,
         MTBS_DISCOVERY_YEAR = Year) %>%
  dplyr::select(MTBS_ID, MTBS_DISCOVERY_YEAR)

#creates study_ID variable and intersects the US states and MTBS shapefiles with data points
clean_study <- studyid %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","litterC_g_m2","soil%C","BD_g_cm3","soilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick") %>%
  # tidyr::gather(key = variable, value = value, -site, -study, -yr_samp, -lat, -long, -veg, -thick, -BD_estimated, -topdepth_cm, -bottomdepth_cm) %>%
  mutate(site = as.factor(site),
         veg = as.factor(veg),
         long = ifelse(is.na(long), 0, long),
         lat = ifelse(is.na(lat), 0, lat),
         # variable = as.factor(variable),
         Study_ID = group_indices_(., .dots = c("study","lat", "long", "veg", "site", "bottomdepth_cm"))) %>%
  dplyr::filter(long != 0 & lat != 0) %>%
  sf::st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  mutate(yr_samp = as.numeric(ifelse(is.na(yr_samp), 0, yr_samp))) %>%
  sf::st_join(., usa_shp) %>%
  mutate(id = row_number(),
         study_year = str_sub(study,-4,-1),
         study_year = ifelse(study_year == 'pub1', 2017, study_year),
         yr_samp = ifelse(is.na(yr_samp) | yr_samp == 0, study_year, yr_samp))

mtbs_test <- mtbs_fire  %>%
  sf::st_intersection(., clean_study) %>%
  mutate(mtbs_keep = ifelse(MTBS_DISCOVERY_YEAR <= yr_samp, 1, 0)) %>%
  filter(mtbs_keep != 0) %>%
  dplyr::select(-mtbs_keep) %>%
  group_by(id) %>%
  summarise(last_burn_year = max(MTBS_DISCOVERY_YEAR))

mtbs_clean <- clean_study %>%
  left_join(., as.data.frame(mtbs_test) %>% dplyr::select(-geometry), by = 'id')

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

clean_study_laea <- clean_study %>%
  st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs')

modis_df <- velox(yearly_modis)$extract_points(sp = clean_study_laea) %>%
  as_tibble()
colnames(modis_df) <- names(yearly_modis)

modis_df2 <- modis_df   %>%
  mutate(id = as.data.frame(clean_study)$id) %>%
  gather(key = key, value = last_burn_year_modis , -id) %>%
  dplyr::select(-key) %>%
  filter(last_burn_year_modis != 0)


clean_study_laea <- clean_study %>%
  st_transform('+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs') %>%
  st_buffer(dis = 10)

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
#move this code to new R script and use new dataframe that has spatial information
#in this table I'm bringing in here, we manually entered article ID and study ID
#data ninja-ing for attribute table
studyid <- as.data.frame(read_csv("alldatall_bystudyid.csv"))
head(studyid)
artstud <- unique(studyid[c("Article_ID", "Study_ID")])

write.csv(artstud, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/articlestudyid.csv")
is.numeric(studyid$topdepth_cm)
is.numeric(studyid$bottomdepth_cm)

#recalculate thickness; there were some issues with this when we looked at alldatall.csv
studyid$thick <- studyid$bottomdepth_cm - studyid$topdepth_cm
#this fixed it


att1 <- unique(studyid[c("Article_ID", "Study_ID", "veg", "topdepth_cm", "bottomdepth_cm", "BD_estimated","yr_samp", "lat","long")])
write.csv(att1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes1.csv")


#att2 <- unique(studyid[c("Article_ID", "Study_ID", "soilC_g_m2", "BGBC_g_m2", "litterC_g_m2", "AGBC_g_m2")])
#head(att2)

sum71 <- summaryBy(litterC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum72 <- summaryBy(soilC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum73 <- summaryBy(BGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum74 <- summaryBy(AGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)

combo1 <- left_join(sum71, sum72, by = c("Article_ID","Study_ID"))
combo2 <- left_join(combo1, sum73, by = c("Article_ID","Study_ID"))
combo3 <- left_join(combo2, sum74, by = c("Article_ID","Study_ID"))

head(combo3)
tail(combo3)

#replace NAs with negative number
combo3[is.na(combo3)] <- -2

combo3$Carbon_pool <- ifelse(combo3$soilC_g_m2.mean > 0, "soil",
                              ifelse(combo3$litterC_g_m2.mean > 0, "litter",
                                     ifelse(combo3$AGBC_g_m2.mean > 0, "AGB", "BGB")))


write.csv(combo3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes2.csv")


soilsubby <- subset.data.frame(alldatall, study == "Rau et al. 2011"| study == "Johnson et al. 2011")
unique(soilsubby$study)

ggplot(soilsubby, aes(x = veg, y = soilC_g_m2, color=study)) + geom_violin()

#quantiles
quantile(soilsubby$soilC_g_m2, 0.9, na.rm=TRUE)

summaryBy(soilC_g_m2~ study, data = soilsubby, FUN = c(meanfxn))
summaryBy(soilC_g_m2~ veg, data = soilsubby, FUN = c(meanfxn))

sdfxn <- function(x)base::sd(x, na.rm = TRUE)
maxfxn <- function(x)base::max(x, na.rm = TRUE)

soilsubby2 <- soilsubby %>%
  mutate(soilC_SD = sd(soilC_g_m2, na.rm=TRUE))

soilsubby2 <- soilsubby %>% group_by(veg,study) %>%
  summarize(soilC_SD = sd(soilC_g_m2, na.rm = TRUE),
         soilC_mean = mean(soilC_g_m2, na.rm = TRUE),
         soilC_SE = sqrt(var(soilC_g_m2, na.rm = TRUE)/sum(!is.na(soilC_g_m2))))


head(soilsubby2)

#summaryBy(soilC_g_m2~ study, data = soilsubby, FUN = c(sdfxn))
#summaryBy(soilC_g_m2~ veg, data = soilsubby, FUN = c(sdfxn))


# Parametric Bootstrap sample
MeanCheat  = soilsubby2$soilC_mean[2]
SECheat = soilsubby2$soilC_SE[2]
SDCheat = soilsubby2$soilC_SD[2]

# Number of random numbers to sample
N = sum(!is.na(soilsubby$soilC_g_m2))
N
?rnorm
Dist1 = rnorm(n=N, mean=MeanCheat, sd=SDCheat)
par(mfrow=c(1,2))
hist(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"], breaks=10)
hist(Dist1, breaks=10)

# New DF
Dist1DF = data.frame(Type="rnorm", soilC_g_m2=Dist1)
ObsDF = data.frame(Type="Obs", 
                   soilC_g_m2=soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"])
Dists = rbind(ObsDF, Dist1DF)
head(Dists
     )
# Overlaid histograms
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_histogram(binwidth=200, alpha=.5, position="identity")

ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

# Add another random distribution
Dist2DF = data.frame(Type = "Dist2",
                     soilC_g_m2 = rnorm(n=N, mean=MeanCheat, sd=SDCheat))

Dists = rbind(Dists, Dist2DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")



## Bootstrap sampling
Boot1DF = data.frame(Type="Boot1", 
                     soilC_g_m2 = sample(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"],
               size=N, replace=TRUE))
               
Dists = rbind(Dists, Boot1DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

## Bootstrap sampling
Boot2DF = data.frame(Type="Boot2", 
                     soilC_g_m2 = sample(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"],
                                         size=N, replace=TRUE))

Dists = rbind(Dists, Boot2DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

