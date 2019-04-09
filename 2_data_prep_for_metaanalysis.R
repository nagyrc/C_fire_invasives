#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018


#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox", "tidyr", "Rmisc", "dplyr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

# Read in alldata.csv
alldata = read_csv("alldata.csv")


####
#pull last year burned from alldata as overwriting the last year burned
unique(alldata$last_year_burned)
yrbn <- alldata %>%
  dplyr::select(study, site, lat, long, last_year_burned, X1) %>%
  group_by(study, site, lat, long, X1) %>%
  distinct(last_year_burned) %>%
  filter(!is.na(last_year_burned)) %>%
  ungroup()

unique(yrbn$last_year_burned)

write.csv(yrbn, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/last_year_burn_overwrite.csv", row.names = FALSE)
###


###


#slims dataframe to key variables...still wide format to use with spatial data in script 4 (adding fire data)
#retain SE and n's here
clean_study <- alldata %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","orgsoilperC", "totsoilperC", "BD_g_cm3","orgsoilC_g_m2","totsoilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick") %>%
  mutate(site = as.factor(site),
         veg = as.factor(veg)) %>%
  mutate(study_year = str_sub(study,-4,-1),
         study_year = ifelse(study_year == 'pub1', 2017, study_year)) %>%
  mutate(yr_samp = ifelse(is.na(yr_samp), study_year, yr_samp)) 

is.na(clean_study$yr_samp)

write.csv(clean_study, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/clean_study.csv", row.names = FALSE)

#create Article_ID
clean_study$Article_IDs <- str_sub(clean_study$study,1,4)
clean_study$Article_IDe <- str_sub(clean_study$study,-4,-1)
clean_study$Article_ID <- paste(toupper(clean_study$Article_IDs),clean_study$Article_IDe)
clean_study$Article_ID <- gsub(" ", "", clean_study$Article_ID) 
clean_study$Article_ID <- gsub("MAHOpub1", "MAHO2018a", clean_study$Article_ID) 
clean_study$Article_ID <- gsub("MAHOpub2", "MAHO2018b", clean_study$Article_ID) 
clean_study$Article_ID <- gsub("RICK985a", "RICK1985a", clean_study$Article_ID)
clean_study$Article_ID <- gsub("RICK985b", "RICK1985b", clean_study$Article_ID)
clean_study$Article_ID <- as.factor(clean_study$Article_ID)

unique(clean_study$Article_ID)
#these look great
head(alldata)

rm(studyid)
#creates study_ID, pool, and setup for Bethany's meta-analysis format (long format)
#clean yr_samp
#take either first year if yr_samp is a range
studyid <- clean_study %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","totsoilC_g_m2","orgsoilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick","Article_ID") %>%
  tidyr::gather(key = pool, value = pool_value, -site, -study, -yr_samp, -lat, -long, -veg, -thick, -BD_estimated, -topdepth_cm, -bottomdepth_cm, -Article_ID) %>%
  dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::mutate(Study_ID = group_indices_(., .dots = c("study", "lat", "long", "veg", "site", "bottomdepth_cm", "pool", "yr_samp"))) %>%
  filter(!is.na(pool_value)) %>%
  separate(yr_samp, c("first", "sec"), sep = "-") %>%
  mutate(yr_samp = as.numeric(first)) %>%
  dplyr::select(-sec, -first)

unique(studyid$pool)
unique(studyid$Study_ID)
#344 studies based on dataset, lat/long, veg, site, soil depth (if applicable), pool, and year sampled

#export long format for later use
write.csv(studyid, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/studyid.csv", row.names = FALSE)



head(clean_study)



######################################
#to retain SEs and n_sampled
#this works, but has some remnant values in other pool fields of SE that might be confusing

clean_studynvar <- alldata %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","orgsoilperC", "totsoilperC", "BD_g_cm3","orgsoilC_g_m2","totsoilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick", "orgsoilC_g_m2_SE", "totsoilC_g_m2_SE", "litterC_g_m2_SE", "BGBC_g_m2_SE", "AGBC_g_m2_SE", "n_sampled") %>%
  mutate(site = as.factor(site),
         veg = as.factor(veg)) %>%
  mutate(study_year = str_sub(study,-4,-1),
         study_year = ifelse(study_year == 'pub1', 2017, study_year)) %>%
  mutate(yr_samp = ifelse(is.na(yr_samp), study_year, yr_samp))



#create Article_ID
clean_studynvar$Article_IDs <- str_sub(clean_studynvar$study,1,4)
clean_studynvar$Article_IDe <- str_sub(clean_studynvar$study,-4,-1)
clean_studynvar$Article_ID <- paste(toupper(clean_studynvar$Article_IDs),clean_studynvar$Article_IDe)
clean_studynvar$Article_ID <- gsub(" ", "", clean_studynvar$Article_ID) 
clean_studynvar$Article_ID <- gsub("MAHOpub1", "MAHO2018a", clean_studynvar$Article_ID) 
clean_studynvar$Article_ID <- gsub("MAHOpub2", "MAHO2018b", clean_studynvar$Article_ID) 
clean_studynvar$Article_ID <- gsub("RICK985a", "RICK1985a", clean_studynvar$Article_ID)
clean_studynvar$Article_ID <- gsub("RICK985b", "RICK1985b", clean_studynvar$Article_ID)
clean_studynvar$Article_ID <- as.factor(clean_studynvar$Article_ID)

studyidSE <- clean_studynvar %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","AGBC_g_m2_SE", "BGBC_g_m2","BGBC_g_m2_SE", "litterC_g_m2","litterC_g_m2_SE", "totsoilC_g_m2","totsoilC_g_m2_SE", "orgsoilC_g_m2", "orgsoilC_g_m2_SE", "topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick","Article_ID", "n_sampled") %>%
  tidyr::gather(key = pool, value = pool_value, -site, -study, -yr_samp, -lat, -long, -veg, -thick, -BD_estimated, -topdepth_cm, -bottomdepth_cm, -Article_ID, -orgsoilC_g_m2_SE, -totsoilC_g_m2_SE, -litterC_g_m2_SE, -BGBC_g_m2_SE, -AGBC_g_m2_SE, -n_sampled) %>%
  #dplyr::mutate_if(is.character, as.factor) %>%
  dplyr::mutate(Study_ID = group_indices_(., .dots = c("study", "lat", "long", "veg", "site", "bottomdepth_cm", "pool", "yr_samp"))) %>%
  filter(!is.na(pool_value)) %>%
  separate(yr_samp, c("first", "sec"), sep = "-") %>%
  mutate(yr_samp = as.numeric(first)) %>%
  dplyr::select(-sec, -first)

#split data into means and raw data
studymeans <- as.data.frame(read_csv("data/study_means.csv"))
smeans <- unique(studymeans$study)

meansonlynvar <- studyidSE %>%
  filter(study %in% smeans) 

write.csv(meansonlynvar, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/meansonlynvar.csv", row.names = FALSE)

######################################








###################################
###plotting example: pool value by year sampled, colored by pool         
studyid %>%
  ggplot(aes(x = pool, y = pool_value)) +
  geom_point() +
  facet_wrap(~Article_ID)

###

#example code to change NAs to zeros (if needed) and create a function (if needed)
#alldata %>%
  #dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","totsoilperC","orgsoilperC","BD_g_cm3","totsoilC_g_m2","orgsoilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick","Article_ID") %>%
  #mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  #mutate(content = BGBC_g_m2*orgsoilperC-topdepth_cm)
         


####################
#plotting

#set crs for all data layers: Albers Equal Area
crs1 <- 'ESRI:102003'
crs1b <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'


#bring in shapefile of US states
usa_shp <- st_read(file.path('data/states_shp'), layer = 'cb_2016_us_state_20m') %>%
  filter(!(NAME %in% c("Alaska", "Hawaii", "Puerto Rico"))) %>%
  st_transform(crs1b) %>%  #Albers equal area
  dplyr::select(STATEFP, STUSPS) %>%
  setNames(tolower(names(.)))

plot(usa_shp["geometry"])

#to show data points across the study area
studyid_pt <- st_as_sf(studyid, coords = c("long", "lat"),
                       crs = 4326) %>%
  st_transform(crs = st_crs(usa_shp))

crs(usa_shp)
crs(studyid_pt)

extent(usa_shp)
extent(studyid_pt)

identical(crs(usa_shp),crs(studyid_pt))
#TRUE

studyid_pt$pool2 <- ifelse(studyid_pt$pool == "AGBC_g_m2", "AGB", ifelse(studyid_pt$pool == "BGBC_g_m2", "BGB", ifelse(studyid_pt$pool == "litterC_g_m2", "litter", ifelse(studyid_pt$pool == "totsoilC_g_m2", "total soil", "organic soil"))))


#plot with pool as the color
is.factor(studyid_pt$pool)
levels(studyid_pt$pool)
plot(studyid_pt["pool2"], key.pos = 1)
plot(usa_shp["geometry"], add = TRUE)


#plot with veg as the color
plot(studyid_pt["veg"], key.pos = 1)
plot(usa_shp["geometry"], add = TRUE)

#plot with Study_ID as the color
plot(studyid_pt["Study_ID"])
plot(usa_shp["geometry"], add = TRUE)
#more than 1500 'studies'; actually only 372 once the rows with NA's were removed



extent(studyid_pt)
head(studyid_pt)

#crop usa_shp to extent of studyid_pt
us_crop <- st_crop(usa_shp, extent(studyid_pt))

#Fig. 1...legend is not quite right
ggplot(data = us_crop) +
  geom_sf() +
  geom_sf(data = studyid_pt, size = 1.5, aes(color = pool2, shape = pool2)) +
  labs(fill = "carbon pool") +
  #theme(legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size = 12), legend.title=element_text(size = 12))

#Fig. 1...legend is still not right
ggplot() +
  geom_sf(data = us_crop) +
  geom_sf(data = studyid_pt, aes(color = pool2, shape = pool2)) +
  scale_color_manual(name = "carbon pool",
   breaks = c("AGB", "BGB", "litter", "organic soil", "total soil"),
   values = c("blue3", "darkgrey", "forestgreen", "darkgrey", "darkgrey")) +
  #theme(legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size = 12), legend.title=element_text(size = 12))


