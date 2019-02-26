#Data exploration continued
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid dataframe
siwf = read_csv("studyid_with_fire.csv")

#or load from script 4
siwf <- baecv_rep

#remove extra X1 column that was added (I believe with export)
siwf <- siwf %>%
  select(-X1_1) %>%

siwf_sep <- siwf %>%
#take either first or last year if yr_samp is a range
#check this with Emily and Bethany; does this choice matter?
  separate(yr_samp, c("first", "sec"), sep = "-") %>%
  mutate(yr_samp = as.numeric(first)) %>%
  select(-sec, -first)

################################
#for abstract values
mean1 <- siwf_sep %>%
  group_by(pool) %>%
  summarise(mean = mean(pool_value))

invaded <- siwf_sep %>%
  mutate(invaded = ifelse(veg == "cheatgrass", "invaded", "native"))
  
invadedmeans <- invaded %>%  
  group_by(pool, invaded) %>%
  summarise(mean = mean(pool_value))

#try this with MTBS as a test to see if it works
#also may want to try a different threshold for burned here (e.g., burned in 10 yrs prior to sampling)
invadedburned1 <- invaded %>%
  mutate(burned = ifelse(!is.na(MTBS_DISCOVERY_YEAR) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_MTBS.csv")



#try this with BAECV as a test to see if it works
invadedburned2 <- invaded %>%
  mutate(burned = ifelse(!is.na(baecv_lyb) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")


#try to combine MTBS and BAECV
invadedburned3 <- invaded %>%
  mutate(burned = ifelse(!is.na(MTBS_DISCOVERY_YEAR) > 0 & !is.na(baecv_lyb) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")
################################




####################################
#split data into means and raw data
studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)

#create means only dataframe
#characterize burned as burned <10 yrs before sampling; using MTBS; could use other fire product
#this is using MTBS only; can include other fire products
meansonly <- siwf_sep %>%
  filter(study %in% smeans) %>%
  group_by(study) %>%
  mutate(paired = ifelse(length(unique(veg)) > 1, "paired", "unpaired")) %>%
  mutate(invaded = ifelse(veg == "cheatgrass" | veg == "sagecheat", "invaded", "native")) %>%
  mutate(burned = ifelse(yr_samp - MTBS_DISCOVERY_YEAR < 10, "burned", "unburned"))
#109 obs

write.csv(meansonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/meansonly.csv")


#create raw data only dataframe
rawsonly <- siwf_sep %>%
  filter(!study %in% smeans)
#1204 obs


#need to add variance here
sumz <- rawsonly %>%
  #fill in all variables that we need to group by here
  #or just use Study_ID if you don't care about seeing other variables
  #group_by(veg, site, yr_samp, topdepth_cm, bottomdepth_cm, pool, study, Study_ID, lat, long, thick) %>%
  group_by(Study_ID) %>%
  summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value))

#then use sumz to left_join with rawsonly based on Study_ID
#this is using MTBS only; can include other fire products
rawsonly2 <- rawsonly %>%
  left_join(as.data.frame(sumz) %>%
  dplyr::select(-geometry)) %>%
  group_by(study) %>%
  mutate(paired = ifelse(length(unique(veg)) > 1, "paired", "unpaired")) %>%
  mutate(invaded = ifelse(veg == "cheatgrass" | veg == "sagecheat", "invaded", "native")) %>%
  mutate(burned = ifelse(yr_samp - MTBS_DISCOVERY_YEAR < 10, "burned", "unburned"))
  
write.csv(rawsonly2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")
