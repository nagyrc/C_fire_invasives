#Data exploration continued
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")


########################################################
#no fire data; #bring in studyid dataframe
siwf <- studyid_sf


#bring in paired Study_IDs
#pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))
pairs <- as.data.frame(read_csv("paired_studyIDs.csv"))
pairs %>%
  dplyr::select(-Article_ID, -pool)


p1 <- list(pairs)
#split data into means and raw data
studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)


####################
#create raw data only dataframe for paired studies
rawsonly <- siwf %>%
  filter(Study_ID == 530 | Study_ID == 535)

#need to add variance here
sumz <- rawsonly %>%
  group_by(Study_ID) %>%
  summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value))

#then use sumz to left_join with rawsonly based on Study_ID
#did not remove geometry
rawsonly2 <- rawsonly %>%
  left_join(as.data.frame(sumz) %>%
              dplyr::select(-geometry))

write.csv(rawsonly2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

################





#######################
#select only paired studies
rawp <- rawsonly2 %>%
  filter(paired == "paired") %>%
  group_by(Study_ID, pool) %>%
  summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  dplyr::select(-geometry)

joiny <- rawsonly3 %>%
  group_by(Study_ID, veg, yrssinceb) %>%
  summarise() 

st_geometry(rawp) = NULL

#add back in the yrs since burn and veg type
rawpj <- rawp %>%
  left_join(as.data.frame(joiny)) 

#transpose to wide format means first
rawpjmw <- rawpj %>%
  select(Study_ID, meanpv, veg, pool) %>%
  spread(key = veg, value = meanpv) %>%
  rename(meancheat = cheatgrass, meansage = sagebrush, meansagecheat = sagecheat, meansalt = salt_desert)

#transpose to wide format n 
rawpjnw <- rawpj %>%
  select(Study_ID, n, veg, pool) %>%
  spread(key = veg, value = n) %>%
  rename(ncheat = cheatgrass, nsage = sagebrush, nsagecheat = sagecheat, nsalt = salt_desert)


#transpose to wide format var 
rawpjvw <- rawpj %>%
  select(Study_ID, var, veg, pool) %>%
  spread(key = veg, value = var) %>%
  rename(varcheat = cheatgrass, varsage = sagebrush, varsagecheat = sagecheat, varsalt = salt_desert)

#join the three tables together
step1 <- rawpjmw %>%
  left_join(rawpjnw)

step2 <- step1 %>%
  left_join(rawpjvw)

rawsonly4 <- step2 %>%
  left_join(joiny)

write.csv(rawsonly4, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/rawsonly4.csv")






###################################


#create means only dataframe
#characterize burned as burned <10 yrs before sampling; using MTBS; could use other fire product
#this is using MTBS only; can include other fire products
meansonly <- siwf %>%
  filter(study %in% smeans) %>%
  group_by(study) %>%
  mutate(invaded = ifelse(veg == "cheatgrass" | veg == "sagecheat", "invaded", "native")) #%>%
#mutate(burned = ifelse(yr_samp - MTBS_DISCOVERY_YEAR < 10, "burned", "unburned"))
#109 obs

write.csv(meansonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/meansonly.csv")


####################





  