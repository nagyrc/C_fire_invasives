#Data summary
#Dr. R. Chelsea Nagy
#created February 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid or siwf dataframe
studyid = read_csv("studyid.csv")
siwf = read_csv("siwf.csv")


############################
#to get a count of mean values vs. raw data
checkwithveg2 <- dplyr::count(studyid, pool, Study_ID, veg)
write.csv(checkwithveg2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/checkwithveg2.csv")

unique(studyid$veg)
sum99 <- summarySE(data = studyid, measurevar = "pool_value", groupvars = c("pool", "veg"))
write.csv(sum99, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/pool_means.csv")


############################
#split into two dataframes of raws and means
rawsonly <- siwf %>%
  filter(!study %in% smeans)

write.csv(rawsonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

meansonly <- siwf %>%
  filter(study %in% smeans) %>%

write.csv(meansonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/meansonly.csv")
############################
#summary of raws only
#AGB, BGB, and litter only
rawmeans <- rawsonly %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(rawmeans) = NULL
write.csv(rawmeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/rawmeans.csv")



#now get into more detail in the soils (organic and total)
unique(rawsonly$topdepth_cm)
unique(rawsonly$bottomdepth_cm)

count(rawsonly$bottomdepth_cm == 5)
#125

count(rawsonly$bottomdepth_cm == 10)
#583
#go with 0-10 for surface soils

count(rawsonly$bottomdepth_cm == 10 & rawsonly$pool == "orgsoilC_g_m2")
#399

count(rawsonly$bottomdepth_cm == 10 & rawsonly$pool == "totsoilC_g_m2")
#184



#for 0-10 cm only
surfacemeans <- rawsonly %>%
  filter(bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(surfacemeans) = NULL

write.csv(surfacemeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/surfacemeans.csv")



tens <- rawsonly %>%
  filter(topdepth_cm == 10 & pool == "orgsoilC_g_m2" & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))
#97 for org soil; 0 for total soil

unique(tens$bottomdepth_cm)
#20
st_geometry(tens) = NULL

write.csv(tens, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/tens.csv")




zerosorg <- rawsonly %>%
  filter(topdepth_cm == 0 & pool == "orgsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerosorg, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerosorg.csv")



zerostot <- rawsonly %>%
  filter(topdepth_cm == 0 & pool == "totsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerostot, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerostot.csv")






########################################
################################
#for abstract values
mean1 <- siwf %>%
  group_by(pool) %>%
  summarise(mean = mean(pool_value))

invaded <- siwf %>%
  mutate(invaded = ifelse(veg == "cheatgrass" | veg == "sagecheat", "invaded", "native"))

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
