#Data exploration continued
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid dataframe
siwf = read_csv("studyid_with_fire.csv")

mean1 <- siwf %>%
  group_by(pool) %>%
  summarise(mean = mean(pool_value))

invaded <- siwf %>%
  mutate(invaded = ifelse(veg == "cheatgrass", "invaded", "native"))
  
invadedmeans <- invaded %>%  
  group_by(pool, invaded) %>%
  summarise(mean = mean(pool_value))

#try this with MTBS as a test to see if it works
invadedburned1 <- invaded %>%
  mutate(burned = ifelse(!is.na(MTBS_DISCOVERY_YEAR) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_MTBS.csv")



#try this with BAECV as a test to see if it works
invadedburned2 <- invaded %>%
  mutate(burned = ifelse(!is.na(lyb_usa_baecv_1984_2015) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")


#try to combine MTBS and BAECV
invadedburned3 <- invaded %>%
  mutate(burned = ifelse(!is.na(lyb_usa_baecv_1984_2015) > 0 & !is.na(lyb_usa_baecv_1984_2015) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")




