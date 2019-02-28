#Data summary
#Dr. R. Chelsea Nagy
#created February 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#no fire data; #bring in studyid dataframe
siwf <- studyid_sf

rawsonly <- siwf %>%
  filter(!study %in% smeans)

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
#97; 0 for total soil

unique(tens$bottomdepth_cm)
#20
st_geometry(tens) = NULL

write.csv(tens, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/tens.csv")


  