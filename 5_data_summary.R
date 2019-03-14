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
  filter(study %in% smeans)

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
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(surfacemeans) = NULL

write.csv(surfacemeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/surfacemeans.csv")


#for 10-20 cm only
tens <- rawsonly %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))
#97 for org soil; 0 for total soil

st_geometry(tens) = NULL

write.csv(tens, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/tens.csv")



#for organic soils with 0 as top depth
zerosorg <- rawsonly %>%
  filter(topdepth_cm == 0 & pool == "orgsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerosorg, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerosorg.csv")


#for total soils with 0 as top depth
zerostot <- rawsonly %>%
  filter(topdepth_cm == 0 & pool == "totsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerostot, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerostot.csv")






########################################





############################
#summary of raws plus simulated raw data

#bring in simulated raw data
simrawdata <- read_csv("simrawdata.csv")
ttt <- read_csv("ttt.csv")


simraw <- simrawdata %>%
  left_join(ttt) %>%
  mutate(pool_value = simvalue) %>%
  dplyr::select(-simvalue, -explode)

#need to join simrawdata and rawsonly
rawsonly <- as.data.frame(read_csv("rawsonly.csv"))
joiny2 <- rawsonly %>%
  full_join(simraw)



#AGB, BGB, and litter only
rawmeans2 <- joiny2 %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(rawmeans2) = NULL
write.csv(rawmeans2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/rawmeans2.csv")



#now get into more detail in the soils (organic and total)
unique(joiny2$topdepth_cm)
unique(joiny2$bottomdepth_cm)

count(joiny2$bottomdepth_cm == 5)
#229

count(joiny2$bottomdepth_cm == 10)
#588
#go with 0-10 for surface soils

count(joiny2$bottomdepth_cm == 10 & joiny2$pool == "orgsoilC_g_m2")
#399

count(joiny2$bottomdepth_cm == 10 & joiny2$pool == "totsoilC_g_m2")
#189



#for 0-10 cm only
surfacemeans2 <- joiny2 %>%
  filter(bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))
#399 for org soil; 189 for total soil
st_geometry(surfacemeans2) = NULL

write.csv(surfacemeans2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/surfacemeans2.csv")


#for 10-20 cm only
tens2 <- joiny2 %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))
#97 for org soil; 0 for total soil
st_geometry(tens2) = NULL

write.csv(tens2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/tens2.csv")



#for organic soils with 0 as top depth
zerosorg2 <- joiny2 %>%
  filter(topdepth_cm == 0 & pool == "orgsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerosorg2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerosorg2.csv")


#for total soils with 0 as top depth
zerostot2 <- joiny2 %>%
  filter(topdepth_cm == 0 & pool == "totsoilC_g_m2") %>%
  group_by(veg) %>%
  mutate(norm_value = pool_value / thick) %>%
  dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

write.csv(zerostot2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerostot2.csv")




########################################
################################
#plotting
head(rawmeans)

ggplot(rawmeans, aes(x = veg, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat="identity") +
  geom_errorbar(aes(ymin = meanpv-se, ymax = meanpv+se),
                width = .2, position = position_dodge(0.9)) + 
  facet_wrap(~pool) + 
  labs(x = "vegetation type", y = "mean carbon content (gC m-2)")

sm1 <- surfacemeans %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(depth = "0-10 cm")

sm2 <- surfacemeans %>%
  filter(pool == "totsoilC_g_m2")

ggplot(sm1, aes(x=pool, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(sm2, aes(x=pool, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

ggplot(tens, aes(x=pool, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

org <- tens %>%
  mutate(depth = "10-20 cm") %>%
  rbind(sm1)

ggplot(org, aes(x=depth, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))
########################################
################################
#for ESA abstract values
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
  dplyr::summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_MTBS.csv")



#try this with BAECV as a test to see if it works
invadedburned2 <- invaded %>%
  mutate(burned = ifelse(!is.na(baecv_lyb) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  dplyr::summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")


#try to combine MTBS and BAECV
invadedburned3 <- invaded %>%
  mutate(burned = ifelse(!is.na(MTBS_DISCOVERY_YEAR) > 0 & !is.na(baecv_lyb) > 0, "burned", "unburned")) %>%
  group_by(pool, invaded, burned) %>%
  dplyr::summarise(mean = mean(pool_value), n = n())

write.csv(invadedburned3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/means_BAECV.csv")
################################
