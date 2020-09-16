#Using simulated raw data
#Dr. R. Chelsea Nagy
#created September 16, 2020

# preprocessing ================================================================

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "ggpubr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid or siwf dataframe
studyid <- as.data.frame(read_csv("studyid.csv"))
siwf <- as.data.frame(read_csv("siwf.csv"))

unique(siwf$masterlyb)

summary(siwf$pool_value)

studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)

#to get a count of mean values vs. raw data
#checkwithveg2 <- dplyr::count(studyid, pool, Study_ID, veg)
#write.csv(checkwithveg2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/checkwithveg2.csv")

#unique(studyid$veg)
#sum99 <- summarySE(data = studyid, measurevar = "pool_value", groupvars = c("pool", "veg"))
#write.csv(sum99, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/pool_means.csv")



#leaving salt desert in for now
#split into two dataframes of raws and means
#rawstdids <- unique(rawsonlynofire$Study_ID)

#rawsonly <- siwf %>%
#filter(Study_ID %in% rawstdids) 

#write.csv(rawsonly, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

#meanstdids <- unique(meansonlynofire$Study_ID)

#meansonly <- siwf %>%
#filter(Study_ID %in% meanstdids) 

#unique(siwf$Article_ID)

#write.csv(meansonly, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/meansonly.csv")

############################

##################################################################################
#summary of raws plus simulated raw data

#bring in simulated raw data
simrawdata <- read_csv("simrawdata.csv")
ttt <- read_csv("ttt.csv")
head(ttt)

simraw <- simrawdata %>%
  left_join(ttt) %>%
  mutate(pool_value = simvalue) %>%
  dplyr::select(-simvalue, -explode) %>%
  filter(veg != "salt_desert")

#####checking data values
look <- simraw %>%
  group_by(veg, pool) %>%
  summarise(mean = mean(pool_value), min = min(pool_value), max = max(pool_value))

siwfnogeom <- siwf

siwfnogeom$geometry <- NULL

lookraw <- siwfnogeom %>%
  filter(veg != "salt_desert") %>%
  group_by(veg, pool) %>%
  summarise(mean = mean(pool_value), min = min(pool_value), max = max(pool_value))
###

simrawsalt <- simrawdata %>%
  left_join(ttt) %>%
  mutate(pool_value = simvalue) %>%
  dplyr::select(-simvalue, -explode) 

unique(simraw$Article_ID)
head(simraw)

#need to join simrawdata and rawsonly
#rawsonly <- as.data.frame(read_csv("rawsonly.csv"))
simraw$Study_ID <-as.factor(simraw$Study_ID)
is.factor(simraw$Study_ID)
rawsonly$Study_ID <-as.factor(rawsonly$Study_ID)
is.factor(rawsonly$Study_ID)

joiny2 <- rawsonly %>%
  full_join(simraw) %>%
  mutate_if(is.character, as.factor) %>%
  filter(veg != "salt_desert") 
#2860 observations

unique(joiny2$study) #42
unique(joiny2$Study_ID) #388

###
#making an option with salt desert; 3274 observations
simrawsalt$Study_ID <-as.factor(simrawsalt$Study_ID)
is.factor(simrawsalt$Study_ID)

joiny2salt <-rawsonly %>%
  full_join(simrawsalt) %>%
  mutate_if(is.character, as.factor)  
unique(joiny2salt$Article_ID) # 41
unique(siwf$Article_ID) #42
unique()
unique(joiny2$Article_ID) #41; does not include BJER1984- salt desert only

saltonly <- joiny2salt %>%
  filter(veg == 'salt_desert') 

unique(saltonly$pool) #all 5 pools
unique(saltonly$study) #5 studies
###

head(joiny2)
write.csv(joiny2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/joiny2.csv")
unique(joiny2$Article_ID)
unique(joiny2$veg)

#need to subset bbb for studies in smeans
#bring in studymeans from script 2
#studymeans <- as.data.frame(read_csv("data/study_means.csv"))
#smeans <- unique(studymeans$study)

#bring in bbb from script 3
#bbb <- as.data.frame(read_csv("data/bbb.csv"))

#fireinfo <- bbb %>%
#filter(study %in% smeans)
#only Diamond et al. 2012 burned in 1996

#then join with joiny2a
#joiny2 <- joiny2a %>%
#inner_join(bbb)
#why is Diamond not in joiny2? Diamond was removed because it didn't have a SE

unique(joiny2$Article_ID)


#for Table 1 (raw + simulated raw)
#AGB, BGB, and litter only
rawmeans2 <- joiny2 %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()

rawmeans2b <- joiny2 %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()
rawmeans2b

st_geometry(rawmeans2) = NULL
write.csv(rawmeans2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawmeans2.csv")






#for Table 1 (raw + simulated)
#for 0-10 cm only
surfacemeans2 <- joiny2 %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup ()

st_geometry(surfacemeans2) = NULL

write.csv(surfacemeans2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/surfacemeans2.csv")


surfacemeans2b <- joiny2 %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup ()




#for Table 1 (raw + simulated)
#for 10-20 cm only
tens2 <- joiny2 %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(tens2) = NULL

write.csv(tens2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/tens2.csv")


tens2b <- joiny2 %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))



###
#make joiny2 again with original veg types
#joiny2 <- rawsonly %>%
#full_join(simraw) %>%
#mutate_if(is.character, as.factor) %>%
#filter(veg != "salt_desert")


#joiny2$pool2 <- ifelse(joiny2$pool == "AGBC_g_m2", "AGB", ifelse(joiny2$pool == "BGBC_g_m2", "BGB", ifelse(joiny2$pool == "litterC_g_m2", "litter", ifelse(joiny2$pool == "totsoilC_g_m2", "total soil", "organic soil"))))
#joiny2 <- arrange(transform(joiny2,
#veg=factor(veg, levels = neworder2)),veg)

#try with subsets for each pool
#AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
#BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
#litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
#orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
#totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

#summary(AGBC2$pool_value)