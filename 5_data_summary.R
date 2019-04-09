#Data summary
#Dr. R. Chelsea Nagy
#created February 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid or siwf dataframe
studyid <- as.data.frame(read_csv("studyid.csv"))
siwf <- as.data.frame(read_csv("siwf.csv"))

studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)

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
#zerosorg <- rawsonly %>%
  #filter(topdepth_cm == 0 & pool == "orgsoilC_g_m2") %>%
  #group_by(veg) %>%
  #mutate(norm_value = pool_value / thick) %>%
  #dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  #mutate(se = sqrt(var)/sqrt(n))

#write.csv(zerosorg, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerosorg.csv")


#for total soils with 0 as top depth
#zerostot <- rawsonly %>%
  #filter(topdepth_cm == 0 & pool == "totsoilC_g_m2") %>%
  #group_by(veg) %>%
  #mutate(norm_value = pool_value / thick) %>%
  #dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  #mutate(se = sqrt(var)/sqrt(n))

#write.csv(zerostot, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerostot.csv")






########################################





############################
#summary of raws plus simulated raw data

#bring in simulated raw data
simrawdata <- read_csv("simrawdata.csv")
ttt <- read_csv("ttt.csv")
head(ttt)

simraw <- simrawdata %>%
  left_join(ttt) %>%
  mutate(pool_value = simvalue) %>%
  dplyr::select(-simvalue, -explode)

unique(simraw$Article_ID)
head(simraw)

#need to join simrawdata and rawsonly
rawsonly <- as.data.frame(read_csv("rawsonly.csv"))
joiny2 <- rawsonly %>%
  full_join(simraw) %>%
  mutate_if(is.character, as.factor) %>%
  filter(veg != "salt_desert")

write.csv(joiny2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/joiny2.csv")
unique(joiny2$Article_ID)

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

#AGB, BGB, and litter only
rawmeans2 <- joiny2 %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()

st_geometry(rawmeans2) = NULL
write.csv(rawmeans2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/rawmeans2.csv")



#now get into more detail in the soils (organic and total)
unique(joiny2$topdepth_cm)
unique(joiny2$bottomdepth_cm)

count(joiny2$bottomdepth_cm == 5)
#229

count(joiny2$bottomdepth_cm == 10)
#564
#go with 0-10 for surface soils

count(joiny2$bottomdepth_cm == 10 & joiny2$pool == "orgsoilC_g_m2")
#399

count(joiny2$bottomdepth_cm == 10 & joiny2$pool == "totsoilC_g_m2")
#165



#for 0-10 cm only
surfacemeans2 <- joiny2 %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup ()
#399 for org soil; 165 for total soil
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
#zerosorg2 <- joiny2 %>%
  #filter(topdepth_cm == 0 & pool == "orgsoilC_g_m2") %>%
  #group_by(veg) %>%
  #mutate(norm_value = pool_value / thick) %>%
  #dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  #mutate(se = sqrt(var)/sqrt(n))

#write.csv(zerosorg2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerosorg2.csv")


#for total soils with 0 as top depth
#zerostot2 <- joiny2 %>%
  #filter(topdepth_cm == 0 & pool == "totsoilC_g_m2") %>%
  #group_by(veg) %>%
  #mutate(norm_value = pool_value / thick) %>%
  #dplyr::summarise(meanpv = mean(norm_value), n = n(), var = var(norm_value)) %>%
  #mutate(se = sqrt(var)/sqrt(n))

#write.csv(zerostot2, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/zerostot2.csv")

#subset soils to appropriate depths
orgsoilmeans010 <- surfacemeans2 %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(depth = "0-10 cm") 
orgsoilmeans1020 <- tens2 %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(depth = "10-20 cm") 
totsoilmeans010 <- surfacemeans2 %>%
  filter(pool == "totsoilC_g_m2") %>%
  mutate(depth = "0-10 cm") 
#totsoilmeans1020 <- tens2 %>%
  #filter(pool == "totsoilC_g_m2")

########################################
################################
#plotting


#raw data only
head(rawmeans)

ggplot(rawmeans, aes(x = veg, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  facet_wrap(~pool) + 
  labs(x = "vegetation type", y = "carbon content (gC m-2)")

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

ggplot(sm2, aes(x=veg, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "vegetation type", y = "total soil carbon content (gC m-2): 0-10 cm")

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
                position=position_dodge(.9)) +
  labs(x = "depth (cm)", y = "soil organic carbon content (gC m-2)")





########################################
#raw plus simulated data

head(joiny2)
joiny2$pool2 <- ifelse(joiny2$pool == "AGBC_g_m2", "AGB", ifelse(joiny2$pool == "BGBC_g_m2", "BGB", ifelse(joiny2$pool == "litterC_g_m2", "litter", ifelse(joiny2$pool == "totsoilC_g_m2", "total soil", "organic soil"))))

# Histogram for each pol-veg combo to look at distributions
ggplot(joiny2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + facet_grid(pool2 ~ veg) + 
  xlab("pool_value") + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.05, "in")) +
  xlab("Carbon Content (gC m-2)") 

#try with subsets for each pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")


head(AGBC)
ggplot(AGBC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 40) + 
  facet_wrap(~veg) + 
  xlab("AGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in"))

ggplot(BGBC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~veg) + 
  xlab("BGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in"))

ggplot(litterC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~veg) + 
  xlab("litter C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in"))

ggplot(orgsoilC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~veg) + 
  xlab("organic soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in"))

ggplot(totsoilC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 40) + 
  facet_wrap(~veg) + 
  xlab("total soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in"))



#plotting means of raw + simulated raw values
orgsoilmeans010$veg <- factor(orgsoilmeans010$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

ggplot(orgsoilmeans010, aes(x=pool, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 0-10 cm")


totsoilmeans010 <- add_row(totsoilmeans010, pool = "totsoilC_g_m2", veg = "sagebrush")
totsoilmeans010$veg <- factor(totsoilmeans010$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

ggplot(totsoilmeans010, aes(x=veg, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "vegetation type", y = "total soil carbon content (gC m-2): 0-10 cm", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))


orgsoilmeans1020$veg <- factor(orgsoilmeans1020$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

ggplot(orgsoilmeans1020, aes(x=pool, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 10-20 cm", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))


org2 <- orgsoilmeans010 %>%
  rbind(orgsoilmeans1020)

org2$veg <- factor(org2$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))


ggplot(org2, aes(x=depth, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "depth (cm)", y = "soil organic carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))


rawmeans2$veg <- factor(rawmeans2$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
rawmeans2 <- add_row(rawmeans2, pool = "litterC_g_m2", veg = "sagecheat")
rawmeans2$pool2 <- ifelse(rawmeans2$pool == "AGBC_g_m2", "AGB", ifelse(rawmeans2$pool == "BGBC_g_m2", "BGB", "litter"))

unique(rawmeans2$pool)
head(rawmeans2)
#ggplot(rawmeans2, aes(x = veg, y = meanpv, fill = veg)) + 
  #geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  #geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se), width = .2, position = position_dodge(0.9)) + 
  #facet_wrap(~pool) + 
  #labs(x = "carbon pool by vegetation type", y = "carbon content (gC m-2)", fill = "vegetation") +
  #theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))

ggplot(rawmeans2, aes(x = pool2, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  labs(x = "carbon pool", y = "carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))



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

head(joiny2)
#analysis with fire
joiny2$timesincefire <- joiny2$yr_samp - joiny2$masterlyb

ggplot(data = joiny2) +
  geom_point(aes(x = timesincefire, y = pool_value)) +
  facet_wrap(~pool)

recentburn <- joiny2 %>%
  filter(timesincefire < 20)
#only 303 observations of 2123
#need to check and make sure fire info came in with simraw data

ggplot(data = recentburn) +
  geom_point(aes(x = timesincefire, y = pool_value)) + 
  facet_wrap(~pool2) +
  xlab("Time since fire (years)") +
  ylab("Carbon content (gC m-2)")
