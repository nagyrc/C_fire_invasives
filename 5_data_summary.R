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

unique(siwf$masterlyb)

summary(siwf$pool_value)

studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)

numstudies <- siwf %>%
  filter(veg != 'salt_desert')

unique(numstudies$Study_ID)
############################
#to get a count of mean values vs. raw data
#checkwithveg2 <- dplyr::count(studyid, pool, Study_ID, veg)
#write.csv(checkwithveg2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/checkwithveg2.csv")

#unique(studyid$veg)
#sum99 <- summarySE(data = studyid, measurevar = "pool_value", groupvars = c("pool", "veg"))
#write.csv(sum99, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/pool_means.csv")


############################

#leaving salt desert in for now
#split into two dataframes of raws and means
rawstdids <- unique(rawsonlynofire$Study_ID)

rawsonly <- siwf %>%
  filter(Study_ID %in% rawstdids) 

write.csv(rawsonly, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

#meanstdids <- unique(meansonlynofire$Study_ID)

#meansonly <- siwf %>%
  #filter(Study_ID %in% meanstdids) 

#unique(siwf$Article_ID)

#write.csv(meansonly, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/meansonly.csv")
############################
#summary of raws only
#AGB, BGB, and litter only
#For Table 1
rawmeans <- siwf %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(rawmeans) = NULL
write.csv(rawmeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawmeans.csv")

#checksagelitter <- rawsonly %>%
  #filter(pool == "litterC_g_m2" & veg == 'sagebrush')
#only 1 value, repeated 72 times





#for 0-10 cm only
surfacemeans <- siwf %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(surfacemeans) = NULL

write.csv(surfacemeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/surfacemeans.csv")


#for 10-20 cm only
tens <- siwf %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()
#97 for org soil; 0 for total soil

st_geometry(tens) = NULL

write.csv(tens, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/tens.csv")


###
AGBCraw <- subset.data.frame(siwf, pool == "AGBC_g_m2")
BGBCraw <- subset.data.frame(siwf, pool == "BGBC_g_m2")
litterCraw <- subset.data.frame(siwf, pool == "litterC_g_m2")
orgsoilCraw010 <- subset.data.frame(siwf, pool == "orgsoilC_g_m2" & topdepth_cm == 0 & bottomdepth_cm == 10)
totsoilCraw010 <- subset.data.frame(siwf, pool == "totsoilC_g_m2" & topdepth_cm == 0 & bottomdepth_cm == 10)
orgsoilCraw1020 <- subset.data.frame(siwf, pool == "orgsoilC_g_m2" & topdepth_cm == 10 & bottomdepth_cm == 20)
totsoilCraw1020 <- subset.data.frame(siwf, pool == "totsoilC_g_m2" & topdepth_cm == 10 & bottomdepth_cm == 20)
orgsoilCraw <- subset.data.frame(siwf, pool == "orgsoilC_g_m2")
totsoilCraw <- subset.data.frame(siwf, pool == "totsoilC_g_m2")

#ranges
range(AGBCraw$pool_value)
range(BGBCraw$pool_value)
range(litterCraw$pool_value)
range(orgsoilCraw010$pool_value)
range(orgsoilCraw1020$pool_value)
range(totsoilCraw010$pool_value)
range(totsoilCraw1020$pool_value)

###




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


##################################################################################
#for deeper subsets
deep1 <- siwf %>%
  filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") %>%
  filter(bottomdepth_cm > 20 & bottomdepth_cm <= 40) %>%
  mutate(Cpercm = pool_value/thick) %>%
  filter(veg != 'salt_desert')

st_geometry(deep1) = NULL

deep1summary <- deep1 %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpvpercm = mean(Cpercm), n = n(), var = var(Cpercm)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  mutate(bottom_depth = "20 - 40 cm")

summary(deep1$Cpercm) 

#rename and reorder veg and color by veg
neworder2 <- c("sagebrush","sagecheat","cheatgrass")

deep1summary <- arrange(transform(deep1summary,
                                  veg=factor(veg, levels = neworder2)),veg)

deep1summary$veg <- plyr::revalue(deep1summary$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

ggplot(deep1summary, aes(x = veg, y = meanpvpercm))+
  geom_bar(stat = "identity")

deep2 <- siwf %>%
  filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") %>%
  filter(bottomdepth_cm > 40) %>%
  mutate(Cpercm = pool_value/thick) %>%
  filter(veg != 'salt_desert')

st_geometry(deep2) = NULL

unique(deep2$topdepth_cm)
#many depths
unique(deep2$bottomdepth_cm)
#many depths
unique(deep2$Article_ID)
#NORT2004; STAR2015; GOER2011; RAU2011; SORE2013


deep2summary <- deep2 %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpvpercm = mean(Cpercm), n = n(), var = var(Cpercm)) %>%
  mutate(se = sqrt(var)/sqrt(n))

summary(deep2$Cpercm)

#rename and reorder veg; color by veg
deep2summary <- arrange(transform(deep2summary,
                            veg=factor(veg, levels = neworder2)),veg)
deep2summary$veg <- plyr::revalue(deep2summary$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

orgonly <- deep2summary %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(bottom_depth = "> 40 cm")

#combine with shallower depth; merge deep1summary with orgonly
orgzz <- deep1summary %>%
  rbind(orgonly)

neworder3 <- c("20 - 40 cm","> 40 cm")

orgzz <- arrange(transform(orgzz,
                                  bottom_depth=factor(bottom_depth, levels = neworder3)),bottom_depth)
orgzz
colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")


#need to remove total soil C for Fig. 3c
orgzzo <- orgzz %>%
  filter(pool == "orgsoilC_g_m2")

#add this plot to manuscript# new Fig. 3c
ggplot(orgzzo, aes(x = bottom_depth, y = meanpvpercm, fill = veg)) +
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  labs(y = "Soil organic C (gC cm-2) per cm thickness", x = "bottom depth sampled (cm)", fill = "vegetation") +
  scale_fill_manual(values = colours) +
  geom_errorbar(aes(ymin=meanpvpercm-se, ymax=meanpvpercm+se), width=.2, position=position_dodge(.9))   +                 # Width of the error bars 
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) 
  



totonly <- deep2summary %>%
  filter(pool == "totsoilC_g_m2")
#mean is 162.35
#se is 8.54
#n is 92

ggplot(totonly, aes(x = veg, y = meanpvpercm)) +
  geom_bar(position=position_dodge(), stat = "identity")
#only 1 bar; just report this number; don't need a figure; see values above

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
orgsoilmeans010 <- surfacemeans %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(depth = "0-10 cm") 
orgsoilmeans1020 <- tens %>%
  filter(pool == "orgsoilC_g_m2") %>%
  mutate(depth = "10-20 cm") 
totsoilmeans010 <- surfacemeans %>%
  filter(pool == "totsoilC_g_m2") %>%
  mutate(depth = "0-10 cm") 
#totsoilmeans1020 <- tens2 %>%
  #filter(pool == "totsoilC_g_m2")


########################################



siwf$pool2 <- ifelse(siwf$pool == "AGBC_g_m2", "AGB", ifelse(siwf$pool == "BGBC_g_m2", "BGB", ifelse(siwf$pool == "litterC_g_m2", "litter", ifelse(siwf$pool == "totsoilC_g_m2", "total soil", "organic soil"))))

# Histogram for each pol-veg combo to look at distributions
#reorder veg for plotting
neworder2 <- c("sagebrush","sagecheat","cheatgrass")

siwf2 <- arrange(transform(siwf, veg=factor(veg, levels = neworder2)),veg) %>%
  filter(veg != 'salt_desert')

#change veg names
siwf2$veg <- plyr::revalue(siwf2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))



#Fig. S1
###original, keep
ggplot(siwf2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + facet_grid(pool2 ~ veg) + 
  xlab("pool_value") + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.05, "in")) +
  xlab("square root (carbon content (gC m-2))") +
  ylab("square root (count)") +
  scale_y_sqrt() +
  scale_x_sqrt() +
  theme(axis.text.x = element_text(angle=90))
#the log scaling of y removes a bunch of rows; so used sqrt instead
###

DistNormDF = data.frame(Type = "Normal", pool_value = DistNorm)
DistGammaDF = data.frame(Type = "Gamma", pool_value = DistGamma)


###
#try adding normal and gamma distributions
#not working
ggplot(siwf, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  xlab("pool_value") + 
  theme_bw() + 
  facet_grid(pool2 ~ veg) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.05, "in")) +
  xlab("square root (carbon content (gC m-2))") +
  ylab("square root (count)") +
  scale_y_sqrt() +
  scale_x_sqrt() +
  theme(axis.text.x = element_text(angle=90)) +
  stat_density(colour="blue", geom="line", position="identity") +
  stat_function(fun=dnorm, args=list(mean=mean(joiny2$pool_value), sd=sd(joiny2$pool_value))) + 
  stat_function(fun=dgamma, args=list(shape=mean(joiny2$pool_value)^2/sd(joiny2$pool_value)^2, scale=sd(joiny2$pool_value)^2/mean(joiny2$pool_value)))





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


orgsoilC2 <- orgsoilCraw %>%
  mutate(depth = ifelse(bottomdepth_cm <= 10, "shallow", ifelse(bottomdepth_cm >10 & bottomdepth_cm <= 20, "mid", "deep")))

totsoilC2 <- totsoilCraw %>%
  mutate(depth = ifelse(bottomdepth_cm <= 10, "shallow", ifelse(bottomdepth_cm >10 & bottomdepth_cm <= 20, "mid", "deep")))

#reorder veg for plotting

AGBC2 <- arrange(transform(AGBCraw, veg=factor(veg, levels = neworder2)),veg) %>%
  filter(veg != 'salt_desert')

BGBC2 <- arrange(transform(BGBCraw, veg=factor(veg, levels = neworder2)),veg) %>%
  filter(veg != 'salt_desert')

litterC2 <- arrange(transform(litterCraw, veg=factor(veg, levels = neworder2)),veg) %>%
  filter(veg != 'salt_desert')


AGBC2$veg <- plyr::revalue(AGBC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))
BGBC2$veg <- plyr::revalue(BGBC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))
litterC2$veg <- plyr::revalue(litterC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

head(AGBC)

summary(AGBC2$pool_value)
summary(rawsonly$pool_value)

#Fig 2a)
ggplot(AGBC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 40) + 
  facet_wrap(~veg) + 
  xlab("AGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12))
  

#Fig 2b)
ggplot(BGBC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~veg) + 
  xlab("BGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12))


#Fig 2c)
ggplot(litterC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~veg, drop = FALSE) + 
  xlab("litter C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12)) + 
  scale_x_continuous(breaks=c(0,250,500)) +
  theme(panel.spacing = unit(0.8, "lines"))

head(orgsoilC2)

neworder <- c("shallow","mid","deep")

#library(plyr)  ## or dplyr (transform -> mutate)
orgsoilC2 <- arrange(transform(orgsoilC2, depth=factor(depth, levels = neworder)),depth) %>%
  filter(veg != 'salt_desert')

orgsoilC2 <- arrange(transform(orgsoilC2,
                               veg=factor(veg, levels = neworder2)),veg)

orgsoilC2$veg <- plyr::revalue(orgsoilC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))


#Figure 2d)
ggplot(orgsoilC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~veg+depth, drop = FALSE) + 
  xlab("organic soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12)) + 
  scale_x_continuous(breaks=c(0,4500,9000))

#+scale_y_sqrt()

totsoilC2 <- arrange(transform(totsoilC2, depth=factor(depth, levels = neworder)),depth) %>%
  filter(veg != 'salt_desert')

totsoilC2 <- arrange(transform(totsoilC2,
                               veg=factor(veg, levels = neworder2)),veg)

totsoilC2$veg <- plyr::revalue(totsoilC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

#Figure 2e)
ggplot(totsoilC2, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram(bins = 40) + 
  facet_wrap(~veg+depth, drop = FALSE) + 
  xlab("total soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 11.5)) + 
  scale_x_continuous(breaks=c(0,3000,6000)) +
  theme(panel.spacing = unit(0.6, "lines"))

#+scale_y_sqrt()



#Plotting for Fig 3
orgsoilmeans010$veg <- factor(orgsoilmeans010$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

#ggplot(orgsoilmeans010, aes(x=pool, y=meanpv, fill=veg)) + 
  #geom_bar(position=position_dodge(), stat="identity") +
  #geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se), width=.2, position=position_dodge(.9)) +
  #labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 0-10 cm")


totsoilmeans010 <- add_row(totsoilmeans010, pool = "totsoilC_g_m2", veg = "sagebrush")
totsoilmeans010$veg <- factor(totsoilmeans010$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

totsoilmeans010$veg <- plyr::revalue(totsoilmeans010$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))


colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")


#Fig. 3d
ggplot(totsoilmeans010, aes(x=veg, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "vegetation type", y = "total soil carbon (gC m-2): 0-10 cm", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours) +
  scale_x_discrete(breaks = c('native sagebrush', 'invaded sagebrush', 'cheatgrass'), 
                     labels = c('native\nsagebrush', 'invaded\nsagebrush', 'cheatgrass'))




#ggplot(orgsoilmeans1020, aes(x=pool, y=meanpv, fill=veg)) + 
  #geom_bar(position=position_dodge(), stat="identity") +
  #geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se), width=.2, position=position_dodge(.9)) +
  #labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 10-20 cm", fill = "vegetation") +
  #theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))

orgsoilmeans1020$veg <- factor(orgsoilmeans1020$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
org2 <- rbind(as.data.frame(orgsoilmeans010), as.data.frame(orgsoilmeans1020))
org2$veg <- factor(org2$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

#rename veg
org2$veg <- plyr::revalue(org2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))


#plot SOC by depth and veg
#Fig. 3b
ggplot(org2, aes(x=depth, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "depth (cm)", y = "soil organic carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours)



rawmeans$geometry <- NULL
rawmeans <- add_row(rawmeans, pool = "litterC_g_m2", veg = "sagecheat")
rawmeans$veg <- factor(rawmeans$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
rawmeans$pool2 <- ifelse(rawmeans$pool == "AGBC_g_m2", "AGB", ifelse(rawmeans$pool == "BGBC_g_m2", "BGB", "litter"))

rawmeans
rawmeans$veg <- plyr::revalue(rawmeans$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")

# For Fig 3a
ggplot(rawmeans, aes(x = pool2, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  labs(x = "carbon pool", y = "carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours)

#####################################


#analysis with fire





###
unique(siwf$yr_samp)
unique(siwf$masterlyb)

#replace NAs with 1950
#joiny2$masterlyb <- joiny2$masterlyb %>%
  #replace_na(1950)

is.numeric(siwf$yr_samp)
is.numeric(siwf$masterlyb)


cheatfire <- siwf %>%
  filter(veg == "cheatgrass")

sagecheatfire <- siwf %>%
  filter(veg == "sagecheat")

sagefire <- siwf %>%
  filter(veg == "sagebrush")

unique(cheatfire$timesincefire)
range(cheatfire$timesincefire, na.rm = TRUE)
#1, 67
range(sagecheatfire$timesincefire, na.rm = TRUE)
#1, 67
range(sagefire$timesincefire, na.rm = TRUE)
#3, 66

ggplot(data = siwf) +
  geom_point(aes(x = timesincefire, y = pool_value)) +
  facet_wrap(~pool)

recentburn <- siwf %>%
  filter(timesincefire < 20) %>%
  filter(veg != 'salt_desert')
#only 684 observations of 1667
#need to check and make sure fire info came in with simraw dataunique(recentburn$veg)
recentburn$veg <- factor(recentburn$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))


recentburn$veg <- plyr::revalue(recentburn$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")
#springgreen4 alternative for native sagebrush

recentburn$pool2 <- ifelse(recentburn$pool == "AGBC_g_m2", "AGB", ifelse(recentburn$pool == "BGBC_g_m2", "BGB", ifelse(recentburn$pool == "litterC_g_m2", "litter", ifelse(recentburn$pool == "totsoilC_g_m2", "total soil", "organic soil"))))


#Fig. 4
ggplot(data = recentburn, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  facet_wrap(~pool2) +
  xlab("Time since fire (years)") +
  ylab("Carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours) +
  xlim(0,20)



#######################
#salt desert only
salty <- siwf %>%
  filter(veg == 'salt_desert') 
  

saltytab <- salty %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup ()

st_geometry(saltytab) = NULL

write.csv(saltytab, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/saltdesert.csv")

unique(salty$study)
#Bradley et al. 2006; Driese and Reiners 1997; Bjerregaard et al. 1984; 
#West 1972; Gill and Burke 1999
########################

#GROUP MEANS  
mean1 <- siwf %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  filter(veg != "salt_desert") %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()

mean1$geometry <- NULL

surfmean1 <- siwf %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  filter(veg != "salt_desert") %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()

surfmean1$geometry <- NULL

tenmean1 <- siwf %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  filter(veg != "salt_desert") %>%
  group_by(pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  ungroup()

tenmean1$geometry <- NULL



#########################
#BELOW NOT USED
#Fig. 4 alternative with subsets for each pool
rbAGB <- subset.data.frame(recentburn, pool2 == "AGB")
rbBGB <- subset.data.frame(recentburn, pool2 == "BGB")
rblitter <- subset.data.frame(recentburn, pool2 == "litter")
rborg <- subset.data.frame(recentburn, pool2 == "organic soil")
rbtot <- subset.data.frame(recentburn, pool2 == "total soil")

#Fig. 4a
ggplot(data = rbAGB, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  xlab("Time since fire (years)") +
  xlim(0,20) +
  ylab("AGB carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  #geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours)

#Fig. 4b
#might not need BGB
ggplot(data = rbBGB, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  xlab("Time since fire (years)") +
  xlim(0,20) +
  ylab("BGB carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  #geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours)

#Fig. 4c
ggplot(data = rblitter, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  xlab("Time since fire (years)") +
  xlim(0,20) +
  ylab("Litter carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours)

#Fig. 4d
ggplot(data = rborg, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  xlab("Time since fire (years)") +
  xlim(0,20) +
  ylab("Soil organic carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  #geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours)

#Fig. 4e
ggplot(data = rbtot, aes(x = timesincefire, y = pool_value, color = veg)) +
  geom_point() + 
  xlab("Time since fire (years)") +
  xlim(0,20) +
  ylab("Total soil carbon content (gC m-2)") +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12), strip.text.x = element_text(size = 12)) +
  #geom_smooth(method = "lm", se=TRUE) + 
  theme(legend.position="bottom") +
  scale_color_manual(values = colours)




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
