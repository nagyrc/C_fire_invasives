#Data summary
#Dr. R. Chelsea Nagy
#created February 28, 2019

# preprocessing ================================================================

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "ggpubr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in siwf dataframe
siwf <- as.data.frame(read_csv("siwf.csv"))

numstudies <- siwf %>%
  filter(veg != 'salt_desert')

unique(numstudies$Study_ID)




############################
#summary of raw data only with means counted as individual points

#For Table 1, AGB, BGB, and litter only
rawmeans <- siwf %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(rawmeans) = NULL
write.csv(rawmeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawmeans.csv")


#For Table 1, for 0-10 cm only, SOC, TC
surfacemeans <- siwf %>%
  filter(topdepth_cm == 0 & bottomdepth_cm == 10) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(surfacemeans) = NULL
write.csv(surfacemeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/surfacemeans.csv")


#For Table 1, for 10-20 cm only, SOC
tens <- siwf %>%
  filter(topdepth_cm == 10 & bottomdepth_cm == 20) %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(tens) = NULL
write.csv(tens, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/tens.csv")



#######################
#salt desert only, for Table S1
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


#subset data into pools for analysis
AGBCraw <- subset.data.frame(siwf, pool == "AGBC_g_m2")
BGBCraw <- subset.data.frame(siwf, pool == "BGBC_g_m2")
litterCraw <- subset.data.frame(siwf, pool == "litterC_g_m2")
orgsoilCraw010 <- subset.data.frame(siwf, pool == "orgsoilC_g_m2" & topdepth_cm == 0 & bottomdepth_cm == 10)
totsoilCraw010 <- subset.data.frame(siwf, pool == "totsoilC_g_m2" & topdepth_cm == 0 & bottomdepth_cm == 10)
orgsoilCraw1020 <- subset.data.frame(siwf, pool == "orgsoilC_g_m2" & topdepth_cm == 10 & bottomdepth_cm == 20)
totsoilCraw1020 <- subset.data.frame(siwf, pool == "totsoilC_g_m2" & topdepth_cm == 10 & bottomdepth_cm == 20)
orgsoilCraw <- subset.data.frame(siwf, pool == "orgsoilC_g_m2")
totsoilCraw <- subset.data.frame(siwf, pool == "totsoilC_g_m2")



##################################################################################
#for deeper subsets (> 20 cm deep)
deep1 <- siwf %>%
  filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") %>%
  filter(bottomdepth_cm > 20 & bottomdepth_cm <= 40) %>%
  mutate(Cpercm = pool_value/thick) %>%
  filter(veg != 'salt_desert')

st_geometry(deep1) = NULL

#these values are for Figure 2c
deep1summary <- deep1 %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpvpercm = mean(Cpercm), n = n(), var = var(Cpercm)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  mutate(bottom_depth = "20 - 40 cm")

summary(deep1$Cpercm) 


deep2 <- siwf %>%
  filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") %>%
  filter(bottomdepth_cm > 40) %>%
  mutate(Cpercm = pool_value/thick) %>%
  filter(veg != 'salt_desert')

st_geometry(deep2) = NULL


#These values are for Figure 2c
deep2summary <- deep2 %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpvpercm = mean(Cpercm), n = n(), var = var(Cpercm)) %>%
  mutate(se = sqrt(var)/sqrt(n))

summary(deep2$Cpercm)



################################################################



#rename and reorder veg and color by veg
neworder2 <- c("sagebrush","sagecheat","cheatgrass")

deep1summary <- arrange(transform(deep1summary,
                                  veg=factor(veg, levels = neworder2)),veg)

deep1summary$veg <- plyr::revalue(deep1summary$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))


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

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")


#need to remove total soil C for Fig. 2c
orgzzo <- orgzz %>%
  filter(pool == "orgsoilC_g_m2")

totonly <- deep2summary %>%
  filter(pool == "totsoilC_g_m2")


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


################################################################


siwf$pool2 <- ifelse(siwf$pool == "AGBC_g_m2", "AGB", ifelse(siwf$pool == "BGBC_g_m2", "BGB", ifelse(siwf$pool == "litterC_g_m2", "litter", ifelse(siwf$pool == "totsoilC_g_m2", "total soil", "organic soil"))))

# Histogram for each pol-veg combo to look at distributions
#reorder veg for plotting
#neworder2 <- c("sagebrush","sagecheat","cheatgrass")

siwf2 <- arrange(transform(siwf, veg=factor(veg, levels = neworder2)),veg) %>%
  filter(veg != 'salt_desert')

#change veg names
siwf2$veg <- plyr::revalue(siwf2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))



################################################################






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






# figure S1 individual plots================================================================
#Fig. S1a
fS1a<- ggplot(AGBC2, aes(x = pool_value)) + 
  geom_histogram(bins = 40) + 
  facet_wrap(~veg) + 
  xlab("AGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12))

fS1a
#Fig. S1b
fS1b<-ggplot(BGBC2, aes(x = pool_value)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~veg) + 
  xlab("BGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),axis.title.y=element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12))


#Fig. S1c
fS1c<-ggplot(litterC2, aes(x = pool_value)) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~veg, drop = FALSE) + 
  xlab("litter C (gC m-2)") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
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


#Fig. S1d
fS1d<-ggplot(orgsoilC2, aes(x = pool_value)) + 
  geom_histogram(bins = 30) + 
  facet_grid( vars(depth),vars(veg), drop = FALSE) + 
  xlab("organic soil C (gC m-2)") + 
  theme_bw() + 
  theme(#panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.key.size =  unit(0.1, "in")) +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        legend.text=element_text(size=14), 
        legend.title=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 12)) + 
  scale_x_continuous(breaks=c(0,4500,9000))

#+scale_y_sqrt()

totsoilC2 <- arrange(transform(totsoilC2, depth=factor(depth, levels = neworder)),depth) %>%
  filter(veg != 'salt_desert')

totsoilC2 <- arrange(transform(totsoilC2,
                               veg=factor(veg, levels = neworder2)),veg)

totsoilC2$veg <- plyr::revalue(totsoilC2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

#Fig. S1e
fS1e<-ggplot(totsoilC2, aes(x = pool_value)) + 
  geom_histogram(bins = 40) + 
  facet_grid(vars(depth),vars(veg), drop = FALSE) + 
  xlab("total soil C (gC m-2)") + 
  theme_bw() + 
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        legend.key.size =  unit(0.1, "in"),
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14), 
        strip.text.x = element_text(size = 11.5)) + 
  scale_x_continuous(breaks=c(0,3000,6000)) +
  theme(panel.spacing = unit(0.6, "lines"))

#+scale_y_sqrt()



# figure S1 all together ========================================================

ggarrange(fS1a,fS1b,fS1c,fS1d,fS1e, ncol=3, nrow=2,legend = "none", labels = "auto") +
  ggsave("figure_S1.png", height=10, width=17)+
  ggsave("figure_S1.pdf", height=10, width=17)



# Plotting for Fig 2 ===========================================================
orgsoilmeans010$veg <- factor(orgsoilmeans010$veg,
                              levels = c("sagebrush", "sagecheat", "cheatgrass"))

#ggplot(orgsoilmeans010, aes(x=pool, y=meanpv, fill=veg)) + 
  #geom_bar(position=position_dodge(), stat="identity") +
  #geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se), width=.2, position=position_dodge(.9)) +
  #labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 0-10 cm")


totsoilmeans010 <- add_row(totsoilmeans010, pool = "totsoilC_g_m2", veg = "sagebrush")
totsoilmeans010$veg <- factor(totsoilmeans010$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

totsoilmeans010$veg <- plyr::revalue(totsoilmeans010$veg, 
                                     c("sagebrush" = "native sagebrush", 
                                       "sagecheat" = "invaded sagebrush"))


colours <- c("native sagebrush" = "seagreen4", 
             "invaded sagebrush" = "yellowgreen",
             "cheatgrass" = "gold")


#Fig. 2d
f2d<-ggplot(totsoilmeans010, aes(x=veg, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    
                position=position_dodge(.9)) +
  labs(x = "vegetation type", 
       y = "total soil carbon (gC m-2): 0-10 cm", 
       fill = "vegetation") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours) +
  scale_x_discrete(breaks = c('native sagebrush', 'invaded sagebrush', 'cheatgrass'), 
                   labels = c('native\nsagebrush', 'invaded\nsagebrush', 'cheatgrass'))

f2d
#ggplot(orgsoilmeans1020, aes(x=pool, y=meanpv, fill=veg)) + 
  #geom_bar(position=position_dodge(), stat="identity") +
  #geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se), width=.2, position=position_dodge(.9)) +
  #labs(x = "vegetation type", y = "organic soil carbon content (gC m-2): 10-20 cm", fill = "vegetation") +
  #theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), legend.text=element_text(size=12), legend.title=element_text(size=12))

#Fig. 2c
f2c<-ggplot(orgzzo, aes(x = bottom_depth, y = meanpvpercm, fill = veg)) +
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  labs(y = "Soil organic C (gC cm-2) per cm thickness", x = "bottom depth sampled (cm)", fill = "vegetation") +
  scale_fill_manual(values = colours) +
  geom_errorbar(aes(ymin=meanpvpercm-se, ymax=meanpvpercm+se),
                width=.2, position=position_dodge(.9))   +    
  theme_classic()+# Width of the error bars 
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) 


orgsoilmeans1020$veg <- factor(orgsoilmeans1020$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
org2 <- rbind(as.data.frame(orgsoilmeans010), as.data.frame(orgsoilmeans1020))
org2$veg <- factor(org2$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))

#rename veg
org2$veg <- plyr::revalue(org2$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))


#plot SOC by depth and veg
#Fig. 2b
f2b<- ggplot(org2, aes(x=depth, y=meanpv, fill=veg)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=meanpv-se, ymax=meanpv+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "depth (cm)", y = "soil organic carbon content (gC m-2)", fill = "vegetation") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours)



rawmeans$geometry <- NULL
rawmeans <- add_row(rawmeans, pool = "litterC_g_m2", veg = "sagecheat")
rawmeans$veg <- factor(rawmeans$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
rawmeans$pool2 <- ifelse(rawmeans$pool == "AGBC_g_m2", "AGB", ifelse(rawmeans$pool == "BGBC_g_m2", "BGB", "litter"))

rawmeans
rawmeans$veg <- plyr::revalue(rawmeans$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")

# For Fig. 2a
f2a<- ggplot(rawmeans, aes(x = pool2, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  labs(x = "carbon pool", y = "carbon content (gC m-2)", fill = "vegetation") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) +
  scale_fill_manual(values = colours)

# fig 2 all together============================================================

ggarrange(f2a, f2b, f2c, f2d, nrow = 2, ncol=2, common.legend = TRUE, 
          labels ="auto",label.x =0.9) +
  ggsave("figure_2.png", width = 8, height = 8) +
  ggsave("figure_2.pdf", width = 8, height = 8)

#analysis with fire=============================================================

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


#Fig. 4 ========================================================================
f4<- ggplot(data = recentburn, aes(x = timesincefire, y = log(pool_value+1), color = veg)) +
  geom_point() + 
  facet_wrap(~pool2, scales = "free_y") +
  xlab("Time since fire (years)") +
  ylab("Carbon content (gC m-2)") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        legend.text=element_text(size=12), 
        legend.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) +
  geom_smooth(method = "lm", se=TRUE, show.legend = F) + 
  theme(legend.position=c(1,0),
        legend.justification = c(1,0)) +
  scale_color_manual(values = colours,
                     name = "Vegetation Type") +
  xlim(0,20) +
  ggsave("figure_4.png", width = 10, height = 6)





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





