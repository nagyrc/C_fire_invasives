#Old figure scripts
#Dr. R. Chelsea Nagy
#created September 16, 2020

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





#analysis with fire=============================================================

unique(siwf$yr_samp)
unique(siwf$masterlyb)

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

#ggplot(data = siwf) +
#geom_point(aes(x = timesincefire, y = pool_value)) +
#facet_wrap(~pool)

recentburn <- siwf %>%
  filter(timesincefire < 20) %>%
  filter(veg != 'salt_desert')
#only 695 observations

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