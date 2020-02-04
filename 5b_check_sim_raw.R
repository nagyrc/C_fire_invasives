#Data summary- checking for influence of simulated raw data
#Dr. R. Chelsea Nagy
#created February 4, 2020

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid or siwf dataframe
studyid <- as.data.frame(read_csv("studyid.csv"))
siwf <- as.data.frame(read_csv("siwf.csv"))

means <- siwf %>%
  filter(pool == "AGBC_g_m2" | pool == "BGBC_g_m2" | pool == "litterC_g_m2") %>%
  group_by(pool, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n)) %>%
  filter(veg != "salt_desert") %>%
  ungroup()

st_geometry(means) = NULL
write.csv(means, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/means.csv")

means <- add_row(means, pool = "litterC_g_m2", veg = "sagecheat")
means$veg <- factor(means$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
means$pool2 <- ifelse(means$pool == "AGBC_g_m2", "AGB", ifelse(means$pool == "BGBC_g_m2", "BGB", "litter"))

means$veg <- plyr::revalue(means$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")


ggplot(means, aes(x = pool2, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  labs(x = "carbon pool", y = "carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) + 
  scale_fill_manual(values = colours)
