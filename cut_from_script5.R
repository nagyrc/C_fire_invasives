#cut from script #5


########################################
################################
#plotting


#raw data only
head(rawmeans)


rawmeans$geometry <- NULL
rawmeans <- add_row(rawmeans, pool = "litterC_g_m2", veg = "sagecheat")
rawmeans$veg <- factor(rawmeans$veg,levels = c("sagebrush", "sagecheat", "cheatgrass"))
rawmeans$pool2 <- ifelse(rawmeans$pool == "AGBC_g_m2", "AGB", ifelse(rawmeans$pool == "BGBC_g_m2", "BGB", "litter"))

rawmeans$veg <- plyr::revalue(rawmeans$veg, c("sagebrush" = "native sagebrush", "sagecheat" = "invaded sagebrush"))

colours <- c("native sagebrush" = "seagreen4", "invaded sagebrush" = "yellowgreen", "cheatgrass" = "gold")

#Fig 3a
ggplot(rawmeans, aes(x = pool2, y = meanpv, fill = veg)) + 
  geom_bar(position = position_dodge(preserve = "single"), stat = "identity") +
  geom_errorbar(aes(ymin = meanpv - se, ymax = meanpv + se),
                width = .2, position = position_dodge(0.9)) + 
  labs(x = "carbon pool", y = "carbon content (gC m-2)", fill = "vegetation") +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14), legend.text=element_text(size=14), legend.title=element_text(size=14)) + 
  scale_fill_manual(values = colours)

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




