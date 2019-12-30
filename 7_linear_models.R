#Linear models to explain C storage
#Dr. R. Chelsea Nagy
#created March 15, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "lme4", "lmerTest")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

joiny2 <- as.data.frame(read_csv("joiny2.csv"))

#joiny2 <- joiny2 %>%
  #filter(veg != "salt_desert") 

unique(joiny2$veg)

head(joiny2)

#joiny2$timesincefire <- joiny2$yr_samp - joiny2$masterlyb

#create categorical variable of years since fire
#recent = < 5 years since fire
#mid = 5 - 20 years since fire
#old = > 20 years since fire
joiny2$firecat <- ifelse(joiny2$timesincefire < 5, "recent", ifelse(joiny2$timesincefire >= 5 & joiny2$timesincefire < 20, "mid", ifelse(joiny2$timesincefire >= 20, "old", NA)))


#create categorical variable of depth
#shallow = bottomdepth_cm < 20
#mid = bottomdepth_cm == 20
#deep = bottomdepth_cm > 20
joiny2$depthcat <- ifelse(joiny2$bottomdepth_cm < 20, "shallow", ifelse(joiny2$bottomdepth_cm == 20, "mid", ifelse(joiny2$bottomdepth_cm > 20, "deep", NA)))


#try with subsets for each pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

lm1 <- lm(pool_value ~ timesincefire + veg, data = AGBC2)
summary(lm1)
#veg is significant in one case and so is timesincefire; overall p-value < 2.2e-16

lm2 <- lm(pool_value ~ timesincefire + veg, data = BGBC2)
summary(lm2)
#veg is sig and so is timesincefire; overall p-value = 6.8583-07

lm3 <- lm(pool_value ~ timesincefire + veg, data = litterC2)
summary(lm3)
#veg and timesincefire are sig; overall p-value < 2.2e-16

lm4 <- lm(pool_value ~ timesincefire + veg, data = orgsoilC2)
summary(lm4)
#veg is sig and so is timesincefire; overall p-value = 0.000604

lm5 <- lm(pool_value ~ timesincefire + veg, data = totsoilC2)
summary(lm5)
#only intercept; overall p-value = 0.1941




#########################
#linear mixed effects models


#AGBC
head(AGBC2)

unique(AGBC2$firecat)
#mid, old, recent, NA

#linear mixed model with veg as fixed effect and Article_ID as random effect
AGBC.model = lmer(pool_value ~ veg + (1|Article_ID), data=AGBC2)
summary(AGBC.model)
#veg (cheatgrass) is sig. (different from sagebrush)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
AGBC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=AGBC2)
summary(AGBC.model2a)
#veg (sagecheat) is sig (different from sagebrush)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
AGBC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=AGBC2)
summary(AGBC.model2b)
#veg (sagecheat) is sig (different from sagebrush)



#BGBC
#linear mixed model with veg as fixed effect and Article_ID as random effect
BGBC.model = lmer(pool_value ~ veg + (1|Article_ID), data=BGBC2)
summary(BGBC.model)
#veg not sig

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
BGBC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=BGBC2)
summary(BGBC.model2a)
#timesincefire is sig

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
BGBC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=BGBC2)
summary(BGBC.model2b)
#fire category is sig. (old) vs. mid




#litterC
#linear mixed model with veg as fixed effect and Article_ID as random effect
litterC.model = lmer(pool_value ~ veg + (1|Article_ID), data=litterC2)
summary(litterC.model)
#nothing sig.

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
litterC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=litterC2)
summary(litterC.model2a)
#timesincefire is sig

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
litterC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=litterC2)
summary(litterC.model2b)
#firecatold is sig. (old is different from mid)



#org soil
#linear mixed model with veg as fixed effect and Article_ID as random effect
orgsoilC.model = lmer(pool_value ~ veg + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model)
#sagecheat is sig., different from sagebrush

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
orgsoilC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2a)
#timesincefire is sig and so is sagecheat (differnet from sagebrush)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
orgsoilC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2b)
#firecatrecent is sig (different from mid); sagecheat is sig (different from sagebrush)

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2c = lmer(pool_value ~ veg + firecat + depthcat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2c)
#firecatrecent is sig (different from mid); depthcatmid is sig (different from deep)

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2d = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2d)
#timesincefire is sig; depthcatmid is sig (different from deep); sagecheat is different from sagebrush


#tot soil
#linear mixed model with veg as fixed effect and Article_ID as random effect
totsoilC.model = lmer(pool_value ~ veg + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model)
#veg(sagecheat and cheatgrass) are different from sagebrush

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
totsoilC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2a)
#nothing sig

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
totsoilC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2b)
#nothing sig

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2c = lmer(pool_value ~ veg + firecat + depthcat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2c)
#depthcatshallow is sig (different from deep)

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2d = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2d)
#depthcatshallow is sig (different from deep)
