#Linear models to explain C storage
#Dr. R. Chelsea Nagy
#created March 15, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "lme4")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

joiny2 <- as.data.frame(read_csv("joiny2.csv"))

joiny2 <- joiny2 %>%
  filter(veg != "salt_desert") %>%
  filter(study != "Cleary et al. 2010")

head(joiny2)

joiny2$timesincefire <- joiny2$yr_samp - joiny2$masterlyb

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
#veg is sig. in one case; timesincefire is not; overall p-value = 0.0001

lm2 <- lm(pool_value ~ timesincefire + veg, data = BGBC2)
summary(lm2)
#only intercept is significant; overall p-value = 0.8597

lm3 <- lm(pool_value ~ timesincefire, data = litterC2)
summary(lm3)
#only intercept; overall p-value = 0.726

lm4 <- lm(pool_value ~ timesincefire + veg, data = orgsoilC2)
summary(lm4)
#veg is sig.; timesincefire is not; overall p-value = 0.01146

lm5 <- lm(pool_value ~ timesincefire + veg, data = totsoilC2)
summary(lm5)
#veg is significant; timesincefire is not; overall p-value = 0.03507




#########################
#linear mixed effects models


#AGBC
head(AGBC2)

unique(AGBC2$firecat)
#only mid and NA

#linear mixed model with veg as fixed effect and Article_ID as random effect
AGBC.model = lmer(pool_value ~ veg + (1|Article_ID), data=AGBC2)
summary(AGBC.model)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
AGBC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=AGBC2)
summary(AGBC.model2a)
#this runs, but gives me this message: boundary (singular) fit: see ?isSingular

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
AGBC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=AGBC2)
summary(AGBC.model2b)
#this gives me an error: Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels
#only mid and NA for AGBC



#BGBC
#linear mixed model with veg as fixed effect and Article_ID as random effect
BGBC.model = lmer(pool_value ~ veg + (1|Article_ID), data=BGBC2)
summary(BGBC.model)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
BGBC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=BGBC2)
summary(BGBC.model2a)
#Error: grouping factors must have > 1 sampled level

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
BGBC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=BGBC2)
summary(BGBC.model2b)
#Error: grouping factors must have > 1 sampled level




#litterC
#linear mixed model with veg as fixed effect and Article_ID as random effect
litterC.model = lmer(pool_value ~ veg + (1|Article_ID), data=litterC2)
summary(litterC.model)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
litterC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=litterC2)
summary(litterC.model2a)
#Error: grouping factors must have > 1 sampled level

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
litterC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=litterC2)
summary(litterC.model2b)
#Error: grouping factors must have > 1 sampled level



#org soil
#linear mixed model with veg as fixed effect and Article_ID as random effect
orgsoilC.model = lmer(pool_value ~ veg + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
orgsoilC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2a)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
orgsoilC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2b)
#Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.00347935 (tol = 0.002, component 1)

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2c = lmer(pool_value ~ veg + firecat + depthcat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2c)

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2d = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), data=orgsoilC2)
summary(orgsoilC.model2d)


#tot soil
#linear mixed model with veg as fixed effect and Article_ID as random effect
totsoilC.model = lmer(pool_value ~ veg + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
totsoilC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2a)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
totsoilC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2b)
#Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge with max|grad| = 0.00347935 (tol = 0.002, component 1)

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2c = lmer(pool_value ~ veg + firecat + depthcat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2c)
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2d = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), data=totsoilC2)
summary(totsoilC.model2d)
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels
