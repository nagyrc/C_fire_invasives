#Linear models to explain C storage
#Dr. R. Chelsea Nagy
#created March 15, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "lme4", "lmerTest")
lapply(x, library, character.only = TRUE, verbose = FALSE)

citation("lme4")
citation("lmerTest")

setwd("data/")

siwf <- as.data.frame(read_csv("siwf.csv"))

siwf3 <- siwf %>%
  filter(veg != 'salt_desert')

#create categorical variable of years since fire
#recent = < 5 years since fire
#mid = 5 - 20 years since fire
#old = > 20 years since fire
siwf3$firecat <- ifelse(siwf3$timesincefire < 5, "recent", ifelse(siwf3$timesincefire >= 5 & siwf3$timesincefire < 20, "mid", ifelse(siwf3$timesincefire >= 20, "old", NA)))


#create categorical variable of depth
#shallow = bottomdepth_cm < 20
#mid = bottomdepth_cm == 20
#deep = bottomdepth_cm > 20
siwf3$depthcat <- ifelse(siwf3$bottomdepth_cm <= 10, "shallow", ifelse(siwf3$bottomdepth_cm <= 20, "mid", ifelse(siwf3$bottomdepth_cm > 20, "deep", NA)))


#try with subsets for each pool
AGBC3 <- subset.data.frame(siwf3, pool == "AGBC_g_m2")
BGBC3 <- subset.data.frame(siwf3, pool == "BGBC_g_m2")
litterC3 <- subset.data.frame(siwf3, pool == "litterC_g_m2")
orgsoilC3 <- subset.data.frame(siwf3, pool == "orgsoilC_g_m2")
totsoilC3 <- subset.data.frame(siwf3, pool == "totsoilC_g_m2")


#for Table 2a
lm1 <- lm(pool_value ~ timesincefire + veg, data = AGBC3)
summary(lm1)
#veg is significant and so is timesincefire; overall p-value = 6.521e-07

lm2 <- lm(pool_value ~ timesincefire + veg, data = BGBC3)
summary(lm2)
#veg is significant in one case; overall p-value = 0.05047

lm3 <- lm(pool_value ~ timesincefire + veg, data = litterC3)
summary(lm3)
#veg and timesincefire are sig; overall p-value = 1.264e-05

lm4 <- lm(pool_value ~ timesincefire + veg, data = orgsoilC3)
summary(lm4)
#veg is sig and so is timesincefire; overall p-value = 0.0005616

lm5 <- lm(pool_value ~ timesincefire + veg, data = totsoilC3)
summary(lm5)
#only intercept; overall p-value = 0.3753




#########################
#linear mixed effects models

unique(AGBC3$firecat)
#mid, old, recent
unique(BGBC3$firecat)
#mid, old
unique(litterC3$firecat)
#mid, old
unique(orgsoilC3$firecat)
#old, mid, recent
unique(totsoilC3$firecat)
#mid, old, recent


unique(orgsoilC3$depthcat)
#shallow, mid, deep
unique(totsoilC3$depthcat)
#shallow, deep, mid




#AGBC
head(AGBC3)

final_mods <- list()

#AGB ===========================================================================
#linear mixed model with veg as fixed effect and Article_ID as random effect
AGBC.model = lmer(log(pool_value+1) ~ veg + (1|Article_ID), 
                  data=AGBC3, REML = FALSE)
summary(AGBC.model)
plot(AGBC.model)
#veg (cheatgrass) is sig. (different from sagecheat)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
AGBC.model2a = lmer(log(pool_value+1) ~ veg * timesincefire + (1|Article_ID),
                    data=AGBC3, REML = FALSE)
summary(AGBC.model2a)
plot(AGBC.model2a)
#veg (cheatgrass) is sig. (different from sagecheat)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
AGBC.model2b = lmer(log(pool_value+1) ~ veg * firecat + (1|Article_ID), 
                    data=AGBC3, REML = FALSE)
summary(AGBC.model2b)
plot(AGBC.model2b)
#veg (cheatgrass) is sig. (different from sagecheat)
BIC(AGBC.model,AGBC.model2a, AGBC.model2b)
anova(AGBC.model,AGBC.model2a, AGBC.model2b, test = "LR")


final_mods$ABG <- lmer(log(pool_value+1) ~ veg * firecat + (1|Article_ID), 
                   data=AGBC3, REML = TRUE)
summary(AGBC_final)

ggplot(AGBC3, aes(y = log(pool_value+1), x= firecat, fill=veg)) +
  geom_boxplot()


#BGBC===========================================================================
#linear mixed model with veg as fixed effect and Article_ID as random effect
BGBC.model = lmer(log(pool_value+1) ~ veg + (1|Article_ID), 
                  data=BGBC3, REML = FALSE)
summary(BGBC.model)
plot(BGBC.model)
#nothing is sig

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
BGBC.model2a = lmer(log(pool_value+1) ~ veg + timesincefire + (1|Article_ID), 
                    data=BGBC3, REML = FALSE)
summary(BGBC.model2a)
plot(BGBC.model2a)
#timesincefire is sig

BGBC.model2c = lmer(log(pool_value+1) ~ veg * timesincefire + (1|Article_ID), 
                    data=BGBC3, REML = FALSE)
summary(BGBC.model2c)
plot(BGBC.model2c)

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
BGBC.model2b = lmer(log(pool_value+1) ~ veg + firecat + (1|Article_ID), 
                    data=BGBC3, REML = FALSE)
summary(BGBC.model2b)
plot(BGBC.model2b)

BGBC.model2d = lmer(log(pool_value+1) ~ veg * firecat + (1|Article_ID), 
                    data=BGBC3, REML = FALSE)
summary(BGBC.model2d)
plot(BGBC.model2d)
#fire category is marginally sig. (old) vs. mid

BIC(BGBC.model,BGBC.model2a,BGBC.model2b,BGBC.model2c,BGBC.model2d)
anova(BGBC.model,BGBC.model2a,BGBC.model2b,BGBC.model2c,BGBC.model2d)

final_mods$BGB = lmer(log(pool_value+1) ~ veg + timesincefire + (1|Article_ID), 
                    data=BGBC3, REML = TRUE)
summary(BGBC_final)

#litterC =======================================================================
#linear mixed model with veg as fixed effect and Article_ID as random effect
litterC.model = lmer(pool_value ~ veg + (1|Article_ID), 
                     data=litterC3, REML = FALSE)
summary(litterC.model)
plot(litterC.model)
#nothing sig.

litterC.model1 = lmer(pool_value ~ timesincefire + (1|Article_ID),
                      data=litterC3, REML=FALSE)
summary(litterC.model1)

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
litterC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), 
                       data=litterC3, REML = FALSE)
summary(litterC.model2a)

litterC.model2c = lmer(pool_value ~ veg * timesincefire + (1|Article_ID),
                       data=litterC3, REML = FALSE)
summary(litterC.model2c)
#timesincefire is sig

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
litterC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID),
                       data=litterC3, REML = FALSE)
summary(litterC.model2b)
#firecatold is sig. (old is different from mid)

litterC.model2d = lmer(pool_value ~ veg * firecat + (1|Article_ID), 
                       data=litterC3, REML = FALSE)
summary(litterC.model2d)
AIC(litterC.model,litterC.model1,litterC.model2a,litterC.model2b,litterC.model2c,litterC.model2d)
BIC(litterC.model,litterC.model1,litterC.model2a,litterC.model2b,litterC.model2c,litterC.model2d)
anova(litterC.model,litterC.model1,litterC.model2a,litterC.model2b,litterC.model2c,litterC.model2d)
anova(litterC.model1,litterC.model2a)

final_mods$litter = lmer(pool_value ~ timesincefire + (1|Article_ID), 
                       data=litterC3, REML = TRUE)
summary(litterC_final)

ggplot(litterC3, aes(x=timesincefire, y=pool_value, color = Article_ID)) +
  geom_line(aes(y=predict(litterC_final)))

#org soil=======================================================================
#linear mixed model with veg as fixed effect and Article_ID as random effect
orgsoilC.model = lmer(log(pool_value) ~ veg + (1|Article_ID), 
                      data=orgsoilC3, REML=FALSE)
summary(orgsoilC.model)
#nothing sig

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
orgsoilC.model2a = lmer(log(pool_value) ~ veg + timesincefire + (1|Article_ID),
                        data=orgsoilC3, REML=FALSE)
summary(orgsoilC.model2a)
#timesincefire is sig 

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
orgsoilC.model2b = lmer(log(pool_value) ~ veg + firecat + (1|Article_ID), 
                        data=orgsoilC3, REML=FALSE)
summary(orgsoilC.model2b)
#firecatrecent is sig (different from mid); sagecheat is marginally sig (different from sagebrush)

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2c = lmer(log(pool_value) ~ veg + firecat + depthcat + (1|Article_ID), 
                        data=orgsoilC3, REML=FALSE)
summary(orgsoilC.model2c)
#firecatrecent is sig (different from mid); depthcatmid is sig (different from deep)

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
orgsoilC.model2d = lmer(log(pool_value) ~ veg + timesincefire + depthcat + (1|Article_ID),
                        data=orgsoilC3, REML=FALSE)
summary(orgsoilC.model2d)
#timesincefire is sig; depthcatmid is sig (different from deep)
AIC(orgsoilC.model,orgsoilC.model2a,orgsoilC.model2b,orgsoilC.model2c,orgsoilC.model2d)
BIC(orgsoilC.model,orgsoilC.model2a,orgsoilC.model2b,orgsoilC.model2c,orgsoilC.model2d)
anova(orgsoilC.model,orgsoilC.model2a,orgsoilC.model2b,orgsoilC.model2c,orgsoilC.model2d)

final_mods$org_s <- lmer(log(pool_value) ~ veg + firecat + depthcat + (1|Article_ID), 
                        data=orgsoilC3, REML=TRUE)
summary(orgsoilC_final)

#tot soil=======================================================================
#linear mixed model with veg as fixed effect and Article_ID as random effect
totsoilC.model = lmer(pool_value ~ veg + (1|Article_ID),
                      data=totsoilC3, REML=FALSE)
summary(totsoilC.model)
#nothing sig

#linear mixed model with veg and timesincefire as fixed effect and Article_ID as random effect
totsoilC.model2a = lmer(pool_value ~ veg + timesincefire + (1|Article_ID), 
                        data=totsoilC3, REML=FALSE)
summary(totsoilC.model2a)
#nothing sig

#linear mixed model with veg and firecat as fixed effect and Article_ID as random effect
totsoilC.model2b = lmer(pool_value ~ veg + firecat + (1|Article_ID), 
                        data=totsoilC3, REML=FALSE)
summary(totsoilC.model2b)
#nothing sig

#linear mixed model with veg, firecat, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2c = lmer(pool_value ~ veg + firecat + depthcat + (1|Article_ID), 
                        data=totsoilC3, REML=FALSE)
summary(totsoilC.model2c)
#depthcatmid is sig (different from deep); depthcatshallow is sig (different from deep)

#linear mixed model with veg, timesincefire, and depthcat as fixed effect and Article_ID as random effect
totsoilC.model2d = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), 
                        data=totsoilC3, REML=FALSE)
summary(totsoilC.model2d)

totsoilC.model2e = lmer(pool_value ~ depthcat + (1|Article_ID), 
                        data=totsoilC3, REML=FALSE)
summary(totsoilC.model2e)
#depthcatmid is sig (different from deep)
AIC(totsoilC.model,totsoilC.model2a,totsoilC.model2b,totsoilC.model2c,totsoilC.model2d,totsoilC.model2e)
BIC(totsoilC.model,totsoilC.model2a,totsoilC.model2b,totsoilC.model2c,totsoilC.model2d,totsoilC.model2e)
anova(totsoilC.model,totsoilC.model2a,totsoilC.model2b,totsoilC.model2c,totsoilC.model2d,totsoilC.model2e)
anova(totsoilC.model2e, totsoilC.model2d)

final_mods$tot = lmer(pool_value ~ veg + timesincefire + depthcat + (1|Article_ID), 
                        data=totsoilC3, REML=TRUE)
summary(totsoilC_final)


# table ========================================================================
library(stargazer)
for(i in 1:length(final_mods)) class(final_mods[[i]]) <- "lmerMod"

# figures =====================================


