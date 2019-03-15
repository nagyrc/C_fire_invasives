#Linear models to explain C storage
#Dr. R. Chelsea Nagy
#created March 15, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

joiny2 <- as.data.frame(read_csv("joiny2.csv"))
head(joiny2)

#try with subsets for each pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

lm1 <- lm(pool_value ~ masterlyb + veg, data = AGBC2)
summary(lm1)
#veg is sig. in one case

lm2 <- lm(pool_value ~ masterlyb + veg, data = BGBC2)
summary(lm2)
#only intercept is significant

#can't run for litter; only 1 veg type
lm3 <- lm(pool_value ~ masterlyb, data = litterC2)
summary(lm3)
#NA

lm4 <- lm(pool_value ~ masterlyb + veg, data = orgsoilC2)
summary(lm4)
#veg is sig.; lyb is not

lm5 <- lm(pool_value ~ masterlyb + veg, data = totsoilC2)
summary(lm5)
#veg and lyb are significant
