#Linear models to explain C storage
#Dr. R. Chelsea Nagy
#created March 15, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

joiny2 <- as.data.frame(read_csv("joiny2.csv"))

joiny2 <- joiny2 %>%
  filter(veg != "salt_desert") %>%
  filter(study != "Cleary et al. 2010")

head(joiny2)

joiny2$timesincefire <- joiny2$yr_samp - joiny2$masterlyb

#try with subsets for each pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

lm1 <- lm(pool_value ~ timesincefire + veg, data = AGBC2)
summary(lm1)
#veg is sig. in one case; timesincefire is not; overall p-value = 2.748e-06

lm2 <- lm(pool_value ~ timesincefire + veg, data = BGBC2)
summary(lm2)
#only intercept is significant; overall p-value = 0.8597

lm3 <- lm(pool_value ~ timesincefire, data = litterC2)
summary(lm3)
#timesincefire is significant (-); overall p-value = 0.010

lm4 <- lm(pool_value ~ timesincefire + veg, data = orgsoilC2)
summary(lm4)
#veg is sig.; timesincefire is not; overall p-value = 0.0087

lm5 <- lm(pool_value ~ timesincefire + veg, data = totsoilC2)
summary(lm5)
#veg is significant; timesincefire is not; overall p-value = 0.03507

