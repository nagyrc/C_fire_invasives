#ANOVAs to explain C storage
#Dr. R. Chelsea Nagy
#created March 19, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#raw plus simulated raw data
joiny2 <- as.data.frame(read_csv("joiny2.csv"))
head(joiny2)

#subset by pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

#subset soils to appropriate depths
orgsoil010 <- orgsoilC2 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0)
orgsoil1020 <- orgsoilC2 %>%
  filter(bottomdepth_cm == 20, topdepth_cm == 10)
totsoil010 <- totsoilC2 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0)



###
#run ANOVAs
fit <- aov(pool_value ~ veg, data = orgsoil010)
summary(fit)
#p = 7.18e-07

fit2 <- aov(pool_value ~ veg, data = orgsoil1020)
summary(fit2)
#p = 0.14

fit3 <- aov(pool_value ~ veg, data = totsoil010)
summary(fit3)
#p = 3.98e-05

fit4 <- aov(pool_value ~ veg, data = litterC2)
summary(fit4)
#p = 5.6e-12

fit5 <- aov(pool_value ~ veg, data = AGBC2)
summary(fit5)
#p = 0.00723

fit6 <- aov(pool_value ~ veg, data = BGBC2)
summary(fit6)
#p < 2e-16


###
#check ANOVA assumptions
plot(fit)
plot(fit2)
plot(fit3)
plot(fit4)
plot(fit5)
plot(fit6)

#test for normality
test.lm = lm(pool_value ~ veg, data = orgsoil010)
shapiro.test(residuals(test.lm))
#p = 2.588e-12; so not normal

test.lm2 = lm(pool_value ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2))
#p = 2.59e-10; so not normal

test.lm3 = lm(pool_value ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3))
#p = 1.74e-12; so not normal

test.lm4 = lm(pool_value ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4))
#p = 5.512e-13; so not normal

test.lm5 = lm(pool_value ~ veg, data = AGBC2)
shapiro.test(residuals(test.lm5))
#p < 2.2e-16; so not normal

test.lm6 = lm(pool_value ~ veg, data = BGBC2)
shapiro.test(residuals(test.lm6))
#p = 6.515e-08; so not normal


###
#try to transform
test.lm1a <- lm(log(pool_value) ~ veg, data = orgsoil010)
shapiro.test(residuals(test.lm1a))
#still not normal

test.lm1b <- lm(sqrt(pool_value) ~ veg, data = orgsoil010)
shapiro.test(residuals(test.lm1b))
#still not normal

test.lm2a <- lm(log(pool_value) ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2a))
#still not normal

test.lm2b <- lm(sqrt(pool_value) ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2b))
#still not normal

test.lm3a <- lm(log(pool_value) ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3a))
#normal

###
fit3b <- aov(log(pool_value) ~ veg, data = totsoil010)
summary(fit3b)
#p = 1.58e-09; compare to non-parametric below
###

test.lm3b <- lm(sqrt(pool_value) ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3b))
#still not normal

test.lm4a <- lm(log(pool_value) ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4a))
#still not normal

test.lm4b <- lm(sqrt(pool_value) ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4b))
#still not normal

test.lm5a <- lm(log(pool_value) ~ veg, data = AGBC2)
shapiro.test(residuals(test.lm5a))
#still not normal

test.lm5b <- lm(sqrt(pool_value) ~ veg, data = AGBC2)
shapiro.test(residuals(test.lm5b))
#still not normal

test.lm6a <- lm(log(pool_value) ~ veg, data = BGBC2)
shapiro.test(residuals(test.lm6a))
#still not normal

test.lm6b <- lm(sqrt(pool_value) ~ veg, data = BGBC2)
shapiro.test(residuals(test.lm6b))
#still not normal


###
#run non-parametrics
kruskal.test(pool_value ~ veg, data = orgsoil010)
#p=3.05e-09

kruskal.test(pool_value ~ veg, data = orgsoil1020)
#p=0.0619

kruskal.test(pool_value ~ veg, data = totsoil010)
#p=4.29e-08

kruskal.test(pool_value ~ veg, data = litterC2)
#p=2.46e-14

kruskal.test(pool_value ~ veg, data = AGBC2)
#p=5.119e-07

kruskal.test(pool_value ~ veg, data = BGBC2)
#p<2.2e-16