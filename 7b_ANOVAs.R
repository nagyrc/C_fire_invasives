#ANOVAs to explain C storage
#Dr. R. Chelsea Nagy
#created March 19, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "FSA")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#raw plus simulated raw data
joiny2 <- as.data.frame(read_csv("joiny2.csv"))
head(joiny2)

joiny2 <- joiny2 %>%
  filter(veg != "salt_desert")
#2247 to 2123 obs.

summary(AGBC2$pool_value)

#subset by pool
AGBC2 <- subset.data.frame(joiny2, pool == "AGBC_g_m2")
BGBC2 <- subset.data.frame(joiny2, pool == "BGBC_g_m2")
litterC2 <- subset.data.frame(joiny2, pool == "litterC_g_m2")
orgsoilC2 <- subset.data.frame(joiny2, pool == "orgsoilC_g_m2")
totsoilC2 <- subset.data.frame(joiny2, pool == "totsoilC_g_m2")

unique(totsoilC2$veg)
#has sagebrush

unique(totsoilC2$bottomdepth_cm)
#5, 10, 100, 15, 20
#160 of 404 observations are 0-10 cm

#subset soils to appropriate depths
orgsoil010 <- orgsoilC2 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0) 
orgsoil1020 <- orgsoilC2 %>%
  filter(bottomdepth_cm == 20, topdepth_cm == 10)
totsoil010 <- totsoilC2 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0)

unique(totsoil010$veg)
#no sagebrush

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
#p = 0.00274

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
#p=0.4231

kruskal.test(pool_value ~ veg, data = litterC2)
#p=0.5052

kruskal.test(pool_value ~ veg, data = AGBC2)
#p=2.144e-07

kruskal.test(pool_value ~ veg, data = BGBC2)
#p<4.034e-10


#Dunn test for multiple comparisons
PT = dunnTest(pool_value ~ veg, data = orgsoil010, method = "none")    
PT
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  5.9285168 3.056832e-09 3.056832e-09
#2 cheatgrass - sagecheat  4.2694903 1.959202e-05 1.959202e-05
#3  sagebrush - sagecheat -0.3678727 7.129682e-01 7.129682e-01

PT2 = dunnTest(pool_value ~ veg, data = orgsoil1020, method = "none")    
PT2
#              Comparison        Z    P.unadj      P.adj
#1 cheatgrass - sagebrush 1.697932 0.08952060 0.08952060
#2 cheatgrass - sagecheat 2.206175 0.02737176 0.02737176
#3  sagebrush - sagecheat 1.198108 0.23087490 0.23087490

PT3 = dunnTest(pool_value ~ veg, data = totsoil010, method = "none")    
PT3
#             Comparison          Z   P.unadj     P.adj
# cheatgrass - sagecheat -0.8010422 0.4231072 0.4231072
###this should have sagebrush...where is sagebrush?
#no sagebrush from 0-10 cm; other depth increments only


PT4 = dunnTest(pool_value ~ veg, data = litterC2, method = "none")    
PT4
#                Comparison          Z   P.unadj     P.adj
#    cheatgrass - sagebrush -0.6662711 0.5052378 0.5052378

PT5 = dunnTest(pool_value ~ veg, data = AGBC2, method = "none")    
PT5
#             Comparison         Z      P.unadj        P.adj
# cheatgrass - sagebrush  4.941802 7.740378e-07 7.740378e-07
# cheatgrass - sagecheat -1.433650 1.516721e-01 1.516721e-01
#  sagebrush - sagecheat -3.514443 4.406770e-04 4.406770e-04

PT6 = dunnTest(pool_value ~ veg, data = BGBC2, method = "none")    
PT6
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -4.4266841 9.569273e-06 9.569273e-06
#2 cheatgrass - sagecheat  0.1880842 8.508106e-01 8.508106e-01
#3  sagebrush - sagecheat  5.6629182 1.488201e-08 1.488201e-08

