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
#p = 1.07e-05

fit2 <- aov(pool_value ~ veg, data = orgsoil1020)
summary(fit2)
#p = 0.14

fit3 <- aov(pool_value ~ veg, data = totsoil010)
summary(fit3)
#p = 0.451

fit4 <- aov(pool_value ~ veg, data = litterC2)
summary(fit4)
#p = 5.34e-15

fit5 <- aov(pool_value ~ veg, data = AGBC2)
summary(fit5)
#p = 0.0002

fit6 <- aov(pool_value ~ veg, data = BGBC2)
summary(fit6)
#p = 2.75e-09


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
#p = 6.45e-11; so not normal

test.lm2 = lm(pool_value ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2))
#p = 9.246e-10; so not normal

test.lm3 = lm(pool_value ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3))
#p = 5.802e-12; so not normal

test.lm4 = lm(pool_value ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4))
#p = 1.462e-11; so not normal

test.lm5 = lm(pool_value ~ veg, data = AGBC2)
shapiro.test(residuals(test.lm5))
#p < 2.2e-16; so not normal

test.lm6 = lm(pool_value ~ veg, data = BGBC2)
shapiro.test(residuals(test.lm6))
#p < 2.2e-16; so not normal


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
#p= 0.16, normal

test.lm2b <- lm(sqrt(pool_value) ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2b))
#still not normal

test.lm3a <- lm(log(pool_value) ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3a))
#p=0.99, normal

###
#why is this here?
fit3b <- aov(log(pool_value) ~ veg, data = totsoil010)
summary(fit3b)
#p = 0.257; compare to non-parametric below
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
#p=5.452e-08

kruskal.test(pool_value ~ veg, data = orgsoil1020)
#p=0.3021

kruskal.test(pool_value ~ veg, data = totsoil010)
#p=0.4431

kruskal.test(pool_value ~ veg, data = litterC2)
#p<2.2e-16

kruskal.test(pool_value ~ veg, data = BGBC2)
#p=1.843e-12

kruskal.test(pool_value ~ veg, data = AGBC2)
#p=2.563e-10


#Dunn test for multiple comparisons
PT = dunnTest(pool_value ~ veg, data = orgsoil010, method = "none")    
PT
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  5.6052772 2.079219e-08 2.079219e-08
#2 cheatgrass - sagecheat  3.4427176 5.759004e-04 5.759004e-04
#3  sagebrush - sagecheat -0.6843736 4.937393e-01 4.937393e-01

PT2 = dunnTest(pool_value ~ veg, data = orgsoil1020, method = "none")    
PT2
#              Comparison         Z   P.unadj     P.adj
#1 cheatgrass - sagebrush 0.6116447 0.5407729 0.5407729
#2 cheatgrass - sagecheat 1.2397524 0.2150670 0.2150670
#3  sagebrush - sagecheat 1.1751932 0.2399174 0.2399174

PT3 = dunnTest(pool_value ~ veg, data = totsoil010, method = "none")    
PT3
#             Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagecheat -0.7670266 0.4430657 0.4430657
###this should have sagebrush...where is sagebrush?
#no sagebrush from 0-10 cm; other depth increments only


PT4 = dunnTest(pool_value ~ veg, data = litterC2, method = "none")    
PT4
#              Comparison        Z      P.unadj        P.adj
#1 cheatgrass - sagebrush 8.271197 1.326212e-16 1.326212e-16


PT5 = dunnTest(pool_value ~ veg, data = BGBC2, method = "none")    
PT5
#              Comparison         Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -7.156016 8.305573e-13 8.305573e-13
#2 cheatgrass - sagecheat -2.569059 1.019751e-02 1.019751e-02
#3  sagebrush - sagecheat  2.925096 3.443497e-03 3.443497e-03


PT6 = dunnTest(pool_value ~ veg, data = AGBC2, method = "none")    
PT6
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -6.1700722 6.825882e-10 6.825882e-10
#2 cheatgrass - sagecheat  0.8533113 3.934867e-01 3.934867e-01
#3  sagebrush - sagecheat  4.9621569 6.971466e-07 6.971466e-07

