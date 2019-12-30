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
#p = 1.67e-07

fit2 <- aov(pool_value ~ veg, data = orgsoil1020)
summary(fit2)
#p = 0.06

fit3 <- aov(pool_value ~ veg, data = totsoil010)
summary(fit3)
#p = 0.451

fit4 <- aov(pool_value ~ veg, data = litterC2)
summary(fit4)
#p = 4.59e-11

fit5 <- aov(pool_value ~ veg, data = AGBC2)
summary(fit5)
#p = 7.76e-08

fit6 <- aov(pool_value ~ veg, data = BGBC2)
summary(fit6)
#p = 1.43e-07


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
#p = 3.982e-11; so not normal

test.lm2 = lm(pool_value ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2))
#p = 2.584e-10; so not normal

test.lm3 = lm(pool_value ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3))
#p = 5.802e-12; so not normal

test.lm4 = lm(pool_value ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4))
#p = 1.508e-11; so not normal

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
#p= 0.0535; just barely normal

test.lm2b <- lm(sqrt(pool_value) ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2b))
#still not normal

test.lm3a <- lm(log(pool_value) ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3a))
#p=0.99, normal

###
#why is this here?
#fit3b <- aov(log(pool_value) ~ veg, data = totsoil010)
#summary(fit3b)
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
#NAs

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
#p=8.817e-10; sig difference

kruskal.test(pool_value ~ veg, data = orgsoil1020)
#p=0.0247; sig difference

kruskal.test(pool_value ~ veg, data = totsoil010)
#p=0.4431

kruskal.test(pool_value ~ veg, data = litterC2)
#p=6.861e-08; sig difference

kruskal.test(pool_value ~ veg, data = BGBC2)
#p=1.194e-09; sig difference

kruskal.test(pool_value ~ veg, data = AGBC2)
#p=8.224e-10; sig difference


#Dunn test for multiple comparisons
PT = dunnTest(pool_value ~ veg, data = orgsoil010, method = "none")    
PT
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  6.2339472 4.548266e-10 4.548266e-10
#2 cheatgrass - sagecheat  3.9208593 8.823376e-05 8.823376e-05
#3  sagebrush - sagecheat -0.6683381 5.039178e-01 5.039178e-01

PT2 = dunnTest(pool_value ~ veg, data = orgsoil1020, method = "none")    
PT2
#              Comparison        Z     P.unadj       P.adj
#1 cheatgrass - sagebrush 1.948743 0.051326160 0.051326160
#2 cheatgrass - sagecheat 2.636474 0.008377254 0.008377254
#3  sagebrush - sagecheat 1.233375 0.217436071 0.217436071

PT3 = dunnTest(pool_value ~ veg, data = totsoil010, method = "none")    
PT3
#             Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagecheat -0.7670266 0.4430657 0.4430657
###this should have sagebrush...where is sagebrush?
#no sagebrush from 0-10 cm; other depth increments only


PT4 = dunnTest(pool_value ~ veg, data = litterC2, method = "none")    
PT4
#              Comparison        Z      P.unadj        P.adj
#1 cheatgrass - sagebrush 5.394782 6.860685e-08 6.860685e-08


PT5 = dunnTest(pool_value ~ veg, data = BGBC2, method = "none")    
PT5
#              Comparison         Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -6.269092 3.631595e-10 3.631595e-10
#2 cheatgrass - sagecheat -2.363445 1.810590e-02 1.810590e-02
#3  sagebrush - sagecheat  2.429147 1.513438e-02 1.513438e-02


PT6 = dunnTest(pool_value ~ veg, data = AGBC2, method = "none")    
PT6
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -6.1321572 8.669536e-10 8.669536e-10
#2 cheatgrass - sagecheat  0.4638587 6.427490e-01 6.427490e-01
#3  sagebrush - sagecheat  4.5765662 4.726702e-06 4.726702e-06

