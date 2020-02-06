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


deep1org <- orgsoilC2 %>%
  filter(bottomdepth_cm > 20 & bottomdepth_cm <= 40) %>%
  mutate(Cpercm = pool_value/thick)

deep2org <- orgsoilC2 %>%
  filter(bottomdepth_cm > 40) %>%
  mutate(Cpercm = pool_value/thick)

###
#run ANOVAs
fit <- aov(pool_value ~ veg, data = orgsoil010)
summary(fit)
#p = 3.01e-05

fit2 <- aov(pool_value ~ veg, data = orgsoil1020)
summary(fit2)
#p = 0.239

fit2b <- aov(Cpercm ~ veg, data = deep1org)
summary(fit2b)
#p = 0.447

fit2c <- aov(Cpercm ~ veg, data = deep2org)
summary(fit2c)
#p = 0.0043

fit3 <- aov(pool_value ~ veg, data = totsoil010)
summary(fit3)
#p = 0.345

fit4 <- aov(pool_value ~ veg, data = litterC2)
summary(fit4)
#p = 9.38e-05

fit5 <- aov(pool_value ~ veg, data = AGBC2)
summary(fit5)
#p = 0.718

fit6 <- aov(pool_value ~ veg, data = BGBC2)
summary(fit6)
#p = 1.12e-05


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
#p = 2.374e-10; so not normal

test.lm2 = lm(pool_value ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2))
#p = 3.12e-10; so not normal

test.lm2b = lm(Cpercm ~ veg, data = deep1org)
shapiro.test(residuals(test.lm2b))
#p = 0.0119; so not normal

test.lm2c = lm(Cpercm ~ veg, data = deep2org)
shapiro.test(residuals(test.lm2c))
#p < 2.2e-16; so not normal

test.lm3 = lm(pool_value ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3))
#p = 6.104e-12; so not normal

test.lm4 = lm(pool_value ~ veg, data = litterC2)
shapiro.test(residuals(test.lm4))
#p = 0.01801; so not normal

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
#p= 0.075; just barely normal

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
#p=2.957e-07; sig difference

kruskal.test(pool_value ~ veg, data = orgsoil1020)
#p=0.412

kruskal.test(Cpercm ~ veg, data = deep1org)
#p=0.5017

kruskal.test(Cpercm ~ veg, data = deep2org)
#p=1.826e-10; sig dfference

kruskal.test(pool_value ~ veg, data = totsoil010)
#p=0.3772

kruskal.test(pool_value ~ veg, data = litterC2)
#p=3.034e-05; sig difference

kruskal.test(pool_value ~ veg, data = BGBC2)
#p=1.169e-07; sig difference

kruskal.test(pool_value ~ veg, data = AGBC2)
#p=0.2735


#Dunn test for multiple comparisons
PT = dunnTest(pool_value ~ veg, data = orgsoil010, method = "none")    
PT
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  5.2447316 1.565100e-07 1.565100e-07
#2 cheatgrass - sagecheat  3.4739144 5.129247e-04 5.129247e-04
#3  sagebrush - sagecheat -0.3883157 6.977824e-01 6.977824e-01

PT2 = dunnTest(pool_value ~ veg, data = orgsoil1020, method = "none")    
PT2
#              Comparison         Z   P.unadj     P.adj
#1 cheatgrass - sagebrush 0.2357685 0.8136123 0.8136123
#2 cheatgrass - sagecheat 0.8588489 0.3904239 0.3904239
#3  sagebrush - sagecheat 1.1812683 0.2374961 0.2374961

PT2deep = dunnTest(Cpercm ~ veg, data = deep2org, method = "none")    
PT2deep
#             Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  5.8128758 6.140864e-09 6.140864e-09
#2 cheatgrass - sagecheat  0.3742805 7.081956e-01 7.081956e-01
#3  sagebrush - sagecheat -5.2971012 1.176555e-07 1.176555e-07

PT3 = dunnTest(pool_value ~ veg, data = totsoil010, method = "none")    
PT3
#             Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagecheat -0.8830407 0.3772143 0.3772143



PT4 = dunnTest(pool_value ~ veg, data = litterC2, method = "none")    
PT4
#              Comparison       Z      P.unadj        P.adj
#1 cheatgrass - sagebrush 4.17093 3.033592e-05 3.033592e-05


PT5 = dunnTest(pool_value ~ veg, data = BGBC2, method = "none")    
PT5
#              Comparison         Z      P.unadj        P.adj
#1 cheatgrass - sagebrush -4.825491 1.396585e-06 1.396585e-06
#2 cheatgrass - sagecheat -1.090709 2.754008e-01 2.754008e-01
#3  sagebrush - sagecheat  3.698128 2.171951e-04 2.171951e-04


PT6 = dunnTest(pool_value ~ veg, data = AGBC2, method = "none")    
PT6
#              Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagebrush -0.3079024 0.7581566 0.7581566
#2 cheatgrass - sagecheat  1.4680008 0.1421040 0.1421040
#3  sagebrush - sagecheat  1.4807148 0.1386826 0.1386826

