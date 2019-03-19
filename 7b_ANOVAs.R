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
#                Comparison          Z      P.unadj        P.adj
#1   cheatgrass - sagecheat -0.9058939 3.649920e-01 3.649920e-01
#2 cheatgrass - salt_desert  5.2171695 1.816780e-07 1.816780e-07
#3  sagecheat - salt_desert  5.6620131 1.496074e-08 1.496074e-08

PT4 = dunnTest(pool_value ~ veg, data = litterC2, method = "none")    
PT4
#                Comparison         Z      P.unadj        P.adj
#1   cheatgrass - sagebrush -1.904938 5.678815e-02 5.678815e-02
#2 cheatgrass - salt_desert -7.909452 2.585252e-15 2.585252e-15
#3  sagebrush - salt_desert -3.234228 1.219722e-03 1.219722e-03

PT5 = dunnTest(pool_value ~ veg, data = AGBC2, method = "none")    
PT5
#                Comparison          Z      P.unadj        P.adj
#1   cheatgrass - sagebrush  4.9112079 9.051707e-07 9.051707e-07
#2   cheatgrass - sagecheat -1.4436057 1.488499e-01 1.488499e-01
#3    sagebrush - sagecheat -3.5114786 4.456213e-04 4.456213e-04
#4 cheatgrass - salt_desert -0.7075371 4.792327e-01 4.792327e-01
#5  sagebrush - salt_desert -1.9823557 4.743944e-02 4.743944e-02
#6  sagecheat - salt_desert  0.1575080 8.748445e-01 8.748445e-01

PT6 = dunnTest(pool_value ~ veg, data = BGBC2, method = "none")    
PT6
#                Comparison           Z      P.unadj        P.adj
#1   cheatgrass - sagebrush -3.82391504 1.313492e-04 1.313492e-04
#2   cheatgrass - sagecheat -0.03047993 9.756843e-01 9.756843e-01
#3    sagebrush - sagecheat  4.61954196 3.845881e-06 3.845881e-06
#4 cheatgrass - salt_desert -6.75210435 1.457160e-11 1.457160e-11
#5  sagebrush - salt_desert -4.91588644 8.838162e-07 8.838162e-07
#6  sagecheat - salt_desert -7.94704307 1.910160e-15 1.910160e-15

