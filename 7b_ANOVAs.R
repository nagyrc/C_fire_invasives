#ANOVAs to explain C storage
#Dr. R. Chelsea Nagy
#created March 19, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "FSA")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#raw data
siwf <- as.data.frame(read_csv("siwf.csv"))

siwf4 <- siwf %>%
  filter(veg != 'salt_desert')

#subset by pool
AGBC3 <- subset.data.frame(siwf4, pool == "AGBC_g_m2")
BGBC3 <- subset.data.frame(siwf4, pool == "BGBC_g_m2")
litterC3 <- subset.data.frame(siwf4, pool == "litterC_g_m2")
orgsoilC3 <- subset.data.frame(siwf4, pool == "orgsoilC_g_m2")
totsoilC3 <- subset.data.frame(siwf4, pool == "totsoilC_g_m2")


unique(totsoilC3$veg)
#has sagebrush

unique(totsoilC3$bottomdepth_cm)

#subset soils to appropriate depths
orgsoil010 <- orgsoilC3 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0) 
orgsoil1020 <- orgsoilC3 %>%
  filter(bottomdepth_cm == 20, topdepth_cm == 10)
totsoil010 <- totsoilC3 %>%
  filter(bottomdepth_cm == 10, topdepth_cm == 0)


unique(totsoil010$veg)
#no sagebrush


deep1org <- orgsoilC3 %>%
  filter(bottomdepth_cm > 20 & bottomdepth_cm <= 40) %>%
  mutate(Cpercm = pool_value/thick)

deep2org <- orgsoilC3 %>%
  filter(bottomdepth_cm > 40) %>%
  mutate(Cpercm = pool_value/thick)

###
#run ANOVAs
fit <- aov(pool_value ~ veg, data = orgsoil010)
summary(fit)
#p = 1.43e-06

fit2 <- aov(pool_value ~ veg, data = orgsoil1020)
summary(fit2)
#p = 0.157

fit2b <- aov(Cpercm ~ veg, data = deep1org)
summary(fit2b)
#p = 0.581

fit2c <- aov(Cpercm ~ veg, data = deep2org)
summary(fit2c)
#p = 0.801

fit3 <- aov(pool_value ~ veg, data = totsoil010)
summary(fit3)
#p = 0.451

fit4 <- aov(pool_value ~ veg, data = litterC3)
summary(fit4)
#p = 0.000165

fit5 <- aov(pool_value ~ veg, data = AGBC3)
summary(fit5)
#p = 0.000205

fit6 <- aov(pool_value ~ veg, data = BGBC3)
summary(fit6)
#p = 0.0268


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
#p = 2.764e-12; so not normal

test.lm2 = lm(pool_value ~ veg, data = orgsoil1020)
shapiro.test(residuals(test.lm2))
#p = 2.236e-10; so not normal

test.lm2b = lm(Cpercm ~ veg, data = deep1org)
shapiro.test(residuals(test.lm2b))
#p = 0.01605; so not normal

test.lm2c = lm(Cpercm ~ veg, data = deep2org)
shapiro.test(residuals(test.lm2c))
#p = 1.069e-09; so not normal

test.lm3 = lm(pool_value ~ veg, data = totsoil010)
shapiro.test(residuals(test.lm3))
#p = 5.802e-12; so not normal

test.lm4 = lm(pool_value ~ veg, data = litterC3)
shapiro.test(residuals(test.lm4))
#p = 0.04043; so not normal

test.lm5 = lm(pool_value ~ veg, data = AGBC3)
shapiro.test(residuals(test.lm5))
#p < 2.2e-16; so not normal

test.lm6 = lm(pool_value ~ veg, data = BGBC3)
shapiro.test(residuals(test.lm6))
#p = 6.941e-09; so not normal


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
#p= 0.05648; just barely normal

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

test.lm4a <- lm(log(pool_value) ~ veg, data = litterC3)
shapiro.test(residuals(test.lm4a))
#still not normal

test.lm4b <- lm(sqrt(pool_value) ~ veg, data = litterC3)
shapiro.test(residuals(test.lm4b))
#still not normal

test.lm5a <- lm(log(pool_value) ~ veg, data = AGBC3)
shapiro.test(residuals(test.lm5a))
#NAs

test.lm5b <- lm(sqrt(pool_value) ~ veg, data = AGBC3)
shapiro.test(residuals(test.lm5b))
#still not normal

test.lm6a <- lm(log(pool_value) ~ veg, data = BGBC3)
shapiro.test(residuals(test.lm6a))
#normal

test.lm6b <- lm(sqrt(pool_value) ~ veg, data = BGBC3)
shapiro.test(residuals(test.lm6b))
#still not normal


###
#run non-parametrics
kruskal.test(pool_value ~ veg, data = orgsoil010)
#p=6.941e-09; sig difference

kruskal.test(pool_value ~ veg, data = orgsoil1020)
#p=0.06407; marginally sig

kruskal.test(Cpercm ~ veg, data = deep1org)
#p=0.7226

kruskal.test(Cpercm ~ veg, data = deep2org)
#p=0.7794

kruskal.test(pool_value ~ veg, data = totsoil010)
#p=0.4431

kruskal.test(pool_value ~ veg, data = litterC3)
#p=7.533e-05; sig difference

kruskal.test(pool_value ~ veg, data = BGBC3)
#p=0.05781; marginally sig difference

kruskal.test(pool_value ~ veg, data = AGBC3)
#p=0.5722


#Dunn test for multiple comparisons
PT = dunnTest(pool_value ~ veg, data = orgsoil010, method = "none")    
PT
#              Comparison          Z      P.unadj        P.adj
#1 cheatgrass - sagebrush  5.8245483 5.726728e-09 5.726728e-09
#2 cheatgrass - sagecheat  4.0908152 4.298595e-05 4.298595e-05
#3  sagebrush - sagecheat -0.4130691 6.795560e-01 6.795560e-01

PT2 = dunnTest(pool_value ~ veg, data = orgsoil1020, method = "none")    
PT2
#              Comparison        Z    P.unadj      P.adj
#1 cheatgrass - sagebrush 1.632824 0.10250596 0.10250596
#2 cheatgrass - sagecheat 2.201460 0.02770346 0.02770346
#3  sagebrush - sagecheat 1.206208 0.22773716 0.22773716

PT2deep = dunnTest(Cpercm ~ veg, data = deep2org, method = "none")    
PT2deep
#               Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagebrush -0.6207312 0.5347765 0.5347765
#2 cheatgrass - sagecheat  0.1414696 0.8874990 0.8874990
#3  sagebrush - sagecheat  0.7049298 0.4808540 0.4808540

PT3 = dunnTest(pool_value ~ veg, data = totsoil010, method = "none")    
PT3
#             Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagecheat -0.7670266 0.4430657 0.4430657



PT4 = dunnTest(pool_value ~ veg, data = litterC3, method = "none")    
PT4
#              Comparison        Z      P.unadj        P.adj
#1 cheatgrass - sagebrush 3.958804 7.532615e-05 7.532615e-05


PT5 = dunnTest(pool_value ~ veg, data = BGBC3, method = "none")    
PT5
#              Comparison          Z    P.unadj      P.adj
#1 cheatgrass - sagebrush -2.3334798 0.01962298 0.01962298
#2 cheatgrass - sagecheat -1.6649249 0.09592774 0.09592774
#3  sagebrush - sagecheat  0.9138251 0.36080874 0.36080874


PT6 = dunnTest(pool_value ~ veg, data = AGBC3, method = "none")    
PT6
#              Comparison          Z   P.unadj     P.adj
#1 cheatgrass - sagebrush  0.9306887 0.3520146 0.3520146
#2 cheatgrass - sagecheat  0.7033014 0.4818679 0.4818679
#3  sagebrush - sagecheat -0.2178996 0.8275073 0.8275073

