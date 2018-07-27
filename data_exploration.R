#Script for data exploration for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created June 29, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(doBy)

setwd("data/")

alldata <- as.data.frame(read_csv("alldata.csv"))
head(alldata)

###
#data exploration
summary(alldata$AGBC_g_m2, na.rm = TRUE)
#why is there a zero???
#753 NAs

zeroAGB <- alldata[alldata$AGBC_g_m2 == 0, ]
#758, so 753 NAs and 5 zeros

ord <- alldata[order(alldata$AGBC_g_m2),] 
head(ord)
#the zeros are from Davies et al. 2009 that had zero cheatgrass biomass

summary(alldata$soilC_g_m2)
#86 NAs
#need to check the units! huge variation
#some of this could be due to different depths

#need to remove NAs...tried na.rm, na.omit, na.exclude
hist(alldata$soilC_g_m2, breaks = 20)


###
#funtion to remove NAs
meanfxn <- function(x)base::mean(x, na.rm = TRUE)


#soil carbon
#check to see if soil C is varying as a function of thickness and bottom depth
sum1 <- summaryBy(soilC_g_m2 ~ study, data = alldata, FUN = c(meanfxn))
sum1
sum2 <- summaryBy(bottomdepth_cm ~ study, data = alldata, FUN = max)
sum3 <- summaryBy(thick ~ study, data = alldata, FUN = max)
sum4 <- summaryBy(thick ~ study, data = alldata, FUN = length)

sumjoin <- left_join(sum1, sum2, by = "study")
sumjoin2 <- left_join(sumjoin, sum3, by = "study")
sumjoin3 <- left_join(sumjoin2, sum4, by = "study")
sumjoin3

p1 <- as.data.frame(sumjoin3)
p1
#this is useful, show Bethany and Emily
plot(sumjoin3$soilC_g_m2.meanfxn~sumjoin3$thick.max)
unique(alldata$thick)
ggplot(sumjoin3, aes(x = thick.max, y = soilC_g_m2.meanfxn, color = study)) + geom_point()


#AGB carbon
sum11 <- summaryBy(AGBC_g_m2 ~ study, data = alldata, FUN = meanfxn)



#BGB carbon
sum21 <- summaryBy(BGBC_g_m2 ~ study, data = alldata, FUN = meanfxn)


#litter carbon
sum31 <- summaryBy(litterC_g_m2 ~ study, data = alldata, FUN = meanfxn)

sumjoin4 <- left_join(sum1, sum11, by = "study")
sumjoin5 <- left_join(sumjoin4, sum21, by = "study")
sumjoin6 <- left_join(sumjoin5, sum31, by = "study")
sumjoin6

getwd()
write.csv(sumjoin6, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/meansbystudy.csv")



#try means by veg category
sum41 <- summaryBy(soilC_g_m2 ~ veg, data = alldata, FUN = c(meanfxn))
sum51 <- summaryBy(AGBC_g_m2 ~ veg, data = alldata, FUN = c(meanfxn))
sum61 <- summaryBy(BGBC_g_m2 ~ veg, data = alldata, FUN = c(meanfxn))
sum71 <- summaryBy(litterC_g_m2 ~ veg, data = alldata, FUN = c(meanfxn))

sumjoin7 <- left_join(sum41, sum51, by = "veg")
sumjoin8 <- left_join(sumjoin7, sum61, by = "veg")
sumjoin9 <- left_join(sumjoin8, sum71, by = "veg")
sumjoin9

write.csv(sumjoin9, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/meansbyveg.csv")

ggplot(sumjoin9, aes(x = veg, y = soilC_g_m2.meanfxn)) + geom_point()
ggplot(sumjoin9, aes(x = veg, y = AGBC_g_m2.meanfxn)) + geom_point()


ggplot(alldata, aes(x = veg, y = soilC_g_m2, color = study)) + geom_point()
ggplot(alldata, aes(x = veg, y = AGBC_g_m2, color = study)) + geom_point()

#normalize soil carbon by thickness
ggplot(alldata, aes(x = veg, y = soilC_g_m2/thick, color = study)) + geom_point()

