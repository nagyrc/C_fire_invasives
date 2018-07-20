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

#data exploration
summary(alldata$AGBC_g_m2, na.rm = TRUE)
#why is there a zero???
#755 NAs

zeroAGB <- alldata[alldata$AGBC_g_m2 == 0, ]
#760, so 755 NAs and 5 zeros

ord <- alldata[order(alldata$AGBC_g_m2),] 
head(ord)
#the zeros are from Davies et al. 2009 that had zero cheatgrass biomass

summary(alldata$soilC_g_m2)
#496 NAs
#need to check the units! huge variation
#some of this could be due to different depths

#need to remove NAs...tried na.rm, na.omit, na.exclude
hist(alldata$soilC_g_m2, breaks = 20)

ggplot(data = alldata, aes(x = study, y = soilC_g_m2)) + geom_bar(stat = "identity")


#this is probably giving weird results because of means vs. raw data
alldata %>% 
  drop_na (soilC_g_m2) %>%
  ggplot(aes(y = soilC_g_m2, x = study)) +
  geom_bar(stat = "identity")
#huge variation, this will tell me what datasets to check for those who have soil data
#check calculations in Mahood1

qplot(alldata$soilC_g_m2, geom = "histogram", color = alldata$study)

ggplot(alldata, aes(x = yr_samp, y = soilC_g_m2, color = study)) + geom_point()

#check to see if C is varying as a function of thickness and bottom depth
sum1 <- summaryBy(soilC_g_m2 ~ study, data = alldata, FUN = mean)
sum2 <- summaryBy(bottomdepth_cm ~ study, data = alldata, FUN = max)
sum2
sum3 <- summaryBy(thick ~ study, data = alldata, FUN = max)

sum4 <- summaryBy(bottomdepth_cm ~ study, data = alldata, FUN = min)
sum4

str(alldata$bottomdepth_cm)
sumjoin <- left_join(sum1, sum2, by = "study")
sumjoin2 <- left_join(sumjoin, sum3, by = "study")
sumjoin2
#check depths on Norton et al. 2004: 99 cm?
#check depths on Rau et al. 2011; Goergen et al. 2011: no bottom depth listed