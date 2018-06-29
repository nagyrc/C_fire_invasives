#Script for data exploration for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created June 29, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(ggplot2)

setwd("data/")

alldata <- as.data.frame(read_csv("alldata.csv"))

summary(alldata$AGBC_g_m2)
#why is there a zero???
#679 NAs

summary(alldata$soilC_g_m2)
#513 NAs
#need to check the units! huge variation

#need to remove NAs...tried na.rm, na.omit, na.exclude
hist(alldata$soilC_g_m2, breaks = 20)

ggplot(data = alldata, aes(x = study, y = soilC_g_m2)) + geom_bar(stat = "identity")

alldata %>% 
  drop_na (soilC_g_m2) %>%
  ggplot(aes(y = soilC_g_m2, x = study)) +
  geom_bar(stat = "identity")
#huge variation, this will tell me what datasets to check for those who have soil data
#check calculations in Mahood1

qplot(alldata$soilC_g_m2, geom = "histogram", color = alldata$study)

ggplot(alldata, aes(x = yr_samp, y = soilC_g_m2, color = study)) + geom_point()

