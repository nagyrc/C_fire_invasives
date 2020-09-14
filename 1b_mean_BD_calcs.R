#Script for data exploration for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 26, 2018; last updated on December 11, 2018 with all data to date

library(tidyverse)

setwd("data/")

alldata <- as.data.frame(read_csv("alldata.csv"))

#find mean BD from studies that have BD data
#0-10 cm
sub1 <- alldata[ which(alldata$topdepth_cm == 0 & alldata$bottomdepth_cm == 10 & alldata$BD_estimated == "no"),]

summary(sub1$BD_g_cm3)
#mean = 1.422; based on 90 observations across studies

#10-20 cm
sub2 <- alldata[ which(alldata$topdepth_cm == 10 & alldata$bottomdepth_cm == 20 & alldata$BD_estimated == "no"),]

summary(sub2$BD_g_cm3)
#mean = 1.35; based on 14 observations 

#0-20 cm mean; none for this exact interval so use all data from above two depths
(1.422 + 1.35)/2
#mean = 1.386; based on 103 observations across studies

#bottom depth = 60 cm
sub3 <- alldata[ which(alldata$bottomdepth_cm == 60 & alldata$BD_estimated == "no"),]

summary(sub3$BD_g_cm3)
#mean = 1.562; based on 13 observations


