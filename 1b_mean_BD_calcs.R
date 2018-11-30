#Script for data exploration for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 26, 2018

setwd("data/")

alldata <- as.data.frame(read_csv("alldata.csv"))

#find mean BD from studies that have BD data
#0-10 cm
sub1 <- alldata[ which(alldata$topdepth_cm == 0 & alldata$bottomdepth_cm == 10 & alldata$BD_estimated == "no"),]

summary(sub1$BD_g_cm3)
#mean = 1.422; based on 95 observations across studies

#10-20 cm
sub2 <- alldata[ which(alldata$topdepth_cm == 10 & alldata$bottomdepth_cm == 20 & alldata$BD_estimated == "no"),]

summary(sub2$BD_g_cm3)
#mean = 1.35; based on 8 observations from Stark BD data

sub3 <- alldata[ which(alldata$bottomdepth_cm == 0 | alldata$bottomdepth_cm == 20 & alldata$BD_estimated == "no"),]

summary(sub3$BD_g_cm3)
#1.408
