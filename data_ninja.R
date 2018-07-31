#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(doBy)

setwd("data/")

###
#in this table I'm bringing in here, we manually entered article ID and study ID
#data ninja-ing for attribute table
studyid <- as.data.frame(read_csv("alldatall_bystudyid.csv"))
head(studyid)
artstud <- unique(studyid[c("Article_ID", "Study_ID")])

write.csv(artstud, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/articlestudyid.csv")
is.numeric(studyid$topdepth_cm)
is.numeric(studyid$bottomdepth_cm)

#recalculate thickness; there were some issues with this when we looked at alldatall.csv
studyid$thick <- studyid$bottomdepth_cm - studyid$topdepth_cm
#this fixed it


att1 <- unique(studyid[c("Article_ID", "Study_ID", "veg", "topdepth_cm", "bottomdepth_cm", "BD_estimated","yr_samp", "lat","long")])
write.csv(att1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes1.csv")


#att2 <- unique(studyid[c("Article_ID", "Study_ID", "soilC_g_m2", "BGBC_g_m2", "litterC_g_m2", "AGBC_g_m2")])
#head(att2)

sum71 <- summaryBy(litterC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum72 <- summaryBy(soilC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum73 <- summaryBy(BGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum74 <- summaryBy(AGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)

combo1 <- left_join(sum71, sum72, by = c("Article_ID","Study_ID"))
combo2 <- left_join(combo1, sum73, by = c("Article_ID","Study_ID"))
combo3 <- left_join(combo2, sum74, by = c("Article_ID","Study_ID"))

head(combo3)
tail(combo3)

#replace NAs with negative number
combo3[is.na(combo3)] <- -2

combo3$Carbon_pool <- ifelse(combo3$soilC_g_m2.mean > 0, "soil",
                              ifelse(combo3$litterC_g_m2.mean > 0, "litter",
                                     ifelse(combo3$AGBC_g_m2.mean > 0, "AGB", "BGB")))


write.csv(combo3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes2.csv")
