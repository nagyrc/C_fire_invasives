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

