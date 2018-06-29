#Script for data exploration for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created June 29, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)

setwd("data/")

alldata <- as.data.frame(read_csv("alldata.csv"))