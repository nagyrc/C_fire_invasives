#Script for data wrangling for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created May 11, 2018

library(tidyverse)
library(plyr)

setwd("data/")

file_names <- list.files()

Jones_soil <- as.data.frame(read_csv("Jones_soil.csv"))
Jones_burn <- as.data.frame(read_csv("Jones_burn.csv"))
Jones_veg_only <- as.data.frame(read_csv("Jones_veg_only.csv"))
Jones_litter_only <- as.data.frame(read_csv("Jones_litter_only.csv"))

head(Jones_soil)
head(Jones_burn)
head(Jones_veg_only)
head(Jones_litter_only)

#merge veg and litter dataframes
Jones_veg_litter <- merge(Jones_veg_only, Jones_litter_only, 
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"))

head(Jones_veg_litter)

#merge veg_litter and soil dataframes
Jones_veg_litter_soil <- merge(Jones_veg_litter, Jones_soil, 
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"))

head(Jones_veg_litter_soil)
