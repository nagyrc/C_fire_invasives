#Script for data wrangling for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created May 11, 2018

library(tidyverse)
library(plyr)

setwd("data/")

file_names <- list.files()

Jones_soil <- as.data.frame(read_csv("Jones_soil.csv"))
Jones_veg <- as.data.frame(read_csv("Jones_veg.csv"))

head(Jones_soil)
head(Jones_veg)
