#Script for data wrangling for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created May 11, 2018

library(plyr)
library(tidyverse)

setwd("data/")

file_names <- list.files()

#Jones data# need lat/long
# does not currently include the pre and post burn data (Jones_burn.csv)
Jones_soil <- as.data.frame(read_csv("Jones_soil.csv"))
Jones_veg_only <- as.data.frame(read_csv("Jones_veg_only.csv"))
Jones_litter_only <- as.data.frame(read_csv("Jones_litter_only.csv"))

head(Jones_soil)
head(Jones_veg_only)
head(Jones_litter_only)

#merge veg and litter dataframes
Jones_veg_litter <- merge(Jones_veg_only, Jones_litter_only, 
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"))

#merge veg_litter and soil dataframes
Jones_vls <- merge(Jones_veg_litter, Jones_soil, 
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"))

#add fields that will be common across studies
Jones_vls$burned <- ifelse(Jones_vls$Burn_trt == 'B' | Jones_vls$Burn_trt == 'C', 'yes','no')
Jones_vls$study <- "Jones et al. 2015"

###
#Weber data# need BD to calculate soil C content
Weber <- as.data.frame(read_csv("Weber.csv"))
head(Weber)
#lat and long data are messed up

#add fields that will be common across studies
Weber$study <- "Weber et al. 2015"

###
#Blank data# need BD to calculate soil C content
Blank <- as.data.frame(read_csv("Blank&Norton.csv"))

#add fields that will be common across studies
unique(Blank$Treatment)
Blank$veg <- ifelse(Blank$Treatment == 'Native interspace' | Blank$Treatment == 'Native shrub', 'sagebrush','cheatgrass')
Blank$study <- "Blank & Norton 2006"

###
#Norton data# 
Norton <- as.data.frame(read_csv("Norton.csv"))

#add fields that will be common across studies
unique(Norton$Trt)
Norton$study <- "Norton et al. 2004"
Norton$veg <- ifelse(Norton$Trt == 'N', 'sagebrush','cheatgrass')

head(Norton)

###
#Stark data# need lat/long
Stark <- as.data.frame(read_csv("Stark.csv"))

head(Stark)
#add fields that will be common across studies
Stark$study <- "Stark et al. 2015"
unique(Stark$VegType)

#remove data from fumigated sagbrush; these were fumigated and buldozed...too different from other studies
Stark2 <- Stark[which(Stark$VegType != "fum sage"),]
Stark2$veg <- ifelse(Stark2$VegType == 'undist sage', 'sagebrush','cheatgrass')
unique(Stark2$veg)

unique(Stark2$`Top depth`)
#0, 10, 20, 40

Stark2$BD <- ifelse(Stark2$`Top depth` == 0, 1.36,
                        ifelse(Stark2$`Top depth` == 10, 1.35,
                               ifelse(Stark2$`Top depth` == 20, 1.455, 1.57)))

head(Stark2)
tail(Stark2)

Stark2$orgC_perc <- Stark2$`org C (g C/kg)` / 10
Stark2$thick <- Stark2$`Bottom depth` - Stark2$`Top depth`
Stark2$orgC_gC_m2 <- Stark2$BD*Stark2$orgC_perc*Stark2$thick/10

Stark2$burn <- ifelse(Stark2$veg == "cheatgrass", "yes", "no")
