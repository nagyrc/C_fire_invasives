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


###
#Davies data# need lat/long
Davies <- as.data.frame(read_csv("Davies.csv"))
Davies$veg <- c("cheatgrass")
Davies$prescribed_burn <- ifelse(Davies$Treatement == 'ungrazed/unburned', 'no','yes')


###
#Bradley data# need BD, lat/long
Bradley_soil <- as.data.frame(read_csv("Bradley_soil.csv"))
Bradley_AGB <- as.data.frame(read_csv("Bradley_AGB.csv"))
head(Bradley_soil)
head(Bradley_AGB)

#remove Rye if only looking at sagebrush (Rye is desert shrub)
Bradley_AGB2 <- Bradley_AGB[which(Bradley_AGB$Site != "Rye"),]
Bradley_AGB2$veg <- ifelse(Bradley_AGB2$burned == 'no', 'sagebrush','cheatgrass')

Bradley_soil2 <- Bradley_soil[which(Bradley_soil$Site != "Rye"),]
Bradley_soil2$veg <- ifelse(Bradley_soil2$burned == 'no', 'sagebrush','cheatgrass')

head(Bradley_soil2)
head(Bradley_AGB2)

###
#Rau data# need soil depths
Rau_inv <- as.data.frame(read_csv("Rau_invaded.csv"))
head(Rau_inv)
Rau_inv$study <- c("Goergen et al. 2011")

unique(Rau_inv$Region)
unique(Rau_inv$Site)
#BF, LH, MP, TP

Rau_sage <- as.data.frame(read_csv("Rau_sagesteppe.csv"))
Rau_sage$study <- c("Rau et al. 2011")
head(Rau_sage)

unique(Rau_sage$Region)
unique(Rau_sage$Site)
unique(Rau_sage$Treatment)

Rau <- rbind(Rau_inv, Rau_sage)
head(Rau)

#add veg category
#check with Ben on what the treatments are to make sure this veg category is correct...
Rau$veg <- ifelse(Rau$Treatment == 'Invaded', 'cheatgrass','sagebrush')
Rau$BD_estimated <- c("no")
Rau$prescribed_burn <- c("no")

#convert root and soil carbon from kg C/ ha to g C/m2
Rau$BGB_g_m2 <- Rau$RTC/10

Rau$SOC_g_m2 <- Rau$TSOC/10
