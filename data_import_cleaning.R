#Script for data wrangling for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created May 11, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)

setwd("data/")

file_names <- list.files()

#Jones data# burns were prescibed burns; do not need to append burn data since prescribed
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

#rm(Jones_vls)
#merge veg_litter and soil dataframes
Jones_vls <- merge(Jones_veg_litter, Jones_soil, 
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"))

#unique(Jones_vls$Burn_trt)
#add fields that will be common across studies
#Jones_vls$pr_burned <- ifelse(Jones_vls$Burn_trt == 'B' | Jones_vls$Burn_trt == 'C', 'yes','no')
Jones_vls$study <- "Jones et al. 2015"

Jones_vls$lat1 <- ifelse(Jones_vls$Site == 'E' , '4564313','4598553')
Jones_vls$long1 <- ifelse(Jones_vls$Site == 'E' , '466314','436294')
Jones_vls$lat <- ifelse(Jones_vls$Site == 'E' , '41.229507','41.536094')
Jones_vls$long <- ifelse(Jones_vls$Site == 'E' , '-117.4019367','-117.7637079')
#Jones_vls$seeded <- Jones_vls$`cheatgrass seeded`
#colnames(trSamp) <- "newname2"
#colnames(df)[colnames(df) == 'oldName'] <- 'newName'
colnames(Jones_vls)[colnames(Jones_vls) == 'cheatgrass seeded'] <- 'seeded'
colnames(Jones_vls)[colnames(Jones_vls) == 'prescribed burn'] <- 'pr_burned'
colnames(Jones_vls)[colnames(Jones_vls) == 'BD estimated'] <- 'BD_estimated'
colnames(Jones_vls)[colnames(Jones_vls) == '%C'] <- 'soil%C'
colnames(Jones_vls)[colnames(Jones_vls) == 'Soil C (g/m2)'] <- 'soilC_g_m2'
colnames(Jones_vls)[colnames(Jones_vls) == 'Bulk density (g/cm3)'] <- 'BD_g_cm3'
colnames(Jones_vls)[colnames(Jones_vls) == 'top depth'] <- 'topdepth_cm'
colnames(Jones_vls)[colnames(Jones_vls) == 'bottom depth'] <- 'bottomdepth_cm'
colnames(Jones_vls)[colnames(Jones_vls) == 'Litter_C_g_m2'] <- 'litterC_g_m2'
colnames(Jones_vls)[colnames(Jones_vls) == 'Total_Vegetation_C_g_m2'] <- 'AGBC_g_m2'
#colnames(Jones_vls)[colnames(Jones_vls) == 'BD_g_m3'] <- 'BD_g_cm3'
colnames(Jones_vls)[colnames(Jones_vls) == 'Year'] <- 'yr_samp'
colnames(Jones_vls)[colnames(Jones_vls) == 'Barrel'] <- 'barrel'
colnames(Jones_vls)[colnames(Jones_vls) == 'Site'] <- 'site'
colnames(Jones_vls)[colnames(Jones_vls) == 'Burn_trt'] <- 'burn_trt'
colnames(Jones_vls)[colnames(Jones_vls) == 'Rep'] <- 'rep'

Jones_vls$thick <- Jones_vls$bottomdepth_cm - Jones_vls$topdepth_cm
head(Jones_vls)


kpJones <- Jones_vls[,c("barrel", "site", "burn_trt","rep","yr_samp","AGBC_g_m2","litterC_g_m2","soil%C","BD_g_cm3","soilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","seeded","pr_burned","study","lat","long","thick")]
head(kpJones)



###
#Weber data# need BD to calculate soil C content; need to append burn info (one lat/long only)
Weber <- as.data.frame(read_csv("Weber.csv"))

#lat and long data are messed up
Weber$lat <- c("42.853")
Weber$long <- c("-112.402")
#add fields that will be common across studies
Weber$study <- "Weber et al. 2015"
Weber$seeded <- c("no") 
Weber$pr_burned <- c("no")

colnames(Weber)[colnames(Weber) == 'BD estimated'] <- 'BD_estimated'
colnames(Weber)[colnames(Weber) == 'top depth'] <- 'topdepth_cm'
colnames(Weber)[colnames(Weber) == 'bottom depth'] <- 'bottomdepth_cm'
colnames(Weber)[colnames(Weber) == '% C'] <- 'soil%C'
colnames(Weber)[colnames(Weber) == 'Sample'] <- 'sample'

Weber$thick <- Weber$bottomdepth_cm - Weber$topdepth_cm

head(Weber)

kpWeber <- Weber[,c("sample","treatment","soil%C","topdepth_cm","bottomdepth_cm","veg","BD_estimated","lat","long","study","seeded","pr_burned","thick")]
head(kpWeber)



###
#Blank data# need BD to calculate soil C content; need to append burn info for diff sites
Blank <- as.data.frame(read_csv("Blank&Norton.csv"))

Blank$lat <- str_sub(Blank$Latitude, 1, str_length(Blank$Latitude) -1)
Blank$long <- str_sub(Blank$Longitude, 1, str_length(Blank$Longitude) -1)

#add fields that will be common across studies
unique(Blank$Treatment)
Blank$veg <- ifelse(Blank$Treatment == 'Native interspace' | Blank$Treatment == 'Native shrub', 'sagebrush','cheatgrass')
Blank$study <- "Blank & Norton 2006"

head(Blank)

colnames(Blank)[colnames(Blank) == 'Site'] <- 'site'
colnames(Blank)[colnames(Blank) == 'Treatment'] <- 'treatment'
colnames(Blank)[colnames(Blank) == 'Rep'] <- 'rep'
colnames(Blank)[colnames(Blank) == '% C'] <- 'soil%C'
colnames(Blank)[colnames(Blank) == 'Top depth'] <- 'topdepth_cm'
colnames(Blank)[colnames(Blank) == 'Bottom depth'] <- 'bottomdepth_cm'
colnames(Blank)[colnames(Blank) == 'BD estimated'] <- 'BD_estimated'

Blank$thick <- Blank$bottomdepth_cm - Weber$topdepth_cm
Blank$pr_burned <- c("no")
Blank$seeded <- c("no")

kpBlank <- Blank[,c("site","treatment","rep","soil%C","topdepth_cm","bottomdepth_cm","BD_estimated","lat","long","veg","study","thick","pr_burned","seeded")]
head(kpBlank)



###
#Norton data# need to append burn info for 7 sites
Norton <- as.data.frame(read_csv("Norton.csv"))

#add fields that will be common across studies
#unique(Norton$Trt)
Norton$study <- "Norton et al. 2004"
Norton$veg <- ifelse(Norton$Trt == 'N', 'sagebrush','cheatgrass')

#Norton$lat <- Norton$latitude
#Norton$long <- Norton$longitude
#unique(Norton$lat)

colnames(Norton)[colnames(Norton) == 'Site'] <- 'site'
colnames(Norton)[colnames(Norton) == 'Trt'] <- 'treatment'
colnames(Norton)[colnames(Norton) == 'Top Depth'] <- 'topdepth_cm'
colnames(Norton)[colnames(Norton) == 'Bottom Depth'] <- 'bottomdepth_cm'
colnames(Norton)[colnames(Norton) == 'Bulk Density'] <- 'BD_g_cm3'
colnames(Norton)[colnames(Norton) == 'Organic C %'] <- 'soil%C'
colnames(Norton)[colnames(Norton) == 'g OC/m2'] <- 'soilC_g_m2'
colnames(Norton)[colnames(Norton) == 'Thickness'] <- 'thick'
colnames(Norton)[colnames(Norton) == 'latitude'] <- 'lat'
colnames(Norton)[colnames(Norton) == 'longitude'] <- 'long'
colnames(Norton)[colnames(Norton) == 'BD estimated'] <- 'BD_estimated'

head(Norton)

kpNorton <- Norton[,c("site","treatment","topdepth_cm","bottomdepth_cm","thick","BD_g_cm3","soil%C","soilC_g_m2","BD_estimated","lat","long","study","veg")]
head(kpNorton)

kpNorton$pr_burned <- c("no")
kpNorton$seeded <- c("no")



###
#Stark data# need to append burn info for 1 site
Stark <- as.data.frame(read_csv("Stark.csv"))

head(Stark)
#add fields that will be common across studies
Stark$study <- "Stark et al. 2015"
unique(Stark$VegType)

#remove data from fumigated sagbrush; these were fumigated and buldozed...too different from other studies
#however, I believe cheatgrass was fumigated too (24 yrs ago)
Stark2 <- Stark[which(Stark$VegType != "fum sage"),]
Stark2$veg <- ifelse(Stark2$VegType == 'undist sage', 'sagebrush','cheatgrass')
unique(Stark2$veg)

unique(Stark2$`Top depth`)
#0, 10, 20, 40

#BD provided from another Stark study 
Stark2$BD <- ifelse(Stark2$`Top depth` == 0, 1.36,
                        ifelse(Stark2$`Top depth` == 10, 1.35,
                               ifelse(Stark2$`Top depth` == 20, 1.455, 1.57)))
Stark2$`BD estimated` <- c("other")

Stark2$soil%C <- Stark2$`org C (g C/kg)` / 10
Stark2$thick <- Stark2$`Bottom depth` - Stark2$`Top depth`
Stark2$soilC_g_cm2 <- Stark2$BD*Stark2$orgC_perc*Stark2$thick/10

#the study states that the veg here has not burned
Stark2$burn <- c("no")
Stark2$lat <- c("39.90333333")
Stark2$long <- c("-108.40083333")
Stark2$seeded <- ifelse(Stark$veg == 'sage', 'no','maybe')


head(Stark2)
tail(Stark2)


###
#Davies data# Need %C
Davies <- as.data.frame(read_csv("Davies.csv"))
Davies$veg <- c("cheatgrass")
Davies$prescribed_burn <- ifelse(Davies$Treatment == 'ungrazed/unburned', 'no','yes')
head(Davies)

Davies$lat <- c("43.48333333")
Davies$long <- c("119.71666667")
Davies$study <- "Davies et al. 2009"


###
#Bradley data# need BD
Bradley_soil <- as.data.frame(read_csv("Bradley_soil.csv"))
Bradley_AGB <- as.data.frame(read_csv("Bradley_AGB.csv"))
head(Bradley_soil)
head(Bradley_AGB)

#remove Rye if only looking at sagebrush (Rye is desert shrub)
Bradley_AGB2 <- Bradley_AGB[which(Bradley_AGB$Site != "Rye"),]
Bradley_AGB2$veg <- ifelse(Bradley_AGB2$burned == 'no', 'sagebrush','cheatgrass')
Bradley_AGB2$study <- c("Bradley et al. 2006")
Bradley_AGB2$lat <- ifelse(Bradley_AGB2$Site == 'Button' , '41.0000000','40.9900000')
Bradley_AGB2$long <- ifelse(Bradley_AGB2$Site == 'Button' , '117.5800000','117.8600000')

Bradley_soil2 <- Bradley_soil[which(Bradley_soil$Site != "Rye"),]
Bradley_soil2$veg <- ifelse(Bradley_soil2$burned == 'no', 'sagebrush','cheatgrass')
Bradley_soil2$study <- c("Bradley et al. 2006")
Bradley_soil2$lat <- ifelse(Bradley_soil2$Site == 'Button' , '41.0000000','40.9900000')
Bradley_soil2$long <- ifelse(Bradley_soil2$Site == 'Button' , '117.5800000','117.8600000')
Bradley_soil2$`Top Depth` <- c(0)
Bradley_soil2$`Bottom Depth` <- c(10)

head(Bradley_soil2)
head(Bradley_AGB2)


###
#Norton et al. 2008 data# need BD
#Figure out treatments; they don't match the publication
#lat = 42.70777778
#long = 108.60583333
Norton_2008 <- as.data.frame(read_csv("Norton_2008.csv"))
head(Norton_2008)

Norton_2008b <- Norton_2008[which(Norton_2008$wet_dry == "dry"),]
Norton_2008c <- Norton_2008b[which(Norton_2008b$grass == "Cheat"),]

Norton_2008c$veg <- ifelse(Norton_2008c$`life form` == 'LS', 'sagecheat','cheatgrass')

Norton_2008c$lat <- c("42.70777778")
Norton_2008c$long <- c("108.60583333")

Norton_2008c$study <- c("Norton et al. 2008")
Norton_2008c$`Top Depth` <- c(0)
Norton_2008c$`Bottom Depth` <- c(10)
head(Norton_2008c)


###
#Mahood data# need info on veg categories and burned/unburned and bottom depth sampled
#make sure Jones data is not repeated from publication
Mahood1 <- as.data.frame(read_csv("Mahood1.csv"))

#bring in lat/long for ff plots to merge in next step
Mahoodll <- as.data.frame(read_csv("Mahood_ff_plot_locations.csv"))

#join lat/long for ffplots
Mahood1ll <- left_join(Mahood1, Mahoodll, by = "plot")

head(Mahood1ll)
#decide on threshold for "cheatgrass dominance"
#use mean of this data for cheatgrass %C?
summary(Mahood1ll$cheatgrass_cover)
#min cheat % cover = 0.333; max cheat % cover = 57.22

Mahood1ll$study <- c("Mahood et al. unpub1")

Mahood2 <- as.data.frame(read_csv("Mahood2.csv"))
head(Mahood2)
#use mean of this data for cheatgrass %C?
#need burned/unburned category here
#need cheatgrass or sage designation here...based on %?

summary(Mahood2$AIG)
#min AIG % cover = 0.00; max AIG % cover = 17.500
summary(Mahood2$Shrub)
#min shrub % cover = 0.00; max shrub % cover = 22.20

Mahood2$study <- c("Mahood et al. unpub2")


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

#check with Ben...what are the CP and FP treatments???
Rau_sage2 <- Rau_sage[which(Rau_sage$Treatment == "CO" | Rau_sage$Treatment == "FI"),]
unique(Rau_sage2$Treatment)

Rau <- rbind(Rau_inv, Rau_sage2)
head(Rau)

#add veg category
#check with Ben and then update this with other treatments???
Rau$veg <- ifelse(Rau$Treatment == 'Invaded' | Rau$Treatement == 'FI', 'cheatgrass','sagebrush')
Rau$BD_estimated <- c("no")
Rau$prescribed_burn <- c("no")

#convert root and soil carbon from kg C/ ha to g C/m2
Rau$BGB_gC_m2 <- Rau$RTC/10

Rau$SOC_g_m2 <- Rau$TSOC/10

head(Rau)


###
#bring in bulk density spatial data
#data downloaded from here: https://water.usgs.gov/GIS/metadata/usgswrd/XML/muid.xml#stdorder
soil <- raster(paste0("muid.e00"))
str(soil)
attributes(soil)

dataonly <- soil@data

#trying to pull out an info table
data2 <- soil@data@MUID.LAYER
#Error: no slot of name "MUID.LAYER" for this object of class ".SingleLayerData"

crs(soil)
#+proj=aea +lat_1=0 +lat_2=29.5 +lat_0=45.5 +lon_0=0 +x_0=0 +y_0=-96
#+datum=NAD27 +units=m +no_defs +ellps=clrk66
#+nadgrids=@conus,@alaska,@ntv2_0.gsb,@ntv1_can.dat 

#not sure what this is plotting
plot(soil[[1]])
plot(soil[[]])
