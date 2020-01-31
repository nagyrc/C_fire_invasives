#Script for data wrangling for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created May 11, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(doBy)
library(dplyr)

setwd("data/")


#Jones data# burns were prescibed burns; do not need to append burn data since prescribed
#does not currently include the pre and post burn data (Jones_burn.csv)
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
                          by = c("Barrel","Site","Litter_trt","Burn_trt","Rep","Year"), all = TRUE)

unique(Jones_vls$Burn_trt)
unique(Jones_vls$Year)
unique(Jones_vls$veg)

#remove B, C, R treatments after initial year (after burning and seeding with cheatgrass)
Jones_vls2008 <- subset.data.frame(Jones_vls, Year == 2008)
Jones_vls2009 <- subset.data.frame(Jones_vls, Year == 2009)
Jones_vls2010 <- subset.data.frame(Jones_vls, Year == 2010)
Jones_vls2011 <- subset.data.frame(Jones_vls, Year == 2011)
Jones_vls2012 <- subset.data.frame(Jones_vls, Year == 2012)

unique(Jones_vls2008$Burn_trt)
Jones2008 <- subset.data.frame(Jones_vls2008, Burn_trt != "R")
unique(Jones2008$Burn_trt)

Jones2009 <- subset.data.frame(Jones_vls2009, Burn_trt == "N")
Jones2010 <- subset.data.frame(Jones_vls2010, Burn_trt == "N")
Jones2011 <- subset.data.frame(Jones_vls2011, Burn_trt == "N")
Jones2012 <- subset.data.frame(Jones_vls2012, Burn_trt == "N")

Jones_vls2a <- rbind(Jones2008,Jones2009)
Jones_vls2b <- rbind(Jones_vls2a,Jones2010)
Jones_vls2c <- rbind(Jones_vls2b,Jones2011)
Jones_vls2 <- rbind(Jones_vls2c,Jones2012)

#unique(Jones_vls$Burn_trt)
#add fields that will be common across studies
Jones_vls2$study <- "Jones et al. 2015"

Jones_vls2$lat1 <- ifelse(Jones_vls2$Site == 'E' , '4564313','4598553')
Jones_vls2$long1 <- ifelse(Jones_vls2$Site == 'E' , '466314','436294')
Jones_vls2$lat <- ifelse(Jones_vls2$Site == 'E' , 41.229507,41.536094)
Jones_vls2$long <- ifelse(Jones_vls2$Site == 'E' , -117.4019367,-117.7637079)
Jones_vls2$Month_sampled <- c("June")
Jones_vls2$last_year_burned <- ifelse(Jones_vls2$Site == 'E' , 1999, 1985)
Jones_vls2$veg <- c("cheatgrass")

colnames(Jones_vls2)[colnames(Jones_vls2) == 'BD estimated'] <- 'BD_estimated'
colnames(Jones_vls2)[colnames(Jones_vls2) == '%C'] <- 'totsoil%C'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Soil C (g/m2)'] <- 'totsoilC_g_m2'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Bulk density (g/cm3)'] <- 'BD_g_cm3'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'top depth'] <- 'topdepth_cm'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'bottom depth'] <- 'bottomdepth_cm'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Litter_C_g_m2'] <- 'litterC_g_m2'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Total_Vegetation_C_g_m2'] <- 'AGBC_g_m2'
#colnames(Jones_vls)[colnames(Jones_vls) == 'BD_g_m3'] <- 'BD_g_cm3'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Year'] <- 'yr_samp'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Barrel'] <- 'barrel'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Site'] <- 'site'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Burn_trt'] <- 'burn_trt'
colnames(Jones_vls2)[colnames(Jones_vls2) == 'Rep'] <- 'rep'

Jones_vls2$BD_estimated <- c("no")

Jones_vls2$thick <- Jones_vls2$bottomdepth_cm - Jones_vls2$topdepth_cm

unique(Jones_vls2$veg)
kpJones <- Jones_vls2[,c("barrel", "site", "burn_trt","rep","yr_samp","AGBC_g_m2","litterC_g_m2","totsoil%C","BD_g_cm3","totsoilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick","Month_sampled")]
head(kpJones)



###
#Weber data# need to append burn info (one lat/long only)
Weber <- as.data.frame(read_csv("Weber.csv"))
head(Weber)
#lat and long data are messed up
Weber$lat <- 42.853
Weber$long <- -112.402
#add fields that will be common across studies
Weber$study <- "Weber et al. 2015"
Weber$seeded <- c("no") 
Weber$pr_burned <- c("no")

colnames(Weber)[colnames(Weber) == 'BD estimated'] <- 'BD_estimated'
colnames(Weber)[colnames(Weber) == 'top depth'] <- 'topdepth_cm'
colnames(Weber)[colnames(Weber) == 'bottom depth'] <- 'bottomdepth_cm'
colnames(Weber)[colnames(Weber) == '% C'] <- 'orgsoil%C'
colnames(Weber)[colnames(Weber) == 'Sample'] <- 'sample'

Weber$thick <- Weber$bottomdepth_cm - Weber$topdepth_cm

#apply mean BD to calculate soil carbon content
#max soil depth is 8 cm so use mean for 0-10 cm
Weber$BD_g_cm3 <- 1.422
Weber$orgsoilC_g_m2 <- Weber$BD_g_cm3*Weber$`orgsoil%C`*Weber$thick*100
Weber$yr_samp <- 2011
Weber$Month_sampled <- c("September")

head(Weber)

kpWeber <- Weber[,c("sample","treatment","orgsoil%C","topdepth_cm","bottomdepth_cm","veg","BD_estimated","lat","long","study","seeded","pr_burned","thick","BD_g_cm3","orgsoilC_g_m2","yr_samp","Month_sampled")]
head(kpWeber)



###
#Blank data# need to append burn info for diff sites
Blankall <- as.data.frame(read_csv("Blank&Norton.csv"))
unique(Blankall$Site)

#remove Succor Creek and Lincoln Bench sites because percent cover of cheatgrass within the sagebrush was not reported
Blank1 <- Blankall %>% 
  filter(Site == "Cindercone Butte" | Site == "Canyon Creek" | Site == "Vernon Hills" | Site == "Simpson Springs" | Site == "Izengood" | Site == "Eden Valley")

#see how many observations should be removed (Canyon Creek AND cheatgrass)
remove <- Blank1 %>%
  filter(Site == "Canyon Creek" & Treatment == "cheatgrass" )
#so 10 observations meet these criteria

Blank <- Blank1 %>%
  filter(!(Site == "Canyon Creek" & Treatment == "cheatgrass" ))

Blank$lat <- as.numeric(str_sub(Blank$Latitude, 1, str_length(Blank$Latitude) -1))
Blank$long <- as.numeric(str_sub(Blank$Longitude, 1, str_length(Blank$Longitude) -1))

head(Blank)
#add fields that will be common across studies
unique(Blank$Treatment)
Blank$veg <- ifelse(Blank$Site == 'Vernon Hills' & Blank$Treatment == 'Native interspace' | Blank$Treatment == 'Native shrub', 'sagebrush','sagecheat')
Blank$study <- "Blank and Norton 2006"
unique(Blank$veg)
head(Blank)

colnames(Blank)[colnames(Blank) == 'Site'] <- 'site'
colnames(Blank)[colnames(Blank) == 'Treatment'] <- 'treatment'
colnames(Blank)[colnames(Blank) == 'Rep'] <- 'rep'
colnames(Blank)[colnames(Blank) == '% C'] <- 'orgsoil%C'
colnames(Blank)[colnames(Blank) == 'Top depth'] <- 'topdepth_cm'
colnames(Blank)[colnames(Blank) == 'Bottom depth'] <- 'bottomdepth_cm'
colnames(Blank)[colnames(Blank) == 'BD estimated'] <- 'BD_estimated'

Blank$thick <- Blank$bottomdepth_cm - Blank$topdepth_cm
Blank$pr_burned <- c("no")
Blank$seeded <- c("no")
Blank$yr_samp <- "2006"

#apply mean BD to calculate soil carbon content
Blank$BD_g_cm3 <- ifelse(Blank$topdepth_cm == 0 , 1.422, 1.35)
Blank$orgsoilC_g_m2 <- Blank$BD_g_cm3*Blank$`orgsoil%C`*Blank$thick*100

kpBlank <- Blank[,c("site","treatment","rep","orgsoil%C","topdepth_cm","bottomdepth_cm","BD_estimated","lat","long","veg","study","thick","pr_burned","seeded","BD_g_cm3","orgsoilC_g_m2", "yr_samp")]
head(kpBlank)



###
#Norton data
Nortonpre <- as.data.frame(read_csv("Norton_updated.csv"))

head(Nortonpre)
#add fields that will be common across studies
#unique(Norton$Trt)
Nortonpre$study <- "Norton et al. 2004"

Norton <- Nortonpre %>%
  filter(!Site == "MW")

#Norton$veg <- ifelse(Norton$Site == 'N', 'sagebrush','sagecheat')
Norton$Month_sampled <- "July-September"
Norton$yr_samp <- 2001
#Norton$lat <- Norton$latitude
#Norton$long <- Norton$longitude
#unique(Norton$lat)

colnames(Norton)[colnames(Norton) == 'Site'] <- 'site'
colnames(Norton)[colnames(Norton) == 'Trt'] <- 'treatment'
colnames(Norton)[colnames(Norton) == 'Top Depth'] <- 'topdepth_cm'
colnames(Norton)[colnames(Norton) == 'Bottom Depth'] <- 'bottomdepth_cm'
colnames(Norton)[colnames(Norton) == 'Bulk Density'] <- 'BD_g_cm3'
colnames(Norton)[colnames(Norton) == 'Organic C %'] <- 'orgsoil%C'
colnames(Norton)[colnames(Norton) == 'g OC/m2'] <- 'orgsoilC_g_m2'
colnames(Norton)[colnames(Norton) == 'Thickness'] <- 'thick'
colnames(Norton)[colnames(Norton) == 'latitude'] <- 'lat'
colnames(Norton)[colnames(Norton) == 'longitude'] <- 'long'
colnames(Norton)[colnames(Norton) == 'BD estimated'] <- 'BD_estimated'

head(Norton)

kpNorton <- Norton[,c("site","treatment","topdepth_cm","bottomdepth_cm","thick","BD_g_cm3","orgsoil%C","orgsoilC_g_m2","BD_estimated","lat","long","study","veg", "Month_sampled", "yr_samp")]
head(kpNorton)

#Norton$check <- Norton$`soil%C` * Norton$thick * Norton$BD_g_cm3 * 100
Norton

kpNorton$pr_burned <- c("no")
kpNorton$seeded <- c("no")



###
#Stark data# need to append burn info for 1 site
#the study states that the veg here has not burned
Stark <- as.data.frame(read_csv("Stark.csv"))

#add fields that will be common across studies
Stark$study <- "Stark et al. 2015"
unique(Stark$VegType)

#fumigated sagebrush were fumigated and buldozed...too different from other studies??
#however, I believe cheatgrass was fumigated too (24 yrs ago)
#Stark2 <- Stark[which(Stark$VegType != "fum sage"),]
Stark$veg <- ifelse(Stark$VegType == 'undist sage', 'sagecheat',
                     ifelse(Stark$VegType == 'fum sage','sagebrush','cheatgrass'))


#BD provided from another Stark study 
Stark$BD_g_cm3 <- ifelse(Stark$`Top depth` == 0, 1.36,
                        ifelse(Stark$`Top depth` == 10, 1.35,
                               ifelse(Stark$`Top depth` == 20, 1.455, 1.57)))
Stark$`BD estimated` <- c("no")

Stark$soil_percC <- Stark$`org C (g C/kg)` / 10
Stark$thick <- Stark$`Bottom depth` - Stark$`Top depth`
Stark$orgsoilC_g_m2 <- Stark$BD_g_cm3 * Stark$soil_percC * Stark$thick * 100

Stark$pr_burned <- c("no")
Stark$lat <- 39.90333333
Stark$long <- -108.40083333
Stark$seeded <- ifelse(Stark$veg == 'sage', 'no','maybe')
Stark$yr_samp <- c("2008-2009")
Stark$Month_sampled <- c("April-June")

head(Stark)

colnames(Stark)[colnames(Stark) == 'Plot'] <- 'plot'
colnames(Stark)[colnames(Stark) == 'Block'] <- 'block'
colnames(Stark)[colnames(Stark) == 'BD estimated'] <- 'BD_estimated'
colnames(Stark)[colnames(Stark) == 'soil_percC'] <- 'orgsoil%C'
colnames(Stark)[colnames(Stark) == 'Top depth'] <- 'topdepth_cm'
colnames(Stark)[colnames(Stark) == 'Bottom depth'] <- 'bottomdepth_cm'

kpStark <- Stark[,c("plot","block","topdepth_cm","bottomdepth_cm","BD_estimated","study","veg","BD_g_cm3","thick","orgsoil%C","orgsoilC_g_m2","pr_burned","lat","long","seeded","yr_samp","Month_sampled")]
head(kpStark)




###
#Davies data# 
#prescribed burn was in 1993
#no other burn from 1936-2007 besides prescribed burn
#Davies <- as.data.frame(read_csv("Davies.csv"))
#Davies$veg <- c("cheatgrass")
#Davies$pr_burned <- ifelse(Davies$Treatment == 'ungrazed/unburned', 'no','yes')

#Davies$lat <- c("43.48333333")
#Davies$long <- c("-119.71666667")
#Davies$study <- "Davies et al. 2009"

#colnames(Davies)[colnames(Davies) == 'Treatment'] <- 'treatment'
#colnames(Davies)[colnames(Davies) == 'Year'] <- 'yr_samp'

#Davies$biomass_g_m2 <- Davies$`Cheatgrass biomass (kg/ha)`/10
#Davies$seeded <- c("no")


#head(Davies)

#kpDavies <- Davies[,c("treatment","block","yr_samp","veg","pr_burned","lat","long","study","biomass_g_m2","seeded")]
#head(kpDavies)




###
#Bradley data# have info on burn history in paper 
Bradley_soil <- as.data.frame(read_csv("Bradley_soil.csv"))
Bradley_AGB <- as.data.frame(read_csv("Bradley_AGB.csv"))
head(Bradley_soil)
head(Bradley_AGB)

Bradley_AGB$veg <- ifelse(Bradley_AGB$Site == 'Rye' & Bradley_AGB$burned == 'no', 'salt_desert',
                          ifelse(Bradley_AGB$burned == 'yes','cheatgrass', 'sagecheat'))
Bradley_AGB$study <- c("Bradley et al. 2006")
Bradley_AGB$lat <- ifelse(Bradley_AGB$Site == 'Button' , 41.0000000,
                           ifelse(Bradley_AGB$Site == 'Rye', 40.5700000, 40.9900000))
Bradley_AGB$long <- ifelse(Bradley_AGB$Site == 'Button' , -117.5800000,
                            ifelse(Bradley_AGB$Site == 'Rye', -118.3400000, -117.8600000))
Bradley_AGB$pr_burned <- c("no")
Bradley_AGB$yr_samp <- c(2004)
Bradley_AGB$seeded <- c("no")

#Bradley_soil2 <- Bradley_soil[which(Bradley_soil$Site != "Rye"),]
Bradley_soil$veg <- ifelse(Bradley_soil$Site == 'Rye' & Bradley_soil$burned == 'no', 'salt_desert',
                          ifelse(Bradley_soil$burned == 'yes','cheatgrass', 'sagecheat'))
Bradley_soil$study <- c("Bradley et al. 2006")
Bradley_soil$lat <- ifelse(Bradley_soil$Site == 'Button' , 41.0000000,
                           ifelse(Bradley_soil$Site == 'Rye', 40.5700000, 40.9900000))
Bradley_soil$long <- ifelse(Bradley_soil$Site == 'Button' , -117.5800000,
                            ifelse(Bradley_soil$Site == 'Rye', -118.3400000, -117.8600000))
Bradley_soil$`Top Depth` <- 0
Bradley_soil$`Bottom Depth` <- 10
Bradley_soil$pr_burned <- c("no")
Bradley_soil$yr_samp <- c(2004)
Bradley_soil$thick <- Bradley_soil$`Bottom Depth` - Bradley_soil$`Top Depth`
Bradley_soil$seeded <- c("no")

colnames(Bradley_soil)[colnames(Bradley_soil) == 'Carbon_perc'] <- 'totsoil%C'
colnames(Bradley_soil)[colnames(Bradley_soil) == 'Site'] <- 'site'
colnames(Bradley_soil)[colnames(Bradley_soil) == 'Sample_Name'] <- 'sample'
colnames(Bradley_soil)[colnames(Bradley_soil) == 'Top Depth'] <- 'topdepth_cm'
colnames(Bradley_soil)[colnames(Bradley_soil) == 'Bottom Depth'] <- 'bottomdepth_cm'

#apply mean BD to calculate soil carbon content
Bradley_soil$BD_g_cm3 <- 1.422
Bradley_soil$totsoilC_g_m2 <- Bradley_soil$BD_g_cm3 * Bradley_soil$`totsoil%C` * Bradley_soil$thick * 100

colnames(Bradley_AGB)[colnames(Bradley_AGB) == 'Site'] <- 'site'
colnames(Bradley_AGB)[colnames(Bradley_AGB) == 'AGB(gC/m2)'] <- 'AGBC_g_m2'


kpBradleysoil <- Bradley_soil
kpBradleyveg <- Bradley_AGB[,c("site","burned","AGBC_g_m2","veg","study","lat","long","pr_burned","yr_samp","seeded")]

head(kpBradleysoil)
head(kpBradleyveg)




###
#Norton et al. 2008 data
Norton_2008 <- as.data.frame(read_csv("Norton_2008.csv"))
head(Norton_2008)

Norton_2008b <- Norton_2008[which(Norton_2008$wet_dry == "dry"),]
Norton_2008c <- Norton_2008b[which(Norton_2008b$grass == "Cheat"),]

Norton_2008c$veg <- ifelse(Norton_2008c$`life form` == 'LS', 'sagecheat','cheatgrass')

Norton_2008c$lat <- 42.70777778
Norton_2008c$long <- -108.60583333

Norton_2008c$study <- c("Norton et al. 2008")
Norton_2008c$`Top Depth` <- 0
Norton_2008c$`Bottom Depth` <- 10
Norton_2008c$thick <- Norton_2008c$`Bottom Depth` - Norton_2008c$`Top Depth`
Norton_2008c$seeded <- c("no")
Norton_2008c$yr_samp <- c(2003)

colnames(Norton_2008c)[colnames(Norton_2008c) == 'Top Depth'] <- 'topdepth_cm'
colnames(Norton_2008c)[colnames(Norton_2008c) == 'Bottom Depth'] <- 'bottomdepth_cm'
colnames(Norton_2008c)[colnames(Norton_2008c) == 'TOC_perc'] <- 'orgsoil%C'
colnames(Norton_2008c)[colnames(Norton_2008c) == 'life form'] <- 'life_form'

head(Norton_2008c)

#apply mean BD to calculate soil carbon content
Norton_2008c$BD_g_cm3 <- 1.422
Norton_2008c$orgsoilC_g_m2 <- Norton_2008c$BD_g_cm3 * Norton_2008c$`orgsoil%C` * Norton_2008c$thick * 100


kpNorton2008 <- Norton_2008c[,c("life_form","rep","orgsoil%C","veg","lat","long","study","topdepth_cm","bottomdepth_cm","thick","seeded","yr_samp", "BD_g_cm3", "orgsoilC_g_m2")]
head(kpNorton2008)



###
#Mahood data
#need info on veg categories and burned/unburned and bottom depth sampled
#make sure Jones data is not repeated from publication
Mahood1 <- as.data.frame(read_csv("Mahood1_updated.csv"))
head(Mahood1)

#bring in lat/long for ff plots to merge in next step
Mahoodll <- as.data.frame(read_csv("Mahood_ff_plot_locations.csv"))

#join lat/long for ffplots
Mahood1ll <- left_join(Mahood1, Mahoodll, by = "plot")

head(Mahood1ll)
#decide on threshold for "cheatgrass dominance"
#use mean of this data for cheatgrass %C; see below
summary(Mahood1ll$cheatgrass_cover)
#min cheat % cover = 0.333; max cheat % cover = 57.22

Mahood1ll$study <- c("Mahood et al. unpub1")
Mahood1ll$seeded <- c("no")
Mahood1ll$pr_burned <- c("no")
#check this year with Adam
Mahood1ll$yr_samp <- 2017
#Mahood1ll$burned <- ifelse(Mahood1ll$burn_status == 'burned', 'yes','no')
Mahood1ll$topdepth_cm <- 0
Mahood1ll$bottomdepth_cm <- 10
Mahood1ll$thick <- Mahood1ll$bottomdepth_cm - Mahood1ll$topdepth_cm

#check this with Adam
#Mahood1ll$veg <- ifelse(Mahood1ll$native_shrub_cover > 0, 'sagecheat','cheatgrass')
Mahood1ll$BD_estimated <- c("no")

colnames(Mahood1ll)[colnames(Mahood1ll) == 'Elevation'] <- 'elevation'
colnames(Mahood1ll)[colnames(Mahood1ll) == 'soil_TC_pct'] <- 'totsoil%C'
colnames(Mahood1ll)[colnames(Mahood1ll) == 'soil_bulk_density'] <- 'BD_g_cm3'
#these calculations are not correct- he didn't multiply by the depth
colnames(Mahood1ll)[colnames(Mahood1ll) == 'soil_carbon_gm2'] <- 'do_not_use'
colnames(Mahood1ll)[colnames(Mahood1ll) == 'Site'] <- 'site'

Mahood1ll$totsoilC_g_m2 <- Mahood1ll$`totsoil%C` * Mahood1ll$BD_g_cm3 * Mahood1ll$thick * 100

head(Mahood1ll)

Mahood1ll <- subset(Mahood1ll, select = -c(lat.y, long.y))
colnames(Mahood1ll)[colnames(Mahood1ll) == 'lat.x'] <- 'lat'
colnames(Mahood1ll)[colnames(Mahood1ll) == 'long.x'] <- 'long'

kpMahood1 <- Mahood1ll[,c("plot", "BD_g_cm3", "totsoil%C", "last_year_severity", "elevation", "fire_frequency", "last_year_burned", "time_since_fire", "allotment", "mean_fire_interval", "totsoilC_g_m2", "lat", "long", "study", "seeded", "pr_burned", "bottomdepth_cm", "topdepth_cm", "thick", "veg", "BD_estimated", "yr_samp", "site")]
head(kpMahood1)

write.csv(kpMahood1, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/kpMahood1.csv")


unique(kpMahood1$site)
unique(kpMahood1$lat)
unique(kpMahood1$veg)
unique(kpMahood1$study)
unique(kpMahood1$yr_samp)
unique(kpMahood1$bottomdepth_cm)
unique(kpMahood1$long)

#Mahood2
Mahood2 <- as.data.frame(read_csv("Mahood2.csv")) 

Mahood2$veg <- ifelse(Mahood2$`Site type` == 'C' | Mahood2$`Site type` == 'D', 'cheatgrass', 'sagebrush')
Mahood2$study <- c("Mahood et al. unpub2")

colnames(Mahood2)[colnames(Mahood2) == 'Plot_TP'] <- 'sample'
colnames(Mahood2)[colnames(Mahood2) == 'Plot'] <- 'plot'
#colnames(Mahood2)[colnames(Mahood2) == 'SOIL_OM_pct'] <- 'soil%OM'
colnames(Mahood2)[colnames(Mahood2) == 'Litter_TC_pct'] <- 'litter%C'


#bring in other file for BD of each site
MahoodBD <- as.data.frame(read_csv("MahoodBD.csv"))

#join BD data
Mahood2BD <- left_join(Mahood2, MahoodBD, by = c("Transect","Site_number"))

colnames(Mahood2BD)[colnames(Mahood2BD) == 'Site_number'] <- 'site'
colnames(Mahood2BD)[colnames(Mahood2BD) == 'Transect'] <- 'transect'
colnames(Mahood2BD)[colnames(Mahood2BD) == 'Site type'] <- 'site_type'
#colnames(Mahood2BD)[colnames(Mahood2BD) == 'bulkDensity.g.cm3.'] <- 'BD_g_cm3'

#bring in other file for BD, lat, long of each site
Mahood2ll <- as.data.frame(read_csv("Mahood2ll.csv"))

#join lat, long data
Mahood2BDll <- left_join(Mahood2BD, Mahood2ll, by = "Plot_TP")

#Mahood2BDll$topdepth_cm <- 0
#Mahood2BDll$bottomdepth_cm <- 10
#Mahood2BDll$thick <- Mahood2BDll$bottomdepth_cm - Mahood2BDll$topdepth_cm

#the conversion factor below is from the lab that did the testing (converting %OM to %totC)
#Mahood2BDll$orgsoilC_g_m2 <- Mahood2BDll$`soil%OM`*Mahood2BDll$BD_g_cm3*Mahood2BDll$thick*100/1.724
Mahood2BDll$seeded <- c("no")
Mahood2BDll$pr_burned <- c("no")
Mahood2BDll$BD_estimated <- c("no")

colnames(Mahood2BDll)[colnames(Mahood2BDll) == 'latitude'] <- 'lat'
colnames(Mahood2BDll)[colnames(Mahood2BDll) == 'longitude'] <- 'long'

#check this year with Adam
Mahood2BDll$yr_samp <- 2017

head(Mahood2BDll)

kpMahood2 <- Mahood2BDll[,c("site", "transect", "site_type", "litter%C", "veg", "study", "lat", "long", "elevation", "seeded", "pr_burned","yr_samp")]
head(kpMahood2)

str(kpMahood2)

write.csv(kpMahood2, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/kpMahood2.csv")




###
#Rau data# need soil depths
#need burn info for lat/longs
#check with Ben on treatments
Rau_inv <- as.data.frame(read_csv("Rau_invaded_akaGoergon2011_updated.csv"))
Rau_inv$study <- c("Goergen et al. 2011")
#Goergen et al. 2011 says 0-90 cm
#check this depth with Ben

#check this year with Ben
Rau_inv$yr_samp <- 2008
Rau_inv$topdepth_cm <- 0
Rau_inv$bottomdepth_cm <- 90
Rau_inv$thick <- Rau_inv$bottomdepth_cm - Rau_inv$topdepth_cm
Rau_inv$Month_sampled <- c('December')

unique(Rau_inv$Region)
#SW
unique(Rau_inv$Site)
#BF, LH, MP, TP

Rau_sage <- as.data.frame(read_csv("Rau_sagesteppe_updated.csv"))
Rau_sage$study <- c("Rau et al. 2011")

#check this year with Ben
Rau_sage$yr_samp <- 2011
Rau_sage$topdepth_cm <- 0
Rau_sage$bottomdepth_cm <- 90
Rau_sage$thick <- Rau_sage$bottomdepth_cm - Rau_sage$topdepth_cm
Rau_sage$Month_sampled <- c("unknown")

unique(Rau_sage$Region)
unique(Rau_sage$Site)
unique(Rau_sage$Treatment)

head(Rau_sage)

#check with Ben...what are the CP and FP treatments???
#CP and FP have a herbicide applied...don't use these
#CO and FI are control and fire/burn...use these
#should FI have a different study_id???
Rau_sage2 <- Rau_sage[which(Rau_sage$Treatment == "CO" | Rau_sage$Treatment == "FI"),]
unique(Rau_sage2$Treatment)

#get rid of Site_old in Rau_inv
#Rau_inv <- subset(Rau_inv, select = -c(Site_old))

Rau <- rbind(Rau_inv, Rau_sage2)

#add veg category
#check with Ben and then update this with other treatments???
#Rau$veg <- ifelse(Rau$Annual_grass > 2, 'sagecheat','sagebrush')
Rau$BD_estimated <- c("no")
Rau$pr_burned <- c("no")

#convert root and soil carbon from kg C/ ha to g C/m2
Rau$BGBC_g_m2 <- Rau$RTC/10
Rau$orgsoilC_g_m2 <- Rau$TSOC/10


colnames(Rau)[colnames(Rau) == 'Region'] <- 'region'
colnames(Rau)[colnames(Rau) == 'Site'] <- 'site'
colnames(Rau)[colnames(Rau) == 'Lat'] <- 'lat'
colnames(Rau)[colnames(Rau) == 'Long'] <- 'long'
colnames(Rau)[colnames(Rau) == 'Treatment'] <- 'treatment'
colnames(Rau)[colnames(Rau) == 'Subplot'] <- 'subplot'
colnames(Rau)[colnames(Rau) == 'Elevation'] <- 'elevation'

head(Rau)

kpRau <- Rau[,c("region", "site", "treatment", "subplot", "elevation", "long", "lat", "study", "veg", "BD_estimated", "pr_burned", "BGBC_g_m2", "orgsoilC_g_m2","yr_samp","topdepth_cm","bottomdepth_cm","thick", "Month_sampled")]
head(kpRau)

str(kpRau)




#Peschel data
Peschel <- as.data.frame(read_csv("Peschel.csv"))
head(Peschel)

Peschel2 <- summaryBy(biomass_g_m2 ~ Site + Treatment, data = Peschel, FUN = sum)
head(Peschel2)

unique(Peschel2$Site)

#add lat, long, study, veg
Peschel2$lat <- ifelse(Peschel2$Site == 'Gros Venture', 43.56944444,
                    ifelse(Peschel2$Site == 'Lower Hoback', 43.29583333,
                           ifelse(Peschel2$Site == 'Upper Hoback', 43.30000000, 43.51444444)))

Peschel2$long <- ifelse(Peschel2$Site == 'Gros Venture', -110.31111111,
                        ifelse(Peschel2$Site == 'Lower Hoback', -110.65750000,
                                ifelse(Peschel2$Site == 'Upper Hoback', -110.66055556, -110.71166667)))

colnames(Peschel2)[colnames(Peschel2) == 'biomass_g_m2.sum'] <- 'AGB_g_m2'
colnames(Peschel2)[colnames(Peschel2) == 'Site'] <- 'site'
colnames(Peschel2)[colnames(Peschel2) == 'Treatment'] <- 'treatment'
Peschel2$study <- 'Peschel et al. 2015'
Peschel2$veg <- 'sagebrush'
Peschel2$Month_sampled <- 'July'
Peschel2$yr_samp <- c(2013)


summary(kpPeschel$AGB_g_m2)

kpPeschel <- Peschel2[,c("site", "treatment", "AGB_g_m2", "lat", "long", "study", "veg", "Month_sampled", "yr_samp")]
head(kpPeschel)



#Norton et al. 2012 data
Norton2012 <- as.data.frame(read_csv("Norton_2012.csv"))
head(Norton2012)
tail(Norton2012)

colnames(Norton2012)[colnames(Norton2012) == 'Year'] <- 'yr_samp'
Norton2012$month <- str_sub(Norton2012$Date, 1, 1)
unique(Norton2012$month)

Norton2012$Month_sampled <- ifelse (Norton2012$month == '3', 'March',
                                    ifelse (Norton2012$month == '4','April',
                                            ifelse (Norton2012$month == '5', 'May', 
                                                    ifelse (Norton2012$month == '6', 'June',
                                                            ifelse (Norton2012$month == '7', 'July',
                                                                    ifelse (Norton2012$month == '8','August',
                                                                            ifelse (Norton2012$month == '9','September',
                                                                                    ifelse (Norton2012$month == '1','January','unknown'))))))))

Norton2012$BD_estimated <- c("yes")

Norton2012$topdepth_cm <- 0
Norton2012$bottomdepth_cm <- 10
Norton2012$thick <- Norton2012$bottomdepth_cm - Norton2012$topdepth_cm
Norton2012$percC <- Norton2012$ug_C_mg / 10
colnames(Norton2012)[colnames(Norton2012) == 'percC'] <- 'orgsoil%C'


unique(Norton2012$spec)


Norton2012$veg <- ifelse(Norton2012$spec == 'Br', 'cheatgrass', 'sagebrush')

Norton2012$lat <- 40.33333333
Norton2012$long <- -112.56666667
Norton2012$study <- 'Norton et al. 2012'

canopy <- subset.data.frame(Norton2012, spec == 'Sa.B')
inter <- subset.data.frame(Norton2012, spec == 'Sa.I')
cheat <- subset.data.frame(Norton2012, spec == 'Br')

#need to make sure they are ordered correctly first
canopy[
  with(canopy, order(yr_samp,Date,Season,block)),
]

inter[
  with(inter, order(yr_samp,Date,Season,block)),
  ]

#make sure they match
identical(canopy[['yr_samp']],inter[['yr_samp']])
identical(canopy[['Date']],inter[['Date']])
identical(canopy[['Season']],inter[['Season']])
identical(canopy[['block']],inter[['block']])
#excellent, they all match, now can do the conversions

sage <- canopy
sage$`orgsoil%C` <- (canopy$`orgsoil%C`*0.38) + (inter$`orgsoil%C`*0.62)
sage$ug_C_mg <- (canopy$ug_C_mg*0.38) + (inter$ug_C_mg*0.62)
sage$spec <- c("Sa")

Norton2012merge <- rbind(sage, cheat)

#colnames(Norton2012merge)[colnames(Norton2012merge) == 'soil%C'] <- 'orgsoil%C'

#apply mean BD for 0-10 cm 
Norton2012merge$BD_g_cm3 <- 1.422
Norton2012merge$orgsoilC_g_m2 <- Norton2012merge$BD_g_cm3*Norton2012merge$`orgsoil%C`*Norton2012merge$thick*100

head(Norton2012merge)
kpNorton2012 <- Norton2012merge[,c("yr_samp", "Date", "Season", "block", "ug_C_mg", "month", "Month_sampled", "BD_estimated", "topdepth_cm", "bottomdepth_cm","thick","orgsoil%C","veg", "lat", "long","study", "BD_g_cm3", "orgsoilC_g_m2")]





#Anderson data
Anderson_bio <- as.data.frame(read_csv("Anderson_biomass.csv"))
Anderson_ED <- as.data.frame(read_csv("Anderson_ED.csv"))

head(Anderson_bio)
head(Anderson_ED)

#create CellName to match with other dataset
Anderson_bio$CellName <- paste0(Anderson_bio$CellType,Anderson_bio$CellNum)

#join biomass with experimental design data
Anderson <- left_join (Anderson_bio, Anderson_ED, by = "CellName")
head(Anderson)

#subset to only the control treatment
Anderson_sub <- subset.data.frame(Anderson, Treatments_No_Grazing == 'Control')

#calculate total biomass of shrubs + herbs; veg was clipped in 1 m2 subplots so the g here are g/m2
Anderson_sub$biomass_g_m2 <- Anderson_sub$gHerb + Anderson_sub$gDShrb + Anderson_sub$gLShrb

Anderson_sub$yr_samp <- c(2013)
 
Anderson_sub$gDShrb
#bring in species occupancy data
Anderson_occ <- as.data.frame(read_csv("Anderson_occ.csv"))
head(Anderson_occ)

#subset to only cheatgrass and sagebrush occupancy
occ <- Anderson_occ[,c("PointID","CellName","Year","ARTR2_occ","BRTE_occ")]

occ
unique(occ$ARTR2_occ)
unique(occ$BRTE_occ)

#check3 <- subset.data.frame(occ, ARTR2_occ == 1)
#check3b <- subset.data.frame(occ, BRTE_occ == 1)
#only cheatgrass left in control treatments

head(Anderson_sub)
head(occ)

#join occupancy data to biomass and experimental design
And2 <- inner_join(Anderson_sub, occ, by = c("PointID", "CellName","Year"))
head(And2)

#select columns for keeping
And3 <- subset(And2, select = c("PtName", "PointID","Year","CellNum","PtNum","CellType","gHerb","gDShrb","gLShrb","CellName","BlockName","biomass_g_m2","yr_samp","ARTR2_occ","BRTE_occ"))

#create veg category by presence/absence of cheatgrass and sagebrush 
And3$veg <- ifelse (And3$ARTR2_occ == 1 & And3$BRTE_occ == 0, 'sagebrush',
                    ifelse (And3$ARTR2_occ == 1 & And3$BRTE_occ == 1,'sagecheat',
                            ifelse (And3$ARTR2_occ == 0 & And3$BRTE_occ == 1, 'cheatgrass', 'other')))

#subset to only blocks that had sagebrush and/or cheatgrass
And4 <- subset.data.frame(And3, veg == 'cheatgrass' | veg == 'sagecheat' | veg == 'sagebrush')
head(And4)

And4$veg
#cheatgrass only; no sage samples;need to apply cheatgrass % C to biomass data

And4$Month_sampled <- "May"

#need to remove columns here
kpAnderson <- And4[,c("PtName", "PointID", "CellNum", "PtNum", "CellName", "BlockName", "biomass_g_m2", "yr_samp", "veg", "Month_sampled")]
kpAnderson$study <- 'Anderson et al. 2018'
kpAnderson$lat <- 43.28333333
kpAnderson$long <- -116.20000000

colnames(kpAnderson)[colnames(kpAnderson) == 'biomass_g_m2'] <- 'AGB_g_m2'

head(kpAnderson)






###########################################
#make all conversions from aboveground biomass g/m2 to aboveground biomass carbon gC/m2

#make cheatgrass %C an object to use later
cheat_percC <- mean(Mahood2$Bromus_TC_pct, na.rm = TRUE)

###

sagepercC <- 0.45 #sage AGB % C = 45; from West 1972 Table 8
saltpercC <- 0.45 #salt desert AGB % C = 45.0; from West 1972 (average of 1968, 69, 70)

sagecheatpercC <- ((cheat_percC/100)+ sagepercC) /2
#0.4383225

#convert aboveground biomass into AGB C by using mean values
kpPeschel$AGBC_g_m2 <- kpPeschel$AGB_g_m2*sagepercC
kpAnderson$AGBC_g_m2 <- kpAnderson$AGB_g_m2*cheat_percC/100



###########################################
#Litter conversion factors (if needed)
sagelitterpercC <- 0.38 #from West 1972 Table 8
cheatlitterpercC <- 0.33667 #from Mahood2
saltlitterpercC <- 0.4555 # from West 1972 (average of 1968, 69, 70)


#BGB conversion factors (if needed)
sageBGBpercC010 <- 0.29 #from Svejcar and Sheley 2001 Table 3
cheatBGBpercC010 <- 0.31 #from Svejcar and Sheley 2001 Table 3
sageBGBpercC1030 <- 0.314 #from Svejcar and Sheley 2001 Table 3
cheatBGBpercC1030 <- 0.337 #from Svejcar and Sheley 2001 Table 3

#saltBGBpercC <- 0.2942 #from West 1972 (average across all depths; all years)
saltBGBpercC230 <-0.277 #from West 1972 (average across all years)
saltBGBpercC3060 <- 0.322 #from West 1972 (average across all years)
saltBGBpercC6090 <- 0.2836 #from West 1972 (average across all years)
  
sagecheatBGBpercC010 <- (sageBGBpercC010+cheatBGBpercC010)/2
###########################################
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}


#merge all data frames
bind1 <- rbind.all.columns(kpJones, kpWeber)
bind2 <- rbind.all.columns(bind1, kpBlank)
bind3 <- rbind.all.columns(bind2, kpNorton)
bind4 <- rbind.all.columns(bind3, kpStark)
bind5 <- rbind.all.columns(bind4, kpBradleysoil)
bind6 <- rbind.all.columns(bind5, kpBradleyveg)
bind7 <- rbind.all.columns(bind6, kpNorton2008)
bind8 <- rbind.all.columns(bind7, kpMahood1)
bind9 <- rbind.all.columns(bind8, kpMahood2)
bind10 <- rbind.all.columns(bind9, kpRau)
bind11 <- rbind.all.columns(bind10, kpPeschel)
bind12 <- rbind.all.columns(bind11, kpNorton2012)
bind13 <- rbind.all.columns(bind12, kpAnderson)

names(bind13)

###########################################
#only need to run the code below one time; then turn off
#get unique lat/longs for Emily to extract burn data
#unique(df[c("yad", "per")])
#lllist <- unique(alldata[c("lat","long")])

#write.csv(lllist, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/uniquelatlong.csv")



###
#add new datasets

#Bansal data
Bansal <- as.data.frame(read_csv("Bansal_data.csv"))

Bansal$brushb <- as.numeric(Bansal$brushb)
Bansal$AGB_g_m2 <- Bansal$afb + Bansal$pfb + Bansal$brushb + Bansal$bulbb_ + Bansal$anngrassb + Bansal$bunchb

summary(Bansal$AGB_g_m2)

Bansalkp <- na.omit(Bansal)

summary(Bansalkp$AGB_g_m2)

Bansalkp <- Bansalkp[,c("year", "site", "comm", "AGB_g_m2")]
Bansalkp$study <- 'Bansal et al. 2014'
Bansalkp$lat <- 43.791665
Bansalkp$long <- -118.4417
Bansalkp$veg <- ifelse (Bansalkp$comm == 'brte', 'cheatgrass', ifelse (Bansalkp$comm == 'mix', 'sagecheat', 'sagebrush'))

colnames(Bansalkp)[colnames(Bansalkp) == 'year'] <- 'yr_samp'

Bansalkp$Month_sampled <- ifelse (Bansalkp$yr_samp == 2010, 'August', 'June')

Bansalkp$AGBC_g_m2 <- Bansalkp$AGB_g_m2*cheat_percC/100

summary(Bansalkp$AGBC_g_m2)

#removing comm field
Bansalkp <- Bansalkp[,c("yr_samp", "site", "veg", "AGB_g_m2", "AGBC_g_m2", "lat", "long", "Month_sampled", "study")]

#need to append Bansalkp to alldata
bind14 <- rbind.all.columns(bind13, Bansalkp)



###
#Porensky data
Porensky <- as.data.frame(read_csv("Porensky_data.csv"))
unique(Porensky$Grazed)

Porensky$Grazed <- gsub("Ungrazed", "ungrazed", Porensky$Grazed)
unique(Porensky$Grazed)

Porensky$type <- gsub("Control", "control", Porensky$type)
unique(Porensky$type)

sub1 <- Porensky %>%
  filter(type == 'control')

sub2 <- sub1 %>%
  group_by(Block, Paddock, Quadrat, yr_samp, Month_sampled, Grazed, type) %>%
  summarize(totwt = sum(`Weight (g)`))

colnames(sub2)[colnames(sub2) == 'totwt'] <- 'AGB_g_m2'

#split Quadrat column to make rep
sub2$rep <- parse_number(sub2$Quadrat)

#convert AGB to carbon
sub2$AGBC_g_m2 <- sub2$AGB_g_m2*cheat_percC/100

#block is kind of like site according to Figure 2a
colnames(sub2)[colnames(sub2) == 'Block'] <- 'site'
colnames(sub2)[colnames(sub2) == 'Paddock'] <- 'paddock'

sub2$study <- 'Porensky et al. 2018'
sub2$veg <- 'cheatgrass'
sub2$lat <- 40.869
sub2$long <- -116.5315

kpPorensky <- sub2[,c("yr_samp", "site", "veg", "AGB_g_m2", "AGBC_g_m2", "lat", "long", "Month_sampled", "study", "rep", "paddock")]

bind15 <- rbind.all.columns(bind14, kpPorensky)






###bring in individual data points (means that are lacking SE or n)
ind_points <- as.data.frame(read_csv("ind_points.csv"))

#repeat rows n times where n is specified

#ind_points$ntimes <- as.numeric(ind_points$n_sampled)
#cp <- as.data.frame(lapply(ind_points, rep, ind_points$ntimes))
cp <- ind_points

#correct names of %C soils columns
colnames(cp)[colnames(cp) == 'orgsoil.C'] <- 'orgsoil%C'
colnames(cp)[colnames(cp) == 'totsoil.C'] <- 'totsoil%C'
colnames(cp)[colnames(cp) == 'orgsoil.C_SE'] <- 'orgsoil%C_SE'
colnames(cp)[colnames(cp) == 'totsoil.C_SE'] <- 'totsoil%C_SE'

cp$uniqvar <- 1:nrow(cp)
#cp <- ind_points[rep(seq_len(nrow(ind_points)), ind_points$ntimes),]
#n.times <- ind_points$n_sampled
#cp <- ind_points[rep(seq_len(nrow(ind_points)), n.times),]
#cp <- rep (ind_points[,], times = ind_points$n_sampled)
#cp <- ind_points[rep(seq(nrow(ind_points)), ind_points$n_sampled),]
#cp <- rep(ind_points, ind_points$n_sampled)

#do conversions of biomass or %C to carbon content
#AGB
cp$AGBC_g_m2 <- ifelse(cp$study == 'Thacker et al. 2009'| cp$study == 'Shinn & Thill 2002' | cp$study == 'Rickard 1985b' | cp$study == 'Diamond et al. 2012' & cp$veg == 'cheatgrass', cp$AGB_g_m2 * cheat_percC/100,
                               ifelse(cp$study == 'Driese and Reiners 1997' | cp$study == 'Rickard 1985a'  & cp$veg == 'sagebrush', cp$AGB_g_m2 * sagepercC, 
                                      ifelse(cp$study == 'Driese and Reiners 1997' | cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert', cp$AGB_g_m2 * saltpercC, cp$AGBC_g_m2)))
#AGB SE
cp$AGBC_g_m2_SE <- ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert', cp$AGB_g_m2_SE * saltpercC, cp$AGBC_g_m2_SE)


#BGB
cp$BGBC_g_m2 <- ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert' & cp$bottomdepth_cm == 30, cp$BGB_g_m2 * saltBGBpercC230, 
                       ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert' & cp$bottomdepth_cm == 60, cp$BGB_g_m2 * saltBGBpercC3060, 
                              ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert' & cp$bottomdepth_cm == 90, cp$BGB_g_m2 * saltBGBpercC6090, cp$BGBC_g_m2)))


#BGB SE
#don't need this; don't have n for Bjerregaard, so can't calculate SE
#cp$BGBC_g_m2_SE <- ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert', cp$BGB_g_m2_SE * saltBGBpercC, cp$BGBC_g_m2_SE)

#litter
cp$litterC_g_m2 <- ifelse(cp$study == 'Rickard 1985b' & cp$veg == 'cheatgrass', cp$litter_g_m2 * cheatlitterpercC,
                       ifelse(cp$study == 'Rickard 1985a'  & cp$veg == 'sagebrush', cp$litter_g_m2 * sagelitterpercC, 
                              ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert', cp$litter_g_m2 * saltlitterpercC, cp$litterC_g_m2)))

#litter SE
cp$litterC_g_m2_SE <- ifelse(cp$study == 'Rickard 1985b' & cp$veg == 'cheatgrass', cp$litter_g_m2_SE * cheatlitterpercC,
                          ifelse(cp$study == 'Rickard 1985a'  & cp$veg == 'sagebrush', cp$litter_g_m2_SE * sagelitterpercC, 
                                 ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$veg == 'salt_desert', cp$litter_g_m2_SE * saltlitterpercC, cp$litterC_g_m2_SE)))

#applying avg BDs to studies that are missing BD
cp$BD_g_cm3 <- ifelse(cp$study == 'Saetre and Stark 2005' & cp$bottomdepth_cm == 10, BD010, cp$BD_g_cm3)


cp$BD_g_cm3 <- ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$bottomdepth_cm == 2, BD010, 
                      ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$bottomdepth_cm == 30, BD020,
                             ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$bottomdepth_cm == 60, BD60, 
                                    ifelse(cp$study == 'Bjerregaard et al. 1984' & cp$bottomdepth_cm == 90, BD90, cp$BD_g_cm3))))

#make sure thick is calculated correctly
cp$thick <- cp$bottomdepth_cm - cp$topdepth_cm

#calculating soil C content
#total
cp$totsoilC_g_m2 <- ifelse(cp$study == 'Boulton et al. 1993', cp$totsoilC_g_m2, cp$`totsoil%C` * cp$BD_g_cm3 *cp$thick *100)
cp$totsoilC_g_m2_SE <- cp$`totsoil%C_SE` * cp$BD_g_cm3 *cp$thick *100

#organic
cp$orgsoilC_g_m2 <- cp$`orgsoil%C` * cp$BD_g_cm3 *cp$thick *100
cp$orgsoilC_g_m2_SE <- cp$`orgsoil%C_SE` * cp$BD_g_cm3 *cp$thick *100


bind16 <- rbind.all.columns(bind15, cp)

#write.csv(bind16, file = "rawsonlypre.csv")




###########################################
#bring in study means
#setwd("/Users/rana7082/Dropbox/C_fire_invasives_R/data/")
studymeans <- as.data.frame(read_csv("study_means.csv"))

studymeans$thick <- studymeans$bottomdepth_cm - studymeans$topdepth_cm
is.numeric(studymeans$thick)

#need to write this section of code to do the following...
#if AGB or litter biomass is biomass only, not carbon, then use mean values from above to calculate...
studymeans$AGBC_g_m2 <- ifelse(studymeans$study == 'Davies et al. 2009'| studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'cheatgrass', studymeans$AGB_g_m2 * cheat_percC/100,
                               ifelse(studymeans$study == 'Pearson 1965'  & studymeans$veg == 'sagebrush', studymeans$AGB_g_m2 * sagepercC, 
                                      ifelse(studymeans$study == 'Witwicki et al. 2013' | studymeans$study == 'Veblen et al. 2015' & studymeans$veg == 'sagecheat', studymeans$AGB_g_m2 * sagecheatpercC, studymeans$AGBC_g_m2)))

#do the same for SE here
studymeans$AGBC_g_m2_SE <- ifelse(studymeans$study == 'Davies et al. 2009'| studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'cheatgrass', studymeans$AGB_g_m2_SE * cheat_percC/100,
                                  ifelse(studymeans$study == 'Pearson 1965'  & studymeans$veg == 'sagebrush', studymeans$AGB_g_m2_SE * sagepercC, 
                                         ifelse(studymeans$study == 'Witwicki et al. 2013' | studymeans$study == 'Veblen et al. 2015' & studymeans$veg == 'sagecheat', studymeans$AGB_g_m2_SE * sagecheatpercC, studymeans$AGBC_g_m2_SE)))

#BGB here
studymeans$BGBC_g_m2 <- ifelse(studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'cheatgrass', studymeans$BGB_g_m2 * cheatBGBpercC010,
                               ifelse(studymeans$study == 'Pearson 1965' | studymeans$study == 'Rickard 1985a' & studymeans$veg == 'sagebrush', studymeans$BGB_g_m2 * sageBGBpercC1030, 
                                      ifelse(studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'sagecheat', studymeans$BGB_g_m2 * sagecheatBGBpercC010, studymeans$BGBC_g_m2)))

studymeans$BGBC_g_m2_SE <- ifelse(studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'cheatgrass', studymeans$BGB_g_m2_SE * cheatBGBpercC010,
                                  ifelse(studymeans$study == 'Pearson 1965' | studymeans$study == 'Rickard 1985a' & studymeans$veg == 'sagebrush', studymeans$BGB_g_m2_SE * sageBGBpercC1030, 
                                         ifelse(studymeans$study == 'Witwicki et al. 2013' & studymeans$veg == 'sagecheat', studymeans$BGB_g_m2_SE * sagecheatBGBpercC010, studymeans$BGBC_g_m2_SE)))


#do the same for litter mean and SE here
#there are no litter mass samples that need to be converted to litter C!!


#apply mean BDs from the closest depth to studymeans data (may not be exact depth match)
BD010 <- 1.422
BD1020 <- 1.35
BD020 <- 1.386
BD60 <- 1.562
BD90 <- BD60

studymeans$totsoilC_g_m2 <- ifelse (studymeans$study == 'Johnson et al. 2011', studymeans$`totsoil%C` * BD020 *studymeans$thick *100, 
                                    ifelse(studymeans$study == 'Bechtold and Inouye 2007', studymeans$`totsoil%C` * BD010 *studymeans$thick *100, 
                                           ifelse(studymeans$study == 'Davies et al. 2007', studymeans$`totsoil%C` * BD020 *studymeans$thick *100,
                                                  ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 10, studymeans$`totsoil%C` * BD1020 *studymeans$thick *100, 
                                                         ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 95, studymeans$`totsoil%C` * BD90 *studymeans$thick *100, studymeans$totsoilC_g_m2)))))

studymeans$totsoilC_g_m2_SE <- ifelse (studymeans$study == 'Johnson et al. 2011', studymeans$`totsoil%C_SE` * BD020 *studymeans$thick *100, 
                                       ifelse(studymeans$study == 'Bechtold and Inouye 2007', studymeans$`totsoil%C_SE` * BD010 *studymeans$thick *100, 
                                              ifelse(studymeans$study == 'Davies et al. 2007', studymeans$`totsoil%C_SE` * BD020 *studymeans$thick *100,
                                                     ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 10, studymeans$`totsoil%C_SE` * BD1020 *studymeans$thick *100, 
                                                            ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 95, studymeans$`totsoil%C_SE` * BD90 *studymeans$thick *100, studymeans$totsoilC_g_m2_SE)))))


studymeans$orgsoilC_g_m2 <- ifelse(studymeans$study == 'Acker 1992', studymeans$`orgsoil%C` * BD010 *studymeans$thick *100, 
                                   ifelse(studymeans$study == 'Gasch et al. 2016', studymeans$`orgsoil%C` * studymeans$BD_g_cm3 *studymeans$thick *100, 
                                          ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 10, studymeans$`orgsoil%C` * BD1020 *studymeans$thick *100, 
                                                 ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 95, studymeans$`orgsoil%C` * BD90 *studymeans$thick *100, studymeans$orgsoilC_g_m2))))

studymeans$orgsoilC_g_m2_SE <- ifelse(studymeans$study == 'Acker 1992', studymeans$`orgsoil%C_SE` * BD010 *studymeans$thick *100, 
                                      ifelse(studymeans$study == 'Gasch et al. 2016', studymeans$`orgsoil%C_SE` * studymeans$BD_g_cm3 *studymeans$thick *100, 
                                             ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 10, studymeans$`orgsoil%C_SE` * BD1020 *studymeans$thick *100, 
                                                    ifelse(studymeans$study == 'Sorensen et al. 2013' & studymeans$topdepth_cm == 95, studymeans$`orgsoil%C_SE` * BD90 *studymeans$thick *100, studymeans$orgsoilC_g_m2_SE))))



###
#Diamond and Bjerregaard studies had C data in addition to biomass data
#studymeans$AGBC_g_m2 <- ifelse(studymeans$study == 'Diamond et al. 2012' | studymeans$study == 'Bjerregaard et al. 1984', studymeans$AGBC_g_m2, studymeans$AGB_g_m2 * cheat_percC / 100)
#studymeans$AGBC_g_m2_SE <- ifelse(studymeans$study == 'Diamond et al. 2012' | studymeans$study == 'Bjerregaard et al. 1984' ,studymeans$AGBC_g_m2_SE, studymeans$AGB_g_m2_SE * cheat_percC / 100)

#update these numbers below now that more studies have been added
#studymeans$litterC_g_m2 <- studymeans$litter_g_m2 * cheatlitterpercC
#studymeans$litterC_g_m2_SE <- studymeans$litter_g_m2_SE * cheatlitterpercC
###

head(studymeans)
studymeans


#bind13 = read_csv("bind13.csv")
bind17 <- rbind.all.columns(bind16, studymeans)


#making sure all numeric fields are numeric
str(bind17)

bind17$litterC_g_m2 <- as.numeric(bind17$litterC_g_m2)
bind17$bottomdepth_cm <- as.numeric(bind17$bottomdepth_cm)
bind17$elevation <- as.numeric(bind17$elevation)
#bind17$cheat_cover <- as.numeric(bind17$cheat_cover)
bind17$BGB_g_m2 <- as.numeric(bind17$BGB_g_m2)
bind17$BGB_g_m2_SE <- as.numeric(bind17$BGB_g_m2_SE)
bind17$litter_g_m2 <- as.numeric(bind17$litter_g_m2)
bind17$litter_g_m2_SE <- as.numeric(bind17$litter_g_m2_SE)
bind17$n_sampled <- as.numeric(bind17$n_sampled)
bind17$totsoilC_g_m2_SE <- as.numeric(bind17$totsoilC_g_m2_SE)

#bind17$X1 <- as.factor(rownames(bind17))


unique(bind17$yr_samp)
unique(bind17$n_sampled)

names(bind17)


colnames(bind17)[colnames(bind17) == 'orgsoil%C'] <- 'orgsoilperC'
colnames(bind17)[colnames(bind17) == 'totsoil%C'] <- 'totsoilperC'
colnames(bind17)[colnames(bind17) == 'orgsoil%C_SE'] <- 'orgsoilperC_SE'
colnames(bind17)[colnames(bind17) == 'totsoil%C_SE'] <- 'totsoilperC_SE'
colnames(bind17)[colnames(bind17) == 'litter%C'] <- 'litterperC'

bind17$X1 <- as.factor(rownames(bind17))

unique(bind17$n_sampled)

#if n_sampled = NA, make it a 1
#bind17$n_sampled <- ifelse(bind17$n_sampled != NA, bind17$n_sampled, 1)
bind17[["n_sampled"]][is.na(bind17[["n_sampled"]])] <- 1

summary(bind17$n_sampled)
names(bind17)


write.csv(bind17, file = "alldata.csv", row.names = FALSE)
