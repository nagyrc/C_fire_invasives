#Data prep for meta-analysis
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid dataframe
#studyid = read_csv("studyid.csv")
siwf = read_csv("siwf.csv")

joiny2 = read_csv("joiny2.csv")

#unique(joiny2$masterlyb) #crap this field is messed up
#####
#rawsonly <- siwf %>%
#filter(!study %in% smeans) %>%
#mutate(Study_ID = factor(Study_ID))

#write.csv(rawsonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

#rawsonly <- as.data.frame(read_csv("rawsonly.csv"))

#unique(rawsonly$Study_ID)
#rawsonly$Study_ID <- as.factor(rawsonly$Study_ID)
#is.numeric(rawsonly$Study_ID)

#unique(rawsonly$veg)
#unique(rawsonly$study)
#unique(rawsonly$pool)

#ccc <- unique(rawsonly[c("Study_ID", "veg", "study", "pool")])

siwf4 <- siwf %>%
  filter(veg != 'salt_desert')

#which studies are paired?
ccz <- siwf4 %>%
  dplyr::group_by(study) %>%
  summarise(numveg = n_distinct(veg,  na.rm = TRUE))

ccz$geometry <- NULL
write.csv(ccz, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/pairedunpaired.csv", row.names = FALSE)

#ccx <- siwf4 %>%
  #group_by(study, pool) %>%
  #summarise(numveg = n_distinct(veg,  na.rm = TRUE))
#ccx$geometry <- NULL
###############
#test a few
test1 <- siwf4 %>%
  filter(study == 'Witwicki et al. 2013')
unique(test1$veg) #2

test2 <- siwf4 %>%
  filter(study == 'Stark et al. 2015')
unique(test2$veg) #3

test3 <- siwf4 %>%
  filter(study == 'Peschel et al. 2015')
unique(test3$veg) #1
#these look good
###############
cct <- siwf4 %>%
  group_by(study) %>%
  summarise(numveg = n_distinct(veg,  na.rm = TRUE)) %>%
  filter(numveg > 1) 
cct$geometry <- NULL

studies <- cct$study
studies

#to start matching up studyids...then look below for more clues on which ones to match
ccc <- siwf4 %>% 
  filter(study %in% studies) %>%
  distinct(Study_ID, veg, study, pool, .keep_all = FALSE)

ccc$geometry <- NULL
write.csv(ccc, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/ccc.csv", row.names = FALSE)

siwf5 <- siwf %>%
  filter(veg != 'salt_desert') %>%
  filter(study %in% studies) 

unique(siwf5$masterlyb)
is.factor(siwf5$masterlyb)
unique(siwf$timesincefire)
#checky <- siwf5 %>%
  #filter(study == "Mahood et al. unpub1") %>%
  #distinct(Study_ID)

###
#to update pairs of studyids in paired_StudyIDs3
#NOTE: the studies below marked as summary data can either be included or not; these are simulated raw data
Blankcheck <- siwf5 %>% 
  filter(study == 'Blank and Norton 2006') %>%
  distinct(Study_ID, veg, pool, site, bottomdepth_cm, .keep_all = FALSE)

Bradleycheck <- siwf5 %>% 
  filter(study == 'Bradley et al. 2006') %>%
  distinct(Study_ID, veg, pool, site, .keep_all = FALSE)
#get rid of 280, 281, 290, 291 if not using salt desert

Goercheck <- siwf5 %>% 
  filter(study == 'Goergen et al. 2011') %>%
  distinct(Study_ID, veg, pool, site, bottomdepth_cm, .keep_all = FALSE)

#this is summary data
Hookcheck <- siwf5 %>% 
  filter(study == 'Hooker et al. 2008') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

#this is summary data
Johncheck <- siwf5 %>% 
  filter(study == 'Johnson et al. 2011') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

#need to average across some cheatgrass sites; see code below
Mahocheck <- siwf5 %>% 
  filter(Article_ID == 'MAHO2018a') %>%
  distinct(Study_ID, veg, pool, site, lat, long, .keep_all = FALSE)

st_geometry(Mahocheck) = NULL
write.csv(Mahocheck, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/Mahood_pairs_check.csv")



Nortcheck <- siwf5 %>% 
  filter(Article_ID == 'NORT2008') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

Nortcheck2012 <- siwf5 %>% 
  filter(Article_ID == 'NORT2012') %>%
  distinct(Study_ID, veg, pool, yr_samp, .keep_all = FALSE)

Starcheck <- siwf5 %>% 
  filter(Article_ID == 'STAR2015') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

Svejcheck <- siwf5 %>% 
  filter(Article_ID == 'SVEJ2001') %>%
  distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

Webecheck <- siwf5 %>% 
  filter(Article_ID == 'WEBE2015') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

Boulcheck <- siwf5 %>% 
  filter(Article_ID == 'BOUL1993') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

#only 4 obs; double checked and this came from ind_points so this makes sense
Boulcheckpre <- siwf5 %>% 
  filter(Article_ID == 'BOUL1993')

#this is summary data
#Gascchecka <- siwf5 %>% 
  #filter(Article_ID == 'GASC2013') %>%
  #distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

#this is summary data
Witwcheck <- siwf5 %>% 
  filter(Article_ID == 'WITW2013') %>%
  distinct(Study_ID, veg, pool, keep_all = FALSE)

#this is summary data
Ackecheck <- siwf5 %>% 
  filter(Article_ID == 'ACKE1992') %>%
  distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

Banscheck <- siwf5 %>% 
  filter(Article_ID == 'BANS2014') %>%
  distinct(Study_ID, veg, pool, site, keep_all = FALSE)

st_geometry(Banscheck) = NULL
write.csv(Banscheck, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/Bansal_pairs_check.csv")

#not sure how to pair these yet
Norton2004check <- siwf5 %>% 
  filter(Article_ID == 'NORT2004') %>%
  distinct(Study_ID, veg, pool, site, topdepth_cm, bottomdepth_cm, lat, long, keep_all = FALSE)

#not sure how to pair these yet
Raucheck <- siwf5 %>% 
  filter(Article_ID == 'RAU2011') %>%
  distinct(Study_ID, veg, pool, site, lat, long, keep_all = FALSE)






#bring in paired Study_IDs
#need to update this
#pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))

###########################
#replace studyIDs in studies that need to be averaged across (Mahood, Rau, Norton et al. 2004)
is.factor(siwf5$Study_ID) #TRUE
siwf5$Study_ID <- as.factor(siwf5$Study_ID)
#creating new levels for Mahood
levels(siwf5$Study_ID) <- c(levels(siwf5$Study_ID), "6002", "6003", "6004") 
#replacing study ids in Mahood
siwf5$Study_ID[siwf5$Study_ID == '665'|siwf5$Study_ID == '670'|siwf5$Study_ID == '675'] <- '6002'
siwf5$Study_ID[siwf5$Study_ID == '640'|siwf5$Study_ID == '645'] <- '6003'
siwf5$Study_ID[siwf5$Study_ID == '610'|siwf5$Study_ID == '615'|siwf5$Study_ID == '620'] <- '6004'

#creating new levels for Rau
levels(siwf5$Study_ID) <- c(levels(siwf5$Study_ID), "3000", "3001", "3002", "3003") 
#replacing study ids in Rau
siwf5$Study_ID[siwf5$Study_ID == '1332'|siwf5$Study_ID == '1337'] <- '3000'
siwf5$Study_ID[siwf5$Study_ID == '1334'|siwf5$Study_ID == '1339'] <- '3001'
siwf5$Study_ID[siwf5$Study_ID == '1322'|siwf5$Study_ID == '1317'] <- '3002'
siwf5$Study_ID[siwf5$Study_ID == '1324'|siwf5$Study_ID == '1319'] <- '3003'



#creating new levels for Norton et al. 2004
levels(siwf5$Study_ID) <- c(levels(siwf5$Study_ID), "4000", "4001", "4002", "4003", "4004", "4005", "4006", "4007") 
levels(siwf5$Study_ID) <- c(levels(siwf5$Study_ID), "5000", "5001", "5002", "5003", "5004", "5005", "5006", "5007") 

#replacing study ids in Norton et al. 2004
siwf5$Study_ID[siwf5$Study_ID == '999'|siwf5$Study_ID == '1094'|siwf5$Study_ID == '789'|siwf5$Study_ID == '1159'|siwf5$Study_ID == '894'|siwf5$Study_ID == '964'|siwf5$Study_ID == '844'] <- '4000'
siwf5$Study_ID[siwf5$Study_ID == '1014'|siwf5$Study_ID == '1099'|siwf5$Study_ID == '794'|siwf5$Study_ID == '1164'|siwf5$Study_ID == '899'|siwf5$Study_ID == '969'|siwf5$Study_ID == '849'] <- '4001'
siwf5$Study_ID[siwf5$Study_ID == '1019'|siwf5$Study_ID == '1104'|siwf5$Study_ID == '799'|siwf5$Study_ID == '1169'|siwf5$Study_ID == '904'|siwf5$Study_ID == '974'|siwf5$Study_ID == '844'] <- '4002'
siwf5$Study_ID[siwf5$Study_ID == '1024'|siwf5$Study_ID == '1109'|siwf5$Study_ID == '804'|siwf5$Study_ID == '1174'|siwf5$Study_ID == '909'|siwf5$Study_ID == '979'|siwf5$Study_ID == '854'] <- '4003'
siwf5$Study_ID[siwf5$Study_ID == '1029'|siwf5$Study_ID == '1114'|siwf5$Study_ID == '809'|siwf5$Study_ID == '1179'|siwf5$Study_ID == '914'|siwf5$Study_ID == '859'] <- '4004'
siwf5$Study_ID[siwf5$Study_ID == '1034'|siwf5$Study_ID == '1119'|siwf5$Study_ID == '1184'|siwf5$Study_ID == '919'] <- '4005'
siwf5$Study_ID[siwf5$Study_ID == '1039'|siwf5$Study_ID == '1124'|siwf5$Study_ID == '924'|siwf5$Study_ID == '864'] <- '4006'
siwf5$Study_ID[siwf5$Study_ID == '1044'|siwf5$Study_ID == '929'] <- '4007'
siwf5$Study_ID[siwf5$Study_ID == '1054'|siwf5$Study_ID == '1129'|siwf5$Study_ID == '814'|siwf5$Study_ID == '1189'|siwf5$Study_ID == '934'|siwf5$Study_ID == '984'|siwf5$Study_ID == '869'] <- '5000'
siwf5$Study_ID[siwf5$Study_ID == '1059'|siwf5$Study_ID == '1134'|siwf5$Study_ID == '819'|siwf5$Study_ID == '1194'|siwf5$Study_ID == '939'|siwf5$Study_ID == '989'|siwf5$Study_ID == '874'] <- '5001'
siwf5$Study_ID[siwf5$Study_ID == '1064'|siwf5$Study_ID == '1139'|siwf5$Study_ID == '824'|siwf5$Study_ID == '1199'|siwf5$Study_ID == '944'|siwf5$Study_ID == '994'|siwf5$Study_ID == '879'] <- '5002'
siwf5$Study_ID[siwf5$Study_ID == '1069'|siwf5$Study_ID == '1144'|siwf5$Study_ID == '829'|siwf5$Study_ID == '1204'|siwf5$Study_ID == '949'|siwf5$Study_ID == '884'] <- '5003'
siwf5$Study_ID[siwf5$Study_ID == '1074'|siwf5$Study_ID == '1149'|siwf5$Study_ID == '834'|siwf5$Study_ID == '954'|siwf5$Study_ID == '889'] <- '5004'
siwf5$Study_ID[siwf5$Study_ID == '1079'|siwf5$Study_ID == '1154'|siwf5$Study_ID == '839'|siwf5$Study_ID == '959'] <- '5005'
siwf5$Study_ID[siwf5$Study_ID == '1084'] <- '5006'
siwf5$Study_ID[siwf5$Study_ID == '1089'] <- '5007'
#checky <- siwf5 %>%
  #filter(study == "Mahood et al. unpub1") %>%
  #distinct(Study_ID)


#updated 3/3/20
pairs <- as.data.frame(read_csv("paired_studyIDs3.csv"))
pairs <- pairs %>%
  dplyr::select(-pool, -Article_ID) %>%
  mutate(pairnum = 1:nrow(pairs))

pairscheat <- pairs %>%
  dplyr::select(cheatgrass_studyID) %>%
  mutate(Study_ID = as.factor(cheatgrass_studyID)) %>%
  dplyr::select(-cheatgrass_studyID)

pairssage <- pairs %>%
  dplyr::select(sagebrush_studyID) %>%
  mutate(Study_ID = as.factor(sagebrush_studyID)) %>%
  dplyr::select(-sagebrush_studyID)

pairssagecheat <- pairs %>%
  dplyr::select(sagecheat_studyID) %>%
  mutate(Study_ID = as.factor(sagecheat_studyID)) %>%
  dplyr::select(-sagecheat_studyID)

#############
#not quite right; need to remove NAs from the names
#StudyIDp <- pairs %>%
  #mutate(Study_IDp = paste(cheatgrass_studyID, sagecheat_studyID, sagebrush_studyID, sep = '_'))







####
#THE CODE BELOW USES ONLY RAW DATA WITH THE MEANS COUNTED ONCE
####


###
pairscheat$Study_ID <- as.factor(pairscheat$Study_ID)
pcheat <- pairscheat %>%
  filter(!is.na(Study_ID))

siwf5$Study_ID <- as.factor(siwf5$Study_ID)
pcheat2 <- semi_join(pcheat, siwf5)
pcheat2

joiny2$Study_ID <- as.factor(joiny2$Study_ID)
pcheat3 <- semi_join(pcheat, joiny2)
pcheat3


#select only the paired studies from raw data
siwf5cheat <- siwf5[siwf5$Study_ID %in% pcheat2$Study_ID,]

#select the paired studies from all data
joiny2cheat <- joiny2[joiny2$Study_ID %in% pcheat3$Study_ID,]



###
psage <- pairssage %>%
  filter(!is.na(Study_ID))

psage2 <- semi_join(psage, siwf5)
psage2

psage3 <- semi_join(psage, joiny2)
psage3

#select only the paired studies from raw data
siwf5sage <- siwf5[siwf5$Study_ID %in% psage2$Study_ID,]

#select the paired studies from all data
joiny2sage <- joiny2[joiny2$Study_ID %in% psage3$Study_ID,]



###
psagecheat <- pairssagecheat %>%
  filter(!is.na(Study_ID))

psagecheat2 <- semi_join(psagecheat, siwf5)
psagecheat2

psagecheat3 <- semi_join(psagecheat, joiny2)
psagecheat3

#select only the paired studies from raw data
siwf5sagecheat <- siwf5[siwf5$Study_ID %in% psagecheat2$Study_ID,]

#select the paired studies from all data
joiny2sagecheat <- joiny2[joiny2$Study_ID %in% psagecheat3$Study_ID,]


####


#####

###RAW DATA ONLY

#step 1 for cheat
cheatpmeans <- siwf5cheat %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(cheatpmeans) = NULL

#step 2 for cheat
#adding code to average across Mahood cheatgrass sites for the pairs and assign new Study_IDs
#cheatpmeansMahood <- rawsonlycheat %>%
  #filter(Article_ID == "MAHO2018a") %>%
  #group_by(site, veg) %>%
  #dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  #mutate(se = sqrt(var)/sqrt(n)) %>%
  #mutate(Study_ID = c(2001: 2005))

#st_geometry(cheatpmeans) = NULL


#sage
sagepmeans <- siwf5sage %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagepmeans) = NULL


#sagecheat
sagecheatpmeans <- siwf5sagecheat %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagecheatpmeans) = NULL




###IF USING SIMULATED DATA TOO
#Run these instead of cheatpmeans, sagepmeans, and sagecheatpmeans
#adding code that includes summary data

#cheat
cheatpmeanssum <- joiny2cheat %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(cheatpmeans) = NULL

#sage
sagepmeanssum <- joiny2sage %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagepmeans) = NULL

#sagecheat
sagecheatpmeanssum <- joiny2sagecheat %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagecheatpmeans) = NULL


###
#this makes Table S3; either with or without simulated raw data
write.csv(cheatpmeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/cheatpmeans.csv")
write.csv(cheatpmeanssum, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/cheatpmeanssum.csv")
write.csv(sagepmeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/sagepmeans.csv")
write.csv(sagepmeanssum, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/sagepmeanssum.csv")
write.csv(sagecheatpmeans, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/sagecheatpmeans.csv")
write.csv(sagecheatpmeanssum, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/sagecheatpmeanssum.csv")








####
#now it's just a matter of putting these values in the correct order in the table so that the pairs line up
#manually pasted in the correct spots in a .csv file

#bring that file in here
rawspmeans <- as.data.frame(read_csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeans.csv"))
#rawspmeanssum <- as.data.frame(read_csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeanssum.csv"))


#need a table of studyid and depth_cat
stepzz1 <- siwf5 %>%
  filter(pool == 'totsoilC_g_m2'|pool == 'orgsoilC_g_m2') %>%
  mutate(depth_cat = ifelse(bottomdepth_cm <20, 'shallow', ifelse(bottomdepth_cm == 20, 'mid', 'deep'))) %>%
  distinct(Study_ID, depth_cat, .keep_all = T)
stepzz1$geometry <- NULL
#paste in these 195 observations (note, some of these are old studyIDs)

#need cheatfires only per pair; diff veg could have burned in diff years
stepzz2 <- siwf5 %>%
  filter(veg == 'cheatgrass') %>%
  distinct(Study_ID, timesincefire, .keep_all = T)
stepzz2$geometry <- NULL
#paste in these 114 observations (note some of these are old studyIDs)
###################################################
#need to replace studyIDs in stepzz1 and stepzz2 for Rau, Mahood and Norton 2004
stepzz1$Study_ID[stepzz1$Study_ID == '665'|stepzz1$Study_ID == '670'|stepzz1$Study_ID == '675'] <- '6002'
stepzz1$Study_ID[stepzz1$Study_ID == '640'|stepzz1$Study_ID == '645'] <- '6003'
stepzz1$Study_ID[stepzz1$Study_ID == '610'|stepzz1$Study_ID == '615'|stepzz1$Study_ID == '620'] <- '6004'

stepzz1$Study_ID[stepzz1$Study_ID == '1332'|stepzz1$Study_ID == '1337'] <- '3000'
stepzz1$Study_ID[stepzz1$Study_ID == '1334'|stepzz1$Study_ID == '1339'] <- '3001'
stepzz1$Study_ID[stepzz1$Study_ID == '1322'|stepzz1$Study_ID == '1317'] <- '3002'
stepzz1$Study_ID[stepzz1$Study_ID == '1324'|stepzz1$Study_ID == '1319'] <- '3003'

stepzz1$Study_ID[stepzz1$Study_ID == '999'|stepzz1$Study_ID == '1094'|stepzz1$Study_ID == '789'|stepzz1$Study_ID == '1159'|stepzz1$Study_ID == '894'|stepzz1$Study_ID == '964'|stepzz1$Study_ID == '844'] <- '4000'
stepzz1$Study_ID[stepzz1$Study_ID == '1014'|stepzz1$Study_ID == '1099'|stepzz1$Study_ID == '794'|stepzz1$Study_ID == '1164'|stepzz1$Study_ID == '899'|stepzz1$Study_ID == '969'|stepzz1$Study_ID == '849'] <- '4001'
stepzz1$Study_ID[stepzz1$Study_ID == '1019'|stepzz1$Study_ID == '1104'|stepzz1$Study_ID == '799'|stepzz1$Study_ID == '1169'|stepzz1$Study_ID == '904'|stepzz1$Study_ID == '974'|stepzz1$Study_ID == '844'] <- '4002'
stepzz1$Study_ID[stepzz1$Study_ID == '1024'|stepzz1$Study_ID == '1109'|stepzz1$Study_ID == '804'|stepzz1$Study_ID == '1174'|stepzz1$Study_ID == '909'|stepzz1$Study_ID == '979'|stepzz1$Study_ID == '854'] <- '4003'
stepzz1$Study_ID[stepzz1$Study_ID == '1029'|stepzz1$Study_ID == '1114'|stepzz1$Study_ID == '809'|stepzz1$Study_ID == '1179'|stepzz1$Study_ID == '914'|stepzz1$Study_ID == '859'] <- '4004'
stepzz1$Study_ID[stepzz1$Study_ID == '1034'|stepzz1$Study_ID == '1119'|stepzz1$Study_ID == '1184'|stepzz1$Study_ID == '919'] <- '4005'
stepzz1$Study_ID[stepzz1$Study_ID == '1039'|stepzz1$Study_ID == '1124'|stepzz1$Study_ID == '924'|stepzz1$Study_ID == '864'] <- '4006'
stepzz1$Study_ID[stepzz1$Study_ID == '1044'|stepzz1$Study_ID == '929'] <- '4007'
stepzz1$Study_ID[stepzz1$Study_ID == '1054'|stepzz1$Study_ID == '1129'|stepzz1$Study_ID == '814'|stepzz1$Study_ID == '1189'|stepzz1$Study_ID == '934'|stepzz1$Study_ID == '984'|stepzz1$Study_ID == '869'] <- '5000'
stepzz1$Study_ID[stepzz1$Study_ID == '1059'|stepzz1$Study_ID == '1134'|stepzz1$Study_ID == '819'|stepzz1$Study_ID == '1194'|stepzz1$Study_ID == '939'|stepzz1$Study_ID == '989'|stepzz1$Study_ID == '874'] <- '5001'
stepzz1$Study_ID[stepzz1$Study_ID == '1064'|stepzz1$Study_ID == '1139'|stepzz1$Study_ID == '824'|stepzz1$Study_ID == '1199'|stepzz1$Study_ID == '944'|stepzz1$Study_ID == '994'|stepzz1$Study_ID == '879'] <- '5002'
stepzz1$Study_ID[stepzz1$Study_ID == '1069'|stepzz1$Study_ID == '1144'|stepzz1$Study_ID == '829'|stepzz1$Study_ID == '1204'|stepzz1$Study_ID == '949'|stepzz1$Study_ID == '884'] <- '5003'
stepzz1$Study_ID[stepzz1$Study_ID == '1074'|stepzz1$Study_ID == '1149'|stepzz1$Study_ID == '834'|stepzz1$Study_ID == '954'|stepzz1$Study_ID == '889'] <- '5004'
stepzz1$Study_ID[stepzz1$Study_ID == '1079'|stepzz1$Study_ID == '1154'|stepzz1$Study_ID == '839'|stepzz1$Study_ID == '959'] <- '5005'
stepzz1$Study_ID[stepzz1$Study_ID == '1084'] <- '5006'
stepzz1$Study_ID[stepzz1$Study_ID == '1089'] <- '5007'

stepzz2$Study_ID[stepzz2$Study_ID == '665'|stepzz2$Study_ID == '670'|stepzz2$Study_ID == '675'] <- '6002'
stepzz2$Study_ID[stepzz2$Study_ID == '640'|stepzz2$Study_ID == '645'] <- '6003'
stepzz2$Study_ID[stepzz2$Study_ID == '610'|stepzz2$Study_ID == '615'|stepzz2$Study_ID == '620'] <- '6004'

stepzz2$Study_ID[stepzz2$Study_ID == '1332'|stepzz2$Study_ID == '1337'] <- '3000'
stepzz2$Study_ID[stepzz2$Study_ID == '1334'|stepzz2$Study_ID == '1339'] <- '3001'
stepzz2$Study_ID[stepzz2$Study_ID == '1322'|stepzz2$Study_ID == '1317'] <- '3002'
stepzz2$Study_ID[stepzz2$Study_ID == '1324'|stepzz2$Study_ID == '1319'] <- '3003'

stepzz2$Study_ID[stepzz2$Study_ID == '999'|stepzz2$Study_ID == '1094'|stepzz2$Study_ID == '789'|stepzz2$Study_ID == '1159'|stepzz2$Study_ID == '894'|stepzz2$Study_ID == '964'|stepzz2$Study_ID == '844'] <- '4000'
stepzz2$Study_ID[stepzz2$Study_ID == '1014'|stepzz2$Study_ID == '1099'|stepzz2$Study_ID == '794'|stepzz2$Study_ID == '1164'|stepzz2$Study_ID == '899'|stepzz2$Study_ID == '969'|stepzz2$Study_ID == '849'] <- '4001'
stepzz2$Study_ID[stepzz2$Study_ID == '1019'|stepzz2$Study_ID == '1104'|stepzz2$Study_ID == '799'|stepzz2$Study_ID == '1169'|stepzz2$Study_ID == '904'|stepzz2$Study_ID == '974'|stepzz2$Study_ID == '844'] <- '4002'
stepzz2$Study_ID[stepzz2$Study_ID == '1024'|stepzz2$Study_ID == '1109'|stepzz2$Study_ID == '804'|stepzz2$Study_ID == '1174'|stepzz2$Study_ID == '909'|stepzz2$Study_ID == '979'|stepzz2$Study_ID == '854'] <- '4003'
stepzz2$Study_ID[stepzz2$Study_ID == '1029'|stepzz2$Study_ID == '1114'|stepzz2$Study_ID == '809'|stepzz2$Study_ID == '1179'|stepzz2$Study_ID == '914'|stepzz2$Study_ID == '859'] <- '4004'
stepzz2$Study_ID[stepzz2$Study_ID == '1034'|stepzz2$Study_ID == '1119'|stepzz2$Study_ID == '1184'|stepzz2$Study_ID == '919'] <- '4005'
stepzz2$Study_ID[stepzz2$Study_ID == '1039'|stepzz2$Study_ID == '1124'|stepzz2$Study_ID == '924'|stepzz2$Study_ID == '864'] <- '4006'
stepzz2$Study_ID[stepzz2$Study_ID == '1044'|stepzz2$Study_ID == '929'] <- '4007'
stepzz2$Study_ID[stepzz2$Study_ID == '1054'|stepzz2$Study_ID == '1129'|stepzz2$Study_ID == '814'|stepzz2$Study_ID == '1189'|stepzz2$Study_ID == '934'|stepzz2$Study_ID == '984'|stepzz2$Study_ID == '869'] <- '5000'
stepzz2$Study_ID[stepzz2$Study_ID == '1059'|stepzz2$Study_ID == '1134'|stepzz2$Study_ID == '819'|stepzz2$Study_ID == '1194'|stepzz2$Study_ID == '939'|stepzz2$Study_ID == '989'|stepzz2$Study_ID == '874'] <- '5001'
stepzz2$Study_ID[stepzz2$Study_ID == '1064'|stepzz2$Study_ID == '1139'|stepzz2$Study_ID == '824'|stepzz2$Study_ID == '1199'|stepzz2$Study_ID == '944'|stepzz2$Study_ID == '994'|stepzz2$Study_ID == '879'] <- '5002'
stepzz2$Study_ID[stepzz2$Study_ID == '1069'|stepzz2$Study_ID == '1144'|stepzz2$Study_ID == '829'|stepzz2$Study_ID == '1204'|stepzz2$Study_ID == '949'|stepzz2$Study_ID == '884'] <- '5003'
stepzz2$Study_ID[stepzz2$Study_ID == '1074'|stepzz2$Study_ID == '1149'|stepzz2$Study_ID == '834'|stepzz2$Study_ID == '954'|stepzz2$Study_ID == '889'] <- '5004'
stepzz2$Study_ID[stepzz2$Study_ID == '1079'|stepzz2$Study_ID == '1154'|stepzz2$Study_ID == '839'|stepzz2$Study_ID == '959'] <- '5005'
stepzz2$Study_ID[stepzz2$Study_ID == '1084'] <- '5006'
stepzz2$Study_ID[stepzz2$Study_ID == '1089'] <- '5007'

stepzz1brief <- stepzz1 %>%
  dplyr::select(Study_ID, depth_cat)

stepzz2brief <- stepzz2 %>%
  dplyr::select(Study_ID, timesincefire)



#IS THERE ANY WAY TO DO THIS OTHER THAN MANUALLY?



#bring in file that has 2 categories pasted in manually
rawspmeanscat <- as.data.frame(read_csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeanscat.csv"))
#rawspmeanssumcat <- as.data.frame(read_csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeanssumcat.csv"))


#change this if using summary data also
dq2 <- rawspmeanscat
#dq2 <- rawspmeanssum








###################################################################
#I DONT THINK I NEED ANY OF THE CODE BELOW
#TO CREATE SOIL DEPTH CATEGORY
#pairspool <- as.data.frame(read_csv("paired_studyIDs3.csv"))
#check depths on soil pools
#check <- pairspool %>%
  #filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") 


#use pcheat2, psage2, psagecheat3 as lists of studyids
#cheatsub <- siwf5 %>%
  #filter(Study_ID %in% pcheat2$Study_ID) 
#is.factor(pcheat2$Study_ID) #TRUE
#is.factor(siwf5$Study_ID) #TRUE

#cheatsub$Study_ID <- as.numeric(cheatsub$Study_ID) 
#cheatsub$masterlyb <- as.numeric(cheatsub$masterlyb) 
#checkzz <- unique(cheatsub[c("Study_ID", "topdepth_cm", "bottomdepth_cm")])

#sagesub <- siwf5 %>%
  #filter(Study_ID %in% psage2$Study_ID) 

#sagesub$Study_ID <- as.numeric(sagesub$Study_ID) 
#sagesub$masterlyb <- as.numeric(sagesub$masterlyb)
#checkzx <- unique(sagesub[c("Study_ID", "topdepth_cm", "bottomdepth_cm")])

#sagecheatsub <- siwf5 %>%
  #filter(Study_ID %in% psagecheat2$Study_ID) 

#sagecheatsub$Study_ID <- as.numeric(sagecheatsub$Study_ID) 
#sagecheatsub$masterlyb <- as.numeric(sagecheatsub$masterlyb)
#checkzy <- unique(sagecheatsub[c("Study_ID", "topdepth_cm", "bottomdepth_cm")])

#checkyy <- rbind (checkzz, checkzx, checkzy)
#checkyy$geometry <- NULL

#testonly <- dq2 %>%
  #semi_join(checkyy) 

#%>% mutate(ifelse(bottomdepth_cm < 20, 'shallow', ifelse(bottomdepth_cm == 20, 'mid', 'deep')))

#other checks#
#sub1 <- siwf5 %>%
  #filter(Study_ID == 414)
#unique(sub1$bottomdepth_cm) #5

#sub2 <- siwf5 %>%
  #filter(Study_ID == 35 | Study_ID == 47 | Study_ID == 36 | Study_ID == 48 | Study_ID == 37)
#unique(sub2$bottomdepth_cm) #10

#sub3 <- siwf5 %>%
  #filter(Study_ID == 549)
#unique(sub3$bottomdepth_cm) #100

#sub4 <- siwf5 %>%
  #filter(Study_ID == 560)
#unique(sub4$bottomdepth_cm) #20

####manually added this categorical variable (depth_cat: shallow, mid, deep) into rawspmeans.csv for use as a fixed effect in meta-analysis

#check2a <- check$Study_ID

#check2 <- joiny2 %>%
  #filter(Study_ID %in% check2a) 

#check3 <- unique(check2[c("Study_ID", "topdepth_cm", "bottomdepth_cm")])
#check3
#all different depth intervals; need to standardize

#shallow <- check3 %>%
  #filter(bottomdepth_cm < 20)
#31 study ids
#write.csv(shallow, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/shallowid.csv")

#shallowid <- shallow %>%
  #dplyr::select(Study_ID)


#mid <- check3 %>%
  #filter(bottomdepth_cm == 20)
#17 study ids
#write.csv(mid, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/midid.csv")

#midid <- mid %>%
  #dplyr::select(Study_ID)


#deep <- check3 %>%
  #filter(bottomdepth_cm > 20)
#14 study ids
#write.csv(deep, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/deepid.csv")

#deepid <- deep %>%
  #dplyr::select(Study_ID)


###################################################
#TO CREATE FIRE CATEGORY
#create categories of yrs since burn for fixed effect in model

#is.numeric(siwf$masterlyb) #TRUE
#is.numeric(siwf$timesincefire) #TRUE
#is.factor(siwf$Study_ID) #TRUE

#unique(siwf$Study_ID)

#siwfnum <- siwf5
#siwfnum$Study_ID <- as.numeric(siwfnum$Study_ID)
#siwfnum$masterlyb <- as.numeric(siwfnum$masterlyb)
#is.numeric(siwfnum$timesincefire) #TRUE
#is.numeric(siwfnum$masterlyb) #TRUE
#is.numeric(siwfnum$Study_ID) #TRUE
#ggg <- unique(siwfnum[c("Study_ID", "masterlyb", "timesincefire", "site", "study")])
#660 = 2001
#675, 680, 685 = 2002
#522, 524 = 2003
#620, 625, 630 =  2004
#635 = 2005


#put in  as # years since fire instead of fire category; can always change later
#use recent < 5yrs; mid 5-20 yrs; old > 20 yrs

####manually added this categorical variable (fire_cat: recentfire, midfire, oldfire) into rawspmeans.csv for use as a fixed effect in meta-analysis








#checkb <- pairs_long %>%
  #filter(!is.na(Study_ID)) 

#listy1 <- checkb$Study_ID

#check2ab <- joiny2 %>%
  #filter(Study_ID %in% listy1) 

#check3b <- unique(check2ab[c("Study_ID", "masterlyb", "yr_samp")])
#check3b

#check3b$yrssince <- check3b$yr_samp - check3b$masterlyb

#recentfire <- check3b %>%
  #filter(yrssince < 5)
#10 study ids
#write.csv(recent, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/recentfireid.csv")

#recentfireid <- recentfire %>%
  #dplyr::select(Study_ID)


#midfire <- check3b %>%
  #filter(yrssince >= 5 & yrssince <= 20)
#11 study ids
#write.csv(midfire, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/midfireid.csv")

#midfireid <- midfire %>%
  #dplyr::select(Study_ID)


#oldfire <- check3b %>%
  #filter(yrssince > 20)
#2 study ids
#write.csv(oldfire, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/oldfireid.csv")

#oldfireid <- oldfire %>%
  #dplyr::select(Study_ID)





###
####################################################
#I DON'T THINK I NEED THE CODE BELOW
joiny <- unique(rawsonly[c("Study_ID", "veg")])
st_geometry(joiny) = NULL


#summarise mean, n, variance
rawp <- rawsonly %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(rawp) = NULL

#add back in veg type
rawpj <- rawp %>%
  left_join(as.data.frame(joiny)) %>%
  dplyr::select(-geometry)

#st_geometry(rawpj) <- NULL
######


















#transpose to wide format means first
rawpjmw <- rawpj %>%
  dplyr::select(Study_ID, meanpv, veg, pool) %>%
  spread(key = veg, value = meanpv) %>%
  rename(meancheat = cheatgrass, meansagecheat = sagecheat)
  

#rename(meansage = sagebrush, meansalt = salt_desert)

#transpose to wide format n 
rawpjnw <- rawpj %>%
  dplyr::select(Study_ID, n, veg, pool) %>%
  spread(key = veg, value = n) %>%
  rename(ncheat = cheatgrass, nsagecheat = sagecheat)

#rename(nsage = sagebrush, nsalt = salt_desert)

#transpose to wide format var 
rawpjvw <- rawpj %>%
  dplyr::select(Study_ID, var, veg, pool) %>%
  spread(key = veg, value = var) %>%
  rename(varcheat = cheatgrass, varsagecheat = sagecheat)

#rename(varsage = sagebrush, varsalt = salt_desert)
  
#join the three tables together

st_geometry(rawpjmw) <- NULL
st_geometry(rawpjvw) <- NULL
st_geometry(rawpjnw) <- NULL

step1 <- rawpjmw %>%
  left_join(rawpjnw)

step2 <- step1 %>%
  left_join(rawpjvw) 

#step2$Study_ID <- NULL

coalesce_by_column <- function(df) {
  return(coalesce(step2[3], step2[4], step2[5], step2[6], step2[7], step2[8]))
}

step3 <- step2 %>%
  group_by(meancheat) %>%
  #dplyr::select(-Study_ID) %>%
  summarise_all(coalesce_by_column)


#rename(meancheat = cheatgrass, meansage = sagebrush, meansagecheat = sagecheat, meansalt = salt_desert)
#rename(ncheat = cheatgrass, nsage = sagebrush, nsagecheat = sagecheat, nsalt = salt_desert)
#rename(varcheat = cheatgrass, varsage = sagebrush, varsagecheat = sagecheat, varsalt = salt_desert)


write.csv(rawsonly4, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/rawsonly4.csv")






###################################


#create means only dataframe
#characterize burned as burned <10 yrs before sampling; using MTBS; could use other fire product
#this is using MTBS only; can include other fire products
meansonly <- siwf %>%
  filter(study %in% smeans) %>%
  group_by(study) %>%
  mutate(invaded = ifelse(veg == "cheatgrass" | veg == "sagecheat", "invaded", "native")) #%>%
#mutate(burned = ifelse(yr_samp - MTBS_DISCOVERY_YEAR < 10, "burned", "unburned"))
#109 obs

write.csv(meansonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/meansonly.csv")


####################







  