#Data prep for meta-analysis
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid dataframe
studyid = read_csv("studyid.csv")
siwf = read_csv("siwf.csv")

joiny2 = read_csv("joiny2.csv")
#####
#rawsonly <- siwf %>%
#filter(!study %in% smeans) %>%
#mutate(Study_ID = factor(Study_ID))

#write.csv(rawsonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")

rawsonly <- as.data.frame(read_csv("rawsonly.csv"))

unique(rawsonly$Study_ID)
rawsonly$Study_ID <- as.factor(rawsonly$Study_ID)
is.numeric(rawsonly$Study_ID)

unique(rawsonly$veg)
unique(rawsonly$study)
unique(rawsonly$pool)

#ccc <- unique(rawsonly[c("Study_ID", "veg", "study", "pool")])

ccc <- rawsonly %>% 
  distinct(Study_ID, veg, study, pool, .keep_all = FALSE)

ccc$geometry <- NULL
write.csv(ccc, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/ccc.csv", row.names = FALSE)



###
#to update pairs of studyids in paired_StudyIDs3
#NOTE: the studies below marked as summary data can either be included or not; these are simulated raw data
Blankcheck <- rawsonly %>% 
  filter(study == 'Blank and Norton 2006') %>%
  distinct(Study_ID, veg, pool, site, bottomdepth_cm, .keep_all = FALSE)

Bradleycheck <- rawsonly %>% 
  filter(study == 'Bradley et al. 2006') %>%
  distinct(Study_ID, veg, pool, site, .keep_all = FALSE)
#get rid of 280, 281, 290, 291 if not using salt desert

Goercheck <- rawsonly %>% 
  filter(study == 'Goergen et al. 2011') %>%
  distinct(Study_ID, veg, pool, site, bottomdepth_cm, .keep_all = FALSE)

#this is summary data
Hookcheck <- joiny2 %>% 
  filter(study == 'Hooker et al. 2008') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

#this is summary data
Johncheck <- joiny2 %>% 
  filter(study == 'Johnson et al. 2011') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

#not sure which of these to pair- check with Emily and Bethany
Mahocheck <- rawsonly %>% 
  filter(Article_ID == 'MAHO2018a') %>%
  distinct(Study_ID, veg, pool, site, lat, long, .keep_all = FALSE)

st_geometry(Mahocheck) = NULL
write.csv(Mahocheck, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/Mahood_pairs_check.csv")



Nortcheck <- rawsonly %>% 
  filter(Article_ID == 'NORT2008') %>%
  distinct(Study_ID, veg, pool, .keep_all = FALSE)

Nortcheck2012 <- rawsonly %>% 
  filter(Article_ID == 'NORT2012') %>%
  distinct(Study_ID, veg, pool, yr_samp, .keep_all = FALSE)

Starcheck <- rawsonly %>% 
  filter(Article_ID == 'STAR2015') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

Svejcheck <- rawsonly %>% 
  filter(Article_ID == 'SVEJ2001') %>%
  distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

Webecheck <- rawsonly %>% 
  filter(Article_ID == 'WEBE2015') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

Banscheck <- rawsonly %>% 
  filter(Article_ID == 'BANS2014') %>%
  distinct(Study_ID, veg, pool, site, keep_all = FALSE)

st_geometry(Banscheck) = NULL
write.csv(Banscheck, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/results/Bansal_pairs_check.csv")


Boulcheck <- rawsonly %>% 
  filter(Article_ID == 'BOUL1993') %>%
  distinct(Study_ID, veg, pool, bottomdepth_cm, keep_all = FALSE)

#this is summary data
Ackecheck <- joiny2 %>% 
  filter(Article_ID == 'ACKE1992') %>%
  distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

#this is summary data
Gascchecka <- joiny2 %>% 
  filter(Article_ID == 'GASC2013') %>%
  distinct(Study_ID, veg, pool, site, yr_samp, keep_all = FALSE)

#this is summary data
Witwcheck <- joiny2 %>% 
  filter(Article_ID == 'WITW2013') %>%
  distinct(Study_ID, veg, pool, keep_all = FALSE)





#bring in paired Study_IDs
#need to update this
#pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))

#updated 12/30/19
pairs <- as.data.frame(read_csv("paired_studyIDs3.csv"))
pairs <- pairs %>%
  dplyr::select(-Article_ID, -pool) %>%
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
StudyIDp <- pairs %>%
  mutate(Study_IDp = paste(cheatgrass_studyID, sagecheat_studyID, sagebrush_studyID, sep = '_'))







####
#ASK EMILY AND BETHANY IF THESE SHOULD BE ONLY RAW DATA OR SIMULATED RAW DATA TOO?
#THE CODE BELOW USES ONLY RAW DATA
####


###
pairscheat$Study_ID <- as.factor(pairscheat$Study_ID)
pcheat <- pairscheat %>%
  filter(!is.na(Study_ID))

rawsonly$Study_ID <- as.factor(rawsonly$Study_ID)
pcheat2 <- semi_join(pcheat, rawsonly)
pcheat2


#select only the paired studies
rawsonlycheat <- rawsonly[rawsonly$Study_ID %in% pcheat2$Study_ID,]

###
psage <- pairssage %>%
  filter(!is.na(Study_ID))

psage2 <- semi_join(psage, rawsonly)
psage2

rawsonlysage <- rawsonly[rawsonly$Study_ID %in% psage2$Study_ID,]



###
psagecheat <- pairssagecheat %>%
  filter(!is.na(Study_ID))

psagecheat2 <- semi_join(psagecheat, rawsonly)
psagecheat2

rawsonlysagecheat <- rawsonly[rawsonly$Study_ID %in% psagecheat2$Study_ID,]


#missing some Study_IDs...is this because they were re-created?
#rawsonly236 <- siwf %>%
  #filter(Study_ID == 236)


####
#total of rawsonlycheat + rawsonlysage + rawsonlysagecheat = 281 + 230 + 166 = 677 != 1176 rawsonly
#this is because I'm subsetting for just the paired raw data
#use these three (rawsonlycheat, rawsonlysage, rawsonlysagecheat) to calculate wide dataframe for meta-analysis


#####
#for all cheat studies except Mahood
cheatpmeans <- rawsonlycheat %>%
  filter(Article_ID != "MAHO2018a") %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(cheatpmeans) = NULL

#adding code to average across Mahood cheatgrass sites for the pairs
cheatpmeansMahood <- rawsonlycheat %>%
  filter(Article_ID == "MAHO2018a") %>%
  group_by(site, veg) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(cheatpmeans) = NULL


sagepmeans <- rawsonlysage %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagepmeans) = NULL



sagecheatpmeans <- rawsonlysagecheat %>%
  group_by(Study_ID, pool) %>%
  dplyr::summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  mutate(se = sqrt(var)/sqrt(n))

st_geometry(sagecheatpmeans) = NULL

#this makes Table S3
write.csv(cheatpmeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/cheatpmeans.csv")
write.csv(sagepmeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/sagepmeans.csv")
write.csv(sagecheatpmeans, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/sagecheatpmeans.csv")

####
#now it's just a matter of putting these values in the correct order in the table so that the pairs line up
#manually pasted in the correct spots in a .csv file

#bring that file in here
rawspmeans <- as.data.frame(read_csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeans2.csv"))

dq2 <- rawspmeans

#check depths on soil pools
check <- pairs_long %>%
  filter(pool == "orgsoilC_g_m2" | pool == "totsoilC_g_m2") %>%
  filter(!is.na(Study_ID)) 

check2a <- check$Study_ID

check2 <- joiny2 %>%
  filter(Study_ID %in% check2a) 

check3 <- unique(check2[c("Study_ID", "topdepth_cm", "bottomdepth_cm")])
check3
#all different depth intervals; need to standardize

shallow <- check3 %>%
  filter(bottomdepth_cm < 20)
#31 study ids
write.csv(shallow, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/shallowid.csv")

shallowid <- shallow %>%
  dplyr::select(Study_ID)


mid <- check3 %>%
  filter(bottomdepth_cm == 20)
#17 study ids
write.csv(mid, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/midid.csv")

midid <- mid %>%
  dplyr::select(Study_ID)


deep <- check3 %>%
  filter(bottomdepth_cm > 20)
#14 study ids
write.csv(deep, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/deepid.csv")

deepid <- deep %>%
  dplyr::select(Study_ID)

####manually added this categorical variable (depth_cat: shallow, mid, deep) into rawspmeans2.csv for use as a fixed effect in meta-analysis

###
#create categories of yrs since burn for fixed effect in model
checkb <- pairs_long %>%
  filter(!is.na(Study_ID)) 

listy1 <- checkb$Study_ID

check2ab <- joiny2 %>%
  filter(Study_ID %in% listy1) 

check3b <- unique(check2ab[c("Study_ID", "masterlyb", "yr_samp")])
check3b

check3b$yrssince <- check3b$yr_samp - check3b$masterlyb

recentfire <- check3b %>%
  filter(yrssince < 5)
#10 study ids
write.csv(recent, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/recentfireid.csv")

recentfireid <- recentfire %>%
  dplyr::select(Study_ID)


midfire <- check3b %>%
  filter(yrssince >= 5 & yrssince <= 20)
#11 study ids
write.csv(midfire, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/midfireid.csv")

midfireid <- midfire %>%
  dplyr::select(Study_ID)


oldfire <- check3b %>%
  filter(yrssince > 20)
#2 study ids
write.csv(oldfire, file = "/Users/rana7082/Dropbox/C_fire_invasives_R/data/oldfireid.csv")

oldfireid <- oldfire %>%
  dplyr::select(Study_ID)

####manually added this categorical variable (fire_cat: recentfire, midfire, oldfire) into rawspmeans2.csv for use as a fixed effect in meta-analysis





###
####################################################
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







  