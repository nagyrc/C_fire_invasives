#Data exploration continued
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#bring in studyid dataframe
studyid = read_csv("studyid.csv")
siwf = read_csv("siwf.csv")



#bring in paired Study_IDs
#pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))
pairs <- as.data.frame(read_csv("paired_studyIDs.csv"))
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



#split data into means and raw data
studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)




#####
rawsonly <- siwf %>%
  filter(!study %in% smeans) %>%
  mutate(Study_ID = factor(Study_ID))

write.csv(rawsonly, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/rawsonly.csv")


unique(rawsonly$Study_ID)


###
pcheat <- pairscheat %>%
  filter(!is.na(Study_ID))

pcheat2 <- semi_join(pcheat, rawsonly)
pcheat2

rawsonlycheat <- rawsonly %>%
  filter(Study_ID == 226 | Study_ID == 206 | Study_ID == 382 | Study_ID == 392 | Study_ID == 372 | Study_ID == 402 | Study_ID == 384 | Study_ID == 394 | Study_ID == 374 | Study_ID == 404 | Study_ID == 1089 | Study_ID == 1102 | Study_ID == 1103 | Study_ID == 1304 | Study_ID == 1309 | Study_ID == 1314 | Study_ID == 1319 | Study_ID == 1454 | Study_ID == 1459 | Study_ID == 205 | Study_ID == 225 | Study_ID == 505 | Study_ID == 530)



###
psage <- pairssage %>%
  filter(!is.na(Study_ID))

psage2 <- semi_join(psage, rawsonly)
psage2

rawsonlysage <- rawsonly %>%
  filter(Study_ID == 144 | Study_ID == 149 | Study_ID == 124 | Study_ID == 129 | Study_ID == 104 | Study_ID == 109 | Study_ID == 84 | Study_ID == 89 | Study_ID == 44 | Study_ID == 49 | Study_ID == 64 | Study_ID == 69 | Study_ID == 1112 | Study_ID == 1113 | Study_ID == 1324 | Study_ID == 1329 | Study_ID == 1334 | Study_ID == 1339 | Study_ID == 1464 | Study_ID == 1469)



###
psagecheat <- pairssagecheat %>%
  filter(!is.na(Study_ID))

psagecheat2 <- semi_join(psagecheat, rawsonly)
psagecheat2

rawsonlysagecheat <- rawsonly %>%
  filter(Study_ID == 387 | Study_ID == 397 | Study_ID == 377 | Study_ID == 407 | Study_ID == 154 | Study_ID == 159 | Study_ID == 134 | Study_ID == 139 | Study_ID == 114 | Study_ID == 119 | Study_ID == 94 | Study_ID == 99 | Study_ID == 54 | Study_ID == 59 | Study_ID == 74 | Study_ID == 79 | Study_ID == 389 | Study_ID == 399 | Study_ID == 379 | Study_ID == 409 | Study_ID == 1094 | Study_ID == 1344 | Study_ID == 1349 | Study_ID == 1354 | Study_ID == 1359 | Study_ID == 510 | Study_ID == 535)





####
#total of rawsonlycheat + rawsonlysage + rawsonlysagecheat = 281 + 230 + 166 = 677 != 1176 rawsonly
#this is because I'm subsetting for just the paired raw data
#use these three (rawsonlycheat, rawsonlysage, rawsonlysagecheat) to calculate wide dataframe for meta-analysis


#####
cheatpmeans <- rawsonlycheat %>%
  group_by(Study_ID, pool) %>%
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


####
#now it's just a matter of putting these values in the correct order in the table so that the pairs line up






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







  