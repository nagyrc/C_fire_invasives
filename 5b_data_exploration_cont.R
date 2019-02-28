#Data exploration continued
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")


########################################################
#no fire data; #bring in studyid dataframe
siwf <- studyid_sf

#need to add years since burn to this
joiny <- siwf %>%
  group_by(Study_ID, veg) %>%
  summarise() 


#bring in paired Study_IDs
#pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))
pairs <- as.data.frame(read_csv("paired_studyIDs.csv"))
pairs <- pairs %>%
  dplyr::select(-Article_ID, -pool) %>%
  mutate(pairnum = 1:nrow(pairs))

pairscheat <- pairs %>%
  dplyr::select(cheatgrass_studyID) %>%
  rename(Study_ID = cheatgrass_studyID)

pairssage <- pairs %>%
  dplyr::select(sagebrush_studyID) %>%
  rename(Study_ID = sagebrush_studyID)

pairssagecheat <- pairs %>%
  dplyr::select(sagecheat_studyID) %>%
  rename(Study_ID = sagecheat_studyID)

#############
#not quite right; need to remove NAs from the names
StudyIDp <- pairs %>%
  mutate(Study_IDp = paste(cheatgrass_studyID, sagecheat_studyID, sagebrush_studyID, sep = '_'))



#split data into means and raw data
studymeans <- as.data.frame(read_csv("study_means.csv"))
smeans <- unique(studymeans$study)


pairs_long <- as.data.frame(read_csv("paired_studyIDs_long.csv"))

#remove rows with NAs
pairs_long2 <- na.omit(pairs_long)

pairs_long3 <- pairs_long2 %>%
  dplyr::select(Study_ID)

as.list(pairs_long3)


#####
rawsonly <- siwf %>%
  filter(!study %in% smeans)

l1 <- as.list(pairscheat)

rawsonlycheat <- rawsonly[rawsonly$Study_ID %in% !is.na(l1), ] 

rawsonlycheat <- rawsonly %>%
  filter(Study_ID %in% pairscheat)

rawsonlysagecheat <- rawsonly %>%
  filter(Study_ID %in% !is.na(pairssagecheat))

rawsonlysage <- rawsonly %>%
  filter(Study_ID %in% !is.na(pairssage))

rawcheat <- rawsonly %>%
  filter(Study_ID %in% !is.na(pairssagecheat)) %>%
  group_by(Study_ID) %>%
  summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value))



####

####################
#create raw data only dataframe for paired studies
rawsonly <- siwf %>%
  filter(Study_ID == 530 | Study_ID == 535)

rawpairsonly <- siwf %>%
  filter(Study_ID %in% pairs_long3)

rawpairsonly <- siwf[siwf$Study_ID %in% pairs_long3, ] 


#then create pair number


x <- rowwise(pairs)


#summarise for the pairs
rawp <- rawsonly %>%
  group_by(Study_ID, pool) %>%
  summarise(meanpv = mean(pool_value), n = n(), var = var(pool_value)) %>%
  dplyr::select(-geometry)

st_geometry(joiny) <- NULL

#add back in veg type
rawpj <- rawp %>%
  left_join(as.data.frame(joiny)) %>%
  dplyr::select(-geometry)

st_geometry(rawpj) <- NULL
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







  