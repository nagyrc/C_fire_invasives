#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018


#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

# Read in alldatall.csv
alldatall = read_csv("alldatall.csv")

###
#automate the studyid csv that we did manually
#create Article ID
studyid <- alldatall
studyid$Article_IDs <- str_sub(studyid$study,1,4)
unique(studyid$Article_IDs)
studyid$Article_IDe <- str_sub(studyid$study,-4,-1)
unique(studyid$Article_IDe)
studyid$Article_ID <- paste(toupper(studyid$Article_IDs),studyid$Article_IDe)
studyid$Article_ID <- gsub(" ", "", studyid$Article_ID) 
studyid$Article_ID <- gsub("MAHOpub1", "MAHO2018a", studyid$Article_ID) 
studyid$Article_ID <- gsub("MAHOpub2", "MAHO2018b", studyid$Article_ID) 

unique(studyid$Article_ID)
#these look great

head(studyid)
  

#creates study_ID variable, creates pool variable, and intersects the US states and MTBS shapefiles with data points
clean_study <- studyid %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","soil%C","BD_g_cm3","soilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick") %>%
  # tidyr::gather(key = variable, value = value, -site, -study, -yr_samp, -lat, -long, -veg, -thick, -BD_estimated, -topdepth_cm, -bottomdepth_cm) %>%
  mutate(site = as.factor(site),
         veg = as.factor(veg),
         long = ifelse(is.na(long), 0, long),
         lat = ifelse(is.na(lat), 0, lat),
         # variable = as.factor(variable),
         pool = ifelse(AGBC_g_m2 > 0, "AGB", ifelse(BGBC_g_m2 > 0, "BGB", ifelse(litterC_g_m2 > 0, "litter", "soil"))), 
         Study_ID = group_indices_(., .dots = c("study","lat", "long", "veg", "site", "bottomdepth_cm", "pool","yr_samp"))) %>%
  #dplyr::filter(long != 0 & lat != 0) %>% 
  sf::st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  mutate(yr_samp = as.numeric(ifelse(is.na(yr_samp), 0, yr_samp))) #%>%
  #sf::st_join(., usa_shp) %>%
  #mutate(id = row_number(),
         #study_year = str_sub(study,-4,-1),
         #study_year = ifelse(study_year == 'pub1', 2017, study_year),
         #yr_samp = ifelse(is.na(yr_samp) | yr_samp == 0, study_year, yr_samp))
#this doesn't seem to be creating the pool variable...check nested ifelse statements





###
#move this code to new R script and use new dataframe that has spatial information
#in this table I'm bringing in here, we manually entered article ID and study ID
#data ninja-ing for attribute table
studyid <- as.data.frame(read_csv("alldatall_bystudyid.csv"))
head(studyid)
artstud <- unique(studyid[c("Article_ID", "Study_ID")])

write.csv(artstud, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/articlestudyid.csv")
is.numeric(studyid$topdepth_cm)
is.numeric(studyid$bottomdepth_cm)

#recalculate thickness; there were some issues with this when we looked at alldatall.csv
studyid$thick <- studyid$bottomdepth_cm - studyid$topdepth_cm
#this fixed it


att1 <- unique(studyid[c("Article_ID", "Study_ID", "veg", "topdepth_cm", "bottomdepth_cm", "BD_estimated","yr_samp", "lat","long")])
write.csv(att1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes1.csv")


#att2 <- unique(studyid[c("Article_ID", "Study_ID", "soilC_g_m2", "BGBC_g_m2", "litterC_g_m2", "AGBC_g_m2")])
#head(att2)

sum71 <- summaryBy(litterC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum72 <- summaryBy(soilC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum73 <- summaryBy(BGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum74 <- summaryBy(AGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)

combo1 <- left_join(sum71, sum72, by = c("Article_ID","Study_ID"))
combo2 <- left_join(combo1, sum73, by = c("Article_ID","Study_ID"))
combo3 <- left_join(combo2, sum74, by = c("Article_ID","Study_ID"))

head(combo3)
tail(combo3)

#replace NAs with negative number
combo3[is.na(combo3)] <- -2

combo3$Carbon_pool <- ifelse(combo3$soilC_g_m2.mean > 0, "soil",
                              ifelse(combo3$litterC_g_m2.mean > 0, "litter",
                                     ifelse(combo3$AGBC_g_m2.mean > 0, "AGB", "BGB")))


write.csv(combo3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes2.csv")


soilsubby <- subset.data.frame(alldatall, study == "Rau et al. 2011"| study == "Johnson et al. 2011")
unique(soilsubby$study)

ggplot(soilsubby, aes(x = veg, y = soilC_g_m2, color=study)) + geom_violin()

#quantiles
quantile(soilsubby$soilC_g_m2, 0.9, na.rm=TRUE)

summaryBy(soilC_g_m2~ study, data = soilsubby, FUN = c(meanfxn))
summaryBy(soilC_g_m2~ veg, data = soilsubby, FUN = c(meanfxn))

sdfxn <- function(x)base::sd(x, na.rm = TRUE)
maxfxn <- function(x)base::max(x, na.rm = TRUE)

soilsubby2 <- soilsubby %>%
  mutate(soilC_SD = sd(soilC_g_m2, na.rm=TRUE))

soilsubby2 <- soilsubby %>% group_by(veg,study) %>%
  summarize(soilC_SD = sd(soilC_g_m2, na.rm = TRUE),
         soilC_mean = mean(soilC_g_m2, na.rm = TRUE),
         soilC_SE = sqrt(var(soilC_g_m2, na.rm = TRUE)/sum(!is.na(soilC_g_m2))))


head(soilsubby2)

#summaryBy(soilC_g_m2~ study, data = soilsubby, FUN = c(sdfxn))
#summaryBy(soilC_g_m2~ veg, data = soilsubby, FUN = c(sdfxn))

