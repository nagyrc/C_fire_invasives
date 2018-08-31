#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018


#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

# Read in alldata.csv
alldata = read_csv("data/alldata.csv")

###

colnames(alldata)[colnames(alldata) == 'soil%C'] <- 'soilperC'

#recalculate thickness- we saw some issues with it in the .csv file
is.numeric(alldata$topdepth_cm)
is.numeric(alldata$bottomdepth_cm)
alldata$thick <- alldata$bottomdepth_cm - alldata$topdepth_cm

#slims dataframe to key variables...still wide format to use with spatial data in script 4 (adding fire data)
clean_study <- alldata %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","soilperC","BD_g_cm3","soilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick") %>%
  mutate(site = as.factor(site),
         veg = as.factor(veg)) %>%
  mutate(yr_samp = as.numeric(ifelse(is.na(yr_samp), 0, yr_samp))) %>%
  mutate(id = row_number(),
         study_year = str_sub(study,-4,-1),
         study_year = ifelse(study_year == 'pub1', 2017, study_year),
         yr_samp = ifelse(is.na(yr_samp) | yr_samp == 0, study_year, yr_samp))



#create Article_ID
alldata$Article_IDs <- str_sub(alldata$study,1,4)
unique(alldata$Article_IDs)
alldata$Article_IDe <- str_sub(alldata$study,-4,-1)
unique(alldata$Article_IDe)
alldata$Article_ID <- paste(toupper(alldata$Article_IDs),alldata$Article_IDe)
alldata$Article_ID <- gsub(" ", "", alldata$Article_ID) 
alldata$Article_ID <- gsub("MAHOpub1", "MAHO2018a", alldata$Article_ID) 
alldata$Article_ID <- gsub("MAHOpub2", "MAHO2018b", alldata$Article_ID) 

unique(alldata$Article_ID)
#these look great


#creates study_ID, pool, and setup for Bethany's meta-analysis format (long format)
studyid <- alldata %>%
  dplyr::select("site","yr_samp","AGBC_g_m2","BGBC_g_m2","litterC_g_m2","soilperC","BD_g_cm3","soilC_g_m2","topdepth_cm","bottomdepth_cm","BD_estimated","veg","study","lat","long","thick","Article_ID") %>%
  tidyr::gather(key = variable, value = value, -site, -study, -yr_samp, -lat, -long, -veg, -thick, -BD_estimated, -topdepth_cm, -bottomdepth_cm, -soilperC, -BD_g_cm3, -Article_ID) %>%
  mutate(variable = as.factor(variable),
         pool = ifelse(AGBC_g_m2 > 0, "AGB", ifelse(BGBC_g_m2 > 0, "BGB", ifelse(litterC_g_m2 > 0, "litter", "soil"))), 
         Study_ID = group_indices_(., .dots = c("study","lat", "long", "veg", "site", "bottomdepth_cm", "pool","yr_samp"))) %>%
#check nested ifelse statements to make sure it is creating pool





###
#start to gather info for attribute table
#this could be moved to a different script
artstud <- unique(studyid[c("Article_ID", "Study_ID")])
write.csv(artstud, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/articlestudyid.csv")


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


soilsubby <- subset.data.frame(alldata, study == "Rau et al. 2011"| study == "Johnson et al. 2011")
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


