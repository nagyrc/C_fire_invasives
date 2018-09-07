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

write.csv(clean_study, file = "clean_study.csv")

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

write.csv(studyid, file = "studyid.csv")


