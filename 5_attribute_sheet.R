#Filling in the attribute sheet for meta-analysis
#Dr. R. Chelsea Nagy
#created September 7, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

studyid = read_csv("data/studyid.csv")

#start to gather info for attribute table
artstud <- unique(studyid[c("Article_ID", "Study_ID")])
write.csv(artstud, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/articlestudyid.csv")


att1 <- unique(studyid[c("Article_ID", "Study_ID", "veg", "topdepth_cm", "bottomdepth_cm", "BD_estimated","yr_samp", "lat","long")])
write.csv(att1, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes1.csv")





#############################
#don't know if any of this below is needed
#att2 <- unique(studyid[c("Article_ID", "Study_ID", "soilC_g_m2", "BGBC_g_m2", "litterC_g_m2", "AGBC_g_m2")])
#head(att2)

unique(studyid$Study_ID)
as.factor(studyid$Study_ID)

studyid <- as.data.frame(studyid)

litter <- studyid[ which(studyid$pool == 'litterC_g_m2'), ]
sum71 <- summaryBy(pool_value ~ Article_ID + Study_ID, data = litter)
sum71
sum72 <- summaryBy(orgsoilC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum72b <- summaryBy(totsoilC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum73 <- summaryBy(BGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)
sum74 <- summaryBy(AGBC_g_m2 ~ Article_ID + Study_ID, data = studyid)

combo1 <- left_join(sum71, sum72, by = c("Article_ID","Study_ID"))
combo1b <- left_join(combo1, sum72b, by = c("Article_ID","Study_ID"))
combo2 <- left_join(combo1b, sum73, by = c("Article_ID","Study_ID"))
combo3 <- left_join(combo2, sum74, by = c("Article_ID","Study_ID"))

head(combo3)
tail(combo3)

#replace NAs with negative number
combo3[is.na(combo3)] <- -2

combo3$Carbon_pool <- ifelse(combo3$orgsoilC_g_m2.mean > 0, "organic soil",
                             ifelse(combo3$totsoilC_g_m2.mean > 0, "total soil",
                                    ifelse(combo3$litterC_g_m2.mean > 0, "litter",
                                           ifelse(combo3$AGBC_g_m2.mean > 0, "AGB", "BGB"))))


write.csv(combo3, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/results/attributes2.csv")




plot(AGBC_g_m2.mean ~ Study_ID, data = combo3)



