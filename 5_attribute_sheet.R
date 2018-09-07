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


