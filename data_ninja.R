#Script for data ninja-ing for the NSF fire invasives carbon project
#Dr. R. Chelsea Nagy
#created July 31, 2018

library(plyr)
library(tidyverse)
library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(doBy)

setwd("data/")

# Read in alldatall.csv
alldatall = read_csv("alldatall.csv")

###
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


# Parametric Bootstrap sample
MeanCheat  = soilsubby2$soilC_mean[2]
SECheat = soilsubby2$soilC_SE[2]
SDCheat = soilsubby2$soilC_SD[2]

# Number of random numbers to sample
N = sum(!is.na(soilsubby$soilC_g_m2))
N
?rnorm
Dist1 = rnorm(n=N, mean=MeanCheat, sd=SDCheat)
par(mfrow=c(1,2))
hist(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"], breaks=10)
hist(Dist1, breaks=10)

# New DF
Dist1DF = data.frame(Type="rnorm", soilC_g_m2=Dist1)
ObsDF = data.frame(Type="Obs", 
                   soilC_g_m2=soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"])
Dists = rbind(ObsDF, Dist1DF)
head(Dists
     )
# Overlaid histograms
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_histogram(binwidth=200, alpha=.5, position="identity")

ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

# Add another random distribution
Dist2DF = data.frame(Type = "Dist2",
                     soilC_g_m2 = rnorm(n=N, mean=MeanCheat, sd=SDCheat))

Dists = rbind(Dists, Dist2DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")



## Bootstrap sampling
Boot1DF = data.frame(Type="Boot1", 
                     soilC_g_m2 = sample(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"],
               size=N, replace=TRUE))
               
Dists = rbind(Dists, Boot1DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

## Bootstrap sampling
Boot2DF = data.frame(Type="Boot2", 
                     soilC_g_m2 = sample(soilsubby$soilC_g_m2[soilsubby$study=="Rau et al. 2011"],
                                         size=N, replace=TRUE))

Dists = rbind(Dists, Boot2DF)
ggplot(Dists, aes(x=soilC_g_m2, fill=Type)) +
  geom_density(alpha=.5, position="identity")

