#Script for bootstrapping to create data from mean values only
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)


###############################
#bring in means
#setwd("/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/")
studymeans <- as.data.frame(read_csv("data/study_means.csv"))

#calculating AGBC from AGB using mean cheatgrass %C from Mahood
#Diamond and Bjerregaard studies had C data in addition to biomass data
studymeans$AGBC_g_m2 <- ifelse(studymeans$study == 'Diamond et al. 2012' | studymeans$study == 'Bjerregaard et al. 1984', studymeans$AGBC_g_m2, studymeans$AGB_g_m2 * meancheat_percC / 100)
studymeans$AGBC_g_m2_SE <- ifelse(studymeans$study == 'Diamond et al. 2012' | studymeans$study == 'Bjerregaard et al. 1984' ,studymeans$AGBC_g_m2_SE, studymeans$AGB_g_m2_SE * meancheat_percC / 100)


meancheatlitter_perC <- 33.667
studymeans$litterC_g_m2 <- studymeans$litter_g_m2 * meancheatlitter_perC/100
studymeans$litterC_g_m2_SE <- studymeans$litter_g_m2_SE * meancheatlitter_perC/100

head(studymeans)
studymeans







#########################
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






################################
########################
# Read in alldata.csv
bind11 = read_csv("data/bind11.csv")
alldata <- rbind.all.columns(bind11, studymeans)

#making sure all numeric fields are numeric
str(alldata)

alldata$litterC_g_m2 <- as.numeric(alldata$litterC_g_m2)
alldata$bottomdepth_cm <- as.numeric(alldata$bottomdepth_cm)
alldata$elevation <- as.numeric(alldata$elevation)
alldata$cheat_cover <- as.numeric(alldata$cheat_cover)


write.csv(alldata, file = "data/alldata.csv")