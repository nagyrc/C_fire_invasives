#Script for bootstrapping to create data from mean values only
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)


###############################
#bring in means
studymeans <- as.data.frame(read_csv("study_means.csv"))



###########################
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

