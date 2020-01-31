#Script for testing distributions to simulate raw data from means
#Dr. R. Chelsea Nagy & Dr. Emily Fusco
#created Feb 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)


###
#need to decide whether or not to keep salt_desert in 
checkveg <- studyid %>%
  filter(veg == "salt_desert") 

unique(checkveg$study) #5 studies
unique(checkveg$pool) #5 pools
###

###############################
#bring in raw data
rawsonlynofire <- as.data.frame(read_csv("rawsonlynofire.csv"))


# Histogram for each pol-veg combo to look at distributions
ggplot(rawsonlynofire, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + facet_grid(pool ~ veg) + 
  xlab("pool_value") + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#try with subsets for each pool
AGBC <- subset.data.frame(rawsonlynofire, pool == "AGBC_g_m2")
BGBC <- subset.data.frame(rawsonlynofire, pool == "BGBC_g_m2")
litterC <- subset.data.frame(rawsonlynofire, pool == "litterC_g_m2")
orgsoilC <- subset.data.frame(rawsonlynofire, pool == "orgsoilC_g_m2")
totsoilC <- subset.data.frame(rawsonlynofire, pool == "totsoilC_g_m2")

head(AGBC)
ggplot(AGBC, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  facet_wrap(~veg) + 
  xlab("AGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

summary(AGBC$pool_value)

ggplot(BGBC, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  facet_wrap(~veg) + 
  xlab("BGB C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(litterC, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  facet_wrap(~veg) + 
  xlab("litter C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(orgsoilC, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  facet_wrap(~veg) + 
  xlab("organic soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggplot(totsoilC, aes(x = pool_value, fill = Article_ID)) + 
  geom_histogram() + 
  facet_wrap(~veg) + 
  xlab("total soil C (gC m-2)") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
###########################
#subset data for aboveground sagebrush

ABGCsb <- subset.data.frame(rawsonlynofire, pool == "AGBC_g_m2" & veg == "sagebrush")

ggplot(ABGCsb, aes(x = veg, y = pool_value, color=study)) + geom_violin()


#calculate for raw data
ABGCsb <- ABGCsb %>%
  mutate(SD = sd(pool_value, na.rm = TRUE)) %>%
  mutate(mean = mean(pool_value, na.rm = TRUE)) %>%
  mutate(SE = sqrt(var(pool_value, na.rm = TRUE)/sum(!is.na(pool_value)))) %>%
  mutate(variance = SD^2) %>%
  mutate(scale = variance/mean) %>%
  mutate(shape = mean/scale)


#########################
# Parametric Bootstrap sample
Mean  = ABGCsb$mean
SE = ABGCsb$SE
SD = ABGCsb$SD
scale = ABGCsb$scale
shape = ABGCsb$shape

# Number of random numbers to sample
N = nrow(ABGCsb)
N
DistNorm = rnorm(n = N, mean = Mean, sd = SD)
DistGamma = rgamma(n = N, shape = shape, scale = scale)

par(mfrow = c(1,2))
hist(ABGCsb$pool_value, breaks = 10)
hist(DistNorm, breaks = 10)
hist(DistGamma, breaks = 10)
#gamma looks better

# New DF
DistNormDF = data.frame(Type = "Normal", pool_value = DistNorm)
DistGammaDF = data.frame(Type = "Gamma", pool_value = DistGamma)
ObsDF = data.frame(Type = "Observed", pool_value = ABGCsb$pool_value)
Dists = rbind(ObsDF, DistNormDF, DistGammaDF)

# Overlaid histograms
ggplot(Dists, aes(x = pool_value, fill = Type)) +
  geom_density(alpha = .5, position = "identity")

# Add another Gamma distribution
DistGamma2DF = data.frame(Type = "Gamma2", pool_value = rgamma(n = N, shape = shape, scale = scale))

Dists = rbind(Dists, DistGamma2DF)
ggplot(Dists, aes(x = pool_value, fill = Type)) +
  geom_density(alpha = .5, position = "identity")




###########################
#subset data for cheatgrass soil carbon

OGSCcg <- subset.data.frame(rawsonlynofire, pool == "orgsoilC_g_m2" & veg == "cheatgrass")


ggplot(OGSCcg, aes(x = veg, y = pool_value, color = study)) + geom_violin()
#maybe check Norton et al. 2004 data...is there anything weird going on there?

#calculate for raw data
OGSCcg <- OGSCcg %>%
  mutate(SD = sd(pool_value, na.rm = TRUE)) %>%
  mutate(mean = mean(pool_value, na.rm = TRUE)) %>%
  mutate(SE = sqrt(var(pool_value, na.rm = TRUE)/sum(!is.na(pool_value)))) %>%
  mutate(variance = SD^2) %>%
  mutate(scale = variance/mean) %>%
  mutate(shape = mean/scale)


#########################
# Parametric Bootstrap sample
Mean = OGSCcg$mean
SE = OGSCcg$SE
SD = OGSCcg$SD
scale = OGSCcg$scale
shape = OGSCcg$shape

# Number of random numbers to sample
N = nrow(OGSCcg)
N
DistNorm = rnorm(n = N, mean = Mean, sd = SD)
DistGamma = rgamma(n = N, shape = shape, scale = scale)
par(mfrow = c(1,2))
hist(OGSCcg$pool_value, breaks = 10)
hist(DistNorm, breaks = 10)
hist(DistGamma, breaks = 10)
#gamma looks better than normal

# Create New DF so that they can all be overlaid
DistNormDF = data.frame(Type = "Normal", pool_value = DistNorm)
DistGammaDF = data.frame(Type = "Gamma", pool_value = DistGamma)
ObsDF = data.frame(Type = "Observed", pool_value = OGSCcg$pool_value)
Dists = rbind(ObsDF, DistNormDF, DistGammaDF)

# Overlaid histograms
ggplot(Dists, aes(x = pool_value, fill = Type)) +
  geom_density(alpha = .5, position = "identity")

# Add another Gamma distribution
DistGamma2DF = data.frame(Type = "Gamma2", pool_value = rgamma(n = N, shape = shape, scale = scale))

Dists = rbind(Dists, DistGamma2DF)
ggplot(Dists, aes(x = pool_value, fill = Type)) +
  geom_density(alpha = .5, position = "identity")

#normal might be slightly better here



##Test the bootstrapped values against the simulated and observed values
## Bootstrap sampling
Boot1 = data.frame(Type = "Boot1", pool_value = sample(OGSCcg$pool_value, size = N, replace = TRUE))

Dists = rbind(Dists, Boot1)
ggplot(Dists, aes(x = pool_value, fill = Type)) +
  geom_density(alpha = .5, position = "identity")



################################
########################
