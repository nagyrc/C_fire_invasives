#Script for bootstrapping to create data from mean values only
#Dr. R. Chelsea Nagy
#created August 30, 2018

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
#lapply(x, install.packages, character.only = TRUE, verbose = FALSE)
lapply(x, library, character.only = TRUE, verbose = FALSE)


###############################
#bring in means
studymeans <- as.data.frame(read_csv("meansonlynvar.csv"))

#bring in raw data
rawdata <- as.data.frame(read_csv("rawsonly.csv"))

# Histogram for each pol-veg combo to look at distributions
ggplot(rawdata, aes(x= pool_value, fill= Article_ID)) + 
  geom_histogram()+ facet_grid(pool ~ veg)+
  xlab("pool_value") + theme_bw() +scale_x_log10() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


###########################

ggplot(studymeans, aes(x = veg, y = pool_value, color=pool)) + geom_violin()

#standard deviation, max, and mean functions
sdfxn <- function(x)base::sd(x, na.rm = TRUE)
maxfxn <- function(x)base::max(x, na.rm = TRUE)
meanfxn <- function(x)base::mean(x, na.rm = TRUE)

#summary stats by pool, veg, and by veg/pool
summaryBy(pool_value~ pool, data = studymeans, FUN = c(meanfxn))
summaryBy(pool_value~ veg, data = studymeans, FUN = c(meanfxn))
summaryBy(pool_value~ cbind(veg, pool), data = studymeans, FUN = c(meanfxn))

#Subset the data to only values that have a mean and SE
studymeanssubset1 <- subset(studymeans, !is.na(n_sampled)) 
                          
studymeanssubset <- subset(studymeanssubset1,!is.na(AGBC_g_m2_SE)
                           |!is.na(BGBC_g_m2_SE)  
                           |!is.na(litterC_g_m2_SE)
                           |!is.na(totsoilC_g_m2_SE)
                           |!is.na(orgsoilC_g_m2_SE))
glimpse(studymeanssubset)

#calculate a study SD for each of the carbon pools
studymeanssubset$SD[!is.na(studymeanssubset$AGBC_g_m2_SE)] <- 
  studymeanssubset$AGBC_g_m2_SE[!is.na(studymeanssubset$AGBC_g_m2_SE)]*
  sqrt(studymeanssubset$n_sampled[!is.na(studymeanssubset$AGBC_g_m2_SE)])

studymeanssubset$SD[!is.na(studymeanssubset$BGBC_g_m2_SE)] <- 
  studymeanssubset$BGBC_g_m2_SE[!is.na(studymeanssubset$BGBC_g_m2_SE)]*
  sqrt(studymeanssubset$n_sampled[!is.na(studymeanssubset$BGBC_g_m2_SE)])

studymeanssubset$SD[!is.na(studymeanssubset$litterC_g_m2_SE)] <- 
  studymeanssubset$litterC_g_m2_SE[!is.na(studymeanssubset$litterC_g_m2_SE)]*
  sqrt(studymeanssubset$n_sampled[!is.na(studymeanssubset$litterC_g_m2_SE)])

studymeanssubset$SD[!is.na(studymeanssubset$totsoilC_g_m2_SE)] <- 
  studymeanssubset$totsoilC_g_m2_SE[!is.na(studymeanssubset$totsoilC_g_m2_SE)]*
  sqrt(studymeanssubset$n_sampled[!is.na(studymeanssubset$totsoilC_g_m2_SE)])

studymeanssubset$SD[!is.na(studymeanssubset$orgsoilC_g_m2_SE)] <- 
  studymeanssubset$orgsoilC_g_m2_SE[!is.na(studymeanssubset$orgsoilC_g_m2_SE)]*
  sqrt(studymeanssubset$n_sampled[!is.na(studymeanssubset$orgsoilC_g_m2_SE)])

#calculate variance
studymeanssubset$variance<-studymeanssubset$SD^2

#calculate scale
studymeanssubset$scale<-studymeanssubset$variance/studymeanssubset$pool_value

#calculate shape
studymeanssubset$shape<-studymeanssubset$pool_value/studymeanssubset$scale

#calculate rate
studymeanssubset$rate<- 1/studymeanssubset$scale




#add a column of random numbers ("explode") because Ricard 1985a has duplicate study ids
studymeanssubset$explode<-sample(1:1000, nrow(studymeanssubset))

#split the dataframe into a list of dataframes based on "explode"
dflist<-split(studymeanssubset, studymeanssubset$explode)

glimpse(dflist)

#now we can run the analysis for each dataframe in the list of dataframes

#list2env(split(studymeanssubset, studymeanssubset$explode), envir=.GlobalEnv)

#studyIDlist<-as.list(unique(studymeanssubset$explode))


#Simulate data using rgamma
?rgamma
dflistsim <- lapply(dflist, function(x) Dist1 = rgamma(n= x$n_sampled, 
                                                       shape= x$shape, 
                                                       scale= x$scale))


# New DF
dflistsim <- lapply(dflist, function(x) Dist1 = rgamma(n= x$n_sampled, 
                                                       shape= x$shape, 
                                                       scale= x$scale))

dflistsimDF<- lapply(dflistsim, function (x) data.frame(Type="rgamma", 
                                                        pool_value=dflistsim))
 

 
data.frame(Type="rnorm", soilC_g_m2=Dist1)
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

