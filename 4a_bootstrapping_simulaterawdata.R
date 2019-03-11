#Script for simulating data based on mean, variance, and n
#Dr. R. Chelsea Nagy & Dr. Emily Fusco
#created Feb 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
#lapply(x, install.packages, character.only = TRUE, verbose = FALSE)
lapply(x, library, character.only = TRUE, verbose = FALSE)


###############################
#bring in means
studymeans <- as.data.frame(read_csv("meansonlynvar.csv"))


###########################


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


#Simulate data using rgamma
#but use the bootstraptest script to make sure you're using the correct distribution

dflistsim <- lapply(dflist, function(x) Dist1 = rgamma(n= x$n_sampled, 
                                                       shape= x$shape, 
                                                       scale= x$scale))

#make list into a new dataframe of simulated raw data
newrawdata<- ldply(dflistsim, cbind)

#rename columns to match original data
newrawdata<-rename(newrawdata, c(".id" = "explode"))
newrawdata<-rename(newrawdata, c("1" = "simvalue"))
newrawdata$explode<-as.factor(newrawdata$explode)

#make a df to connect new raw data to original data
key<- unique(studymeanssubset[c("Study_ID", "explode")])
key$explode<-as.factor(key$explode)

#join the newrawdata to the key to get a list of simulated values by study id
simrawdata<-newrawdata%>%
  left_join(key)
