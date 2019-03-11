#Script for simulating data based on mean, variance, and n
#Dr. R. Chelsea Nagy & Dr. Emily Fusco
#created Feb 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)


###############################
#bring in means
meansonlynvar <- as.data.frame(read_csv("meansonlynvar.csv"))


###########################


#Subset the data to only values that have n_sampled and SE
studymeanssubset1 <- subset(meansonlynvar, !is.na(n_sampled))
#94 observations
                          
sms1 <- subset(studymeanssubset1, !is.na(AGBC_g_m2_SE) & pool == "AGBC_g_m2")
sms2 <- subset(studymeanssubset1, !is.na(BGBC_g_m2_SE) & pool == "BGBC_g_m2")
sms3 <- subset(studymeanssubset1, !is.na(litterC_g_m2_SE) & pool == "litterC_g_m2")
sms4 <- subset(studymeanssubset1, !is.na(totsoilC_g_m2_SE) & pool == "totsoilC_g_m2")
sms5 <- subset(studymeanssubset1, !is.na(orgsoilC_g_m2_SE) & pool == "orgsoilC_g_m2")

smsa <- rbind(sms1, sms2)
smsb <- rbind(smsa, sms3)
smsc <- rbind(smsb, sms4)
sms <- rbind(smsc, sms5)
#40 observations

glimpse(sms)

#make sure all SEs of other pools are NA...there were some remnant values from the gather step to make the Study_IDs
sms$AGBC_g_m2_SE <- ifelse(sms$pool == "AGBC_g_m2", sms$AGBC_g_m2_SE, NA)
sms$BGBC_g_m2_SE <- ifelse(sms$pool == "BGBC_g_m2", sms$BGBC_g_m2_SE, NA)
sms$litterC_g_m2_SE <- ifelse(sms$pool == "litterC_g_m2", sms$litterC_g_m2_SE, NA)
sms$totsoilC_g_m2_SE <- ifelse(sms$pool == "totsoilC_g_m2", sms$totsoilC_g_m2_SE, NA)
sms$orgsoilC_g_m2_SE <- ifelse(sms$pool == "orgsoilC_g_m2", sms$orgsoilC_g_m2_SE, NA)



#calculate a study SD for each of the carbon pools
sms$SD[!is.na(sms$AGBC_g_m2_SE)] <- 
  sms$AGBC_g_m2_SE[!is.na(sms$AGBC_g_m2_SE)] * sqrt(sms$n_sampled[!is.na(sms$AGBC_g_m2_SE)])

sms$SD[!is.na(sms$BGBC_g_m2_SE)] <- 
  sms$BGBC_g_m2_SE[!is.na(sms$BGBC_g_m2_SE)] * sqrt(sms$n_sampled[!is.na(sms$BGBC_g_m2_SE)])

sms$SD[!is.na(sms$litterC_g_m2_SE)] <- 
  sms$litterC_g_m2_SE[!is.na(sms$litterC_g_m2_SE)] * sqrt(sms$n_sampled[!is.na(sms$litterC_g_m2_SE)])

sms$SD[!is.na(sms$totsoilC_g_m2_SE)] <- 
  sms$totsoilC_g_m2_SE[!is.na(sms$totsoilC_g_m2_SE)] * sqrt(sms$n_sampled[!is.na(sms$totsoilC_g_m2_SE)])

sms$SD[!is.na(sms$orgsoilC_g_m2_SE)] <- 
  sms$orgsoilC_g_m2_SE[!is.na(sms$orgsoilC_g_m2_SE)] * sqrt(sms$n_sampled[!is.na(sms$orgsoilC_g_m2_SE)])



#calculate variance
sms$variance <- sms$SD^2

#calculate scale
sms$scale <- sms$variance/sms$pool_value

#calculate shape
sms$shape <- sms$pool_value/sms$scale

#calculate rate
sms$rate <- 1/sms$scale




#add a column of random numbers ("explode") because Ricard 1985a has duplicate study ids
sms$explode <- sample(1:1000, nrow(sms))

#split the dataframe into a list of dataframes based on "explode"
dflist <- split(sms, sms$explode)

glimpse(dflist)



#now we can run the analysis for each dataframe in the list of dataframes
#Simulate data using rgamma
#but use the bootstraptest script to make sure you're using the correct distribution

dflistsim <- lapply(dflist, function(x) Dist1 = rgamma(n = x$n_sampled, shape = x$shape, scale = x$scale))

#make list into a new dataframe of simulated raw data
newrawdata <- ldply(dflistsim, cbind)

#rename columns to match original data
newrawdata <- rename(newrawdata, c(".id" = "explode"))
newrawdata <- rename(newrawdata, c("1" = "simvalue"))
newrawdata$explode <- as.factor(newrawdata$explode)

#make a df to connect new raw data to original data
key <- unique(sms[c("Study_ID", "explode")])
key$explode <- as.factor(key$explode)

#join the newrawdata to the key to get a list of simulated values by study id
simrawdata <- newrawdata %>%
  left_join(key)

write.csv(simrawdata, file = "/Users/rana7082-su/Dropbox/C_fire_invasives_R/data/simrawdata.csv", row.names = FALSE)
###
