#Data summary
#Dr. R. Chelsea Nagy
#created February 28, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

#no fire data; #bring in studyid dataframe
siwf <- studyid_sf

rawsonly <- siwf %>%
  filter(!study %in% smeans)
