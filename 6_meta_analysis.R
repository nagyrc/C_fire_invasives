#meta-analysis
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("MCMCglmm", "MCMCvis", "metafor")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")


## get full dataset from github (includes all cases and studies)
d1 <- read.csv("Meta_analysis_sheet_manual.csv", header = T)
head(d1)
summary(d1) # no missing values in n, mean, variance
dim(d1) #39 x 14


################################################
## Calculating Effect Size and Variance
################################################

#for the paired sites

# Computing observation-level SD
#converting se and ci into sd
#for us "control" will probably be the sagecheat sites
dq2$SD_control[dq2$var_statistic == "SE"] <- dq2$var_control[dq2$var_statistic == "SE"]*sqrt(dq2$n_control[dq2$var_statistic == "SE"])
dq2$SD_control[dq2$var_statistic == "CI"] <- (sqrt(dq2$n_control[dq2$var_statistic == "CI"]))*(((dq2$mean_control[dq2$var_statistic == "CI"] + dq2$var_control[dq2$var_statistic == "CI"])- dq2$mean_control[dq2$var_statistic == "CI"])/1.96)

#will need to calculate sd for the invaded and invaded fire sites as well

# Computing the pooled standard deviations for the Hedges' index
#this is the equation in the book
#inv is the invasion treatment
dq2$SD_pool_inv <- sqrt((dq2$SD_control^2*(dq2$n_control-1)+dq2$SD_inv^2*(dq2$n_inv-1))/(dq2$n_control+dq2$n_inv-2))

## Compute J (an adjustment for small n)
#J is also in the book- best for studies with small sample sizes
#calculate one per effect size- here this is control vs invasion
dq2$df_inv = dq2$n_control + dq2$n_inv - 2
dq2$J_inv = 1 - (3 / (4*dq2$df_inv -1))

## Compute effect sizes (Hedge's g) with the J adjustment
#result is the effect size for each row in the data set 
dq2$gInv <- ((dq2$mean_inv - dq2$mean_control)/ dq2$SD_pool_inv)*dq2$J_inv

## Calculate Variance (MEV)-- Whitlock 2014
#calculates the variance of hedges statistic
dq2$var_d_inv = ((dq2$n_control + dq2$n_inv/dq2$n_control * dq2$n_inv) + dq2$gInv^2/(2*(dq2$n_control + dq2$n_inv -2))) * ((dq2$n_control + dq2$n_inv)/(dq2$n_control + dq2$n_inv -2))

####################################################################
## Meta-Analysis
###################################################################
library(MCMCglmm)
library(MCMCvis)

#subset data by carbon pool

# set priors
#non informative uniform priors
prior <- list(R=list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

# run random effects model, three chains
# we will get an error because we don't have replication with our study ID. this is OK
#make nitt smaller- 10,000 when we start to help run faster
#fit= dq2
m1a_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)

m1b_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)

m1c_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)
summary(m1a_inv)
summary(m1b_inv)
summary(m1c_inv)

#NOTE that if we want to include a fixed effect then gInv~ would have a variable listed

# combine 3 chains into 1 mcmc object
m1_inv=mcmc.list(m1a_inv[[1]], m1b_inv[[1]], m1c_inv[[1]])


#THIS IS HOW WE CHECK THE MODEL#

# diagnostics to ensure good model behavior
inv_overall <- MCMCsummary(m1_inv, params = "(Intercept)", n.eff = T)

#we want this density plot to look relatively smooth
#if not smooth, increase burnin and increase number of iterations
MCMCtrace(m1_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m1_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal
#up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m1_inv[ , "(Intercept)"]) 


#we want the posteriors to converge on 1
#if they dont, up burnin and interations
gelman.diag(m1_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m1_inv[ , "(Intercept)"])


#If everything looks good here, then the intercept value is the effect

