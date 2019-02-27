#meta-analysis
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("tidyverse", "sf", "assertthat", "purrr", "httr", "plyr", "stringr", "raster", "ggplot2", "doBy", "reshape", "velox")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

require(MCMCglmm)
require(MCMCvis)
require(metafor)

setwd("C:\\Users\\Localadmin\\Dropbox\\Research\\Current projects\\Invasion and Global Change Meta\\Analysis\\Data")

## get full dataset from github (includes all cases and studies)
d1 <- read.csv("Inv_GC_database_19Oct2018.csv", header = T)
head(d1)
summary(d1) # no missing values in n, mean, variance
dim(d1) #183 cases

## simplify taxon classes to one taxon level per study
d1$taxon1[d1$species_taxon == "plant" | d1$species_taxon == "algae"] = "Producer"
d1$taxon1[d1$species_taxon == "crustacean" | d1$species_taxon == "fish" | 
            d1$species_taxon == "insect"| d1$species_taxon == "mollusc"] = "Consumer"
table(d1$taxon1)

###################################################################
## Calculate Effect Sizes-- Hedge's g
###################################################################

## Correcting effect signs in d
## to be consistent across studies, want a higher mean response value to be beneficial for response organism/system 
## "no" benefit values mean that the sign of the mean values extracted should be swapped
## for example, mortality would be "no", survival would be "yes"
d1$mean_control[d1$benefit == "no"] = -1*d1$mean_control[d1$benefit == "no"] 
d1$mean_inv[d1$benefit == "no"] = -1*d1$mean_inv[d1$benefit == "no"] 
d1$mean_gc[d1$benefit == "no"] = -1*d1$mean_gc[d1$benefit == "no"] 
d1$mean_inv_gc[d1$benefit == "no"] = -1*d1$mean_inv_gc[d1$benefit == "no"] 

## check variance stats
table(d1$var_statistic) ## 1 CI, remainder are SE

## Computing standard deviations of observations
d1$SD_control[d1$var_statistic == "SE"] <- d1$var_control[d1$var_statistic == "SE"]*sqrt(d1$n_control[d1$var_statistic == "SE"])
d1$SD_inv[d1$var_statistic == "SE"]     <- d1$var_inv[d1$var_statistic == "SE"]*sqrt(d1$n_inv[d1$var_statistic == "SE"])
d1$SD_gc[d1$var_statistic == "SE"]      <- d1$var_gc[d1$var_statistic == "SE"]*sqrt(d1$n_gc[d1$var_statistic == "SE"])
d1$SD_inx[d1$var_statistic == "SE"]     <- d1$var_inv_gc[d1$var_statistic == "SE"]*sqrt(d1$n_inv_gc[d1$var_statistic == "SE"])

d1$SD_control[d1$var_statistic == "CI"] <- (sqrt(d1$n_control[d1$var_statistic == "CI"]))*(((d1$mean_control[d1$var_statistic == "CI"] + d1$var_control[d1$var_statistic == "CI"])- d1$mean_control[d1$var_statistic == "CI"])/1.96)
d1$SD_inv[d1$var_statistic == "CI"] <- (sqrt(d1$n_control[d1$var_statistic == "CI"]))*(((d1$mean_control[d1$var_statistic == "CI"] + d1$var_control[d1$var_statistic == "CI"])- d1$mean_control[d1$var_statistic == "CI"])/1.96)
d1$SD_gc[d1$var_statistic == "CI"] <- (sqrt(d1$n_control[d1$var_statistic == "CI"]))*(((d1$mean_control[d1$var_statistic == "CI"] + d1$var_control[d1$var_statistic == "CI"])- d1$mean_control[d1$var_statistic == "CI"])/1.96)
d1$SD_inx[d1$var_statistic == "CI"] <- (sqrt(d1$n_control[d1$var_statistic == "CI"]))*(((d1$mean_control[d1$var_statistic == "CI"] + d1$var_control[d1$var_statistic == "CI"])- d1$mean_control[d1$var_statistic == "CI"])/1.96)


# Computing the pooled standard deviations for the Hedges' index, single treatment effects
d1$SD_pool_inv <- sqrt((d1$SD_control^2*(d1$n_control-1)+d1$SD_inv^2*(d1$n_inv-1))/(d1$n_control+d1$n_inv-2))
d1$SD_pool_gc <- sqrt((d1$SD_control^2*(d1$n_control-1)+d1$SD_gc^2*(d1$n_gc-1))/(d1$n_control+d1$n_gc-2))

# computing pooled standard deviations for Hedge's index, interction effect (following Jackson et al. 2016)
inv_gc_SD_pool <-sqrt((d1$SD_inv^2*(d1$n_inv-1)+d1$SD_gc^2*(d1$n_gc-1))/(d1$n_inv+d1$n_gc-2)) 
d1$SD_pool_inx <- sqrt((d1$SD_inx^2*(d1$n_inv_gc-1) + (inv_gc_SD_pool)^2*(d1$n_inv + d1$n_gc)) / (d1$n_inv_gc + d1$n_inv + d1$n_gc -2))


## Compute J (an adjustment for small n) following Jackson et al. 2016 for interaction
d1$df_inv = d1$n_control + d1$n_inv - 2
d1$df_gc = d1$n_control + d1$n_gc - 2
d1$df_intx = d1$n_inv_gc + d1$n_inv + d1$n_gc -2

d1$J_inv = 1 - (3 / (4*d1$df_inv -1))
d1$J_gc = 1 - (3 / (4*d1$df_gc -1))
d1$J_intx = 1 - (3 / (4*d1$df_intx -1))

## Compute observed effect sizes (Hedge's g) for single treatment effects, relative to control
d1$gInv <- ((d1$mean_inv - d1$mean_control)/ d1$SD_pool_inv)*d1$J_inv
d1$gGC <- ((d1$mean_gc - d1$mean_control)/ d1$SD_pool_gc)*d1$J_gc

## Compute observed effect sizes (Hedge's g) for interaction treatment effects, relative to additive expectation (following Jackson et al. 2016)
d1$intx_pred <- (d1$mean_inv - d1$mean_control) + (d1$mean_gc - d1$mean_control) + d1$mean_control
d1$gInv_GC <- ((d1$mean_inv_gc - d1$intx_pred)/ d1$SD_pool_inx)*d1$J_intx

## examine effect sizes
par(mfrow = c(3,1))
hist(d1$gInv, xlim = c(-8, 17), main = "Invasive", xlab = "Effect size") 
hist(d1$gGC, xlim = c(-8, 17), main = "Global Change", xlab = "Effect size")
hist(d1$gInv_GC, xlim = c(-8, 17), main = "Interaction", xlab = "Effect size")

## retain all cases
d2 <- d1

## remove very large effect sizes
d1 = d1[d1$gInv > -20, ]
dim(d1) # removed 3 cases

##################################################################
## sign change for negative interaction predictions (Jackson et al. 2016)
#################################################################
dim(d1[d1$intx_pred < 0 , ]) #35 observations have negative interaction effect

d1$gInv_GC[d1$intx_pred < 0 ] = d1$gInv_GC[d1$intx_pred < 0 ] *-1


#####################################################################
## Calculate Variance (MEV)-- from Raj, Whitlock 2014
########################################################################
## observed
d1$var_d_inv = ((d1$n_control + d1$n_inv/d1$n_control * d1$n_inv) + d1$gInv^2/(2*(d1$n_control + d1$n_inv -2))) * ((d1$n_control + d1$n_inv)/(d1$n_control + d1$n_inv -2))
d1$var_d_gc = ((d1$n_control + d1$n_gc/d1$n_control * d1$n_gc) + d1$gGC^2/(2*(d1$n_control + d1$n_gc -2))) * ((d1$n_control + d1$n_gc)/(d1$n_control + d1$n_gc -2))
d1$var_d_intx = ((d1$n_inv + d1$n_gc + d1$n_inv_gc/(d1$n_inv + d1$n_gc) * d1$n_inv_gc) + d1$gInv_GC^2/(2*((d1$n_inv + d1$n_gc) + d1$n_inv_gc -2))) * (((d1$n_inv + d1$n_gc) + d1$n_inv_gc)/((d1$n_inv + d1$n_gc) + d1$n_inv_gc -2))

hist(d1$var_d_inv, main = "Invasion", xlim = c(0, 100))
hist(d1$var_d_gc, main = "Global Change", xlim = c(0, 100))
hist(d1$var_d_intx, main = "Interaction", xlim = c(0, 100))

####################################################################
## Data subset for analysis
####################################################################
## NOTE: do sensitivity analysis with all GCs after constrained analysis
fit <- d1[d1$gc_factor == "drought" | d1$gc_factor == "nitrogen" | d1$gc_factor == "temperature", ]
summary(fit)
dim(fit) #171 cases

###############################################################
## Model: Overall Invasion Effect
##############################################################
## priors used for all  models (uninformative)
prior <- list(R=list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

## run 3 chains
m1a_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)

m1b_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)

m1c_inv <- MCMCglmm(gInv ~ 1, random = ~studyID, mev = fit$var_d_inv,
                    prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                    data = fit, pr = T, saveX = T, saveZ = T)

## check each chain
summary(m1a_inv)
summary(m1b_inv)
summary(m1c_inv)

## combine chains
m1_inv=mcmc.list(m1a_inv[[1]], m1b_inv[[1]], m1c_inv[[1]])

## examine results, check convergence
print(inv_overall <- MCMCsummary(m1_inv, params = "(Intercept)", n.eff = T))
MCMCtrace(m1_inv, params = "(Intercept)", pdf = F, ind = T)
#autocorr.plot(m1_inv) #all
autocorr.plot(m1_inv[, "(Intercept)"]) 
#gelman.plot(m1_inv[ , "(Intercept)"]) #not working
geweke.diag(m1_inv[ , "(Intercept)"])
gelman.diag(m1_inv[ , "(Intercept)"])

###############################################################
## Model: Overall GC Effect
##############################################################
rm(prior)
gc()

## priors used for all  models (uninformative)
prior <- list(R=list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

## run 3 chains
m1a_gc <- MCMCglmm(gGC ~ 1, random = ~studyID, mev = fit$var_d_gc,
                   prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                   data = fit, pr = T, saveX = T, saveZ = T)

m1b_gc <- MCMCglmm(gGC ~ 1, random = ~studyID, mev = fit$var_d_gc,
                   prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                   data = fit, pr = T, saveX = T, saveZ = T)

m1c_gc <- MCMCglmm(gGC ~ 1, random = ~studyID, mev = fit$var_d_gc,
                   prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                   data = fit, pr = T, saveX = T, saveZ = T)


## check each chain
summary(m1a_gc)
summary(m1b_gc)
summary(m1c_gc)

## combine chains
m1_gc=mcmc.list(m1a_gc[[1]], m1b_gc[[1]], m1c_gc[[1]])

## examine results, check convergence
gc_overall <- MCMCsummary(m1_gc, params = "(Intercept)", n.eff = T)
MCMCtrace(m1_gc, params = "(Intercept)", pdf = F, ind = T)
#autocorr.plot(m1_gc) #all
autocorr.plot(m1_gc[, "(Intercept)"]) 
#gelman.plot(m1_gc[ , "(Intercept)"]) #not working
geweke.diag(m1_gc[ , "(Intercept)"])
gelman.diag(m1_gc[ , "(Intercept)"])



###############################################################
## Model: Overall Interaction Effect
##############################################################
rm(prior)
gc()

## priors used for all  models (uninformative)
prior <- list(R=list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

## run 3 chains
m1a_intx <- MCMCglmm(gInv_GC ~ 1, random = ~studyID, mev = fit$var_d_intx,
                     prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                     data = fit, pr = T, saveX = T, saveZ = T)

m1b_intx <- MCMCglmm(gInv_GC ~ 1, random = ~studyID, mev = fit$var_d_intx,
                     prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                     data = fit, pr = T, saveX = T, saveZ = T)

m1c_intx <- MCMCglmm(gInv_GC ~ 1, random = ~studyID, mev = fit$var_d_intx,
                     prior = prior, nitt = 1000000, burnin = 10000, thin = 100, verbose = T,
                     data = fit, pr = T, saveX = T, saveZ = T)


## check each chain
summary(m1a_intx)
summary(m1b_intx)
summary(m1c_intx)

## combine chains
m1_intx=mcmc.list(m1a_intx[[1]], m1b_intx[[1]], m1c_intx[[1]])

## examine results, check convergence
intx_overall <- MCMCsummary(m1_intx, params = "(Intercept)", n.eff = T)
MCMCtrace(m1_intx, params = "(Intercept)", pdf = F, ind = T)
#autocorr.plot(m1_intx) #all
autocorr.plot(m1_intx[, "(Intercept)"]) 
#gelman.plot(m1_intx[ , "(Intercept)"]) #not working
geweke.diag(m1_intx[ , "(Intercept)"])
gelman.diag(m1_intx[ , "(Intercept)"])

#########################################################
## PLOT: Overall Effects
##############################################################
## summarize posteriors of interest
inv <- summary(m1_inv[ , "(Intercept)"])
gc <- summary(m1_gc[ , "(Intercept)"])
intx <- summary(m1_intx[ , "(Intercept)"])

# get quantiles for plotting
coef = data.frame(rbind(inv$quantiles, gc$quantiles, intx$quantiles))
coef$variable <- c("Invasive", "Global Change", "Interaction")
coef

## plot coefficeint for each treatment (posterior mean and 95% CI for effect size of posterior) 
par(mfrow = c(1,1))
forest (x = coef[ , 3], ci.lb = coef[ , 1], ci.ub = coef[ , 5], slab = coef$variable, 
        xlab = "Effect Size",psize=1, cex = 1) 


