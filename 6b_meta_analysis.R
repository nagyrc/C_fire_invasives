#meta-analysis
#Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("MCMCglmm", "MCMCvis", "metafor")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

citation("MCMCglmm")
citation("metafor")

## get full dataset from github (includes all cases and studies)
#dq2 <- read.csv("Meta_analysis_sheet_manual.csv", header = T)
dq2 <- read.csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeans2.csv", header = T)
head(dq2)
tail(dq2)
summary(dq2) # no missing values in n, mean, variance



################################################
## Calculating Effect Size and Variance
################################################

#for the paired sites

# Computing observation-level SD
#converting se and ci into sd
#for us "control" will probably be the sagecheat sites
#Not necessary, we already have SD calculated

#dq2$SD_control[dq2$var_statistic == "SE"] <- dq2$varsage[dq2$var_statistic == "SE"]*sqrt(dq2$nsage[dq2$var_statistic == "SE"])
#dq2$SD_control[dq2$var_statistic == "CI"] <- (sqrt(dq2$nsage[dq2$var_statistic == "CI"]))*(((dq2$meansage[dq2$var_statistic == "CI"] + dq2$varsage[dq2$var_statistic == "CI"])- dq2$meansage[dq2$var_statistic == "CI"])/1.96)

#will need to calculate sd for all three veg categories
head(dq2)
dq2$sdsage <- sqrt(dq2$varsage)
dq2$sdsagecheat <- sqrt(dq2$varsagecheat)
dq2$sdcheat <- sqrt(dq2$varcheat)


# Computing the pooled standard deviations for the Hedges' index
#this is the equation in the book
#at this point we have three groups - cheat (invaded, burned), sagecheat (invaded, unburned), and sage (uninvaded, unburned)

dq2$SD_hedge_cheat_v_sage <- sqrt((dq2$sdsage^2*(dq2$nsage-1)+dq2$sdcheat^2*(dq2$ncheat-1))/(dq2$nsage+dq2$ncheat-2))
dq2$SD_hedge_cheat_v_sagecheat <- sqrt((dq2$sdsagecheat^2*(dq2$nsagecheat-1)+dq2$sdcheat^2*(dq2$ncheat-1))/(dq2$nsagecheat+dq2$ncheat-2))
dq2$SD_hedge_sagecheat_v_sage <- sqrt((dq2$sdsage^2*(dq2$nsage-1)+dq2$sdsagecheat^2*(dq2$nsagecheat-1))/(dq2$nsage+dq2$nsagecheat-2))


## Compute J (an adjustment for small n)
#J is also in the book- best for studies with small sample sizes
#calculate one per effect size- here this is control vs invasion
dq2$df_cheat_v_sage = dq2$nsage + dq2$ncheat - 2
dq2$J_cheat_v_sage = 1 - (3 / (4*dq2$df_cheat_v_sage - 1))

dq2$df_cheat_v_sagecheat = dq2$nsagecheat + dq2$ncheat - 2
dq2$J_cheat_v_sagecheat = 1 - (3 / (4*dq2$df_cheat_v_sagecheat - 1))

dq2$df_sagecheat_v_sage = dq2$nsage + dq2$nsagecheat - 2
dq2$J_sagecheat_v_sage = 1 - (3 / (4*dq2$df_sagecheat_v_sage - 1))

## Compute effect sizes (Hedge's g) with the J adjustment
#result is the effect size for each row in the data set 
dq2$g_cheat_v_sage <- ((dq2$meancheat - dq2$meansage)/ dq2$SD_hedge_cheat_v_sage)*dq2$J_cheat_v_sage
dq2$g_cheat_v_sagecheat <- ((dq2$meancheat - dq2$meansagecheat)/ dq2$SD_hedge_cheat_v_sagecheat)*dq2$J_cheat_v_sagecheat
dq2$g_sagecheat_v_sage <- ((dq2$meansagecheat - dq2$meansage)/ dq2$SD_hedge_sagecheat_v_sage)*dq2$J_sagecheat_v_sage


###
#explore Hedge's g
plot(dq2$pool, dq2$g_cheat_v_sage)
plot(dq2$pool, dq2$g_cheat_v_sagecheat)
plot(dq2$pool, dq2$g_sagecheat_v_sage)

plot(dq2$g_cheat_v_sage, dq2$g_cheat_v_sagecheat)
plot(dq2$g_sagecheat_v_sage, dq2$g_cheat_v_sagecheat)
plot(dq2$g_cheat_v_sage, dq2$g_sagecheat_v_sage)

plot(dq2$Article_ID, dq2$g_cheat_v_sage)
plot(dq2$Article_ID, dq2$g_cheat_v_sagecheat)
plot(dq2$Article_ID, dq2$g_sagecheat_v_sage)

summary(dq2$g_cheat_v_sage)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-2.8029 -0.8358  0.5795  0.4308  0.8052  7.0844      27

summary(dq2$g_cheat_v_sagecheat)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-0.91954 -0.39820 -0.01347 -0.03948  0.33284  0.59109       20

summary(dq2$g_sagecheat_v_sage)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-1.03766 -0.05919  0.16217  0.18132  0.70275  1.03849       23 
###


## Calculate Variance (MEV)-- Whitlock 2014
#calculates the variance of hedges statistic
dq2$var_d_cheat_v_sage = ((dq2$nsage + dq2$ncheat/dq2$nsage * dq2$ncheat) + dq2$g_cheat_v_sage^2/(2*(dq2$nsage + dq2$ncheat -2))) * ((dq2$nsage + dq2$ncheat)/(dq2$nsage + dq2$ncheat -2))
dq2$var_d_cheat_v_sagecheat = ((dq2$nsagecheat + dq2$ncheat/dq2$nsagecheat * dq2$ncheat) + dq2$g_cheat_v_sagecheat^2/(2*(dq2$nsagecheat + dq2$ncheat -2))) * ((dq2$nsagecheat + dq2$ncheat)/(dq2$nsagecheat + dq2$ncheat -2))
dq2$var_d_sagecheat_v_sage = ((dq2$nsage + dq2$nsagecheat/dq2$nsage * dq2$nsagecheat) + dq2$g_sagecheat_v_sage^2/(2*(dq2$nsage + dq2$nsagecheat -2))) * ((dq2$nsage + dq2$nsagecheat)/(dq2$nsage + dq2$nsagecheat -2))


####################################################################
## Meta-Analysis
###################################################################

#subset data by carbon pool
orgsoil <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_cheat_v_sage))
orgsoil2 <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_sagecheat_v_sage))
orgsoil3 <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_cheat_v_sagecheat))

# set priors
#non informative uniform priors
prior <- list(R = list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

# run random effects model, three chains
# we will get an error because we don't have replication with our study ID. this is OK
#make nitt smaller- 10,000 when we start to help run faster
#fit= dq2

#this is with depth category in the model (could be factor or numeric) as a fixed effect
m1a_inv <- MCMCglmm(g_cheat_v_sage ~  depth_cat, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

#got this message when I added in just cheatfire, in addition to depth_cat
#Warning message:
#In MCMCglmm(g_cheat_v_sage ~ depth_cat + cheatfire, random = ~Article_ID,  :
              #some fixed effects are not estimable and have been removed. Use singular.ok=TRUE to sample these effects, but use an informative prior!

#this is without depth as a fixed effect
#m1a_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    #prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    #data = orgsoil, pr = T, saveX = T, saveZ = T)

m1b_inv <- MCMCglmm(g_cheat_v_sage ~ depth_cat, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

m1c_inv <- MCMCglmm(g_cheat_v_sage ~ depth_cat, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

summary(m1a_inv)
summary(m1b_inv)
summary(m1c_inv)

#need to run this without Article_ID as random effect; random effect removed here
m2a_inv <- MCMCglmm(g_sagecheat_v_sage ~ depth_cat, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)


m2b_inv <- MCMCglmm(g_sagecheat_v_sage ~ depth_cat, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)


m2c_inv <- MCMCglmm(g_sagecheat_v_sage ~ depth_cat, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)

summary(m2a_inv)
summary(m2b_inv)
summary(m2c_inv)

#ok to leave Article_ID in here as random effect
m3a_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

m3b_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

m3c_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

summary(m3a_inv)
summary(m3b_inv)
summary(m3c_inv)



###
###
#effect of cheat vs. sage
#NOTE that if we want to include a fixed effect then gInv~ would have a variable listed

# combine 3 chains into 1 mcmc object
m1_inv = mcmc.list(m1a_inv[[1]], m1b_inv[[1]], m1c_inv[[1]])

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






###
#effect of sagecheat vs. sage
# combine 3 chains into 1 mcmc object
m2_inv = mcmc.list(m2a_inv[[1]], m2b_inv[[1]], m2c_inv[[1]])


#THIS IS HOW WE CHECK THE MODEL#
# diagnostics to ensure good model behavior
inv_overall <- MCMCsummary(m2_inv, params = "(Intercept)", n.eff = T)

#we want this density plot to look relatively smooth; if not smooth, increase burnin and increase number of iterations
MCMCtrace(m2_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m2_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal; up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m2_inv[ , "(Intercept)"]) 

#we want the posteriors to converge on 1; if they dont, up burnin and interations
gelman.diag(m2_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m2_inv[ , "(Intercept)"])

#If everything looks good here, then the intercept value is the effect




###
#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m3_inv = mcmc.list(m3a_inv[[1]], m3b_inv[[1]], m3c_inv[[1]])


#THIS IS HOW WE CHECK THE MODEL#
# diagnostics to ensure good model behavior
inv_overall <- MCMCsummary(m3_inv, params = "(Intercept)", n.eff = T)

#we want this density plot to look relatively smooth; if not smooth, increase burnin and increase number of iterations
MCMCtrace(m3_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m3_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal; up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m3_inv[ , "(Intercept)"]) 

#we want the posteriors to converge on 1; if they dont, up burnin and interations
gelman.diag(m3_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m3_inv[ , "(Intercept)"])

#If everything looks good here, then the intercept value is the effect



###
###
###
#total soil; effect of cheat vs. sagecheat
#only 2 studies and both are 0 - 10 cm; so don't need depth_cat
totsoil <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_cheat_v_sagecheat))

m4a_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

m4b_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

m4c_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

summary(m4a_inv)
summary(m4b_inv)
summary(m4c_inv)

#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m4_inv = mcmc.list(m4a_inv[[1]], m4b_inv[[1]], m4c_inv[[1]])

#THIS IS HOW WE CHECK THE MODEL#

# diagnostics to ensure good model behavior
inv_overall <- MCMCsummary(m4_inv, params = "(Intercept)", n.eff = T)

#we want this density plot to look relatively smooth
#if not smooth, increase burnin and increase number of iterations
MCMCtrace(m4_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m4_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal
#up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m4_inv[ , "(Intercept)"]) 


#we want the posteriors to converge on 1
#if they dont, up burnin and interations
gelman.diag(m4_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m4_inv[ , "(Intercept)"])


#If everything looks good here, then the intercept value is the effect

