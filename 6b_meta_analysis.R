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
dq2 <- read.csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeanscat.csv", header = T)
head(dq2)
tail(dq2)
summary(dq2) # there are some missing variance values from ind points



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
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-2.80293 -0.12836  0.57953 -0.06095  0.65436  1.14530       76 

summary(dq2$g_cheat_v_sagecheat)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-1.2521 -0.4486 -0.1834 -0.1798  0.2414  0.5911      58 

summary(dq2$g_sagecheat_v_sage)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#-1.03766 -0.22098  0.08901  0.15766  0.67002  1.03849       67
###


## Calculate Variance (MEV)-- Whitlock 2014
#calculates the variance of hedges statistic
dq2$var_d_cheat_v_sage = ((dq2$nsage + dq2$ncheat/dq2$nsage * dq2$ncheat) + dq2$g_cheat_v_sage^2/(2*(dq2$nsage + dq2$ncheat -2))) * ((dq2$nsage + dq2$ncheat)/(dq2$nsage + dq2$ncheat -2))
dq2$var_d_cheat_v_sagecheat = ((dq2$nsagecheat + dq2$ncheat/dq2$nsagecheat * dq2$ncheat) + dq2$g_cheat_v_sagecheat^2/(2*(dq2$nsagecheat + dq2$ncheat -2))) * ((dq2$nsagecheat + dq2$ncheat)/(dq2$nsagecheat + dq2$ncheat -2))
dq2$var_d_sagecheat_v_sage = ((dq2$nsage + dq2$nsagecheat/dq2$nsage * dq2$nsagecheat) + dq2$g_sagecheat_v_sage^2/(2*(dq2$nsage + dq2$nsagecheat -2))) * ((dq2$nsage + dq2$nsagecheat)/(dq2$nsage + dq2$nsagecheat -2))


####################################################################
## Meta-Analysis
####################################################################

#subset data by carbon pool; first organic soil
orgsoil <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_cheat_v_sage)) #8
orgsoil2 <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #16
orgsoil3 <- subset(dq2, pool == "orgsoilC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #15

# set priors
#non informative uniform priors
prior <- list(R = list(V = 1e-10, nu = -1), G = list(G1 = list(V = 1e-10, nu = -1)))

# run random effects model, three chains
# we will get an error because we don't have replication with our study ID. this is OK
#make nitt smaller- 10,000 when we start to help run faster
#fit= dq2

#removed depth_cat; not equal representation across depths
m1a_inv <- MCMCglmm(g_cheat_v_sage ~  1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

#got this message when I added in just cheatfire, in addition to depth_cat
#Warning message:
#In MCMCglmm(g_cheat_v_sage ~ depth_cat + cheatfire, random = ~Article_ID,  :
              #some fixed effects are not estimable and have been removed. Use singular.ok=TRUE to sample these effects, but use an informative prior!

#this is without depth as a fixed effect
#m1a_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    #prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    #data = orgsoil, pr = T, saveX = T, saveZ = T)

m1b_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

m1c_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

summary(m1a_inv)
summary(m1b_inv)
summary(m1c_inv)

#these are having trouble running; tried removing depth_cat; still won't run
m2a_inv <- MCMCglmm(g_sagecheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)


m2b_inv <- MCMCglmm(g_sagecheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)


m2c_inv <- MCMCglmm(g_sagecheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil2$var_d_sagecheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil2, pr = T, saveX = T, saveZ = T)

summary(m2a_inv)
summary(m2b_inv)
summary(m2c_inv)

#ok to leave Article_ID in here as random effect; removed depth_cat
m3a_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

m3b_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

m3c_inv <- MCMCglmm(g_cheat_v_sagecheat ~ depth_cat, random = ~ Article_ID, mev = orgsoil3$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil3, pr = T, saveX = T, saveZ = T)

summary(m3a_inv)
summary(m3b_inv)
summary(m3c_inv)



###
###
#effect of cheat vs. sage
#NOTE that if we want to include a fixed effect then gInv~ would have a variable listed


#nitro_inv <- m2_inv[,"(Intercept)"][[1]] + m2_inv[,"gc_factornitrogen"][[1]]
#drought_inv <- m2_inv[,"(Intercept)"][[1]] + m2_inv[,"gc_factordrought"][[1]]
#temp_inv <- m2_inv[,"(Intercept)"][[1]] ###similar to my deep

#deepSOC <-m1a_inv[,"(Intercept)"][[1]] 
#catshallowSOC <- m1a_inv[,"(Intercept)"][[1]] + m1a_inv[,"depth_catshallow"][[1]]
#catmidSOC <- m1a_inv[,"(Intercept)"][[1]] + m1a_inv[,"depth_catmid"][[1]]

#deepSOC <-m1b_inv[,"(Intercept)"][[1]] 
#catshallowSOC <- m1b_inv[,"(Intercept)"][[1]] + m1b_inv[,"depth_catshallow"][[1]]
#catmidSOC <- m1b_inv[,"(Intercept)"][[1]] + m1b_inv[,"depth_catmid"][[1]]

#deepSOC <-m1c_inv[,"(Intercept)"][[1]] 
#catshallowSOC <- m1c_inv[,"(Intercept)"][[1]] + m1c_inv[,"depth_catshallow"][[1]]
#catmidSOC <- m1c_inv[,"(Intercept)"][[1]] + m1c_inv[,"depth_catmid"][[1]]

# combine 3 chains into 1 mcmc object; cheat_v_sage
m1_inv <- mcmc.list(m1a_inv[[1]], m1b_inv[[1]], m1c_inv[[1]])
m1_inv

summary(m1_inv)

#deepSOC <- m1_inv[,"(Intercept)"][[1]] 
#catshallowSOC <- m1_inv[,"(Intercept)"][[1]] + m1_inv[,"depth_catshallow"][[1]]
#catmidSOC <- m1_inv[,"(Intercept)"][[1]] + m1_inv[,"depth_catmid"][[1]]

#modobj1 <- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#deepSOC <- m1_inv[,"(Intercept)"][[2]] 
#catshallowSOC <- m1_inv[,"(Intercept)"][[2]] + m1_inv[,"depth_catshallow"][[2]]
#catmidSOC <- m1_inv[,"(Intercept)"][[2]] + m1_inv[,"depth_catmid"][[2]]

#modobj2<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#deepSOC <-m1_inv[,"(Intercept)"][[3]] 
#catshallowSOC <- m1_inv[,"(Intercept)"][[3]] + m1_inv[,"depth_catshallow"][[3]]
#catmidSOC <- m1_inv[,"(Intercept)"][[3]] + m1_inv[,"depth_catmid"][[3]]

#modobj3<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#modobj <- mcmc.list(modobj1, modobj2, modobj3)
#modobj

#this has the numbers for Table 3
#summary(modobj)



#THIS IS HOW WE CHECK THE MODEL#

#new1_inv <- mcmc(cbind(temp_inv, nitro_inv, drought_inv))
summary(m1_inv)

# diagnostics to ensure good model behavior
m1inv_overall <- MCMCsummary(m1_inv, params = c("(Intercept)", "depth_catshallow", "depth_catmid"), n.eff = T)
m1inv_overall

#we want this density plot to look relatively smooth
#if not smooth, increase burnin and increase number of iterations
MCMCtrace(m1_inv, params = c("(Intercept)", "depth_catshallow", "depth_catmid"), pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m1_inv[, c("(Intercept)", "depth_catshallow", "depth_catmid")]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal
#up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")]) 
#Error in gelman.preplot(x, bin.width = bin.width, max.bins = max.bins,  : 
#Insufficient iterations to produce Gelman-Rubin plot

#we want the posteriors to converge on 1
#if they dont, up burnin and interations
gelman.diag(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")])


#If everything looks good here, then the intercept value is the effect






###
#effect of sagecheat vs. sage
# combine 3 chains into 1 mcmc object
m2_inv = mcmc.list(m2a_inv[[1]], m2b_inv[[1]], m2c_inv[[1]])
summary(m2_inv)

#deepSOC <- m2_inv[,"(Intercept)"][[1]] 
#catshallowSOC <- m2_inv[,"(Intercept)"][[1]] + m2_inv[,"depth_catshallow"][[1]]
#catmidSOC <- m2_inv[,"(Intercept)"][[1]] + m2_inv[,"depth_catmid"][[1]]

#modobj4 <- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#deepSOC <- m2_inv[,"(Intercept)"][[2]] 
#catshallowSOC <- m2_inv[,"(Intercept)"][[2]] + m2_inv[,"depth_catshallow"][[2]]
#catmidSOC <- m2_inv[,"(Intercept)"][[2]] + m2_inv[,"depth_catmid"][[2]]

#modobj5<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#deepSOC <-m2_inv[,"(Intercept)"][[3]] 
#catshallowSOC <- m2_inv[,"(Intercept)"][[3]] + m2_inv[,"depth_catshallow"][[3]]
#catmidSOC <- m2_inv[,"(Intercept)"][[3]] + m2_inv[,"depth_catmid"][[3]]

#modobj6<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

#modobjj <- mcmc.list(modobj4, modobj5, modobj6)
#modobjj

#this has the numbers for Table 3
#summary(modobjj)



#THIS IS HOW WE CHECK THE MODEL#
# diagnostics to ensure good model behavior
m2inv_overall <- MCMCsummary(m2_inv, params = "(Intercept)", n.eff = T)
m2inv_overall

#we want this density plot to look relatively smooth; if not smooth, increase burnin and increase number of iterations
MCMCtrace(m2_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m2_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal; up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m2_inv[ , "(Intercept)"]) 
#Error in gelman.preplot(x, bin.width = bin.width, max.bins = max.bins,  : 
#Insufficient iterations to produce Gelman-Rubin plot

#we want the posteriors to converge on 1; if they dont, up burnin and interations
gelman.diag(m2_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m2_inv[ , "(Intercept)"])

#If everything looks good here, then the intercept value is the effect




###
#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m3_inv = mcmc.list(m3a_inv[[1]], m3b_inv[[1]], m3c_inv[[1]])
summary(m3_inv)

#if using depth categories, do this instead
deepSOC <- m3_inv[,"(Intercept)"][[1]] 
catshallowSOC <- m3_inv[,"(Intercept)"][[1]] + m3_inv[,"depth_catshallow"][[1]]
catmidSOC <- m3_inv[,"(Intercept)"][[1]] + m3_inv[,"depth_catmid"][[1]]

modobj7 <- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

deepSOC <- m3_inv[,"(Intercept)"][[2]] 
catshallowSOC <- m3_inv[,"(Intercept)"][[2]] + m3_inv[,"depth_catshallow"][[2]]
catmidSOC <- m3_inv[,"(Intercept)"][[2]] + m3_inv[,"depth_catmid"][[2]]

modobj8<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

deepSOC <-m3_inv[,"(Intercept)"][[3]] 
catshallowSOC <- m3_inv[,"(Intercept)"][[3]] + m3_inv[,"depth_catshallow"][[3]]
catmidSOC <- m3_inv[,"(Intercept)"][[3]] + m3_inv[,"depth_catmid"][[3]]

modobj9<- mcmc(cbind(deepSOC, catshallowSOC, catmidSOC))

modobjjj <- mcmc.list(modobj7, modobj8, modobj9)
modobjjj

#this has the numbers for Table 3
summary(modobjjj)



#THIS IS HOW WE CHECK THE MODEL#
# diagnostics to ensure good model behavior
m3inv_overall <- MCMCsummary(m3_inv, params = "(Intercept)", n.eff = T)
m3inv_overall

#we want this density plot to look relatively smooth; if not smooth, increase burnin and increase number of iterations
MCMCtrace(m3_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m3_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal; up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m3_inv[ , "(Intercept)"]) 
#Error in gelman.preplot(x, bin.width = bin.width, max.bins = max.bins,  : 
#Insufficient iterations to produce Gelman-Rubin plot

#we want the posteriors to converge on 1; if they dont, up burnin and interations
gelman.diag(m3_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m3_inv[ , "(Intercept)"])

#If everything looks good here, then the intercept value is the effect



####################################################################
#total soil; effect of cheat vs. sagecheat
totsoil <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #4
#totsoil2 <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#totsoil3 <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #0

#only 1 level of depth_cat factor; so don't use depth_cat here
m4a_inv <- MCMCglmm(g_cheat_v_sagecheat ~ 1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

m4b_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

m4c_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = totsoil$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = totsoil, pr = T, saveX = T, saveZ = T)

summary(m4a_inv)
summary(m4b_inv)
summary(m4c_inv)

#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m4_inv = mcmc.list(m4a_inv[[1]], m4b_inv[[1]], m4c_inv[[1]])
summary(m4_inv)

#THIS IS HOW WE CHECK THE MODEL#

# diagnostics to ensure good model behavior
m4inv_overall <- MCMCsummary(m4_inv, params = "(Intercept)", n.eff = T)
m4inv_overall

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

####################################################################
#AGB
agb <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #3
#agb2 <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#agb3 <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #1

#use cheatfire here instead of depth_cat since AGB; get rid of cheatfire
m5a_inv <- MCMCglmm(g_cheat_v_sagecheat ~ 1, random = ~ Article_ID, mev = agb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = agb, pr = T, saveX = T, saveZ = T)

m5b_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = agb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = agb, pr = T, saveX = T, saveZ = T)

m5c_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, random = ~ Article_ID, mev = agb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1000, verbose = T,
                    data = agb, pr = T, saveX = T, saveZ = T)

summary(m5a_inv)
summary(m5b_inv)
summary(m5c_inv)

#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m5_inv = mcmc.list(m5a_inv[[1]], m5b_inv[[1]], m5c_inv[[1]])
summary(m5_inv)

#THIS IS HOW WE CHECK THE MODEL#

# diagnostics to ensure good model behavior
m5inv_overall <- MCMCsummary(m5_inv, params = "(Intercept)", n.eff = T)
m5inv_overall


#we want this density plot to look relatively smooth
#if not smooth, increase burnin and increase number of iterations
MCMCtrace(m5_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m5_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal
#up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m5_inv[ , "(Intercept)"]) 


#we want the posteriors to converge on 1
#if they dont, up burnin and interations
gelman.diag(m5_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m5_inv[ , "(Intercept)"])


#If everything looks good here, then the intercept value is the effect


####################################################################
#BGB

bgb <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #4
#bgb2 <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#bgb3 <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #1


#use cheatfire here instead of depth_cat since AGB; won't run with Article ID in there
#removing cheatfire since not significant
m6a_inv <- MCMCglmm(g_cheat_v_sagecheat ~ 1, mev = bgb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = bgb, pr = T, saveX = T, saveZ = T)

m6b_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, mev = bgb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = bgb, pr = T, saveX = T, saveZ = T)

m6c_inv <- MCMCglmm(g_cheat_v_sagecheat ~  1, mev = bgb$var_d_cheat_v_sagecheat,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = bgb, pr = T, saveX = T, saveZ = T)

summary(m6a_inv)
summary(m6b_inv)
summary(m6c_inv)

#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m6_inv = mcmc.list(m6a_inv[[1]], m6b_inv[[1]], m6c_inv[[1]])
summary(m6_inv)

#THIS IS HOW WE CHECK THE MODEL#

# diagnostics to ensure good model behavior
m6inv_overall <- MCMCsummary(m6_inv, params = "(Intercept)", n.eff = T)
m6inv_overall

#we want this density plot to look relatively smooth
#if not smooth, increase burnin and increase number of iterations
MCMCtrace(m6_inv, params = "(Intercept)", pdf = F, ind = T)

#autocorr.plot(m1_inv) #all
#this will tell us whether our thinning variable is okay
#if many tall bars, increase thinning
autocorr.plot(m6_inv[, "(Intercept)"]) 

#assess convergence
#Trace plot. we want all the parameter estimates to be similar and horizontal
#up the burnin and iterations if they are headed in an up or down direction
gelman.plot(m6_inv[ , "(Intercept)"]) 


#we want the posteriors to converge on 1
#if they dont, up burnin and interations
gelman.diag(m6_inv[ , "(Intercept)"])

#dont worry about this one for now if gelman diagram looks good
geweke.diag(m6_inv[ , "(Intercept)"])


#If everything looks good here, then the intercept value is the effect

####################################################################
#litter

#litter <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #0
#litter2 <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#litter3 <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #0
