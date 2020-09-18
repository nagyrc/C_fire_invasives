#meta-analysis
#Dr. Jenica Allen and Dr. R. Chelsea Nagy
#created February 20, 2019

#load multiple libraries 
x <- c("MCMCglmm", "MCMCvis", "metafor")
lapply(x, library, character.only = TRUE, verbose = FALSE)

setwd("data/")

citation("MCMCglmm")
citation("metafor")

dq2 <- read.csv("/Users/rana7082/Dropbox/C_fire_invasives_R/results/rawspmeanscat.csv", header = T)
head(dq2)
tail(dq2)
summary(dq2) 



################################################
## Calculating Effect Size and Variance
################################################

#for the paired sites

# Computing observation-level SD
#converting se and ci into sd

#will need to calculate sd for all three veg categories
head(dq2)
dq2$sdsage <- sqrt(dq2$varsage)
dq2$sdsagecheat <- sqrt(dq2$varsagecheat)
dq2$sdcheat <- sqrt(dq2$varcheat)


# Computing the pooled standard deviations for the Hedges' index
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


#depth category is not included; not equal representation across depths
m1a_inv <- MCMCglmm(g_cheat_v_sage ~  1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

m1b_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

m1c_inv <- MCMCglmm(g_cheat_v_sage ~ 1, random = ~ Article_ID, mev = orgsoil$var_d_cheat_v_sage,
                    prior = prior, nitt = 100000, burnin = 10000, thin = 1, verbose = T,
                    data = orgsoil, pr = T, saveX = T, saveZ = T)

summary(m1a_inv)
summary(m1b_inv)
summary(m1c_inv)

#may need to run these multiple times
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

#ok to leave Article_ID in here as random effect
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

# combine 3 chains into 1 mcmc object; cheat_v_sage
m1_inv <- mcmc.list(m1a_inv[[1]], m1b_inv[[1]], m1c_inv[[1]])
m1_inv

summary(m1_inv)



#THIS IS HOW WE CHECK THE MODEL#

summary(m1_inv)

# diagnostics to ensure good model behavior
m1inv_overall <- MCMCsummary(m1_inv, params = c("(Intercept)", "depth_catshallow", "depth_catmid"), n.eff = T)
m1inv_overall

#we want this density plot to look relatively smooth; if not smooth, increase burnin and increase number of iterations
MCMCtrace(m1_inv, params = c("(Intercept)", "depth_catshallow", "depth_catmid"), pdf = F, ind = T)

#this will tell us whether our thinning variable is okay; if many tall bars, increase thinning
autocorr.plot(m1_inv[, c("(Intercept)", "depth_catshallow", "depth_catmid")]) 

#assess convergence
gelman.plot(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")]) 

gelman.diag(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")])

geweke.diag(m1_inv[ , c("(Intercept)", "depth_catshallow", "depth_catmid")])






###
#effect of sagecheat vs. sage
# combine 3 chains into 1 mcmc object
m2_inv = mcmc.list(m2a_inv[[1]], m2b_inv[[1]], m2c_inv[[1]])
summary(m2_inv)




#CHECK THE MODEL#
# diagnostics to ensure good model behavior
m2inv_overall <- MCMCsummary(m2_inv, params = "(Intercept)", n.eff = T)
m2inv_overall

MCMCtrace(m2_inv, params = "(Intercept)", pdf = F, ind = T)

autocorr.plot(m2_inv[, "(Intercept)"]) 

#assess convergence
gelman.plot(m2_inv[ , "(Intercept)"]) 

gelman.diag(m2_inv[ , "(Intercept)"])

geweke.diag(m2_inv[ , "(Intercept)"])




###
#effect of cheat vs. sagecheat
# combine 3 chains into 1 mcmc object
m3_inv = mcmc.list(m3a_inv[[1]], m3b_inv[[1]], m3c_inv[[1]])
summary(m3_inv)

#for depth categories
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



#CHECK THE MODEL#
# diagnostics to ensure good model behavior
m3inv_overall <- MCMCsummary(m3_inv, params = "(Intercept)", n.eff = T)
m3inv_overall

MCMCtrace(m3_inv, params = "(Intercept)", pdf = F, ind = T)

autocorr.plot(m3_inv[, "(Intercept)"]) 

#assess convergence
gelman.plot(m3_inv[ , "(Intercept)"]) 

gelman.diag(m3_inv[ , "(Intercept)"])

geweke.diag(m3_inv[ , "(Intercept)"])



####################################################################
#total soil; effect of cheat vs. sagecheat
totsoil <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #4
#totsoil2 <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#totsoil3 <- subset(dq2, pool == "totsoilC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #0

#only 1 level of depth category factor; so don't use depth category here
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

#this has the numbers for Table S5
summary(m4_inv)

#CHECK THE MODEL#

# diagnostics to ensure good model behavior
m4inv_overall <- MCMCsummary(m4_inv, params = "(Intercept)", n.eff = T)
m4inv_overall

MCMCtrace(m4_inv, params = "(Intercept)", pdf = F, ind = T)

autocorr.plot(m4_inv[, "(Intercept)"]) 

#assess convergence
gelman.plot(m4_inv[ , "(Intercept)"]) 

gelman.diag(m4_inv[ , "(Intercept)"])

geweke.diag(m4_inv[ , "(Intercept)"])



####################################################################
#AGB
agb <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #3
#agb2 <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#agb3 <- subset(dq2, pool == "AGBC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #1

#did not use fire info or depth category for AGB 
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

#this has the numbers for Table S5
summary(m5_inv)

#CHECK THE MODEL#

# diagnostics to ensure good model behavior
m5inv_overall <- MCMCsummary(m5_inv, params = "(Intercept)", n.eff = T)
m5inv_overall

MCMCtrace(m5_inv, params = "(Intercept)", pdf = F, ind = T)

autocorr.plot(m5_inv[, "(Intercept)"]) 

#assess convergence
gelman.plot(m5_inv[ , "(Intercept)"]) 

gelman.diag(m5_inv[ , "(Intercept)"])

geweke.diag(m5_inv[ , "(Intercept)"])



####################################################################
#BGB

#bgb <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #4
#all from same study, so did not run

#bgb2 <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#bgb3 <- subset(dq2, pool == "BGBC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #1



####################################################################
#litter

#litter <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_cheat_v_sagecheat)) #0
#litter2 <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_cheat_v_sage)) #0
#litter3 <- subset(dq2, pool == "litterC_g_m2" & !is.na(var_d_sagecheat_v_sage)) #0
