# code snippet for chelsea
libs <- c("tidyverse", "doBy","ggpubr","viridis", "raster","nlme","lmerTest",
          "rcompanion","lsmeans","vegan")
iini <-function(x){
  # stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}
lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

all <- read_csv("all.csv")
# Figures 2&3: site differences vs year differences -------------------------------

resp <- c("Bromus_TN_pct",    
          "Bromus_TC_pct" ,    "Bromus_CN" ,        "Other_TN_pct"  ,    "Other_TC_pct",     
          "Other_CN"       ,   "Poa_TN_pct" ,       "Poa_TC_pct"   ,     "Poa_CN"       ,    
          "Litter_TN_pct"   ,  "Litter_TC_pct",     "Litter_CN"   ,      #"SOIL_SurSo4_kg_ha",
          #"SOIL_Ca_kg_ha"    , "SOIL_Mg_kg_ha" ,  #  "SOIL_OM_pct",       "SOIL_TN_pct"      ,
          "SOIL_CN" ,    "soil_n_kg_ha"    ,  "soil_c_kg_ha", "total_mineral_n",
          "NO3_kg_ha","NH4_kg_ha")

vars=c(4:18,21,39,40)

# making a bunch of lists to use in the loop
ll=list()
mods = list()
coefs <- list()
for(i in 1:length(resp)){
  
  # getting rid of nas in the response variable
  d <- filter(all, is.na(dplyr::select(all, resp[i]))==F );dim(d)
  
  
  
  # trying interactin term vs no interaction term
  f<-formula(paste(resp[i],"~0+ Year + Site.type + Year*Site.type"))
  f1<-formula(paste(resp[i],"~0+ Year + Site.type"))
  
  # so for example for you.. something like 
  # f <- formula(paste(aboveground_c ~ site_type))
  
  # using a linear mixed effects model for each one
  # then for you, replace random with ~1|study 
  fit <- lme(f,
             random = ~1|Site_number,
             data=d, na.action = na.omit, contrasts = T,
             method = "ML")
  fit1 <- lme(f1,
              random = ~1|Site_number,
              data=d, na.action = na.omit, contrasts = T,
              method = "ML")
  
  # testing which model is better and using that
  if(BIC(fit)<BIC(fit1)){
    mods[[i]] <- update(fit, method="REML")
  }else{
    mods[[i]] <- update(fit1, method="REML")
  }
  
  # making a table of the model results for each model
  cc <- summary(mods[[i]])
  coefs[[i]] <- cc$tTable %>%
    as.data.frame() %>%
    tibble::rownames_to_column("variable")%>%
    dplyr::rename(p = "p-value", t="t-value", se = "Std.Error")%>%
    dplyr::mutate(pr = round(p, 3),
                  response = resp[i],
                  ps = ifelse(.$p<0.001,"***", 
                              ifelse(.$p<0.01& .$p>0.001, "**",
                                     ifelse(.$p<0.05 & .$p>0.01, "*", "")))) %>%
    mutate(val_std = paste0(round(Value, 2)," (",
                            round(se, 2),")",
                            ps))
  
  # calculating means and standard deviations with a post-hoc thing,
  # then saving to a list
  marginal <- lsmeans(mods[[i]], ~ Site.type:Year)
  
  post_hoc<- cld(marginal,
                 alpha   = 0.05,
                 Letters = letters,
                 adjust  = "tukey")
  
  ll[[i]]<- post_hoc %>%
    as.data.frame() %>%
    dplyr::rename(Median = lsmean,
                  Bca.lower = lower.CL,
                  Bca.upper = upper.CL)
  
  ll[[i]]$variable <- resp[i]
  
  
}

# linear mixed model results 
cc <- do.call("rbind", coefs) %>%
  dplyr::select(variable, val_std, response) %>%
  spread(key = variable, value = val_std, fill = "")

write_csv(cc, "/home/a/projects/Jones_Study/figures/site_diffs_mods.csv")

# post hoc stuff
dd <- do.call("rbind", ll) %>% 
  as_tibble() 

# plotting ---------------------------------------------------------------------
pd = position_dodge(.3)

lut_variables <- c("Bromus_TN_pct" = "Bromus N (%)", "Bromus_TC_pct" = "Bromus C (%)",
                   "Bromus_CN" = "Bromus C:N", "Other_TN_pct" = "Other N (%)",
                   "Other_TC_pct" = "Other C (%)", "Other_CN" = "Other C:N", 
                   "Poa_TN_pct" = "Poa N (%)", "Poa_TC_pct" = "Poa C (%)",
                   "Poa_CN" = "Poa C:N", "Litter_TN_pct" = "Litter N (%)",
                   "Litter_TC_pct" = "Litter C (%)", "Litter_CN" = "Litter C:N",
                   "SOIL_SurSo4_kg_ha" = "Soil SurSo4 (kg/ha)",
                   "SOIL_Ca_kg_ha" = "Soil Ca (kg/ha)", 
                   "SOIL_Mg_kg_ha" = "Soil Mg (kg/ha)",
                   "SOIL_CN" = "Soil C:N", "soil_n_kg_ha" = "Soil Total N (kg/ha)",
                   "soil_c_kg_ha" = "Soil OM (kg/ha)",
                   "total_mineral_n" = "Soil Mineral N (kg/ha)",
                   "NO3_kg_ha" = "Soil Nitrate (kg/ha)",
                   "NH4_kg_ha" = "Soil Ammonium (kg/ha)")

notsoil <- dplyr::filter(dd, substr(variable,1,1) != "S" & 
                           substr(variable,1,1) != "L" & 
                           substr(variable,1,1) != "s" & 
                           substr(variable,1,1) != "t" &
                           substr(variable,1,1) != "N" ) %>%
  mutate(variable = factor(lut_variables[variable],
                           levels = c("Bromus C (%)","Bromus N (%)","Bromus C:N",
                                      "Poa C (%)","Poa N (%)","Poa C:N",
                                      "Other C (%)","Other N (%)","Other C:N")))
soil <- dplyr::filter(dd, substr(variable,1,1) == "S" | 
                        substr(variable,1,1) == "L"| 
                        substr(variable,1,1) == "s" |
                        substr(variable,1,1) == "t"|
                        substr(variable,1,1) == "N") %>%
  mutate(variable = factor(lut_variables[variable],
                           levels= c( "Soil OM (kg/ha)", "Soil Total N (kg/ha)",
                                      "Soil C:N",
                                      "Soil Mineral N (kg/ha)", "Soil Nitrate (kg/ha)", 
                                      "Soil Ammonium (kg/ha)",
                                      "Litter C (%)",  "Litter N (%)","Litter C:N")))

lut_col <- c("springgreen4", "deepskyblue2", "darkgoldenrod2", "chocolate4")

ggplot(notsoil, aes(x = Year, y = Median, color = Site.type, shape = Site.type)) +
  geom_errorbar(aes(ymin=Bca.lower,
                    ymax=Bca.upper),
                width=.6, size=0.7, 
                position=pd, show.legend = F) +
  geom_point(size=3, position=pd, alpha = 1) +
  scale_color_manual(name="Invasion Stage", values = lut_col)+
  scale_shape_manual(name = "Invasion Stage", values = 15:18)+
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom") +
  labs(y=NULL, x=NULL) +
  facet_wrap(~variable, scales = "free",nrow = 3, ncol=3) +
  ggsave("figures/figure_3_tissue.pdf") 

ggplot(soil, aes(x = Year, y = Median, color = Site.type, shape=Site.type)) +
  geom_errorbar(aes(ymin=Bca.lower,
                    ymax=Bca.upper),
                width=0.6, size=0.7, 
                position=pd, show.legend = F) +
  geom_point(size=3, position=pd, alpha=1) +
  scale_color_manual(name="Invasion Stage", values = lut_col)+
  scale_shape_manual(name = "Invasion Stage", values = c(15,16,17,18))+
  theme_bw() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = "bottom") +
  labs(y=NULL, x=NULL) +
  facet_wrap(~variable, scales = "free", nrow=3, ncol=3)+
  ggsave("figures/figure_3_soil_litter.pdf")