library(data.table)
library(knitr)
library(kableExtra)
library(viridis)
library(dplyr)
in_dir<-'FILEPATH'

inf<-fread(paste0(in_dir,'/decomp_inf_10_31.csv'))
ifr<-fread(paste0(in_dir,'/decomp_ifr_10_31.csv'))
raw_data<-fread(paste0(in_dir,'/stage_0_1_residuals_wave0.csv'))
#-------------------------------------
hier <- fread(paste0(in_dir,"/location_meta.csv"))
inf<-merge(inf[,-1], hier[,.(super_region_name,lancet_label,location_name, location_id)], by='location_id')
ifr<-merge(ifr[,-1], hier[,.(super_region_name,lancet_label,location_name, location_id)], by='location_id')

ifr<-ifr %>% mutate(across(where(is.numeric), function(x) x*1000))
ifr<-ifr %>% mutate(across(where(is.numeric), round, 2))
ifr<-ifr[,`:=`(decomp_BMI=ifelse(decomp_BMI>=0,paste0("+", format(decomp_BMI,nsmall=2)), paste0(format(decomp_BMI, nsmall=2))),
               decomp_ifr_age=ifelse(decomp_ifr_age>=0,paste0("+", format(decomp_ifr_age,nsmall=2)), paste0(format(decomp_ifr_age, nsmall=2))),
               decomp_proportion_1000ppl_km=ifelse(decomp_proportion_1000ppl_km>=0,paste0("+", format(decomp_proportion_1000ppl_km,nsmall=2)), paste0(format(decomp_proportion_1000ppl_km, nsmall=2))),
               decomp_summed_bat_species=ifelse(decomp_summed_bat_species>=0,paste0("+", format(decomp_summed_bat_species,nsmall=2)), paste0(format(decomp_summed_bat_species, nsmall=2))),
               decomp_smoke_mean=ifelse(decomp_smoke_mean>=0,paste0("+", format(decomp_smoke_mean,nsmall=2)), paste0(format(decomp_smoke_mean, nsmall=2))),
               decomp_GDP_mean=ifelse(decomp_GDP_mean>=0,paste0("+", format(decomp_GDP_mean,nsmall=2)), paste0(format(decomp_GDP_mean, nsmall=2))),
               decomp_air_pollution=ifelse(decomp_air_pollution>=0,paste0("+", format(decomp_air_pollution,nsmall=2)), paste0(format(decomp_air_pollution, nsmall=2))),
               decomp_copd=ifelse(decomp_copd>=0,paste0("+", format(decomp_copd,nsmall=2)), paste0(format(decomp_copd, nsmall=2))),
               decomp_cancer=ifelse(decomp_cancer>=0,paste0("+", format(decomp_cancer,nsmall=2)), paste0(format(decomp_cancer, nsmall=2))))]
names(ifr)<-c('location_id','BMI', 'Population density', "mean",'Age pattern', 'Previous betacoronavirus exposure', 'Smoking prevalence', "GDP per capita", "COPD prevalence", "Cancer prevalence", "Air pollution", 'Raw IFR per 1000 infections','Adjusted IFR per 1000 infections','Super region', 'Country', 'location_name')
ifr<-ifr[order(ifr$`Super region`)]
ifr<-ifr[,c("Country","Raw IFR per 1000 infections",'Age pattern','BMI','Population density', 'Previous betacoronavirus exposure', 'Smoking prevalence', "GDP per capita", "Air pollution", "COPD prevalence", "Cancer prevalence", "Adjusted IFR per 1000 infections", "Super region")]

pal.fnc = colorRamp(c("forestgreen", "yellow", "firebrick"))
ifr[ , raw.ifr.col:=`Raw IFR per 1000 infections`]
ifr[raw.ifr.col>8.9 , raw.ifr.col:=8.9]
ifr[ , adj.ifr.col:=`Adjusted IFR per 1000 infections`]
max.val.ifr = 8.9
ifr[adj.ifr.col>6.9 , adj.ifr.col:=6.9]
max.val.ifr2 = 6.9

knitr::kable(ifr[,c("Country","Raw IFR per 1000 infections",'Age pattern',"Air pollution", 'BMI',"Cancer prevalence","COPD prevalence","GDP per capita",  'Population density','Previous betacoronavirus exposure', 'Smoking prevalence',  "Adjusted IFR per 1000 infections")],
              align = c('l',"c","c","c","c","c","c","c",'c','c', 'c', 'c'), caption='Figure 1b. Infection-fatality ratio') %>% 
              pack_rows(index = table(ifr$`Super region`)) %>%
              column_spec(2, color = "black",background = rgb(pal.fnc(ifr$raw.ifr.col/max.val.ifr) %>% replace(., is.na(.), 200), maxColorValue=255))%>%
               column_spec(12, color = "black",background = rgb(pal.fnc(ifr$adj.ifr.col/max.val.ifr2) %>% replace(., is.na(.), 200), maxColorValue=255))%>%
              kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
              add_header_above(c(" " = 2, "Marginal effect of standardizing to the global mean" = 9, " "=1))
    
inf<-inf %>% mutate(across(where(is.numeric), function(x) x*1000))
inf<-inf %>% mutate(across(where(is.numeric), round, 0))
inf<-inf[,`:=`(decomp_ifr_age=ifelse(decomp_ifr_age>=0,paste0("+", format(decomp_ifr_age,nsmall=0)), paste0(format(decomp_ifr_age, nsmall=0))),
               decomp_proportion_1000ppl_km=ifelse(decomp_proportion_1000ppl_km>=0,paste0("+", format(decomp_proportion_1000ppl_km,nsmall=0)), paste0(format(decomp_proportion_1000ppl_km, nsmall=0))),
               decomp_summed_bat_species=ifelse(decomp_summed_bat_species>=0,paste0("+", format(decomp_summed_bat_species,nsmall=0)), paste0(format(decomp_summed_bat_species, nsmall=0))),
               decomp_alt_under100=ifelse(decomp_alt_under100>=0,paste0("+", format(decomp_alt_under100,nsmall=0)), paste0(format(decomp_alt_under100, nsmall=0))),
               decomp_GDP_mean=ifelse(decomp_GDP_mean>=0,paste0("+", format(decomp_GDP_mean,nsmall=0)), paste0(format(decomp_GDP_mean, nsmall=0))))]

names(inf)<-c('location_id','Population density', "mean",'Seasonality', 'Previous betacoronavirus exposure', "GDP per capita", "Altitude", 'Raw infections per 1000 population', 'Adjusted infections per 1000 population','Super region', 'Country', 'location_name')
inf<-inf[order(inf$`Super region`)]

inf<-inf[,c("Country",'Raw infections per 1000 population','Seasonality', "Altitude","GDP per capita",'Population density','Previous betacoronavirus exposure', 'Adjusted infections per 1000 population', "Super region")]
pal.fnc = colorRamp(c("forestgreen", "yellow", "firebrick"))
inf[ , raw.inf.col:=`Raw infections per 1000 population`]
inf[raw.inf.col>817, raw.inf.col:=817]
inf[ , adj.inf.col:=`Adjusted infections per 1000 population`]
max.val.inf = 817
inf[adj.inf.col>797 , adj.inf.col:=797]
max.val.inf2 = 797

knitr::kable(inf[,c("Country",'Raw infections per 1000 population', 'Seasonality',"Altitude","GDP per capita", 'Population density', 'Previous betacoronavirus exposure', 'Adjusted infections per 1000 population')],
             align = c('l','c','c',"c","c","c",'c', 'c'), caption='Figure 1a. Cumulative infections per 1000 population') %>% 
  pack_rows(index = table(inf$`Super region`)) %>%
  column_spec(2, color = "black",
              background = rgb(pal.fnc(inf$raw.inf.col/max.val.inf) %>% replace(., is.na(.), 200), maxColorValue=255))%>%
  column_spec(8, color = "black",
              background = rgb(pal.fnc(inf$adj.inf.col/max.val.inf2) %>% replace(., is.na(.), 200), maxColorValue=255))%>%
   kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  add_header_above(c(" " = 2, "Marginal effect of standardizing to the global mean" = 5, " "=1))
