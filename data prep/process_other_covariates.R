library(data.table)
library(readxl)
library(tidyverse)
library(dplyr)
library(plyr)

rename <- dplyr::rename
select <- dplyr::select

# COVID hierarchy
hier<-hierarchy<-fread(paste(root_dir,"hierarchy_metadata.csv"))
loc_lv3<-hier[level==3,]
lv34<-hier[level %in% c(3,4),]

ref_dir <- "FILEPATH"
outdir<-'FILEPATH'

# -- Trust in government / interpersonal trust
WVS <- data.table(readRDS(paste0(ref_dir,"/data/00_raw/EVS_WVS_Cross-National_Wave_7_joint_core_R_v1_2.rds")))

## select columns of interest
WVS <- WVS[,.(location_name = as.character(as_factor(B_COUNTRY, levels = "labels")),
              govt_trust = Q71, 
              interpersonal_trust = Q57)]

## calculate actual trust and remove NAs
WVS[,`:=`(
  govt_trust_simple = ifelse(govt_trust %in% 1:2, 1, 0), 
  interpersonal_trust_simple = ifelse(interpersonal_trust == 1, 1, 0)
)]
WVS <- WVS[!is.na(govt_trust) | !is.na(interpersonal_trust_simple)]

## calculate
WVS <- WVS[,.(govt_trust = mean(govt_trust_simple, na.rm = T)*100, 
              interpersonal_trust = mean(interpersonal_trust_simple, na.rm = T)*100),
           by = location_name]

## =========================================
## Recode location names
## =========================================

recode_WVS_locs <- list(
  "Macau SAR" = "REMOVE",
  "Taiwan ROC" = "Taiwan (Province of China)", 
  "Hong Kong SAR" = "REMOVE",
  "Russia" = "Russian Federation", 
  "Bosnia Herzegovina" = "Bosnia and Herzegovina", 
  'Bolivia'="Bolivia (Plurinational State of)",
  'Czech Rep.'="Czechia",
  'Iran'="Iran (Islamic Republic of)",
  'South Korea'="Republic of Korea",
  'United States'="United States of America",
  'Vietnam'="Viet Nam"
)

## recode
WVS[,location_name := revalue(location_name, unlist(recode_WVS_locs))]
WVS <- WVS[location_name != "REMOVE"]

## merge
WVS <- merge(WVS, hier[level==3,.(location_id, location_name)], by = "location_name")
setcolorder(WVS, "location_id")
WVS[,location_name := NULL]

# -- Trust in science

WGM <- data.table(read_xlsx(paste0(ref_dir,"/data/00_raw/wgm2018-dataset-crosstabs-all-countries.xlsx"), sheet = 1))
WGM <- WGM[-c(1:2),c(1:4)]
setnames(WGM, c("country", "question", "answer", "percent"))
WGM <- WGM[question %like% "trust science"]
WGM_clean <- WGM[answer == "A lot", .(location_name = country, trust_science = round(as.numeric(percent), 5)*100)]

## =========================================
## Recode location names
## =========================================
recode_WGM_locs <- list(
  "Congo, Rep." = "Democratic Republic of the Congo",
  "Ivory Coast" = "Côte d'Ivoire",
  "Russia" = "Russian Federation",
  "Taiwan" = "Taiwan (Province of China)",
  'Bolivia'="Bolivia (Plurinational State of)",
  'Czech Republic'="Czechia",
  'Iran'="Iran (Islamic Republic of)",
  'South Korea'="Republic of Korea",
  'United States'="United States of America",
  'Vietnam'="Viet Nam",
  'Moldova'="Republic of Moldova",
  'Laos'="Lao People's Democratic Republic",
  'Tanzania'="United Republic of Tanzania",
  'The Gambia'='Gambia',  
  'Venezuela'="Venezuela (Bolivarian Republic of)",
  'Kosovo'="REMOVE",
  'Northern Cyprus'="REMOVE",
  'Macedonia'='North Macedonia'
  
)

## print removed locations
removed_WGM_locs <- names(recode_WGM_locs[recode_WGM_locs == "REMOVE"])
removed_WGM_locs

## recode
WGM_clean[,location_name := revalue(location_name, unlist(recode_WGM_locs))]
WGM_clean <- WGM_clean[location_name != "REMOVE"]

WGM <- merge(WGM_clean, hier[level==3,.(location_id, location_name)], by = "location_name")
setcolorder(WGM, "location_id")
WGM[,location_name := NULL]

# -- JEE score
JEE <- data.table(read_xlsx(paste0(ref_dir,"/data/00_raw/JEE.xlsx"), sheet = 2))
setnames(JEE, "country", "location_name")

## =========================================
## Recode location names
## =========================================
recode_JEE_locs <- list(
  "Kyrgyz Republic" = "Kyrgyzstan", 
  "Russia" = "Russian Federation", 
  "St Lucia" = "Saint Lucia", 
  "St Vincent and The Grenadines" = "Saint Vincent and the Grenadines", 
  "Micronesia" = "Micronesia (Federated States of)", 
  "eSwatini" = "Eswatini", 
  "Congo (Democratic Republic)" = "Democratic Republic of the Congo", 
  "St Kitts and Nevis" = "Saint Kitts and Nevis", 
  "Congo (Brazzaville)" = "Congo", 
  "São Tomé and Príncipe" = "Sao Tome and Principe",
  'Bolivia'="Bolivia (Plurinational State of)",
  'Brunei'="Brunei Darussalam",
  'Czech Republic'="Czechia",
  'Iran'="Iran (Islamic Republic of)",
  'Lao PDR'="Lao People's Democratic Republic",
  'Moldova'="Republic of Moldova",
  'North Korea'="Democratic People's Republic of Korea",
  'South Korea'="Republic of Korea",
  'Syria'= "Syrian Arab Republic",
  'Tanzania'="United Republic of Tanzania",
  'United States'="United States of America",
  'Venezuela'="Venezuela (Bolivarian Republic of)",
  'Vietnam'="Viet Nam"
)

## recode
JEE[,location_name := revalue(location_name, unlist(recode_JEE_locs))]

## merge
JEE <- merge(JEE, hier[level==3,.(location_id, location_name)], by = "location_name")
setcolorder(JEE, "location_id")
JEE[,location_name := NULL]

## remove incomplete
JEE <- JEE[status == "Completed"]

## change names
JEE <- JEE[,
           .(location_id, 
             jee_readyscore_overall = ReadyScore,
             jee_readyscore_prevent = prevent, 
             jee_readyscore_detect = detect, 
             jee_readyscore_respond = respond, 
             jee_readyscore_other = other)]


# -- Global health security index, original 2019 version
## function to read all sheets
read_excel_allsheets <- function(filename){
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  x <- lapply(x, data.table)
  names(x) <- sheets
  x
}

## read
GHSI <- read_excel_allsheets(paste0(ref_dir,"/data/00_raw/GHSI/Extraction.xlsx"))
## name score as name of sheet
for (i in names(GHSI)){
  setnames(GHSI[[i]], "score", paste0("ghsi_", i))
}

## merge all
GHSI <- Reduce(function(x, y) merge(x, y, by = c("location_name")), GHSI)

## remove average
GHSI <- GHSI[location_name != "AVERAGE"]

## =========================================
## Recode location names
## =========================================
recode_GHSI_locs <- list(
  "Kyrgyz Republic" = "Kyrgyzstan", 
  "Russia" = "Russian Federation", 
  "St Lucia" = "Saint Lucia", 
  "St Vincent and The Grenadines" = "Saint Vincent and the Grenadines", 
  "Micronesia" = "Micronesia (Federated States of)", 
  "eSwatini (Swaziland)" = "Eswatini", 
  "Congo (Democratic Republic)" = "Democratic Republic of the Congo", 
  "St Kitts and Nevis" = "Saint Kitts and Nevis", 
  "Congo (Brazzaville)" = "Congo", 
  "São Tomé and Príncipe" = "Sao Tome and Principe",
  'Bolivia'="Bolivia (Plurinational State of)",
  'Brunei'="Brunei Darussalam",
  'Czech Republic'="Czechia",
  'Iran'="Iran (Islamic Republic of)",
  'Laos'="Lao People's Democratic Republic",
  'Moldova'="Republic of Moldova",
  'North Korea'="Democratic People's Republic of Korea",
  'South Korea'="Republic of Korea",
  'Syria'= "Syrian Arab Republic",
  'Tanzania'="United Republic of Tanzania",
  'United States'="United States of America",
  'Venezuela'="Venezuela (Bolivarian Republic of)",
  'Vietnam'="Viet Nam"
)

## recode
GHSI[,location_name := revalue(location_name, unlist(recode_GHSI_locs))]
GHSI <- GHSI[location_name != "Liechtenstein"]

## merge
GHSI <- merge(GHSI, hier[level==3,.(location_id, location_name)], by = "location_name")
setcolorder(GHSI, "location_id")
GHSI[,location_name := NULL]

# -- UHC
UHC <- fread(paste0(ref_dir,"/00_raw/IHME_GBD_2019_UHC_1990_2019_VALUES.csv"))
UHC <- UHC[year_id == 2019 & indicator_name %like% "UHC", .(location_id, uhc_19 = val)]
UHC <- UHC[location_id %in% hier[level==3, location_id],]

# HAQI
x = 1099L
df_pop_in <- read.csv(paste0(ref_dir,"/all_populations.csv"))
df_cov <- get_covariate_estimates(
  covariate_id = x, 
  gbd_round_id = 6, 
  decomp_step = "iterative",
  # location_id = 1,
  location_id = unique(df_pop_in$location_id),
  year_id = 2019
)
df_cov<-merge(df_cov, hier[level==3, 'location_id'], by='location_id')
setDT(df_cov)
HAQI<-df_cov[,HAQI_mean := mean(mean_value, na.rm=T), by=list(location_id)]
HAQI<-HAQI[,.(location_id, HAQI_mean)]
HAQI<-HAQI[!duplicated(HAQI_mean)]

poli_covs<-fread(paste0(ref_dir,"df_political_and_social_correlates.csv"))
disease_covs<-poli_covs[,c('location_id', 'gini','electoral_pop','vdem_libdem','gov_effect','state_fragility',
                           'federal_ind','bureaucracy_corrupt')]

covariates<-merge(disease_covs, HAQI, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, WVS, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, WGM, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, JEE, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, GHSI, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, UHC, all.x=T, all.y=T, by='location_id')


#process corruption variable
distrust<-fread(paste0(ref_dir,'/data/00_raw/CPI2020_GlobalTables_2020.csv'))
distrust<-merge(distrust, hier[level==3, .(location_id, ihme_loc_id)], by.x="ISO3", by.y="ihme_loc_id")
setnames(distrust, 'CPI score 2020', 'trust_govt_cpi')
distrust2<-distrust[,.(location_id, trust_govt_cpi)]

covariates<-merge(covariates,distrust2, all.x=T, all.y=T, by='location_id')


#gallup govt trust

gallup<-fread(paste0(ref_dir,'/confidence_in_govt.csv'))
gallup<-gallup[,.(location_id, pct)]
setnames(gallup, 'pct','trust_govt_gallup')
covariates<-merge(covariates,gallup, all.x=T, all.y=T, by='location_id')

#VDEM data
libdem<-fread(paste0(ref_dir,'data/00_raw/VParty data.csv'))
libdem<-merge(libdem, hier[level==3, .(location_id, ihme_loc_id)], by.x="iso3", by.y="ihme_loc_id")
libdem<-libdem[, .(location_id, lib_cons, v2pariglef_gov)]
setnames(libdem, c('lib_cons', 'v2pariglef_gov'),c('lib_cons_avg', 'v2pariglef_gov_avg'))
covariates<-merge(covariates,libdem, all.x=T, all.y=T, by='location_id')

vparty_liberal_populism<-fread(paste0(ref_dir,'vparty_liberal_populism.csv'))
vparty_lib_pop<-merge(vparty_liberal_populism, hier[level==3, .(location_id, ihme_loc_id)], by.x='iso3', by.y='ihme_loc_id')
vparty_lib_pop<-vparty_lib_pop[,.(location_id, v2xpa_illiberal_gov, v2xpa_popul_gov)]
vparty_lib_pop[, v2xpa_illiberal_gov:=as.numeric(v2xpa_illiberal_gov)]
vparty_lib_pop[, v2xpa_popul_gov:=as.numeric(v2xpa_popul_gov)]
covariates<-merge(covariates,vparty_lib_pop, all.x=T, all.y=T, by='location_id')


democ_panel<-fread(paste0(ref_dir,'/data/00_raw/democ_panel.csv'))
democ_panel<-merge(democ_panel[year==2020,], hier[level==3, .(location_id, ihme_loc_id)], by.x="iso3", by.y="ihme_loc_id")
democ_panel<-democ_panel[, .(location_id, v2x_polyarchy, v2x_mpi)]
democ_panel$v2x_polyarchy<-as.numeric(democ_panel$v2x_polyarchy)
democ_panel$v2x_mpi<-as.numeric(democ_panel$v2x_mpi)
covariates<-merge(covariates,democ_panel, all.x=T, all.y=T, by='location_id')

df_pop <- df_pop_in %>%
  filter(sex_id == 3) %>%
  filter(age_group_id ==22)

#add on two additional UHC metrics
uhc_both<-fread(paste0(ref_dir,'gbd_overview_uhc_agg_148.csv'))
ncd_coverage<-uhc_both[health_group=='ncd', .(location_id, uhc_coverage)]
setnames(ncd_coverage, 'uhc_coverage', 'ncd_coverage')

cmnn_coverage<-uhc_both[health_group=='cmnn', .(location_id, uhc_coverage)]
setnames(cmnn_coverage, 'uhc_coverage', 'cmnn_coverage')

new_uhc<-merge(cmnn_coverage,ncd_coverage, by='location_id')

covariates<-merge(covariates, new_uhc, by='location_id', all.x=T)

covariates<-covariates[!duplicated(covariates)]
covariates<-covariates[!is.na(covariates$location_id)]
write.csv(covariates, paste0(outdir,'/unscaled_covariates.csv'))

# Center and scale data
scale_vars<-c(scale_vars, 'ncd_coverage', 'cmnn_coverage')
location_id<-covariates$location_id
covariates2<-covariates[,lapply(.SD, function(x) scale(x)), .SDcols = scale_vars]
names(covariates2)<-scale_vars
covariates2$location_id<-location_id
write.csv(covariates2, paste0(outdir,'/prepped_covariates.csv'))


#add on other comorbidities
source(paste0(ref_dir,"/get_outputs.R"))
cancer<-get_outputs("cause", 
                           gbd_round_id=6,
                           decomp_step='step5',
                           version='best',
                           location_id=lv34,
                           year_id=2019,
                           age_group_id=27,
                           sex_id=3,
                           measure_id=5,
                           metric_id=3,
                           cause_id=410)

copd<- get_outputs("cause", 
                         gbd_round_id=6,
                         decomp_step='step5',
                         version='best',
                         location_id=lv34,
                         year_id=2019,
                         age_group_id=27,
                         sex_id=3,
                         measure_id=5,
                         metric_id=3,
                         cause_id=509)

copd<-copd[,c('location_id','val')]
names(copd)<-c('location_id','copd')

cancer<-cancer[,c('location_id','val')]
names(cancer)<-c('location_id','cancer')

# Center and scale data
new_covs<-merge(copd, cancer, by='location_id')
scale_vars<-c('copd','cancer')
location_id<-new_covs$location_id
new_covs2<-new_covs[,lapply(.SD, function(x) scale(x)), .SDcols = scale_vars]
names(new_covs2)<-scale_vars
new_covs2$location_id<-location_id
write.csv(new_covs2, paste0(ref_dir,'/prepped_covariates_cancer_copd.csv'))




