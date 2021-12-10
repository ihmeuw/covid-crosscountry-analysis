# 02_launch_data_prep.R
# Called from 01_data_prep_draws_launcher.R

library(data.table)
library(readxl)
library(tidyverse)
library(dplyr)
require(lubridate)
library(argparse)
library(MASS)

rename <- dplyr::rename
select <- dplyr::select

parser <- argparse::ArgumentParser()
parser$add_argument("--draw")
parser$add_argument("--seir-version")
parser$add_argument("--experiment")
parser$add_argument("--RR")
parser$add_argument("--end_date")

args <- parser$parse_args()
for (key in names(args)) {
  assign(key, args[[key]])
}

for (arg in names(args)){
  print(paste0(arg, ": ", get(arg)))
}

#function to get empirical y-hats from model runs
get_yhat<-function(model_fit, ndraws, xmatrix){
  betas = coef(model_fit)
  vcov = vcov(model_fit)
  betas_simulated = MASS::mvrnorm(ndraws, betas, vcov)
  beta_mat<-t(as.matrix(betas_simulated))
  yhat = beta_mat %*% xmatrix
  return(yhat)
}

#set seed = draw number
num<-as.numeric(gsub("draw_", "", draw))
set.seed(num)

# COVID hierarchy
root_dir<-'FILEPATH'
df_locmeta <- read.csv(paste0(root_dir,"/hierarchy.csv"))
ref_dir_draw <- paste0(root_dir, seir_version, "/reference/output_draws/")
ref_dir <- paste0(root_dir, seir_version, "/reference/output_summaries/")

start_date<-as.Date('2020-01-01')
period0_date<-as.Date(end_date)
period4_date_start<-as.Date('2020-01-01')
period4_date_end<-as.Date('2020-10-15')


outdir<-'FILEPATH'

hier<-hierarchy<-fread(paste(root_dir,"hierarchy_metadata.csv"))
loc_lv3<-hier[level %in% c(3,4),]

# Two time periods of interest:
# 1.	Period 0 = all: full time period for which we have data; spanning all of 2020 to the present 
# 2.	Period 4 = Pre-variants and vaccines: Jan 1 2020-Oct 15 2020

# 
#output folder for draws
out.draws<-paste0(outdir, seir_version)
dir.create(paste0(out.draws,"/draws/"))
system(paste0('chmod -R 775 /',out.draws,"/draws/"))

#read in draw-level infections and deaths outside of loop
daily_infections<-fread(file.path(ref_dir_draw, "daily_infections.csv"))
daily_deaths<-fread(file.path(ref_dir_draw, "daily_deaths.csv"))

print(draw)
dat_filenames <- c("daily_infections", "daily_deaths","daily_cases","mobility", "testing", "pneumonia")
dat_list <- sapply(
  X = dat_filenames, 
  FUN = function(x) {
    if(x=="daily_infections"){
      cat(x, "\n")
      q<-daily_infections[,.(location_id, date, get(draw))]
      names(q)<-c('location_id','date','mean')
      q
    } else if (x=="daily_deaths"){
      cat(x, "\n")
      q<-daily_deaths[,.(location_id, date, get(draw))]
      names(q)<-c('location_id','date','mean')
      q
    } else {
      cat(x, "\n")
      read.csv(file.path(ref_dir, paste0(x, ".csv")), as.is = TRUE)
    }
  },
  simplify = FALSE
)

lapply(dat_list, dim)
lapply(dat_list, names)


all_dates <- as.vector(sort(unique(do.call("c", lapply(dat_list, function(x) x$date)))))
all_locs <- as.vector(sort(unique(do.call("c", lapply(dat_list, function(x) x$location_id)))))
df_all_tmp1 <- expand.grid(location_id = all_locs, date = all_dates, stringsAsFactors = FALSE) %>%
  arrange(location_id, date)
df_all_tmp1$date<-as.Date(df_all_tmp1$date, origin="1970-01-01")

print('nm in dat_filenames')
for (nm in dat_filenames) {
  if (FALSE) {
    nm <- "daily_cases"
  }
  df_tmp <- dat_list[[nm]]
  varnames <- names(df_tmp)[!names(df_tmp) %in% c("location_id", "date")]
  names(df_tmp)[names(df_tmp) %in% varnames] <- paste0(nm, "_", names(df_tmp)[names(df_tmp) %in% varnames])
  df_tmp$date<-as.Date(df_tmp$date)
  df_all_tmp1 <- left_join(df_all_tmp1, df_tmp, by = c("location_id", "date"))
}

# calculate cumulative infections and IFR
df_all_tmp2 <- df_all_tmp1 %>%
  mutate(
    monthday = paste0(month(date), "_", day(date)),
    daily_infections2 = ifelse(is.na(daily_infections_mean), 0, daily_infections_mean)) %>%
  group_by(location_id) %>%
  mutate(
    cumulative_infections = cumsum(daily_infections2) ) %>%
  as.data.frame(.)
print('pneumonia update') 
df_all_tmp2_2021 <- df_all_tmp2 %>% 
  filter(year(date) == 2021) %>% 
  select(location_id, monthday, p2 = pneumonia_mean)

df_all <- df_all_tmp2 %>% 
  left_join(df_all_tmp2_2021, by = c("location_id", "monthday")) %>%
  as.data.frame(.) %>%
  mutate(
    year_id = year(date),
    pneumonia_mean2 = ifelse(year_id %in% c(2019, 2020), p2, pneumonia_mean) )

#add on vaccine data
vax<-fread(paste0(root_dir,seir_version,'/reference/output_summaries/cumulative_vaccinations_all_vaccinated.csv'))[,.(location_id, date, mean)]
pop<-fread(paste0(root_dir,seir_version,'/reference/output_miscellaneous/populations.csv'))[age_group_id == 22 & sex_id == 3, .(location_id, population)]
vax<-merge(vax, pop, by='location_id')
vax[,proportion_coverage:=mean/population]
vax<-vax[,.(location_id, date, proportion_coverage)]
df_all$date<-as.Date(df_all$date)
df_all<-merge(df_all, vax, by=c('location_id', 'date'), all.x=T)

# Now add on variants
escape_inf<-fread(paste0(root_dir,seir_version,'/reference/output_summaries/daily_infections_variant.csv'))[,.(location_id,date,mean)]
total_inf<-fread(paste0(root_dir,seir_version,'/reference/output_summaries/daily_infections.csv'))[,.(location_id,date,mean)]
setnames(escape_inf, 'mean', 'escape_inf')
escape_var<-merge(escape_inf,total_inf, by=c('location_id','date'))
escape_var[mean==0, mean:=0.1]
escape_var<-escape_var[,escape_prev:=escape_inf/mean]
escape_var<-escape_var[,.(location_id,date,escape_prev)]
df_all$date<-as.Date(df_all$date)
df_all<-merge(df_all, escape_var, by=c('location_id', 'date'), all.x=T)

df_all_outfile <- paste0(out.draws,"/df_outcome_bylocday_v", seir_version, "_", draw,".csv")
fwrite(df_all, df_all_outfile, row.names = FALSE)

df_all<-merge(df_all,hier[, c("location_id",'level')], by="location_id")
setDT(df_all)
df_all<-df_all[level %in% c(3,4)]
df_all[,level:=NULL]

####

version_id <-"version"
input_dir_agespecific <- "FILEPATH"

df_ifr <- read.csv(file.path(input_dir_agespecific, "ifr_preds_5yr.csv"))
df_sero <- read.csv(file.path(input_dir_agespecific, "seroprev_preds_5yr.csv"))

df_pop_in <- read.csv(paste0(root_dir,"/all_populations.csv"))
age_group_ids_5yr <- c(1, 6:20, 30:32, 235)
df_pop <- df_pop_in %>%
  filter(sex_id == 3) %>%
  filter(age_group_id %in% age_group_ids_5yr)

for (x in c("df_ifr", "df_sero", "df_pop")) {
  cat(x, "\n")
  fwrite(get(x), file.path(paste0(out.draws,"/",x, "_", version_id, "_",draw,".csv")))
}

dat <- sapply(c("ifr","sero", "pop"), function(x) {
  read.csv(file.path(paste0(out.draws,"/df_", x, "_", version_id, "_",draw,".csv")))
}, simplify = FALSE)

df_age <- dat[["ifr"]] %>%
  select(age_start = age_group_start, age_end = age_group_end, ifr) %>%
  left_join(dat[["sero"]] %>% select(age_start = age_group_start, age_end = age_group_end, seroprev) ) %>%
  mutate(
    age_mid = (age_start + age_end) / 2
  )

df_pop <- dat[["pop"]] %>%
  select(location_id, age_start = age_group_years_start, age_end = age_group_years_end, population) %>%
  mutate(age_end = ifelse(age_end == 125, age_end, age_end - 1) ) %>%
  left_join(df_age, by = "age_start") %>%
  mutate(
    infections = seroprev * population) %>%
  group_by(location_id) %>%
  mutate(
    infection_weight = infections / sum(infections) ) %>%
  as.data.frame(.)

df_global <- df_pop %>%
  filter(location_id == 1) %>%
  group_by(.) %>%
  summarize(
    ifr_global = weighted.mean(x = ifr, w = infection_weight)
  )

df_loc_ratios <- df_pop %>%
  group_by(location_id) %>%
  summarize(
    expected_ifr = weighted.mean(x = ifr, w = infection_weight),
    standardized_ifr_ratio = expected_ifr / df_global$ifr_global ) %>%
  left_join(df_locmeta[, c("location_id", "location_name", "region_name", "super_region_name")])

## get latitude
lat  <- get_covariate_estimates(
  covariate_id = 56,
  gbd_round_id = 6,
  age_group_id = 22,
  decomp_step = 'step4',
  location_id = loc_lv3$location_id,
  year_id = 2019
)

lat<- lat[,.(location_id, mean_value)]
setnames(lat, 'mean_value', 'mean_latitude')
lat$n_hemisphere <- ifelse(lat$mean_latitude > 0, 1, 0)
df_all<-merge(df_all, lat, by='location_id', all.x=T)
#Canadian, German, and Spanish subnats all missing hemisphere designation. All are in northern hemisphere
df_all[is.na(n_hemisphere), n_hemisphere:=1]

# IFR lag, age standardization and seasonality adjustment
df_pop <- df_pop_in %>%
  filter(age_group_id == 22 & sex_id == 3)

df_all_tmp <- df_all %>%
  as.data.frame(.) %>%
  left_join(df_pop[, c("location_id", "population")], by = "location_id") %>%
  mutate(
    log_pneumonia_mean2 = log(pneumonia_mean2),
    daily_infection_rate2 = daily_infections2 / population,
    log_daily_infection_rate2 = log(daily_infection_rate2)
  )

fit_tmp <- lm(
  formula = log_daily_infection_rate2 ~ log_pneumonia_mean2, 
  data = df_all_tmp %>% filter(!log_daily_infection_rate2 == -Inf)
)

x=data.frame(intercept=1,log_pneumonia_mean2=df_all_tmp[,'log_pneumonia_mean2'])
xmat<-t(as.matrix(x))
df_all_tmp$log_infection_rate_pred<-as.vector(get_yhat(fit_tmp, ndraws=1, xmat))

x2=data.frame(intercept=1,log_pneumonia_mean2=RR)
xmat2<-t(as.matrix(x2))
df_all_tmp$log_infection_rate_pred_reference<-as.vector(get_yhat(fit_tmp, ndraws=1, xmat))

print('pneumonia lm')
df_all_tmp2 <- df_all_tmp %>%
  mutate(
    adjustment = log_infection_rate_pred - log_infection_rate_pred_reference,
    daily_infection_rate2_adjusted = exp(log_daily_infection_rate2 - adjustment),
    daily_infections2_adjusted = daily_infection_rate2_adjusted * population
  )

# period 0 

df_all2n <- as.data.frame(df_all_tmp2) %>%
  mutate(date2 = as_date(date)) %>%
  filter(date2 < period0_date & date2>=start_date) %>%
  mutate(period = 0)

setDT(df_all2n)
df_all2n[is.na(daily_infections2_adjusted), daily_infections2_adjusted := 0]

df_all2n <- as.data.frame(df_all2n) %>%
  arrange(location_id, date2) %>%
  group_by(location_id) %>%
  mutate(daily_infections_lag9 = lag(daily_infections_mean, n = 9),
         cumulative_infections_adj = cumsum(daily_infections2_adjusted)) %>%
  group_by(location_id, period) %>%
  summarize(
    period_deaths = sum(daily_deaths_mean, na.rm = TRUE),
    period_infections = sum(daily_infections_lag9, na.rm = TRUE),
    cumulative_infections_adj = sum(daily_infections2_adjusted)) %>%
  as.data.frame(.) %>%
  mutate(period_ifr = period_deaths / period_infections)

df_all3n <- df_all2n %>%
  left_join(df_loc_ratios[, c("location_id", "standardized_ifr_ratio")], by = "location_id") %>%
  mutate(period_ifr_age_adjusted = period_ifr * standardized_ifr_ratio)
df_ifr_std<-df_all3n

# period 4
df_all2n2 <- as.data.frame(df_all_tmp2) %>%
  mutate(date2 = as_date(date)) %>%
  filter(date2 < period4_date_end & date2>=start_date) %>%
  mutate(period = 4)

setDT(df_all2n2)
df_all2n2[is.na(daily_infections2_adjusted), daily_infections2_adjusted := 0]

df_all2n2 <- as.data.frame(df_all2n2) %>%
  arrange(location_id, date2) %>%
  group_by(location_id) %>%
  mutate(daily_infections_lag9 = lag(daily_infections_mean, n = 9),
         cumulative_infections_adj = cumsum(daily_infections2_adjusted)) %>%
  group_by(location_id, period) %>%
  summarize(
    period_deaths = sum(daily_deaths_mean, na.rm = TRUE),
    period_infections = sum(daily_infections_lag9, na.rm = TRUE),
    cumulative_infections_adj = sum(daily_infections2_adjusted)) %>%
  as.data.frame(.) %>%
  mutate(period_ifr = period_deaths / period_infections)

df_all3n2 <- df_all2n2 %>%
  left_join(df_loc_ratios[, c("location_id", "standardized_ifr_ratio")], by = "location_id") %>%
  mutate(period_ifr_age_adjusted = period_ifr * standardized_ifr_ratio)

df_ifr_std<-rbind(df_ifr_std, df_all3n2)


#write csvs of raw IFR / cumulative infections by period and location id

df_ifr_std_tmp<-merge(df_ifr_std, df_pop[,c('location_id', 'population')], by='location_id')
df_ifr_std_tmp$period_cum_inf_pop<-df_ifr_std_tmp$period_infections/df_ifr_std_tmp$population
df_ifr_std_tmp$period_cum_inf_pop_adj<-df_ifr_std_tmp$cumulative_infections_adj/df_ifr_std_tmp$population
out.df<-df_ifr_std_tmp[,c('location_id','period','period_deaths','period_infections','period_ifr','period_ifr_age_adjusted','period_cum_inf_pop','period_cum_inf_pop_adj')]
fwrite(out.df, paste0(out.draws,"/unadjusted_and_stage1_infections_IFR_new_pneumonia_", draw,'.csv'))

#global
setDT(out.df)
out.df[period==0 & location_id %in% hier[level==3,location_id], global_deaths:=sum(period_deaths)]
out.df[period==0 & location_id %in% hier[level==3,location_id], global_case:=sum(period_infections)]
out.df[period==0 & location_id %in% hier[level==3,location_id], global_ifr:=global_deaths / global_case]
out<-out.df[!duplicated(out.df[,.(global_deaths, global_case)]) & !is.na(global_ifr)]
out$location_id<-1
out<-merge(out, df_pop[,c('location_id', 'population')], by='location_id')
out$period_cum_inf_pop<-out$period_infections/out$population

fwrite(out, paste0(out.draws,"/global_ifr_", draw,'.csv'))

library(plyr)
# # Contextual variables:

#altitude
alt<-fread(paste0(ref_dir, "proportion_under_100m.csv"))
alt<-alt[,.(location_id, mean)]
setnames(alt, c('mean'), c('alt_under100'))
#keep only national (level 3) data to match the other sources
alt<-alt[location_id %in% hier[level %in% c(3,4), location_id],]

#air polution
air_pollution<-fread(paste0(ref_dir, "air_pollution_pm_2_5.csv"))
air_pollution<-air_pollution[,.(location_id, mean)]
setnames(air_pollution, c('mean'), c('air_pollution'))
#keep only national (level 3) data to match the other sources
air_pollution<-air_pollution[location_id %in% hier[level %in% c(3,4), location_id],]

#population density
density<-fread(pste0(ref_dir, '/all_outputs_2020_full.csv'))
density_1000ppl_km<- density[pop_density %in% c('1000-2500 ppl/sqkm','2500-5000 ppl/sqkm','5000-10000 ppl/sqkm','10000-20000 ppl/sqkm',
                                                '>=20000 ppl/sqkm'),.(location_id, pop_proportion)]
density_1000ppl_km[,proportion_1000ppl_km := sum(pop_proportion), by=location_id]
density_1000ppl_km[,pop_proportion:=NULL]
density_1000ppl_km<-density_1000ppl_km[!duplicated(density_1000ppl_km),]
density_1000ppl_km<-density_1000ppl_km[location_id %in% hier[level %in% c(3,4), location_id],]

density_500ppl_km<- density[pop_density %in% c('1000-2500 ppl/sqkm','2500-5000 ppl/sqkm','5000-10000 ppl/sqkm','10000-20000 ppl/sqkm',
                                               '>=20000 ppl/sqkm', '500-1000 ppl/sqkm'), .(location_id, pop_proportion)]
density_500ppl_km[,proportion_500ppl_km := sum(pop_proportion), by=location_id]
density_500ppl_km[,pop_proportion:=NULL]
density_500ppl_km<-density_500ppl_km[!duplicated(density_500ppl_km),]
density_500ppl_km<-density_500ppl_km[location_id %in% hier[level %in% c(3,4), location_id],]

density_5000ppl_km<- density[pop_density %in% c('5000-10000 ppl/sqkm','10000-20000 ppl/sqkm',
                                                '>=20000 ppl/sqkm'), .(location_id, pop_proportion)]
density_5000ppl_km[,proportion_5000ppl_km := sum(pop_proportion), by=location_id]
density_5000ppl_km[,pop_proportion:=NULL]
density_5000ppl_km<-density_5000ppl_km[!duplicated(density_5000ppl_km),]
density_5000ppl_km<-density_5000ppl_km[location_id %in% hier[level %in% c(3,4), location_id],]

density_10000ppl_km<- density[pop_density %in% c('10000-20000 ppl/sqkm',
                                                 '>=20000 ppl/sqkm'), .(location_id, pop_proportion)]
density_10000ppl_km[,proportion_10000ppl_km := sum(pop_proportion), by=location_id]
density_10000ppl_km[,pop_proportion:=NULL]
density_10000ppl_km<-density_10000ppl_km[!duplicated(density_10000ppl_km),]
density_10000ppl_km<-density_10000ppl_km[location_id %in% hier[level %in% c(3,4), location_id],]

density_20000ppl_km<- density[pop_density %in% c('>=20000 ppl/sqkm'), .(location_id, pop_proportion)]
density_20000ppl_km[,proportion_20000ppl_km := sum(pop_proportion), by=location_id]
density_20000ppl_km[,pop_proportion:=NULL]
density_20000ppl_km<-density_20000ppl_km[!duplicated(density_20000ppl_km),]
density_20000ppl_km<-density_20000ppl_km[location_id %in% hier[level %in% c(3,4), location_id],]

density<-merge(density_500ppl_km, density_1000ppl_km, by='location_id')
density<-merge(density_5000ppl_km, density, by='location_id')
density<-merge(density_10000ppl_km, density, by='location_id')
density<-merge(density_20000ppl_km, density, by='location_id')
density<-density[!is.na(density$proportion_1000ppl_km)] #two locations missing here: 367 and 396 - Comoros and N Mariana Islands

# -- GDP 2019
df_gdp <- get_covariate_estimates(
  covariate_id = 851, 
  gbd_round_id = 6,
  decomp_step = "iterative",
  location_id = hier[level %in% c(3,4),location_id],
  year_id = 2019
)
df_gdp[,GDP_mean:=mean_value]
df_gdp<-df_gdp[,.(location_id, GDP_mean)]

# -- Hospital Beds per capita
#read in raw data
hosp_beds <- fread(paste0(ref_dir,'/hospital_capacity.csv'))
hosp_beds<-hosp_beds[,.(location_id, all_bed_capacity)]
#aggregate up to USA
hosp_beds<-merge(hosp_beds, hierarchy[,.(location_id,level, parent_id)], by="location_id")
most_detailed<-hosp_beds[parent_id==102]
most_detailed[,HospBeds_Baseline:=sum(all_bed_capacity), by=list(parent_id)]
most_detailed[,location_id:=parent_id]
most_detailed[,parent_id:=NULL]
most_detailed[,all_bed_capacity:=NULL]
most_detailed[,level:=3]
most_detailed<-most_detailed[!duplicated(most_detailed)]
setnames(hosp_beds, 'all_bed_capacity','HospBeds_Baseline')
hosp_beds[,parent_id:=NULL]
hosp_beds2<-rbind(most_detailed, hosp_beds)
hosp_beds<-hosp_beds2[location_id %in%  hier[level %in% c(3,4),location_id],]
hosp_beds[,level:=NULL] #missing for Italy, Spain and Canada subnats
hosp_beds<-merge(hosp_beds,df_pop[,c('location_id', 'population')], by='location_id')
hosp_beds[,BedsPerCapita:=HospBeds_Baseline/population]
hosp_beds[,HospBeds_Baseline:=NULL]
hosp_beds[,population:=NULL]

# -- BMI
BMI <- get_covariate_estimates(
  covariate_id = 68,
  gbd_round_id = 6,
  age_group_id = 22,
  decomp_step = 'step4',
  location_id = hier[level %in% c(3,4),location_id],
  year_id = 2019
) 
## take weighted mean across sex
pop <- get_population(
  gbd_round_id = 6,
  decomp_step = "step4",
  location_id  = hier[level %in% c(3,4),location_id],
  year_id = 2019,
  sex_id = c(1,2)
)
pop[, tot_pop := sum(population), by = .(location_id, year_id)]
pop <- pop[, .(location_id, year_id, sex_id, pop_weight = population/tot_pop)]
BMI <- merge(BMI, pop, by = c("location_id", "year_id", "sex_id"))
BMI <- merge(BMI, hier, by = "location_id")
BMI <- BMI[,.(BMI = weighted.mean(mean_value, pop_weight)), by = location_id]

# -- Smoking
SMOKE<-fread(paste0(ref_dir, "smoking_prevalence.csv"))
SMOKE<-SMOKE[,.(location_id, mean)]
setnames(SMOKE, c('mean'), c('smoke_mean'))
smoke<-SMOKE[location_id %in% hier[level %in% c(3,4), location_id],]

#add on THE and GHES!
the<-fread(paste0(ref_dir,'the_pc.csv'))
the<-the[year_id==2020, .(ihme_loc_id,mean)]
the<-merge(the, hier[level==3, .(ihme_loc_id, location_id, level)], by="ihme_loc_id")
the<-the[location_id %in% hier[level %in% c(3,4), location_id],]
setnames(the, 'mean', 'THEpc')
the[,level:=NULL]
the[,ihme_loc_id:=NULL]

ghes<-fread(paste0(ref_dir,'ghes_pc.csv'))
ghes<-ghes[year_id==2020, .(ihme_loc_id,mean)]
ghes<-merge(ghes, hier[level==3, .(ihme_loc_id, location_id, level)], by="ihme_loc_id")
ghes<-ghes[location_id %in% hier[level %in% c(3,4), location_id],]
setnames(ghes, 'mean', 'GHESpc')
ghes[,level:=NULL]
ghes[,ihme_loc_id:=NULL]

#merge all on loc_id (only pneumonia, mandates & variants are time-varying)
covariates<-merge(df_gdp, hosp_beds, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, BMI, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, smoke, all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, density,all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, alt,all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, air_pollution,all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, the,all.x=T, all.y=T, by='location_id')
covariates<-merge(covariates, ghes,all.x=T, all.y=T, by='location_id')

cov_sub2<-covariates[!duplicated(covariates),]

#now merge with the response variables by date
df_all$date<-as.Date(df_all$date)
final_df<-merge(df_all, covariates, by=c('location_id'))

#break into two time periods
final_df$date<-as.Date(final_df$date)
setDT(final_df)
full_df_0<-final_df[date >= start_date & date<period0_date]
full_df_4<-final_df[date >= start_date & date<period4_date_end]

#get cumulative cases, deaths and infections per time period
full_df0a<-full_df_0[,.(cum_cases=cumsum(daily_cases_mean),cum_deaths=cumsum(daily_deaths_mean)), by=list(location_id,date)]
full_df0a<-full_df0a[date==period0_date-1]
full_df0a$cfr<-full_df0a$cum_deaths / full_df0a$cum_cases
full_df0a$ifr<-full_df0a$cum_deaths / full_df0a$cum_infections
full_df0a[, date:=NULL]
setDT(df_ifr_std)
df_ifr_std0<-df_ifr_std[period==0, .(location_id,period_ifr_age_adjusted, cumulative_infections_adj)]
df_ifr_std0<-df_ifr_std0[,.(period_ifr_age_adjusted=mean(period_ifr_age_adjusted),cumulative_infections_adj=max(cumulative_infections_adj)), by=list(location_id)]
final_df0a<-merge(full_df0a,df_ifr_std0, by='location_id')

full_df4a<-full_df_4[,.(cum_cases=cumsum(daily_cases_mean),cum_deaths=cumsum(daily_deaths_mean)), by=list(location_id,date)]
full_df4a<-full_df4a[date==period4_date_end-1]
full_df4a$cfr<-full_df4a$cum_deaths / full_df4a$cum_cases
full_df4a$ifr<-full_df4a$cum_deaths / full_df4a$cum_infections
full_df4a[, date:=NULL]
setDT(df_ifr_std)
df_ifr_std4<-df_ifr_std[period==4, .(location_id,period_ifr_age_adjusted, cumulative_infections_adj)]
df_ifr_std4<-df_ifr_std4[,.(period_ifr_age_adjusted=mean(period_ifr_age_adjusted),cumulative_infections_adj=max(cumulative_infections_adj)), by=list(location_id)]
final_df4a<-merge(full_df4a,df_ifr_std4, by='location_id')


full_df0<-full_df_0[,.(daily_cases_mean=mean(daily_cases_mean, na.rm=T), daily_deaths_mean=mean(daily_deaths_mean,na.rm=T),
                       mobility_mean=mean(mobility_mean,na.rm=T), mobility_min=min(mobility_mean,na.rm=T), testing_mean=mean(testing_mean,na.rm=T), vax_coverage_max=max(proportion_coverage, na.rm=T),variant_prev=mean(escape_prev, na.rm=T)),by=list(location_id)]
full_df0<-merge(full_df0,final_df0a, by="location_id")
full_df0<-merge(full_df0, cov_sub2, by="location_id")
full_df0<-merge(full_df0,df_pop[,c('location_id', 'population')], by="location_id")
#make cum / daily infections over pop
full_df0[,cum_inf_pop:=cumulative_infections_adj/population]

full_df4<-full_df_4[,.(daily_cases_mean=mean(daily_cases_mean, na.rm=T), daily_deaths_mean=mean(daily_deaths_mean,na.rm=T),
                       mobility_mean=mean(mobility_mean,na.rm=T), mobility_min=min(mobility_mean,na.rm=T), testing_mean=mean(testing_mean,na.rm=T), vax_coverage_max=max(proportion_coverage, na.rm=T),variant_prev=mean(escape_prev, na.rm=T)),by=list(location_id)]
full_df4<-merge(full_df4,final_df4a, by="location_id")
full_df4<-merge(full_df4, cov_sub2, by="location_id")
full_df4<-merge(full_df4,df_pop[,c('location_id', 'population')], by="location_id")
#make cum / daily infections over pop
full_df4[,cum_inf_pop:=cumulative_infections_adj/population]

#mandate data - % in place on a given day
mandate_data <- fread(paste0(ref_dir,"/all_mandate_data_prepped.csv"))
#cleaning up specific locations
mandate_data[, educational_fac := ifelse(location_id==85 & date >= "2020-09-25", 1, educational_fac)]
mandate_data[, all_noness_business := ifelse(location_id==85 & date >= "2020-09-25", 1, all_noness_business)]
mandate_data[location_id == 35, location_name := 'Georgia']
mandate_data_lvl3 = merge(mandate_data, hier[level == 4,.(location_id, parent_id, level)], by = 'location_id')
mandate_data_lvl3 = mandate_data_lvl3[,.(parent_id, location_id, date, stay_home, educational_fac, all_noness_business, travel_limit, any_gathering_restrict, any_business, location_name)]
mandate_data_lvl3[, location_id := parent_id]
mandate_data_lvl3 = as.data.table(melt(mandate_data_lvl3, id.vars = c('location_id', 'date', 'location_name')))
mandate_data_lvl3[is.na(value), value := 0]
mandate_data_lvl3 = mandate_data_lvl3[,lapply(.SD, mean), by = list(location_id, date, variable), .SDcols = c('value')]
#for locations with subnationals, taking the 66% in place rule for a location to have the mandate enacted
mandate_data_lvl3[value<=0.66, value_n:=0]
mandate_data_lvl3[value>0.66, value_n:=1]
mandate_data_lvl3[,value:=NULL]
setnames(mandate_data_lvl3, 'value_n','value')
mandate_data_lvl3 = dcast(mandate_data_lvl3, location_id+date~variable, value.var = 'value')
mandate_data_lvl3 = as.data.table(mandate_data_lvl3)

mandate_data_lvl3 = mandate_data_lvl3[!location_id %in% unique(mandate_data$location_id)]
mandate_data_lvl3 = merge(mandate_data_lvl3, hier[,.(location_name, location_id)], by = 'location_id')

mandate_data = rbind(mandate_data, mandate_data_lvl3, fill = T)
setDT(mandate_data)
mandate_data <- mandate_data[,.(location_id, location_name, date, stay_home, educational_fac, all_noness_business, travel_limit, any_gathering_restrict, any_business)]
mandate_data<-as.data.table(mandate_data)
mandate_data[is.na(stay_home), stay_home := 0]
mandate_data[is.na(educational_fac), educational_fac := 0]
mandate_data[is.na(all_noness_business), all_noness_business := 0]
mandate_data[is.na(travel_limit), travel_limit := 0]
mandate_data[is.na(any_gathering_restrict), any_gathering_restrict := 0]
mandate_data[is.na(any_business), any_business := 0]
mandate_data<-mandate_data[location_id %in% hier[level %in% c(3,4), location_id]]
mandate_data[,mand_on:=stay_home+educational_fac+all_noness_business+travel_limit+any_gathering_restrict+any_business]
mandate_data[,fraction_mandate_on:=mand_on/6]
mandate_data[,.(location_id, date, fraction_mandate_on)]
mandate_data<-mandate_data[location_id %in% final_df$location_id,]
mandate_data<-merge(mandate_data, lat, by='location_id', all.x=T)
#Canadian, German, and Spanish subnats all missing hemisphere designation. All are in northern hemisphere
mandate_data[is.na(n_hemisphere), n_hemisphere:=1]

#mean fraction on by wave
mandate_data_0<-mandate_data[date >= start_date & date<period0_date]
mandate_data_4<-mandate_data[date >= start_date & date<period4_date_end]


mandate_data_0<-mandate_data_0[,mean_fraction_mandate:=mean(fraction_mandate_on),by=list(location_id)]
mandate_data_0<-unique(mandate_data_0[,.(location_id, mean_fraction_mandate)])
mandate_data_4<-mandate_data_4[,mean_fraction_mandate:=mean(fraction_mandate_on),by=list(location_id)]
mandate_data_4<-unique(mandate_data_4[,.(location_id, mean_fraction_mandate)])

full_df0<-merge(full_df0, mandate_data_0, by='location_id', all.x=T)
full_df4<-merge(full_df4, mandate_data_4, by='location_id', all.x=T)

#output unscaled version for later analyses here:
unscaled<-full_df0
fwrite(unscaled, paste0(out.draws, '/draws/df0_unscaled_fig4_', draw,".csv"))
unscaled<-full_df4
fwrite(unscaled, paste0(out.draws, '/draws/df4_unscaled_fig4_', draw,".csv"))


#take log of testing, vaccines and GDP. Need 'pad' for 0s, which is 5% of median value
full_df0[,`:=` (testing_mean=log(testing_mean), GDP_mean=log(GDP_mean), vax_coverage_max=log(vax_coverage_max+0.015), BMI=log(BMI), alt_under100=log(alt_under100+0.015),
proportion_1000ppl_km=log(proportion_1000ppl_km), smoke_mean=log(smoke_mean), air_pollution=log(air_pollution))]
full_df4[,`:=` (testing_mean=log(testing_mean), GDP_mean=log(GDP_mean), BMI=log(BMI), alt_under100=log(alt_under100+0.015),
         proportion_1000ppl_km=log(proportion_1000ppl_km), smoke_mean=log(smoke_mean), air_pollution=log(air_pollution))]

fwrite(full_df0, paste0(out.draws, '/draws/final_df_wave0_covariates_scaled_', draw,".csv"))
fwrite(full_df4, paste0(out.draws, '/draws/final_df_wave4_covariates_scaled_', draw,".csv"))
