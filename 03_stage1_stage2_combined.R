
#load in libraries and data frame from prep
library(dplyr)
library(data.table)
library(readxl)
library(tidyverse)
library(rgdal)
library(ggrepel)
library(RColorBrewer)
library(argparse)
library(car)
library(MASS)

parser <- argparse::ArgumentParser()
parser$add_argument("--draw")
parser$add_argument("--seir-version")
parser$add_argument("--experiment")

args <- parser$parse_args()
for (key in names(args)) {
  assign(key, args[[key]])
}

for (arg in names(args)){
  print(paste0(arg, ": ", get(arg)))
}


#set seed = draw_NUM
num<-as.numeric(gsub("draw_", "", draw))
set.seed(num)
outdir<-'FILEPATH'
root_dir<-'FILEPATH'

hier<-hierarchy<-fread(paste(root_dir,"hierarchy_metadata.csv"))
bats<-fread(paste0(root_dir,'/bats/bat_covariate.csv'))
cancer<-fread(paste0(root_dir,'/prepped_covariates_cancer_copd.csv'))
df_pop_in <- read.csv(paste0(root_dir,"/all_populations.csv"))

pca_corruption<-readRDS(paste0(root_dir,'/pca_multiple_imputation_2vars_corruption.RDS'))
pca_govt_trust<-readRDS(paste0(root_dir,'/pca_multiple_imputation_2vars_trust.RDS'))
pca_trust_corrupt<-readRDS(paste0(root_dir,'/pca_multiple_imputation_4vars.RDS'))

#Get single draw estimates of PCA variables (numbered 1 - 100, so need to add 1 to the "NUM" value)

#183 observations for trust & corruption variables together
df1 <- data.frame(location_id=rep(NA,183),govt_trust_corrupt_pca=rep(NA,183))
df1$location_id<-pca_trust_corrupt$data$location_id
govt_trust_corrupt_pca<-pca_trust_corrupt$pca_mi_result[[num+1]]
setnames(govt_trust_corrupt_pca, paste0('draw_',num+1), 'govt_trust_corrupt_pca')
df1$govt_trust_corrupt_pca<-govt_trust_corrupt_pca

#143 observations for trust variables alone
df2<- data.frame(location_id=rep(NA,143),govt_trust_pca=rep(NA,143))
df2$location_id<-pca_govt_trust$data$location_id
govt_trust_pca<-pca_govt_trust$pca_mi_result[[num+1]]*-1
setnames(govt_trust_pca, paste0('draw_',num+1), 'govt_trust_pca')
df2$govt_trust_pca <-govt_trust_pca

#179 observations for corruption variables alone
df3<- data.frame(location_id=rep(NA,179),govt_corrupt_pca=rep(NA,179))
df3$location_id<-pca_corruption$data$location_id
govt_corrupt_pca <-pca_corruption$pca_mi_result[[num+1]]*-1
setnames(govt_corrupt_pca, paste0('draw_',num+1), 'govt_corrupt_pca')
df3$govt_corrupt_pca <-govt_corrupt_pca

#keep all observations, fill missings with NA
df<-left_join(df1, df3, by='location_id', all.x=T)
df2<-left_join(df, df2, by='location_id', all.x=T)

#function for getting an esimate of y hat per draw:
get_yhat<-function(model_fit, ndraws, xmatrix){
  betas = coef(model_fit)
  vcov = vcov(model_fit)
  betas_simulated = MASS::mvrnorm(ndraws, betas, vcov)
  beta_mat<-t(as.matrix(betas_simulated))
  yhat = beta_mat %*% xmatrix
  return(yhat)
}

dir.create(paste0(outdir, seir_version, "/", experiment, "/", "final_outputs/"))

# process analytic datasets

prep_stage1_data <- function(csv_path, wave_id) {
   final_df_tmp <- copy(fread(csv_path))
  final_df_tmp <- final_df_tmp[!(location_id %in% c(189)),] # remove tanzania
  final_df_tmp <- final_df_tmp[!is.na(BMI),]
  final_df_tmp <- final_df_tmp[!is.na(proportion_1000ppl_km),]
  final_df_tmp <- merge(final_df_tmp, bats, by='location_id') 
  final_df_tmp <- merge(final_df_tmp, cancer, by='location_id')
  final_df_tmp$log_period_ifr_age_adjusted<-log(final_df_tmp$period_ifr_age_adjusted)
  final_df_tmp$log_cum_inf_pop<-log(final_df_tmp$cum_inf_pop)
  
  final_df_tmp<-merge(final_df_tmp, hier[,.(location_id, parent_id, level,location_name)], by='location_id')
  final_df_tmp[level==4, parent_id:=parent_id]
  final_df_tmp[level==3, parent_id:=location_id]
  obs_per_parent<-as.data.frame(table(final_df_tmp$parent_id))
  names(obs_per_parent)<-c('parent_id', 'num')
  final_df_tmp<-merge(final_df_tmp,df_pop_in[df_pop_in$age_group_id==22 & df_pop_in$sex_id==3,c('location_id', 'population')], by.x='parent_id', by.y='location_id')
  setnames(final_df_tmp, c("population.x", "population.y"), c('loc_pop', 'parent_pop'))
  final_df_tmp[, weight:=loc_pop/parent_pop]
  #try a second weighting option to mitigate "double weight" of places with subnats
  final_df_tmp$parent_id<-as.factor(final_df_tmp$parent_id)
  final_df_tmp<-merge(final_df_tmp, obs_per_parent, by='parent_id')
  final_df_tmp$weight2<-ifelse(final_df_tmp$weight==1 & final_df_tmp$num>1, final_df_tmp$weight/(final_df_tmp$num-1), final_df_tmp$weight)
  
  #need to scale beds per capita, THE, and GHE here now. 
  location_id<-final_df_tmp$location_id
  scale_vars<-c('BedsPerCapita', 'THEpc','GHESpc')
  data_scale<-final_df_tmp[,lapply(.SD, function(x) scale(x, scale=TRUE)), .SDcols = scale_vars]
  names(data_scale)<-scale_vars
  data_scale$location_id<-location_id
  set(final_df_tmp, , scale_vars, NULL)
  final_df_tmp<-merge(final_df_tmp, data_scale, by='location_id')
  
  return(list(data = final_df_tmp, wave_id = wave_id))
  
}


# -- all data
final_df_list <- lapply(0:4, function(x) {
  csv_path_tmp <- paste0(root_dir, seir_version,"/", experiment,"/draws/final_df_wave", x, "_covariates_scaled_", draw,".csv")
  prep_stage1_data(csv_path = csv_path_tmp, wave_id = x)
})

# -- seroprev locations only
seroprev<-fread(paste0(root_dir,'/global_serology_summary.csv'))
seroprev<-seroprev[manual_outlier==0] #207 unique locations
seroprev_locids<-unique(seroprev$location_id)

final_df_list_seroprev <- lapply(final_df_list, function(x) {
  x$data <- subset(x$data, location_id %in% seroprev_locids)
  return(x)
})


#####
# models and plotting

run_stage1 <- function(data_object, depvar, covars) {
  dat <- as.data.frame(data_object$data)
  wave_id <- data_object$wave_id
  
  #if(wave_id==0){covars<-covars[covars!="GDP_mean"]}
  formula_tmp <- as.formula(paste0(depvar, " ~ ", paste(covars, collapse = " + ")))
  fit_tmp <- glm(formula=formula_tmp, weights=weight, data=dat, family=gaussian(link='identity'))
  vif<-car::vif(fit_tmp)
  vifs<-as.data.frame(vif)
  fwrite(vifs, paste0(root_dir, seir_version, "/", experiment, "/", "/draws/vifs_",depvar,"_", draw, ".csv"))

  # fit_tmp <- glm(formula=formula_tmp, data=dat, family=gaussian(link='identity'))
  # fit_tmp <- lm(formula=formula_tmp, weights=weight, data=dat)
  
  out_tmp <- data.frame(cov_name = c("(Intercept)", covars))
  out_tmp$beta__tmp <- round(sapply(out_tmp$cov_name, function(x) coef(summary(fit_tmp))[x, 1] ), 5)
  out_tmp$std.err__tmp <- round(sapply(out_tmp$cov_name, function(x) coef(summary(fit_tmp))[x, 2] ), 5)
  out_tmp$aic__tmp <- AIC(fit_tmp)
  out_tmp$pval__tmp <- round(sapply(out_tmp$cov_name, function(x) coef(summary(fit_tmp))[x, 4] ), 4)
  names(out_tmp) <- gsub("__tmp", paste0("_", wave_id), names(out_tmp))
  
  
  #dat$predicted_log <- predict(fit_tmp, dat)
  df<-dat[,covars]
  df$intercept<-1
  df<-df[, c('intercept', covars)]
  
  #x<-matrix(c(rep(1, nrow(dat)),rep(0, nrow(dat)*length(covars))), ncol=length(covars)+1, byrow=FALSE)
  xmat<-t(as.matrix(df))
  dat$predicted_log <-as.vector(get_yhat(fit_tmp, ndraws=1, xmat))
  
  betas = coef(fit_tmp)
  vcov = vcov(fit_tmp)
  betas_simulated = MASS::mvrnorm(1, betas, vcov) 
  
  out_tmp <- data.frame(cov_name = c("(Intercept)", covars), beta__tmp = round(betas_simulated, 5))
  # out_tmp$beta__tmp <- round(betas_simulated, 5)
  # out_tmp$std.err__tmp <- round(sapply(out_tmp$cov_name, function(x) coef(summary(fit_tmp))[x, 2] ), 5)
  # out_tmp$aic__tmp <- AIC(fit_tmp)
  # out_tmp$pval__tmp <- round(sapply(out_tmp$cov_name, function(x) coef(summary(fit_tmp))[x, 4] ), 4)
  names(out_tmp) <- gsub("__tmp", paste0("_", wave_id), names(out_tmp))
  
  # betas_out<-as.data.frame(betas_simulated)
  # betas_out$var<-c('intercept', covars)
  #fwrite(betas_out, paste0(root_dir, seir_version, "/", experiment, "/", "final_outputs/stage1_results_",version_id,"_", draw, ".csv"))
  
  dat$predicted <- exp(dat$predicted_log)
  dat$logscale_residual <- dat[, depvar] - dat$predicted_log
  # dat$stage1_residual_v2 <- residuals(fit_tmp, type = "response")
  # dat$residual_function <- residuals(fit_tmp)
  
  ## plots
  plot_dat_tmp <- lapply(covars, function(covar) {
    ggplot(data=data_object$data, aes(x=get(covar), y=get(depvar)))+theme_bw()+geom_point()+
      xlab(covar)+ylab(depvar)
  })
  
  return(list(data = dat, wave_id = wave_id, fit_object = fit_tmp, results = out_tmp, plot_objects = plot_dat_tmp))
}

run_stage2 <- function(stage1_output, covars_stage2, covs_filepath, verbose = FALSE) {
 
  try({
    df_covs <- fread(covs_filepath)
    df_covs[,'govt_corrupt_pca':=NULL]
    df_covs[,'govt_trust_pca':=NULL]
    df_covs[,'govt_trust_corrupt_pca':=NULL]
    data <- stage1_output$data
    data <- merge(data, df_covs, by='location_id', all.x=T)
    data <- merge(data, hier[,.(location_id, ihme_loc_id)], by='location_id', all.x=T)
    data$res <- data$logscale_residual
    data<-merge(data, df2, by='location_id')

    ## drop subnats
    data <- data.table(data)
    data[,exclude:= 0]
    data[level>3, exclude:= 1]
    data <- as.data.frame(data)
    ## subset out location in the regression below
    
    dat_by_covar <- sapply(names(covars_stage2), function(covar) {
      
      label_tmp <- covars_stage2[covar]
      if (verbose) cat(covar, "\n")
      # print(covar)
      data$covar_tmp <- data[, covar]
      data2 <- data %>%
        filter(!is.na(res) & !is.na(covar_tmp))
      fit_tmp <- lm(formula = as.formula(paste0("res ~ ", covar)),
                    data = data2[data2[,"exclude"] ==0,])
      
      # coefs <- list(
      #   slope = round(summary(fit_tmp)$coefficients[2,1],5),
      #   std.err = round(summary(fit_tmp)$coefficients[2,2],5),
      #   pval = round(summary(fit_tmp)$coefficients[2,4],4)
      # )
      betas = coef(fit_tmp)
      vcov = vcov(fit_tmp)
      betas_simulated = MASS::mvrnorm(1, betas, vcov) 
      betas_out<-as.data.frame(betas_simulated)
      betas_out$covar<-c('intercept', covar)
      
        data3 <- data2 %>%
        filter(res> 2 | res< -2 | covar_tmp> 2 | covar_tmp< -2)
      
      p <- ggplot() +
        geom_point(aes(x = data[, covar], y = data[, "res"])) +
        geom_hline(aes(yintercept=0), col='red', lty=2) +
        #   ggtitle(paste0('slope: ', coefs$slope,'; p-value: ', coefs$pval)) +
        theme_bw() + ylab('Residuals') +
        xlab(label_tmp) + geom_smooth(method='lm', se=FALSE,aes(x=data2$covar, y=data2$res)) +
        geom_text_repel(aes(x = data3$covar_tmp, y = data3$res, label = data3$ihme_loc_id))
      
      out_covar <- list(covar = covar, fit_object = fit_tmp, coefs = betas_out, plot_dat = p )
      
      return(out_covar)
    }, simplify = FALSE)
    
    return(dat_by_covar)
    
  })
  
  
}



covars_stage2_v1 <- c(
  HAQI_mean = "HAQI (scaled)", 
  uhc_19 = "UHC (scaled)", 
  ncd_coverage = "UHC for Non-Communicable Diseases (scaled)", 
  cmnn_coverage= "UHC for Communicable, Maternal and Neonatal Diseases (scaled)", 
  ghsi_0_overall = "GHSI score (scaled)", 
  ghsi_1_prevent = "GHSI score -prevent (scaled)", 
  ghsi_2_detect = "GHSI score -detect (scaled)", 
  ghsi_3_respond = "GHSI score - respond (scaled)", 
  ghsi_4_health_sector = "GHSI score -health sector (scaled)", 
  ghsi_5_commit = "GHSI score - commit (scaled)", 
  ghsi_6_risk = "GHSI score - risk (scaled)",  
  jee_readyscore_overall = "JEE score (scaled)", 
  jee_readyscore_prevent = "JEE score - Prevent (scaled)", 
  jee_readyscore_detect = "JEE score - Detect (scaled)", 
  jee_readyscore_respond = "JEE score - Respond (scaled)", 
  jee_readyscore_other = "JEE score - Other (scaled)", 
  THEpc = "Total Health Expenditure per capita (scaled)",
  GHESpc = "Government Health Spending per capita (scaled)", 
  gini = "GINI Index (scaled)", 
  bureaucracy_corrupt = "Bureaucracy corruption index (scaled)", 
  gov_effect = "Government effectiveness (scaled)", 
  v2x_mpi = "Multiplicative Polyarchy Index (scaled)", 
  v2x_polyarchy = "Electoral Democracy Index (EDI) (scaled)", 
  state_fragility = "State Fragility (scaled)", 
  lib_cons_avg = "Economic left / right scale average (scaled)", 
  v2pariglef_gov_avg = "Conservative Liberal Values Average (scaled)", 
  interpersonal_trust = "Interpersonal Trust (scaled)", 
  trust_science = "Trust in Science (scaled)", 
  govt_trust = "Governmental Trust (scaled)", 
  trust_govt_cpi = "Governmental Corruption - CPI (scaled)", 
  trust_govt_gallup = "Governmental Trust - Gallup (scaled)", 
  govt_trust_pca = " Governmental Trust - PCA (scaled)", 
  govt_corrupt_pca = "Governmental Corruption - PCA (scaled)",
  govt_trust_corrupt_pca = "Governmental Trust & Corruption - PCA (scaled)",
  electoral_pop = "Populist leader (scaled)", 
  federal_ind = "Index of federalism  (scaled)", 
  mobility_min = "Minimum mobility percentage from baseline (scaled)", 
  # testing_mean = "Diagnostic Tests per 100,000 population (log, scaled)", 
  mean_fraction_mandate = "Mean fraction of mandates on (scaled)", 
  BedsPerCapita = "Hospital beds at baseline per capita (scaled)",
  v2xpa_illiberal_gov = "Government Illiberalism (scaled)",
  v2xpa_popul_gov = "Government Use of Populist Rhetoric (scaled)"
  #vax_coverage_max = "Vaccine Coverage (log, scaled)"
)


run_stage1_stage2 <- function(data_object, depvar, covars_stage1, covars_stage2, version_id) {
    
  stage1_tmp <- lapply(data_object, function(x) {
    run_stage1(data_object = x, depvar = depvar, covars = covars_stage1 )
  })
  names(stage1_tmp) <- sapply(stage1_tmp, function(x) x$wave_id)
  
  df_stage1_results <- lapply(stage1_tmp, function(x) x$results) %>%
    reduce(left_join, by = "cov_name")
  
  stage2_tmp <- sapply(stage1_tmp, function(x) {
    run_stage2(
      stage1_output = x, 
      covars_stage2 = covars_stage2, 
      covs_filepath = paste0(root_dir,"/prepped_covariates.csv"),
      verbose = FALSE
    )
  }, simplify = FALSE)
  
  dat_stage2_results <- lapply(stage2_tmp, function(x) {
    do.call("rbind", lapply(x, function(y) { 
      data.frame(append(list(covar = y$covar), y$coefs))  
    }))
  })
  
  df_stage2_results <- lapply(names(dat_stage2_results), function(x) {
    df_x <- dat_stage2_results[[x]]
    df_x<-df_x[,c('betas_simulated', 'covar.1')]
    names(df_x)<-c('betas', 'covar')
    df_x<-df_x[df_x$covar!='intercept',]
    names_tmp <- c('betas')
    df_x %>% setnames(old = names_tmp, new = paste0(names_tmp, "_", x))
  }) %>%
    reduce(left_join, by = "covar")
  
  out <- list(
    version_id = version_id,
    stage1_tmp = stage1_tmp, df_stage1_results = df_stage1_results,
    stage2_tmp = stage2_tmp, df_stage2_results = df_stage2_results
  )
  return(out)
  
}


df_versions <- expand.grid(metric = c("infections", "ifr"), dataset = c("all", "sero")) %>%
  # df_versions <- expand.grid(metric = c("infections", "ifr"), dataset = c("all")) %>%
  mutate(
    metric_var = ifelse(metric == "ifr", "log_period_ifr_age_adjusted", "log_cum_inf_pop"),
    version_id = paste0(metric, "_", dataset) )


all_results <- lapply(1:nrow(df_versions), function(i) {
  
  version_tmp <- df_versions[i, "version_id"]
  cat(version_tmp, "\n")
  if (df_versions[i, "dataset"] == "sero") {
    data_tmp <- final_df_list_seroprev
  } else {
    data_tmp <- final_df_list
    # data_tmp <- df_list_expr1
  }
  if (substr(version_tmp, 1, 3) == "inf") {
    covars_stage1_tmp <- c( "proportion_1000ppl_km","GDP_mean", "alt_under100", "summed_bat_species")
  } else {
    covars_stage1_tmp <- c("BMI", "proportion_1000ppl_km", "smoke_mean","GDP_mean",  "air_pollution", "summed_bat_species","cancer", 'copd')
  }
  tmp <- run_stage1_stage2(
    data_object = data_tmp, 
    depvar = df_versions[i, "metric_var"], 
    covars_stage1 =  covars_stage1_tmp,
    covars_stage2 = covars_stage2_v1, 
    version_id = version_tmp
  )
  out <- list(result = tmp, version_id = version_tmp)
  return(out)
})
names(all_results) <- sapply(all_results, function(x) x$version_id)


stage1_results<- rbind(all_results[[1]]$result$df_stage1_results,all_results[[2]]$result$df_stage1_results,all_results[[3]]$result$df_stage1_results,all_results[[4]]$result$df_stage1_results)
stage1_results$dep_group<-c(rep(all_results[[1]]$version_id, nrow(all_results[[1]]$result$df_stage1_results)), 
                            rep(all_results[[2]]$version_id, nrow(all_results[[2]]$result$df_stage1_results)),
                            rep(all_results[[3]]$version_id, nrow(all_results[[3]]$result$df_stage1_results)),
                            rep(all_results[[4]]$version_id, nrow(all_results[[4]]$result$df_stage1_results)))

fwrite(stage1_results, paste0(outdir, seir_version, "/", experiment, "/", "final_outputs/stage1_results_", draw, ".csv"))


stage2_results<- rbind(all_results[[1]]$result$df_stage2_results,all_results[[2]]$result$df_stage2_results,all_results[[3]]$result$df_stage2_results,all_results[[4]]$result$df_stage2_results)
stage2_results$dep_group<-c(rep(all_results[[1]]$version_id, nrow(all_results[[1]]$result$df_stage2_results)),
                            rep(all_results[[2]]$version_id, nrow(all_results[[2]]$result$df_stage2_results)),
                            rep(all_results[[3]]$version_id, nrow(all_results[[3]]$result$df_stage2_results)),
                            rep(all_results[[4]]$version_id, nrow(all_results[[4]]$result$df_stage2_results)))

fwrite(stage2_results, paste0(outdir, seir_version, "/", experiment, "/", "final_outputs/stage2_results_", draw, ".csv"))

#for figure 2:
stage_1_dat1<-all_results[[2]]$result$stage1_tmp$`0`$data
stage_1_dat1$wave<-0
stage_1_dat1$var<-'ifr_all'

stage_1_dat2<-all_results[[2]]$result$stage1_tmp$`4`$data
stage_1_dat2$wave<-4
stage_1_dat2$var<-'ifr_all'

stage_1_dat3<-all_results[[1]]$result$stage1_tmp$`0`$data
stage_1_dat3$wave<-0
stage_1_dat3$var<-'infections_all'

stage_1_dat4<-all_results[[1]]$result$stage1_tmp$`4`$data
stage_1_dat4$wave<-4
stage_1_dat4$var<-'infections_all'
fig2<-rbind(stage_1_dat1, stage_1_dat2, stage_1_dat3, stage_1_dat4)
fwrite(fig2, paste0(outdir, seir_version, "/", experiment, "/", "fig2_data_", draw, ".csv"))


#for figure 2:
stage_1_dat1<-all_results[[4]]$result$stage1_tmp$`0`$data
stage_1_dat1$wave<-0
stage_1_dat1$var<-'ifr_sero'

stage_1_dat2<-all_results[[4]]$result$stage1_tmp$`4`$data
stage_1_dat2$wave<-4
stage_1_dat2$var<-'ifr_sero'

stage_1_dat3<-all_results[[3]]$result$stage1_tmp$`0`$data
stage_1_dat3$wave<-0
stage_1_dat3$var<-'infections_sero'

stage_1_dat4<-all_results[[3]]$result$stage1_tmp$`4`$data
stage_1_dat4$wave<-4
stage_1_dat4$var<-'infections_sero'
fig2<-rbind(stage_1_dat1, stage_1_dat2, stage_1_dat3, stage_1_dat4)
fwrite(fig2, paste0(outdir, seir_version, "/", experiment, "/", "fig2_data_sero_", draw, ".csv"))

