#set these:
seir_version<-'SEIR VERSION'
experiment<-"Experiment Name"
suffix<-gsub("Experiment","", experiment)

root_dir<-'FILEPATH'
in_dir<-'FILEPATH'
ref_dir_draw <- paste0(root_dir, seir_version, "/reference/output_draws/")
ref_dir <- paste0(root_dir, seir_version, "/reference/output_summaries/")

hier<-hierarchy<-fread(paste(root_dir,"hierarchy_metadata.csv"))

filenames=list.files(path=paste0(in_dir), pattern="fig2_data_",full.names=TRUE)
a<-fread(file=filenames[1],header=T)
lv3<-a[a$level==3,location_id]

start_date<-as.Date('2020-01-01')
period0_date<-as.Date('2021-09-30')
period4_date_start<-as.Date('2020-01-01')
period4_date_end<-as.Date('2020-10-15')

#summarize stage 1

# wave 0
filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment,'/final_outputs/'), pattern="stage1",full.names=TRUE)
get.list<-function(x,vars){
    a<-fread(file=x,header=T)[cov_name!="(Intercept)",..vars]
    val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/final_outputs//stage1_results_'), "", x)
    val<-gsub('.csv', "", val)
    setnames(a, "beta_0",val)
    a
    }
  
  vars<-c('cov_name','dep_group','beta_0')
  datalist = lapply(filenames, function(x) get.list(x,vars))
  o_draws<-c()
  setDT(o_draws)
  o_draws$num<-0:99
  o_draws[, draw:=paste0('draw_',num)]
  q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('cov_name','dep_group'))}, datalist)
  setDT(q)
  q<-setcolorder(q, c('cov_name','dep_group', o_draws$draw))
  n<-names(q[,3:102])
  q$var_mean <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.5, na.rm=T))
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  sd.cols = n
  th<-0
  q[,exceed.count:=sum(.SD>th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  q[,below.count:=sum(.SD<th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  #2 sided p-value
  q[var_mean<0,p.value:=exceed.count/(100/2)] #2 sided p-value
  q[var_mean>0,p.value:=below.count/(100/2)] #2 sided p-value
  r<-q[,-c(3:102)]
  r<-r[dep_group=="infections_all",]
  p<-q[,-c(3:102)]
  p<-p[dep_group=="ifr_all",]
  o<-rbind(r,p)
  
  write.csv(o,paste0(root_dir,'/stage1_results_wv0_', suffix, '.csv'))
  #stage 1 - wave 4
  filenames=list.files(path= paste0(root_dir,seir_version,'/', experiment,'/final_outputs/'), pattern="stage1",full.names=TRUE)
  get.list<-function(x,vars){
    a<-fread(file=x,header=T)[cov_name!="(Intercept)",..vars]
    val<-gsub( paste0(root_dir,seir_version,'/', experiment,'/final_outputs//stage1_results_'), "", x)
    val<-gsub('.csv', "", val)
    setnames(a, "beta_4",val)
    a
  }

  vars<-c('cov_name','dep_group','beta_4')
  datalist = lapply(filenames, function(x) get.list(x,vars))
  o_draws<-c()
  setDT(o_draws)
  o_draws$num<-0:99
  o_draws[, draw:=paste0('draw_',num)]
  q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('cov_name','dep_group'))}, datalist)
  setDT(q)
  q<-setcolorder(q, c('cov_name','dep_group', o_draws$draw))
  n<-names(q[,3:102])
  q$var_mean <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.5, na.rm=T))
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  sd.cols = n
  th<-0
  q[,exceed.count:=sum(.SD>th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  q[,below.count:=sum(.SD<th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  #2 sided p-value
  q[var_mean<0,p.value:=exceed.count/(100/2)] #2 sided p-value
  q[var_mean>0,p.value:=below.count/(100/2)] #2 sided p-value
  r<-q[,-c(3:102)]
  r<-r[dep_group=="infections_all",]
  p<-q[,-c(3:102)]
  p<-p[dep_group=="ifr_all",]
  
  o<-rbind(r,p)
  
  write.csv(o,paste0(root_dir,'/stage1_results_wv4_', suffix, '.csv'))
  
  #stage 2 summary
  #wave 0
  filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment,'/final_outputs/'), pattern="stage2",full.names=TRUE)
  get.list<-function(x,vars){
    a<-fread(file=x,header=T)[,..vars]
    val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/final_outputs//stage2_results_'), "", x)
    val<-gsub('.csv', "", val)
    setnames(a, "betas_0",val)
    a
  }
  
  vars<-c('covar','dep_group','betas_0')
  datalist = lapply(filenames, function(x) get.list(x,vars))
  o_draws<-c()
  setDT(o_draws)
  o_draws$num<-0:99
  o_draws[, draw:=paste0('draw_',num)]
  q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('covar','dep_group'))}, datalist)
  setDT(q)
  q<-setcolorder(q, c('covar','dep_group', o_draws$draw))
  n<-names(q[,3:102])
  q$var_mean <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.5, na.rm=T))
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  q$var_mean <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.5, na.rm=T))
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  sd.cols = n
  th<-0
  q[,exceed.count:=sum(.SD>th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  q[,below.count:=sum(.SD<th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  #2 sided p-value
  q[var_mean<0,p.value:=exceed.count/(100/2)] #2 sided p-value
  q[var_mean>0,p.value:=below.count/(100/2)] #2 sided p-value
  r<-q[,-c(3:102)]
  r<-r[dep_group=="infections_all",]
  r[order(p.value),]
  p<-q[,-c(3:102)]
  p<-p[dep_group=="ifr_all",]
  
  outA<-q[,-c(3:102)]
  outA$wave<-0
  
  #wave 4
  filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment,'/final_outputs/'), pattern="stage2",full.names=TRUE)
  get.list<-function(x,vars){
    a<-fread(file=x,header=T)[,..vars]
    val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/final_outputs//stage2_results_'), "", x)
    val<-gsub('.csv', "", val)
    setnames(a, "betas_4",val)
    a
  }
  
  vars<-c('covar','dep_group','betas_4')
  datalist = lapply(filenames, function(x) get.list(x,vars))
  o_draws<-c()
  setDT(o_draws)
  o_draws$num<-0:99
  o_draws[, draw:=paste0('draw_',num)]
  q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('covar','dep_group'))}, datalist)
  setDT(q)
  q<-setcolorder(q, c('covar','dep_group', o_draws$draw))
  n<-names(q[,3:102])
  q$var_mean <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.5, na.rm=T))
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  sd.cols = n
  th<-0
  q[,exceed.count:=sum(.SD>th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  q[,below.count:=sum(.SD<th),by=seq_len(nrow(q)), .SDcols=sd.cols]
  #2 sided p-value
  q[var_mean<0,p.value:=exceed.count/(100/2)] #2 sided p-value
  q[var_mean>0,p.value:=below.count/(100/2)] #2 sided p-value
  r<-q[,-c(3:102)]
  r<-r[dep_group=="infections_all",]
  r[order(p.value),]
  p<-q[,-c(3:102)]
  p<-p[dep_group=="ifr_all",]
  
  outB<-q[,-c(3:102)]
  outB$wave<-4
  
  out<-rbind(outA,outB)
  
  write.csv(out,paste0(outdir,'stage2_results_', suffix, '.csv'))
  
  #predict out residuals:
  #need to get mean values of data included in model - will be done by reading in bats, and then using just the first draw of raw data given no variability
  bats<-fread(pasteo(in_dir,'bat_covariate.csv'))
  bat_mean<-mean(bats$summed_bat_species)
  dat0<-fread(paste0(root_dir,seir_version,'/', experiment,'/fig2_data_draw_0.csv'))
  BMI_mean<-mean(dat0$BMI)
  GDP_mean<-mean(dat0$GDP_mean)
  air_pollution_mean<-mean(dat0$air_pollution)
  alt_under100_mean<-mean(dat0$alt_under100)
  proportion_1000ppl_km_mean<-mean(dat0$proportion_1000ppl_km)
  smoke_mean<-mean(dat0$smoke_mean)

  pred_out_inf0<-model_inf0[1,2]+model_inf0[2,2]*GDP_mean+model_inf0[3,2]*alt_under100_mean+model_inf0[4,2]*proportion_1000ppl_km_mean+model_inf0[5,2]*bat_mean
  pred_out_inf4<-model_inf4[1,2]+model_inf4[2,2]*GDP_mean+model_inf4[3,2]*alt_under100_mean+model_inf4[4,2]*proportion_1000ppl_km_mean+model_inf4[5,2]*bat_mean

  pred_out_ifr0<-model_ifr0[1,2]+model_ifr0[2,2]*BMI_mean+model_ifr0[3,2]*GDP_mean+model_ifr0[4,2]*air_pollution_mean+model_ifr0[5,2]*cancer_mean+model_ifr0[6,2]*copd_mean+model_ifr0[7,2]*proportion_1000ppl_km_mean+model_ifr0[8,2]*smoke_mean + model_ifr0[9,2]*bat_mean
  pred_out_ifr4<-model_ifr4[1,2]+model_ifr4[2,2]*BMI_mean+model_ifr4[3,2]*GDP_mean+model_ifr4[4,2]*air_pollution_mean+model_ifr4[5,2]*cancer_mean+model_ifr4[6,2]*copd_mean+model_ifr4[7,2]*proportion_1000ppl_km_mean+model_ifr4[8,2]*smoke_mean + model_ifr4[9,2]*bat_mean
  
  #raw data by draw:
  filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment), pattern="fig2_data_",full.names=TRUE)
  filenames<-filenames[1:100]
  get.list<-function(x,vars){
    a<-fread(file=x,header=T)[,..vars]
    val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/fig2_data_', "", x))
    val<-gsub('.csv', "", val)
    setnames(a, "logscale_residual",val)
    a
  }
  vars<-c('location_id','var','wave','logscale_residual')
  datalist = lapply(filenames, function(x) get.list(x,vars))
  o_draws<-c()
  setDT(o_draws)
  o_draws$num<-0:99
  o_draws[, draw:=paste0('draw_',num)]
  q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('location_id','var', 'wave'))}, datalist)
  setDT(q)
  q<-setcolorder(q, c('location_id','var','wave', o_draws$draw))
  n<-names(q[,4:103])
  q$var_mean <- rowMeans(q[, ..n])
  q$var_lower <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.025, na.rm=T))
  q$var_upper <- apply(q[, ..n], MARGIN=1, FUN=function(x) quantile(x, 0.975, na.rm=T))
  t<-q[,-c(4:103)]
  t1<-t[var=="ifr_all",]
  setnames(t1, c('var_mean','var_lower','var_upper'), c('ifr_mean','ifr_lower','ifr_upper'))
  t2<-t[var=="infections_all",]
  setnames(t2, c('var_mean','var_lower','var_upper'), c('inf_mean','inf_lower','inf_upper'))
  t<-merge(t1,t2, by=c('location_id','wave'))
  
  df0<-t[wave==0,]
  df0$intercept_ifr0<-pred_out_ifr0 
  df0$intercept_inf0<-pred_out_inf0
  df0[,centered_pred_ifr_all := (intercept_ifr0 + ifr_mean)]
  df0[,centered_pred_infections_all := (intercept_inf0 + inf_mean)]
  
  df4<-t[wave==4,]
  df4$intercept_ifr4<-pred_out_ifr4
  df4$intercept_inf4<-pred_out_inf4
  df4[,centered_pred_ifr_all := intercept_ifr4 + ifr_mean]
  df4[,centered_pred_infections_all := intercept_inf4 + inf_mean]
  
  df0_tmp<-merge(df0, hier[,c("location_id", "ihme_loc_id", "super_region_name", "level")], by='location_id')
  df0_tmp<-df0_tmp[level == 3,]
  df0_tmp[, `:=` (centered_pred_infections_exp = exp(centered_pred_infections_all), centered_pred_ifr_exp= exp(centered_pred_ifr_all))]
  
  df4_tmp<-merge(df4, hier[,c("location_id", "ihme_loc_id", "super_region_name", "level")], by='location_id')
  df4_tmp<-df4_tmp[level == 3,]
  df4_tmp[, `:=` (centered_pred_infections_exp = exp(centered_pred_infections_all),  centered_pred_ifr_exp = exp(centered_pred_ifr_all))]
  
  df0<-df0_tmp[location_id %in% lv3, .(location_id, wave, ifr_mean, centered_pred_ifr_all, centered_pred_ifr_exp, inf_mean, centered_pred_infections_all, centered_pred_infections_exp)]
  write.csv(df0, paste0(outdir,'stage_1_residuals_wave0.csv'), row.names=F)
  
  df4<-df4_tmp[location_id %in% lv3, .(location_id, wave, ifr_mean, centered_pred_ifr_all, centered_pred_ifr_exp, inf_mean, centered_pred_infections_all, centered_pred_infections_exp)]
  write.csv(df4,  paste0(outdir,'stage_1_residuals_wave4.csv'), row.names=F)
  
 #data for decomposition analysis 
  filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment), pattern="unadjusted_and_stage1_infections_IFR_new_pneumonia_",full.names=TRUE)
  d<-fread(filenames[1])
  id<-d[period==0 & location_id %in% lv3, location_id]
  dat<-data.frame(location_id=rep(NA, 177))
  dat$location_id<-id
  var_names<-c('period_ifr', 'period_ifr_age_adjusted', 'period_cum_inf_pop', 'period_cum_inf_pop_adj')
  for(v in var_names){
    filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment), pattern="unadjusted_and_stage1_infections_IFR_new_pneumonia_",full.names=TRUE)
    get.list<-function(x,vars){
      a<-fread(file=x,header=T)[period==0,..vars]
      val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/unadjusted_and_stage1_infections_IFR_new_pneumonia_', "", x))
      val<-gsub('.csv', "", val)
      setnames(a, v,val)
      a
    }
    
    vars<-c('location_id',v)
    datalist = lapply(filenames, function(x) get.list(x,vars))
    o_draws<-c()
    setDT(o_draws)
    o_draws$num<-0:99
    o_draws[, draw:=paste0('draw_',num)]
    q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('location_id'))}, datalist)
    setDT(q)
    q<-setcolorder(q, c('location_id', o_draws$draw))
    
    n<-names(q[,2:101])
    q$var_mean <- rowMeans(q[, ..n])
    i<-q[,-c(2:101)]
    setnames(i, 'var_mean', v)
    i<-i[location_id %in% lv3,]
    dat<-merge(dat, i, by='location_id')
  }
  
  #merge with stage1 residuals:
  data<-merge(dat, df0, by='location_id')
  write.csv(data,paste0(outdir,'stage_0_1_residuals_wave0.csv'))
  
  #now all other locs
  filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment), pattern="unadjusted_and_stage1_infections_IFR_new_pneumonia_",full.names=TRUE)
  d<-fread(filenames[1])
  id<-d[period==4 & location_id %in% lv3, location_id]
  dat<-data.frame(location_id=rep(NA, 177))
  dat$location_id<-id
  var_names<-c('period_ifr', 'period_ifr_age_adjusted', 'period_cum_inf_pop', 'period_cum_inf_pop_adj')
  for(v in var_names){
    filenames=list.files(path=paste0(root_dir,seir_version,'/', experiment), pattern="unadjusted_and_stage1_infections_IFR_new_pneumonia_",full.names=TRUE)
    get.list<-function(x,vars){
      a<-fread(file=x,header=T)[period==4,..vars]
      val<-gsub(paste0(root_dir,seir_version,'/', experiment,'/fig2_data_', "", x))
      val<-gsub('.csv', "", val)
      setnames(a, v,val)
      a
    }
    
    vars<-c('location_id',v)
    datalist = lapply(filenames, function(x) get.list(x,vars))
    o_draws<-c()
    setDT(o_draws)
    o_draws$num<-0:99
    o_draws[, draw:=paste0('draw_',num)]
    q<-Reduce(function(x,y){merge(x,y,all = TRUE, by=c('location_id'))}, datalist)
    setDT(q)
    q<-setcolorder(q, c('location_id', o_draws$draw))
    
    n<-names(q[,2:101])
    q$var_mean <- rowMeans(q[, ..n])
    i<-q[,-c(2:101)]
    setnames(i, 'var_mean', v)
    i<-i[location_id %in% lv3,]
    dat<-merge(dat, i, by='location_id')
  }
  
  #merge with stage1 residuals:
  data<-merge(dat, df4, by='location_id')
  write.csv(data,paste0(outdir,'stage_0_1_residuals_wave4.csv'))
  