##############################################################
#######Launches data prep and models for each experiment #####
##############################################################

library(data.table)
library(readxl)
library(tidyverse)
library(dplyr)
require(lubridate)
library(MASS)
detach('package:plyr',unload=T)
tmpinstall <- system("mktemp -d --tmpdir=/tmp", intern = TRUE)
.libPaths(c(tmpinstall, .libPaths()))
devtools::install_github("ihmeuw/ihme.covid", upgrade = "never")
invisible(loadNamespace("ihme.covid"))

in_dir<-'FILEPATH'
outdir<-'FILEPATH'

rename <- dplyr::rename
select <- dplyr::select


config<-fread(paste0(in_dir,'/config.csv'))
i<-1 #row of the experiment of interest
seir_version<-toString(config[i, 'seir_version'])
experiment<-toString(config[i, 'experiment'])
RR <- toString(config[i, 'RR'])
end_date<-toString(config[i, 'end_date'])
end_date<-format(as.Date(end_date, format='%m/%d/%Y'), format='%Y-%m-%d')

ref_dir_draw <- paste0(in_dir, "/reference/output_draws/")
ref_dir <- paste0(in_dir, seir_version, "/reference/output_summaries/")
df_locmeta <- read.csv(paste0(in_dir,"/hierarchy.csv"))

hier<-fread(paste(in_dir,"hierarchy_metadata.csv"))
dir.create(paste0(outdir, seir_version, "/",experiment))
system(paste0('chmod -R 775 /',paste0(outdir, seir_version,"/",experiment)))

draws<-c()
setDT(draws)
draws$num<-0:99 #100 draws
draws[, draw:=paste0('draw_',num)]
#stage 0 & 1 - data prep and adjustments for seasonality and age
for(draw in draws$draw){
    # Set up job
    command = paste0(
      "qsub -l m_mem_free=20G,archive=TRUE,fthread=26,h_rt=02:00:00",
      " -N ", draw, " -q d.q -cwd -P project -e FILEPATH -o FILEPATH -terse FILEPATH.sh ",
      "-i FILEPATH.img -s ",
      in_dir,"/02_launch_draw_prep.R",
      " --draw ",draw, " --seir-version ", seir_version, " --experiment ", experiment, " --RR ", RR, " --end_date ",end_date)

    # Submit job
    
    job_id=system(command, intern=TRUE)
}

#wait for all 100 data prep jobs to finish
Sys.sleep(180)

draws<-c()
setDT(draws)
draws$num<-0:99
draws[, draw:=paste0('draw_',num)]
#stage 1 & 2 combined
for(draw in draws$draw){
  # Set up job
  command = paste0(
    "qsub -l m_mem_free=2G,archive=TRUE,fthread=2,h_rt=01:00:00",
    " -N ", draw, " -q d.q -cwd -P project -e FILEPATH -o FILEPATH -terse FILEPATH.sh ",
    "-i FILEPATH.img -s ",
    in_dir,"03_stage1_stage2_combined.R",
    " --draw ",draw, " --seir-version ", seir_version, " --experiment ", experiment)
  
  # Submit job
  job_id=system(command, intern=TRUE)
}

