library(ggplot2)
library(data.table)
library(ggpubr)
library(scales)
library(cowplot)
library(ggsci)
library(gridExtra)
in_dir<-'FILEPATH'
data<-fread(paste0(in_dir,'/stage2_results.csv'))
data[covar=="v2pariglef_gov_avg",`:=`(var_mean=var_mean*-1, var_upper=var_lower*-1, var_lower=var_upper*-1)]
data_m<-data[dep_group %in% c('ifr_all','infections_all'),]
data_m$wave<-ifelse(data_m$wave==1, "Wave 1", 
                    ifelse(data_m$wave==2, "Wave 2", 
                           ifelse(data_m$wave==3, "Wave 3",
                                  ifelse(data_m$wave==4, " (Jan 2020 to Oct 2020)", " (Jan 2020 to Sept 2021)"))))
data_m$wave <- factor(data_m$wave,levels = c("Wave 1", "Wave 2", "Wave 3", " (Jan 2020 to Oct 2020)"," (Jan 2020 to Sept 2021)"))

data_m$c<-data_m$covar
data_m<-data_m[!covar %in% c('federal_ind','mobility_min', 'mean_fraction_mandate', 'testing_mean','v2x_mpi','govt_trust_corrupt_pca', 'v2xpa_illiberal_gov',
                             'v2xpa_popul_gov','lib_cons_avg', 'v2pariglef_gov_avg', 'bureaucracy_corrupt', 'trust_govt_cpi', 'govt_trust', 'trust_govt_gallup'),]
data_m$group<-ifelse(data_m$covar %in% c('ghsi_0_overall','jee_readyscore_overall','ghsi_5_commit','ghsi_2_detect','ghsi_4_health_sector','ghsi_1_prevent','ghsi_3_respond',
                                         'ghsi_6_risk','jee_readyscore_detect','jee_readyscore_respond','jee_readyscore_prevent','jee_readyscore_other'), 'Pandemic preparedness indices',
                     ifelse(data_m$covar %in% c('HAQI_mean','uhc_19','ncd_coverage','cmnn_coverage','BedsPerCapita', 'GHESpc', "THEpc"), 'Healthcare capacity indicators',
                            ifelse(data_m$covar %in% c('bureaucracy_corrupt','trust_govt_cpi','govt_corrupt_pca','v2x_polyarchy','v2xpa_illiberal_gov','v2xpa_popul_gov',
                                                       'gov_effect','state_fragility','lib_cons_avg','v2pariglef_gov_avg','electoral_pop'), "Governance indicators", "Social indicators")))

data_m$covar<-factor(data_m$covar, levels=c('BedsPerCapita',
                                            'uhc_19',
                                            'ncd_coverage',
                                            'cmnn_coverage',
                                            'HAQI_mean',
                                            "THEpc",
                                            'GHESpc',
                                            'v2x_polyarchy',
                                            'gov_effect',
                                            'electoral_pop',
                                            'govt_corrupt_pca',
                                            'state_fragility',
                                            'gini',
                                            'govt_trust_pca',
                                            'interpersonal_trust',
                                            'trust_science',
                                            'ghsi_0_overall',
                                            'ghsi_1_prevent',
                                            'ghsi_2_detect',
                                            'ghsi_3_respond',
                                            'ghsi_4_health_sector',
                                            'ghsi_5_commit',
                                            'ghsi_6_risk',
                                            'jee_readyscore_overall',
                                            'jee_readyscore_prevent',
                                            'jee_readyscore_detect',
                                            'jee_readyscore_respond',
                                            'jee_readyscore_other'),
                     labels=c( "Hospital beds per capita",
                               "Universal health coverage",
                               'Universal health coverage - NCDs',
                               'Universal health coverage - CMNNs',
                               "Healthcare access and quality index",
                               "Health spending per capita",
                               "Government health spending per capita",
                               "Electoral democracy index",
                               "Government effectiveness",
                               "Electoral populism",
                               "Government corruption - PCA",
                               "State fragility",
                               "Income inequality", 
                               "Trust in government - PCA", 
                               "Interpersonal trust",
                               "Trust in science",
                               "Global health security index overall score",
                               "Global health security index prevent score",
                               "Global health security index detect score",
                               "Global health security index respond score",
                               "Global health security index health sector score",
                               "Global health security index compliance with international norms score",
                               "Global health security index risk environment score",
                               "Joint external evaluation overall score", 
                               "Joint external evaluation prevent score",
                               "Joint external evaluation detect Score",
                               "Joint external evaluation respond score",
                               "Joint external evaluation other score"))

data_m$outcome<-ifelse(data_m$dep_group=="ifr_all", "Infection fatality ratio", "Infections per capita")
data_m$label<-paste0(data_m$covar,data_m$wave)
data_m$label<-factor(data_m$label, 
                     levels=rev(c( "Hospital beds per capita (Jan 2020 to Sept 2021)",
                                   "Hospital beds per capita (Jan 2020 to Oct 2020)",
                                   "Universal health coverage (Jan 2020 to Sept 2021)",
                                   "Universal health coverage (Jan 2020 to Oct 2020)",
                                   "Universal health coverage - NCDs (Jan 2020 to Sept 2021)",
                                   "Universal health coverage - NCDs (Jan 2020 to Oct 2020)",
                                   "Universal health coverage - CMNNs (Jan 2020 to Sept 2021)",
                                   "Universal health coverage - CMNNs (Jan 2020 to Oct 2020)",
                                   "Healthcare access and quality index (Jan 2020 to Sept 2021)",
                                   "Healthcare access and quality index (Jan 2020 to Oct 2020)",
                                   "Health spending per capita (Jan 2020 to Sept 2021)",
                                   "Health spending per capita (Jan 2020 to Oct 2020)",
                                   "Government health spending per capita (Jan 2020 to Sept 2021)",
                                   "Government health spending per capita (Jan 2020 to Oct 2020)",
                                   "Electoral democracy index (Jan 2020 to Sept 2021)",
                                   "Electoral democracy index (Jan 2020 to Oct 2020)",
                                   "Government effectiveness (Jan 2020 to Sept 2021)",
                                   "Government effectiveness (Jan 2020 to Oct 2020)",
                                   "Electoral populism (Jan 2020 to Sept 2021)",
                                   "Electoral populism (Jan 2020 to Oct 2020)",
                                   "Government corruption - PCA (Jan 2020 to Sept 2021)",
                                   "Government corruption - PCA (Jan 2020 to Oct 2020)",
                                   "State fragility (Jan 2020 to Sept 2021)",
                                   "State fragility (Jan 2020 to Oct 2020)",
                                   "Income inequality (Jan 2020 to Sept 2021)",
                                   "Income inequality (Jan 2020 to Oct 2020)",
                                   "Trust in government - PCA (Jan 2020 to Sept 2021)",
                                   "Trust in government - PCA (Jan 2020 to Oct 2020)",
                                   "Interpersonal trust (Jan 2020 to Sept 2021)",
                                   "Interpersonal trust (Jan 2020 to Oct 2020)",
                                   "Trust in science (Jan 2020 to Sept 2021)",
                                   "Trust in science (Jan 2020 to Oct 2020)",
                                   "Global health security index overall score (Jan 2020 to Sept 2021)",
                                   "Global health security index overall score (Jan 2020 to Oct 2020)",
                                   "Global health security index prevent score (Jan 2020 to Sept 2021)",
                                   "Global health security index prevent score (Jan 2020 to Oct 2020)",
                                   "Global health security index detect score (Jan 2020 to Sept 2021)",
                                   "Global health security index detect score (Jan 2020 to Oct 2020)",
                                   "Global health security index respond score (Jan 2020 to Sept 2021)",
                                   "Global health security index respond score (Jan 2020 to Oct 2020)",
                                   "Global health security index health sector score (Jan 2020 to Sept 2021)",
                                   "Global health security index health sector score (Jan 2020 to Oct 2020)",
                                   "Global health security index compliance with international norms score (Jan 2020 to Sept 2021)",
                                   "Global health security index compliance with international norms score (Jan 2020 to Oct 2020)",
                                   "Global health security index risk environment score (Jan 2020 to Sept 2021)",
                                   "Global health security index risk environment score (Jan 2020 to Oct 2020)",
                                   "Joint external evaluation overall score (Jan 2020 to Sept 2021)", 
                                   "Joint external evaluation overall score (Jan 2020 to Oct 2020)", 
                                   "Joint external evaluation prevent score (Jan 2020 to Sept 2021)",
                                   "Joint external evaluation prevent score (Jan 2020 to Oct 2020)",
                                   "Joint external evaluation detect Score (Jan 2020 to Sept 2021)",
                                   "Joint external evaluation detect Score (Jan 2020 to Oct 2020)",
                                   "Joint external evaluation respond score (Jan 2020 to Sept 2021)",
                                   "Joint external evaluation respond score (Jan 2020 to Oct 2020)",
                                   "Joint external evaluation other score (Jan 2020 to Sept 2021)",
                                   "Joint external evaluation other score (Jan 2020 to Oct 2020)")),
                     ordered = TRUE)


colors<-c( "#CD1D2C", "mediumseagreen")
data_m$`Statistically significant at 95% level with Bonferonni correction`<-ifelse(data_m$p.value<0.0018,"Yes","No")
data_m$outcome <- factor(data_m$outcome, levels=c('Infections per capita','Infection fatality ratio'))
data_m$group<-factor(data_m$group, levels=c('Pandemic preparedness indices', 'Healthcare capacity indicators', 
                                            'Governance indicators','Social indicators'))


plot<-ggplot(data=data_m, aes(y=label, x=var_mean, xmin=var_lower, xmax=var_upper, col=`Statistically significant at 95% level with Bonferonni correction`))+ 
  #this adds the effect sizes to the plot
  geom_point()+ 
  #adds the CIs
  geom_errorbarh(height=0)+
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)+
  scale_color_manual(name = "Statistically significant at 95% level with Bonferonni correction",values =colors, drop=FALSE)+
  scale_x_continuous(limits=c(-1.15,0.6))+theme(aspect.ratio = 1)+
  facet_grid(group~outcome, scale='free', space='free', switch='y')+theme_bw()+scale_y_discrete(position = "right")+theme(legend.position = "bottom")+
  labs(x='Change in outcomes associated with a one standard deviation increase',  y='')+ggtitle('Figure 3: Associations between key preparedness, capacity, governance, and social indicators and infections and infection fatality ratio')+ theme(
    strip.text.y.left = element_text(angle = 0),
    strip.background = element_rect(
      color="white", fill='white',linetype="solid"
    )
  )
outdir<-'FILEPATH'
ggsave(paste0(outdir,"/FIGURE2.pdf", dpi="print", device="pdf",plot=plot, height=25, width=40, units="cm"))