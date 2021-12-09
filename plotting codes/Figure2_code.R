library(data.table)
library(ggplot2)
library(tidyr)
library(ggsci)
library(ggrepel)

in_dir<-'FILEPATH'
options(scipen = 100, digits = 4)
hier <- readRDS(paste0(in_dir,"/20210518_hier.RDS")

#original:
df0_tmp<-fread(paste0(in_dir,'/stage_1_residuals_wave0.csv'))
df0_tmp[, `:=` (xvar = centered_pred_infections_exp, yvar = centered_pred_ifr_exp)]
df0_tmp<-merge(df0_tmp, hier, by='location_id')
df4_tmp<-fread(paste0(in_dir,'stage_1_residuals_wave4.csv')
df4_tmp[, `:=` (xvar = centered_pred_infections_exp, yvar = centered_pred_ifr_exp)]
df4_tmp<-merge(df4_tmp, hier, by='location_id')
#merge in period deaths - no variability so use draw 1
filenames=list.files(path=in_dir, pattern="unadjusted_and_stage1_infections_IFR_new_pneumonia_",full.names=TRUE)
a<-fread(file=filenames[1],header=T)
period_deaths0<-a[period==0, .(location_id, period_deaths)]
period_deaths4<-a[period==4, .(location_id, period_deaths)]

df0_tmp<-merge(df0_tmp, period_deaths0, by='location_id', all.x=T)
df4_tmp<-merge(df4_tmp, period_deaths4, by='location_id', all.x=T)

df_plot <- rbind(df0_tmp, df4_tmp, fill=T) %>%
  dplyr::mutate(
    wave_label = NA, 
    wave_label = ifelse(wave == 0, "Jan. 1, 2020 to Sept. 30, 2021", wave_label),
    wave_label = ifelse(wave == 4, "Jan. 1, 2020 to Oct. 15, 2020", wave_label))

mean_ifr <- mean(df_plot$yvar)
mean_infections <- mean(df_plot$xvar)
df_plot_means <- df_plot %>%
  group_by(wave_label) %>%
  dplyr::summarize(
    mean_ifr = mean(yvar),
    mean_infections = mean(xvar) ) %>%
  as.data.frame(.)

plot_base <- ggplot(data=df_plot) + 
  aes(x=xvar,  y=yvar, col=super_region_name) +
  theme_bw() +
  scale_x_log10(labels = function(x) sprintf("%g", x)) +
  scale_y_log10(labels = function(x) sprintf("%g", x)) +
  facet_wrap(~wave_label, scales = "free") +
  theme(strip.background = element_rect(fill="#00468BFF")) +
  theme(strip.text = element_text(colour = 'white'))+  theme(aspect.ratio = 1) 

plot1 <- plot_base +
  labs(
    x = "Standardised infections per capita", 
    y = "Standardised infection fatality ratio",
    # title = "Infections/population and IFR by location",
    title = "Figure 2: Standardised infections per capita and standardised infection fatality ratios",
    color = "GBD super-region",
    size= "Cumulative Deaths") +
  #guides(size = FALSE) + 
  
  geom_hline(data = filter(df_plot, wave_label == df_plot_means[1, "wave_label"]), 
             aes(yintercept=df_plot_means[1, "mean_ifr"]), col='blue', lty=1, alpha = 0.3) +
  geom_vline(data = filter(df_plot, wave_label == df_plot_means[1, "wave_label"]), 
             aes(xintercept=df_plot_means[1, "mean_infections"]), col='blue', lty=1, alpha = 0.3) +
  
  geom_hline(data = filter(df_plot, wave_label == df_plot_means[2, "wave_label"]), 
             aes(yintercept=df_plot_means[2, "mean_ifr"]), col='blue', lty=1, alpha = 0.3) +
  geom_vline(data = filter(df_plot, wave_label == df_plot_means[2, "wave_label"]), 
             aes(xintercept=df_plot_means[2, "mean_infections"]), col='blue', lty=1, alpha = 0.3)

df_pop_in <- read.csv(paste0(in_dir,"/all_populations.csv"))
df_plot<-merge(df_plot, df_pop_in[df_pop_in$age_group_id==22 & df_pop_in$sex_id==3,c('location_id', 'population')],by='location_id')
df_plot_labels <- df_plot %>%
  arrange(desc(population))
plot_locs<-df_plot_labels[1:60,]
plot_ids<-unique(plot_locs$location_id)
df_plot$label<-ifelse(df_plot$location_id %in% plot_ids, 1, 0)


plot1<-plot1 + 
  # geom_point(aes(size = loc_pop_infections_all), alpha = 0.4) +
  geom_point(aes(size = period_deaths), alpha = 0.4) +
  scale_size_continuous(range=c(0.5,15))+#breaks=c(0,5000,50000,500000, 1000000), labels=c('5,000', '5,000','50,000','500,000', "1,000,000")) +
  scale_color_lancet() 

setDT(df_plot)
p<-plot1 +
  geom_text_repel(data = df_plot[wave_label == df_plot_means[1, "wave_label"] & label==1,],
                  aes(x = xvar, y = yvar, label = ihme_loc_id),color = "black", show.legend = FALSE) +
  geom_text_repel(data = df_plot[wave_label == df_plot_means[2, "wave_label"] & label==1,],
                  aes(x = xvar, y = yvar, label = ihme_loc_id), color = "black", show.legend = FALSE)
# geom_point(alpha = 0.5)
ggsave("~/FIGURE2.pdf", dpi="print", device="pdf",plot=p, height=28,width=40, units="cm")
