dat_path <- "FILEPATH"
in_dir<-"FILEPATH"
df_draw1 <- read.csv(file.path(dat_path, "df0_unscaled_fig4_draw_0.csv"), as.is = TRUE)

df_covs <- read.csv(paste0(in_dir,"/unscaled_covariates.csv"), as.is = TRUE)

hier <- readRDS(paste0(in_dir,"/20210518_hier.RDS"))
library(dplyr)

#get only high-middle and high income
wb2020<-readstata13::read.dta13(paste0(in_dir,'wb_historical_incgrps_20.dta'))
hmic_locids<-wb2020[INC_GROUP %in% c("H","UM"),ISO3_RC]
ggplot_info_new <- function(
  data, xvar, yvar, 
  main_title = "", xlab = "", ylab = "", log10x = FALSE, log10y = FALSE, 
  colorvar = "super_region_name", include_color_legend = FALSE, color_legend_title = NA, color_alpha = 1.0,
  sizevar = "population", size_range = c(1, 10), show_lm = TRUE,
  label_n_largest = NA, label_size = 3, custom_addition = NA, add_meanlines = TRUE) {
  if (FALSE) {
    data <- df1
    xvar = "govt_trust_pca"; yvar = "mobility_min"
    main_title = "tmp_main_title"; xlab = "tmp_x_label"; ylab = "tmp_ylabel"
    log10x = FALSE
    log10y = FALSE
    colorvar = "super_region_name"; color_alpha <- 1.0
    include_color_legend = TRUE; color_legend_title = "GBD super-region"
    sizevar = "population"; size_range = c(1, 10)
    custom_addition <- NA; label_n_largest <- 30; label_size <- 3
    show_lm <- TRUE; add_meanlines <- TRUE
  }
  require(ggplot2)
  require(ggsci)
  require(ggrepel)
  dat_tmp <- as.data.frame(data)
  dat_tmp[, c("x_var", "y_var")] <- dat_tmp[, c(xvar, yvar)]
  if(yvar=='vax_coverage_max'){
    dat_tmp<-dat_tmp[dat_tmp$ihme_loc_id %in% hmic_locids,]
    dat_tmp2 <- dat_tmp %>% filter(!is.na(x_var) & !is.na(y_var))
    lm<-lm(dat_tmp2$y_var~dat_tmp2$x_var)
    b0<-lm$coefficients[1]
    b1<-lm$coefficients[2]
    se<-summary(lm)$coefficients[2,2]
    
    y.fit <- b1 * dat_tmp2[,'x_var'] + b0
    b0_l<-b0-1.96*se
    b0_u<-b0+1.96*se
    dat_tmp2$y.fit<-y.fit[!is.na(y.fit)]
    pval<-format(round(summary(lm)$coefficients[2,4], 4), scientific=FALSE)
    pval<-ifelse(pval==0, 0.0001, pval )
    pval<-format(pval, scientific=FALSE)
    pval_lan<-ifelse(pval==0.0001, "p-value<", "p-value: ")
    annotations1 <- data.frame(
      xpos = c(Inf), ypos =  c(-Inf),
      annotateText =paste0(pval_lan,pval),
      hjustvar = c(1), 
      vjustvar = c(-1)
    )
    
    
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y.fit + 1.96 * se)
    dat_tmp2$slope.upper<-slope.upper[!is.na(slope.upper)]
    slope.lower <- suppressWarnings(y.fit - 1.96 * se)
    dat_tmp2$slope.lower<-slope.lower[!is.na(slope.lower)]
    dat_tmp2$y.fit<-y.fit[!is.na(y.fit)]
    
    lanc_col<-pal_lancet("lanonc")(7)
    lanc_col_sub<-lanc_col[-5]
    
    p <- ggplot(data=dat_tmp2) + 
      aes(x=x_var,  y=y_var) +
      theme_bw() +
      guides(size = FALSE) +
      labs(x = xlab, y = ylab, title = main_title) + 
      geom_point(alpha = color_alpha, aes(color=factor(super_region_name), size=population)) +
      scale_size_continuous(range = size_range) + 
      scale_y_continuous(ylab,c(0.2,0.4,0.6,0.8,1), labels=c('20%','40%','60%','80%','100%'), limits=c(0,1))+
      #ggsci::scale_color_lancet()+ #S Asia is missing so need to move colors down by one
      scale_color_manual(values=lanc_col_sub)+
      labs(caption=annotations1$annotateText)
    #geom_text(data = annotations1, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText))
    
  } else {
      dat_tmp2 <- dat_tmp %>% filter(!is.na(x_var) & !is.na(y_var))
      lm<-lm(dat_tmp2$y_var~dat_tmp2$x_var)
      b0<-lm$coefficients[1]
      b1<-lm$coefficients[2]
      se<-summary(lm)$coefficients[2,2]
      
      y.fit <- b1 * dat_tmp2[,'x_var'] + b0
      b0_l<-b0-1.96*se
      b0_u<-b0+1.96*se
      dat_tmp2$y.fit<-y.fit[!is.na(y.fit)]
      pval<-format(round(summary(lm)$coefficients[2,4], 4), scientific=FALSE)
      pval<-ifelse(pval==0, 0.0001, pval )
      pval<-format(pval, scientific=FALSE)
      pval_lan<-ifelse(pval==0.0001, "p-value<", "p-value: ")
      annotations1 <- data.frame(
        xpos = c(Inf), ypos =  c(-Inf),
        annotateText =paste0(pval_lan,pval),
        hjustvar = c(1), 
        vjustvar = c(-1)
      )
      
      
      # Warnings of mismatched lengths are suppressed
      slope.upper <- suppressWarnings(y.fit + 1.96 * se)
      dat_tmp2$slope.upper<-slope.upper[!is.na(slope.upper)]
      slope.lower <- suppressWarnings(y.fit - 1.96 * se)
      dat_tmp2$slope.lower<-slope.lower[!is.na(slope.lower)]
      dat_tmp2$y.fit<-y.fit[!is.na(y.fit)]
      
      p <- ggplot(data=dat_tmp2) + 
        aes(x=x_var,  y=y_var) +
        theme_bw() +
        guides(size = FALSE) +
        labs(x = xlab, y = ylab, title = main_title) + 
        geom_point(alpha = color_alpha, aes(color=factor(super_region_name), size=population)) +
        scale_size_continuous(range = size_range) + 
        scale_y_continuous(ylab,c(0.2,0.4,0.6,0.8,1), labels=c('20%','40%','60%','80%','100%'), limits=c(0,1))+
        ggsci::scale_color_lancet()+
        labs(caption=annotations1$annotateText)
      #geom_text(data = annotations1, aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar, label = annotateText))
      
  }
  if (show_lm) {
    p<-p+geom_abline(aes(slope=b1, intercept=b0), col='black')+
      geom_abline(aes(slope=b1, intercept=b0_l), col='black', lty=2)+
      geom_abline(aes(slope=b1, intercept=b0_u), col='black', lty=2)
  }
  
  p <- p + labs(color = "GBD super-region")
  
  if (!is.na(custom_addition)) p <- p + custom_addition
  
  if (!is.na(label_n_largest)) {
    df_plot_labels <- as.data.frame(dat_tmp2) %>%
      arrange(desc(population)) %>%
      .[1:label_n_largest, ] %>%
      mutate(sizetmp = 100)
    
    p <- p + ggrepel::geom_text_repel(
      data = df_plot_labels, aes(
        x = x_var, y = y_var,
        label = ihme_loc_id), size = label_size, color = "black") # show.legend = FALSE)
  }
  
  if (add_meanlines) {
    xmean <- mean(dat_tmp2$x_var, na.rm = TRUE)
    ymean <- mean(dat_tmp2$y_var, na.rm = TRUE)
    p <- p + geom_hline(yintercept = ymean, col = "darkgray", alpha = 0.9)
    p <- p + geom_vline(xintercept = xmean, col = "darkgray", alpha = 0.9)
  }
  
  
  return(p)
}
df_draw1 <- read.csv(file.path(dat_path, "df0_unscaled_fig4_draw_0.csv"), as.is = TRUE)

df_covs <- read.csv(paste0(in_dir,"unscaled_covariates.csv"), as.is = TRUE)

hier <- readRDS(paste0(in_dir,"/home/j/temp/reed/misc/20210518_hier.RDS"))
library(dplyr)
df1 <- df_covs %>%
  left_join(as.data.frame(hier)[, c("location_id",'level', "super_region_name", "ihme_loc_id")]) %>%
  # left_join(as.data.frame(df_draw1[, c("location_id", "population")]))
  left_join(as.data.frame(df_draw1), by = "location_id")
#we want mobility to be a positive change
df1$mobility_min <- (df1$mobility_min)/-100
df1$govt_trust_pca<-df1$govt_trust_pca*1
df1<-df1[df1$location_id!=189,]
df1<-df1[df1$level==3,]
setDT(df1)

summary(df1$vax_coverage_max)
df1[vax_coverage_max>1, vax_coverage_max:=1]

df_var_labels <- as.data.frame(do.call("rbind", list(
  c("govt_trust_pca", "Trust in government"),
  c("interpersonal_trust", "Interpersonal trust"),
  # c("uhc_19", "Universal healthcare index"),
  c("govt_corrupt_pca", "Government corruption"),
  c("mobility_min", "Reduction in mobility from baseline"),
  c("vax_coverage_max", "Vaccine coverage")
)), stringsAsFactors = FALSE)

df_plot_data <- expand.grid(
  colvars = c("govt_trust_pca", "interpersonal_trust",  "govt_corrupt_pca"), 
  # rowvars = c("uhc_19", "gov_effect", "state_fragility"), stringsAsFactors = FALSE ) %>%
  rowvars = c( "mobility_min","vax_coverage_max"), stringsAsFactors = FALSE ) %>%
  left_join(df_var_labels %>% rename(col_label = V2), by = c("colvars" = "V1")) %>%
  left_join(df_var_labels %>% rename(row_label = V2), by = c("rowvars" = "V1"))

plot_dat <- lapply(1:nrow(df_plot_data), function(i) {
  print(paste(df_plot_data$col_label, df_plot_data$row_label))
  ggplot_info_new(
    data = df1, 
    xvar = df_plot_data[i, "colvars"],
    yvar = df_plot_data[i, "rowvars"],
    xlab = df_plot_data[i, "col_label"],
    ylab = df_plot_data[i, "row_label"], 
    color_alpha = 0.8,
    label_n_largest = 30,
    label_size = 3
  )
})

library(ggpubr)
a<-do.call("ggarrange", c(plot_dat, ncol=3, nrow=2, common.legend=TRUE, legend='bottom'))
a<-annotate_figure(
  a,
  fig.lab = "Figure 4: Association between trust and vaccine coverage and reduction in mobility",
  fig.lab.pos = c("top.left")
)
outpath<-'FILEPATH'
ggsave(paste0(outpath,"/FIGURE4.pdf"), dpi="print", device="pdf",plot=a, height=20, width=30, units="cm")
