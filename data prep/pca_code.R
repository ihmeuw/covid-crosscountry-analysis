library(missMDA)
library(dplyr)

in_dir<-'FILEPATH'
out_dir<-'FILEPATH'
df_in <- read.csv(paste0(in_dir,"/unscaled_covariates.csv"))
df_locmeta <- read.csv(paste0(in_dir,"/20210517_df_locmeta.csv"))

df <- df_in %>%
  select(location_id, trust_govt_cpi, trust_govt_gallup, govt_trust, bureaucracy_corrupt) %>%
  left_join(df_locmeta[, c("location_id", "location_name")]) %>%
  mutate(
    cpi_scaled = scale(trust_govt_cpi),
    gallup_scaled = scale(trust_govt_gallup),
    govt_trust_scaled = scale(govt_trust),
    bureaucracy_corrupt_scaled = scale(bureaucracy_corrupt)
  )

get_pca_component <- function(dat, pca_vars, component_num, ncp = NA, n_imputations = 1) {
  if (FALSE) {
    # dat <- df
    # pca_vars <- c("cpi_scaled", "gallup_scaled", "govt_trust_scaled", "bureaucracy_corrupt_scaled")
    # # pca_vars <- c("cpi_scaled", "bureaucracy_corrupt_scaled")
    # ncp = 1
    # component_num = 1
    # n_imputations = 10
    ##
    dat = df2
    ncp = 1
    component_num = 1
    pca_vars = c("gallup_scaled", "govt_trust_scaled")
    n_imputations = 100
  }

    if (0) {
      lapply(pca_vars, function(x) {
        cat(x, "\n")
        table(is.na(df[, x]))
      })
    }

  require(dplyr)

  # single imputation
  dat_tmp1 <- as.data.frame(dat)[, pca_vars]

  if (is.na(ncp)) {
    result1 <- missMDA::imputePCA(X = dat_tmp1, scale = TRUE)
    result2 <- FactoMineR::PCA(result1$completeObs, scale.unit = TRUE, graph = FALSE)
  } else {
    result1 <- missMDA::imputePCA(X = dat_tmp1, scale = TRUE, ncp = ncp)
    result2 <- FactoMineR::PCA(result1$completeObs, scale.unit = TRUE, ncp = ncp, graph = FALSE)
  }

  obs_complete <- result1$completeObs
  colnames(obs_complete) <- paste0(colnames(result1$completeObs), "2")

  dat_tmp2 <- cbind(dat, obs_complete) %>%
    mutate(pca_result = result2$ind$coord[, component_num])




  ## multiple imputation
  if (n_imputations > 1) {

    if (is.na(ncp)) {
      tmp <- missMDA::MIPCA(X = dat_tmp1, scale = TRUE, nboot = n_imputations)
    } else {
      tmp <- missMDA::MIPCA(X = dat_tmp1, scale = TRUE, ncp = ncp, nboot = n_imputations)
      # tmp <- missMDA::MIPCA(
      #   X = dat_tmp1 %>% filter(!(is.na(gallup_scaled) & is.na(govt_trust_scaled))),
      #   scale = TRUE, ncp = ncp, nboot = n_imputations
      # )
    }

    tmp2 <- tmp$res.MI

    pca_mi_result <- lapply(1:n_imputations, function(i) {
      if (FALSE) {
        i <- 1
      }
      tmp3 <- tmp2[[i]]

      if (is.na(ncp)) {
        result2_tmp <- FactoMineR::PCA(tmp3, scale.unit = TRUE, graph = FALSE)
      } else {
        result2_tmp <- FactoMineR::PCA(tmp3, scale.unit = TRUE, ncp = ncp, graph = FALSE)
      }
      df_out <- data.frame(tmpvar = result2_tmp$ind$coord[, component_num])
      colnames(df_out) <- paste0("draw_", i)
      return(df_out)
    })

    df_pca_mi_result <- do.call("cbind", pca_mi_result)

  }


  out1 <- list(
    data = dat_tmp2,
    result_imputation = result1,
    result_pca = result2,
    pca_vars = pca_vars
  )

  if (n_imputations > 1) {
    out2 <- append(out1, list(
      pca_mi_result = pca_mi_result,
      df_pca_mi_result = df_pca_mi_result,
      mipca_function_output = tmp
    ))
  } else {
    out2 <- out1
  }
  return(out2)
}



df1 <- df %>%
  filter(!(is.na(cpi_scaled) & is.na(gallup_scaled) & is.na(govt_trust_scaled) & is.na(bureaucracy_corrupt_scaled)))

df2 <- df %>%
  filter(!(is.na(gallup_scaled) & is.na(govt_trust_scaled)) )

df3 <- df %>%
  filter(!(is.na(cpi_scaled) & is.na(bureaucracy_corrupt_scaled)) )


x1 <- get_pca_component(
  dat = df1, ncp = 3, component_num = 1,
  pca_vars = c("cpi_scaled", "gallup_scaled", "govt_trust_scaled", "bureaucracy_corrupt_scaled"),
  n_imputations = 100
)

x2 <- get_pca_component(
  dat = df2, ncp = 1, component_num = 1,
  pca_vars = c("gallup_scaled", "govt_trust_scaled"),
  n_imputations = 100
)

x3 <- get_pca_component(
  dat = df3, ncp = 1, component_num = 1,
  pca_vars = c("cpi_scaled", "bureaucracy_corrupt_scaled"),
  n_imputations = 100
)

plot_pca <- function(x) {
  if (FALSE) {
    x <- x1
  }
  vars_tmp <- x$pca_vars
  pairs(x$data[, c(vars_tmp, "pca_result")])
  pairs(x$data[, c(paste0(vars_tmp, "2"), "pca_result")])
}

plot_pca(x1)
plot_pca(x2)
plot_pca(x3)

if (0) {
  saveRDS(x1, paste0(in_dir,"/pca_multiple_imputation_4vars.RDS"))
  saveRDS(x2, paste0(in_dir,"/pca_multiple_imputation_2vars_trust.RDS"))
  saveRDS(x3, paste0(in_dir,"/pca_multiple_imputation_2vars_corruption.RDS"))
}