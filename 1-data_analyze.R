library(dplyr)
library(magrittr)
library(tidyr)
library(gdata)
library(stringr)
library(brms)
library(MASS)
library(bayesplot)
library(tidybayes) 


######################################### CORRECT RESPONSES ANALYSIS #########################################
df_ncr <- readRDS("df_ncr.rds")

### contrast coding for the predictors ###
contrasts(df_ncr$category)
contrasts(df_ncr$category) <- contr.sdif(2)
contrasts(df_ncr$category)

contrasts(df_ncr$cat2)
contrasts(df_ncr$cat2) <- contr.sum(3)/2
contrasts(df_ncr$cat2)

contrasts(df_ncr$difficulty)
contrasts(df_ncr$difficulty) <- contr.sdif(3)
contrasts(df_ncr$difficulty)

# reorder factor for contrast setting 
df_ncr$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_ncr$group)
contrasts(df_ncr$group) <- contr.sdif(2)
contrasts(df_ncr$group)

# model responses
ncr_model <- brm(ncr ~ cat2*difficulty*group, 
                 family = poisson(link="log"), data = df_ncr,
                 chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "ncr_model")

interaction_loc <-hypothesis(ncr_model, "cat22:difficulty2M1 > 0")
interaction_loc2 <-hypothesis(ncr_model, "cat22:difficulty3M2 > 0")

# model df for plotting
ncr_model_df <- ncr_model %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  separate(col = "parameter", into = c("x","parameter"), sep = "\\_") %>%
  subset(parameter !="Intercept") %>% dplyr::select(-x)

# recode levels to Ortaake readable
ncr_model_df$parameter %<>% dplyr::recode(`cat21` = "El Þekli",`cat22` = "Ýþaret Yeri", `group2M1` = "Erken",
                                          `difficulty2M1` = "Orta-Kolay", `difficulty3M2` = "Zor-Orta",
                                          `cat21:difficulty2M1` = "El Þekli*Orta-Kolay",
                                          `cat21:difficulty3M2` = "El Þekli*Zor-Orta",
                                          `cat22:difficulty2M1` = "Ýþaret Yeri*Orta-Kolay",
                                          `cat22:difficulty3M2` = "Ýþaret Yeri*Zor-Orta",
                                          `cat21:group2M1`= "El Þekli*Erken",
                                          `cat22:group2M1`= "Ýþaret Yeri*Erken",
                                          `difficulty2M1:group2M1` = "Orta-Kolay*Erken",
                                          `difficulty3M2:group2M1` = "Zor-Orta*Erken",
                                          `cat21:difficulty2M1:group2M1` = "El Þekli*Orta-Kolay*Erken",
                                          `cat21:difficulty3M2:group2M1` = "El Þekli*Zor-Orta*Erken",
                                          `cat22:difficulty2M1:group2M1` = "Ýþaret Yeri*Orta-Kolay*Erken",
                                          `cat22:difficulty3M2:group2M1` = "Ýþaret Yeri*Zor-Orta*Erken") %>%
  reorder.factor(new.order = c("El Þekli", "Ýþaret Yeri", "Erken", "Orta-Kolay",
                               "Zor-Orta", "El Þekli*Orta-Kolay", "El Þekli*Zor-Orta", "Ýþaret Yeri*Orta-Kolay", "Ýþaret Yeri*Zor-Orta",
                               "El Þekli*Erken", "Ýþaret Yeri*Erken", "Orta-Kolay*Erken", "Zor-Orta*Erken",
                               "El Þekli*Orta-Kolay*Erken", "El Þekli*Zor-Orta*Erken", "Ýþaret Yeri*Orta-Kolay*Erken", "Ýþaret Yeri*Zor-Orta*Erken"))



saveRDS(ncr_model_df, "ncr_model_df.rds")

############################################ TIME COURSE ANALYSIS ############################################
df_time <- readRDS("df_cum_time.rds")

### contrast coding for the predictors ###
contrasts(df_time$category)
contrasts(df_time$category) <- contr.sdif(2)
contrasts(df_time$category)

contrasts(df_time$cat2)
contrasts(df_time$cat2) <- contr.sum(3)/2
contrasts(df_time$cat2)

contrasts(df_time$difficulty)
contrasts(df_time$difficulty) <- contr.sdif(3)
contrasts(df_time$difficulty)


df_time$group %<>% reorder.factor(new.order = c("Late","Native"))
contrasts(df_time$group)
contrasts(df_time$group) <- contr.sdif(2)
contrasts(df_time$group)


df_time$time %<>% as.factor()
contrasts(df_time$time)
contrasts(df_time$time) <- contr.sdif(6)
contrasts(df_time$time)

# regression model
time_model <- brm(time_cum ~ cat2*group*time*difficulty,
                   family = poisson(link="log"), data = df_time,
                   chains = 4, cores = 4, iter= 3000, warmup = 2000, file = "time_model")

# time model df
time_model_df <- time_model %>%
  mcmc_intervals_data(pars = vars(starts_with("b_")), prob_outer = 0.95) %>%
  separate(col = "parameter", into = c("x","parameter"), sep = "\\_") %>%
  subset(parameter !="Intercept") %>% dplyr::select(-x)

# recode parameters for easy reading
time_model_df %<>% subset(parameter %in% c("group2M1","time2M1","time3M2",
                                            "time4M3","time5M4","time6M5",
                                            "group2M1:time2M1","group2M1:time3M2",
                                            "group2M1:time4M3","group2M1:time5M4",
                                            "group2M1:time6M5"))
time_model_df$parameter %<>% dplyr::recode(`group2M1` = "Erken", `time2M1` = "20s-10s",`time3M2` = "30s-20s",
                                            `time4M3` = "40s-30s", `time5M4` = "50s-40s", `time6M5` = "60s-50s",
                                            `group2M1:time2M1` = "Erken*20s-10s", `group2M1:time3M2` = "Erken*30s-20s",
                                            `group2M1:time4M3` = "Erken*40s-30s", `group2M1:time5M4` = "Erken*50s-40s",
                                            `group2M1:time6M5` = "Erken*60s-50s") %>%
  reorder.factor(new.order = c("Erken","20s-10s","30s-20s","40s-30s","50s-40s","60s-50s",
                               "Erken*20s-10s","Erken*30s-20s","Erken*40s-30s","Erken*50s-40s","Erken*60s-50s"))

saveRDS(time_model_df, "time_model_df.rds")




