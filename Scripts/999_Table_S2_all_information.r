library(tidyverse)

# Table S2 including all information about stocks
# Stock information: stock id, stock name, scientific name, common name, FAO area, GRSF_uuid
# SPM: model type, r prior, r posterior, K prior, K posterior, psi prior, psi posterior, 
# Productivity hindcast: productivity length, mean productivity (95% CI), slope (95% CI, p value)
# Climate change effects: slope of temperature, slope of chlorophyll
# Productivity forecast: 2050s-2020s (three SSPs), 2090s-2020s (three SSPs)

############################################################## stock information
stock <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, stocklong, scientificname, commonname, primary_FAOarea, GRSF_uuid)

############################################################################ SPM
# SPM model type
spm_type <- read_rds("Outputs/JABBA results/0_best_jabba_final.rds") %>% 
  select(stockid, Model)

# SPM prior and posterior
best_model <- read_rds("Outputs/JABBA results/0_best_JABBA_final.rds") %>%
  mutate(file_name = paste0("Outputs/JABBA results/", stockid, "_seed1_", Model, "_jabba.rdata"))

# make best model file name
best_model_file_name <- best_model$file_name
spm_information <- NULL
for(i in best_model_file_name) {
  
  # i <- best_model_file_name[1]
  load(i)
  stockid <- str_sub(jabba$assessment, 1, -7)
  period <- paste0(head(jabba$yr, n = 1), "-", tail(jabba$yr, n = 1))
  prior_r <- paste0(round(jabba$settings$r.pr[1], 3), ", ", round(jabba$settings$r.pr[2],3)) # mean, log sd
  prior_K <- paste0(format(jabba$settings$K.pr[1], scientific = TRUE, digits = 4), ", ", round(jabba$settings$K.pr[2],3)) # mean, log sd
  prior_psi <- paste0(round(jabba$settings$psi.pr[1], 3), ", ", round(jabba$settings$psi.pr[2],3)) # mean, log sd
  posterior_r <- paste0(round(mean(jabba$pars_posterior$r), 3), ", ", round(sd(log(jabba$pars_posterior$r)), 3))
  posterior_K <- paste0(format(mean(jabba$pars_posterior$K), scientific = TRUE, digits = 4), ", ", round(sd(log(jabba$pars_posterior$K)), 3))
  posterior_psi <- paste0(round(mean(jabba$pars_posterior$psi), 3), ", ", round(sd(log(jabba$pars_posterior$psi)), 3))
  
  spm_information_loop <- data.frame(stockid = stockid,
                                     period = period,
                                     prior_r = prior_r,
                                     prior_K = prior_K,
                                     prior_psi = prior_psi,
                                     posterior_r = posterior_r,
                                     posterior_K = posterior_K,
                                     posterior_psi = posterior_psi)
  
  spm_information <- bind_rows(spm_information, spm_information_loop)
  
}

spm <- left_join(spm_type, spm_information)

####################################################### productivity information
productivity_hindcast <- read_rds("Outputs/Productivity/productivity_characteristics.rds") %>%
  select(
    stockid, p_mean_lci, p_mean_mean, p_mean_uci,
    p_slope_lci, p_slope_mean, p_slope_uci,
    p_slope_significance_95
  ) %>%
  mutate(
    productivity_hindcast = paste0(round(p_mean_mean, 3), " (", round(p_mean_lci, 3), " - ", round(p_mean_uci, 3), ")"),
    productivity_hindcast_slope = paste0(round(p_slope_mean, 3), " (", round(p_slope_lci, 3), " - ", round(p_slope_uci, 3), ", ", round(p_slope_significance_95, 3), ")"),
  )

productivity_hindcast <- productivity_hindcast %>% 
  select(stockid, productivity_hindcast, productivity_hindcast_slope)

######################################################### climate change effects
random_effects <- read_rds("Outputs/GLMM results/random_effects.rds") %>% 
  select(1,5,6) 

random_effects_summary <- random_effects %>% 
  group_by(scientificname) %>% 
  summarise(median_T = median(temperature_adjusted)*100,
            low_T = quantile(temperature_adjusted, 0.025)*100,
            high_T = quantile(temperature_adjusted, 0.975)*100,
            median_CHL = median(chlorophyll_adjusted),
            low_CHL = quantile(chlorophyll_adjusted, 0.025),
            high_CHL = quantile(chlorophyll_adjusted, 0.975)) 

random_effects_summary <- random_effects_summary %>% 
  mutate(T_effect = paste0(round(median_T, 3), " (", round(low_T, 3), " - ", round(high_T, 3), ")"),
         CHL_effect = paste0(round(median_CHL, 3), " (", round(low_CHL, 3), " - ", round(high_CHL, 3), ")"))

random_effects_summary <- random_effects_summary %>% 
  select(scientificname, T_effect, CHL_effect)

########################################################## productivity forecast
# 50s - 20s
productivity_change_50s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_50s_20s.rds") %>% 
  mutate(productivity_change_50s = paste0(round(p_change_mean, 3), " (", round(p_change_2.5, 3), " - ", round(p_change_97.5, 3), ")"))

productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  select(stockid, ssp, productivity_change_50s)

productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  pivot_wider(names_from = ssp, values_from = productivity_change_50s)

productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  rename(ssp126_50s = ssp126, ssp245_50s = ssp245, ssp585_50s = ssp585) %>% 
  filter(ESM == "IPSL-CM6A-LR")

# 90s - 20s
productivity_change_90s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_90s_20s.rds") %>% 
  mutate(productivity_change_90s = paste0(round(p_change_mean, 3), " (", round(p_change_2.5, 3), " - ", round(p_change_97.5, 3), ")"))

productivity_change_90s_20s <- productivity_change_90s_20s %>% 
  select(stockid, ssp, productivity_change_90s)

productivity_change_90s_20s <- productivity_change_90s_20s %>% 
  pivot_wider(names_from = ssp, values_from = productivity_change_90s)

productivity_change_90s_20s <- productivity_change_90s_20s %>% 
  rename(ssp126_90s = ssp126, ssp245_90s = ssp245, ssp585_90s = ssp585) %>% 
  filter(ESM == "IPSL-CM6A-LR")

######################################################## combine all information
data <- left_join(stock, spm) %>% 
  left_join(productivity_hindcast) %>% 
  left_join(random_effects_summary) %>% 
  left_join(productivity_change_50s_20s) %>% 
  left_join(productivity_change_90s_20s)

data <- data %>% 
  arrange(primary_FAOarea, stockid)

write_csv(data, "Tables/Table_S2.csv")







