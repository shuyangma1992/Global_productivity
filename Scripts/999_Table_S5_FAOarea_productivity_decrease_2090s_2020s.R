library(tidyverse)

# FAO area productivity decrease 2090s - 2020s 
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds")

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname, primary_FAOarea)

productivity_projection <- left_join(productivity_projection, stock_success)

productivity_projection <- productivity_projection %>%
  select(year, ssp, primary_FAOarea, ESM, stockid, 10:1009) %>%
  pivot_longer(-c(year, ssp, primary_FAOarea, ESM, stockid), names_to = "number", values_to = "productivity")

# productivity 2020-2029
productivity_projection_2020s <- productivity_projection %>%
  filter(year %in% seq(2020, 2029, 1)) %>% 
  group_by(ssp, primary_FAOarea, ESM, number) %>%
  summarise(
    mean_20s = mean(productivity, na.rm = T)
  ) %>%
  ungroup()

# productivity 2090-2099
productivity_projection_2090s <- productivity_projection %>%
  filter(year %in% seq(2090, 2099, 1)) %>% 
  group_by(ssp, primary_FAOarea, ESM, number) %>%
  summarise(
    mean_90s = mean(productivity, na.rm = T)
  ) %>%
  ungroup()

productivity_projection <- left_join(productivity_projection_2020s, productivity_projection_2090s)

# FAO area productivity decrease
productivity_projection <- productivity_projection %>% 
  mutate(delta_p = mean_90s - mean_20s)

# decrease percentage
productivity_projection <- productivity_projection %>% 
  mutate(delta_p_percentage = delta_p / mean_20s)

# 95% confidence interval
productivity_projection <- productivity_projection %>% 
  group_by(primary_FAOarea, ssp, ESM) %>% 
  summarize(value_mean = mean(delta_p),
            value_2.5 = quantile(delta_p, 0.025),
            value_97.5 = quantile(delta_p, 0.975),
            percentage_mean = mean(delta_p_percentage),
            percentage_2.5 = quantile(delta_p_percentage, 0.025),
            percentage_97.5 = quantile(delta_p_percentage, 0.975)) %>% 
  ungroup()

# delete FAO area 51 and 58
productivity_projection <- productivity_projection %>% 
  filter(!primary_FAOarea %in% c("51", "58"))

write.csv(productivity_projection, file = "Tables/Table_S5_projection_FAOarea_productivity_decrease.csv")











