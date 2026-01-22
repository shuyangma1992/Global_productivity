library(tidyverse)

# Global productivity decrease 2090s - 2020s 
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds")

productivity_projection <- productivity_projection %>%
  select(year, ssp, ESM, stockid, 10:1009) %>%
  pivot_longer(-c(year, ssp, ESM, stockid), names_to = "number", values_to = "productivity")

# productivity 2020-2029
productivity_projection_2020s <- productivity_projection %>%
  filter(year %in% seq(2020, 2029, 1)) %>% 
  group_by(ssp, ESM, number) %>%
  summarise(
    mean_20s = mean(productivity, na.rm = T)
  ) %>%
  ungroup()

# productivity 2090-2099
productivity_projection_2090s <- productivity_projection %>%
  filter(year %in% seq(2090, 2099, 1)) %>% 
  group_by(ssp, ESM, number) %>%
  summarise(
    mean_90s = mean(productivity, na.rm = T)
  ) %>%
  ungroup()

productivity_projection <- left_join(productivity_projection_2020s, productivity_projection_2090s)

# global productivity decrease
productivity_projection <- productivity_projection %>% 
  mutate(delta_p = mean_90s - mean_20s)

# decrease percentage
productivity_projection <- productivity_projection %>% 
  mutate(delta_p_percentage = delta_p / mean_20s)

# 95% confidence interval
productivity_projection <- productivity_projection %>% 
  group_by(ssp, ESM) %>% 
  summarize(value_mean = mean(delta_p),
            value_2.5 = quantile(delta_p, 0.025),
            value_97.5 = quantile(delta_p, 0.975),
            percentage_mean = mean(delta_p_percentage),
            percentage_2.5 = quantile(delta_p_percentage, 0.025),
            percentage_97.5 = quantile(delta_p_percentage, 0.975)) %>% 
  ungroup()

write.csv(productivity_projection, file = "Tables/Table_S4_projection_global_productivity_decrease.csv")





