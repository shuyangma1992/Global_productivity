library(tidyverse)

# Table S1a FAO major fishing area ------------------------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# FAO area
productivity_characteristics <- productivity_characteristics %>%
  group_by(primary_FAOarea) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  ) 

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%  
  filter(!primary_FAOarea %in% c("51", "58")) %>% # too few stocks
  mutate(primary_FAOarea = factor(primary_FAOarea, levels = primary_FAOarea)) 

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# save table
write.csv(productivity_characteristics, "Tables/Table_S1a_FAO_area.csv")


# Table S1b Major taxa ----------------------------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# fishery type
productivity_characteristics <- productivity_characteristics %>%
  group_by(FisheryType) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  )

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%
  mutate(FisheryType = factor(FisheryType, levels = FisheryType))

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# save table
write.csv(productivity_characteristics, "Tables/Table_S1b_major_taxa.csv")



# Table S1c Family --------------------------------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# family
productivity_characteristics <- productivity_characteristics %>%
  group_by(family) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  )

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%
  mutate(family = factor(family, levels = family))

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# save table
write.csv(productivity_characteristics, "Tables/Table_S1c_family.csv")












