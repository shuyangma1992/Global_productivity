library(tidyverse)

# 1 Productivity data for DFA  ----------------------------------------------------------------------
# calculate mean and sd etc, for following DFA
productivity <- read_rds("Outputs/Productivity/productivity.rds")

# set seed and select 1000 productivity time series for each stock
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

productivity_summary <- productivity %>%
  select(year, stockid, as.character(resample_number)) %>%
  drop_na() %>%
  rowwise(year, stockid) %>%
  summarise(
    p_mean = mean(c_across(1:1000)),
    p_sd = sd(c_across(1:1000)),
    p_median = median(c_across(1:1000)),
    p_2.5 = quantile(c_across(1:1000), 0.025),
    p_97.5 = quantile(c_across(1:1000), 0.975)
  )

write_rds(productivity_summary, file = "Outputs/Productivity/productivity_for_DFA.rds")

# 2 Productivity period for DFA -------------------------------------------------
# load data
productivity <- read_rds("Outputs/Productivity/productivity_for_DFA.rds") %>%
  select(year, stockid, p_mean) %>%
  drop_na() %>%
  arrange(year) %>%
  filter(year >= 1950)

# stock information
# delete FAO area 51, 58, less than 5 stocks
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  filter(!primary_FAOarea %in% c("51", "58"))

productivity_stock_number <- NULL
for (i in unique(stock_success$primary_FAOarea)) {
  # i="21"
  stock_success_loop <- stock_success %>%
    filter(primary_FAOarea == i)
  stock_number <- nrow(stock_success_loop)
  
  productivity_loop <- productivity %>%
    filter(stockid %in% stock_success_loop$stockid) %>%
    drop_na()
  
  productivity_loop <- productivity_loop %>%
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(
      FAOarea = i,
      stock_number = stock_number
    )
  
  productivity_stock_number <- bind_rows(productivity_stock_number, productivity_loop)
}

# stock_number_proportion
productivity_stock_number <- productivity_stock_number %>%
  rowwise() %>%
  mutate(stock_number_proportion = n / stock_number)

# productivity period for DFA
DFA_period <- NULL
for (i in unique(productivity_stock_number$FAOarea)) {
  # i=21
  productivity_stock_number_loop <- productivity_stock_number %>%
    filter(FAOarea == i)
  period <- range(productivity_stock_number_loop %>%
                    filter(stock_number_proportion >= 0.6) %>%
                    select(year))
  stock_number <- productivity_stock_number_loop$stock_number[1]
  DFA_period <- bind_rows(
    DFA_period,
    data.frame(
      FAOarea = i,
      start_year = period[1], end_year = period[2],
      stock_number = stock_number
    )
  )
}

# save DFA_period
write_rds(DFA_period, file = "Outputs/Productivity/productivity_period_for_DFA.rds")

