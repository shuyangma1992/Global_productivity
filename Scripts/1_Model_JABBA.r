library(tidyverse)
library(JABBA)
library(foreach)
library(doParallel)

# 1 JABBA model with 200000 iterations ----------------------------------------------------------------
# load JABBA function
load("Functions/function_JABBA_iteration_200000.R")

# stocks data
stocks_data <- read_rds("Data/stocks_data_for_JABBA.rds")
stocks_list <- unique(stocks_data$stockid) # 728 stocks

# species information
species_information <- read_rds("Data/fishbase_information.rds")

# prior r
prior_r <- read_rds("Data/prior_r.rds")

# prior K
prior_K <- read_rds("Data/prior_K.rds")

# prior psi
prior_psi <- read_rds("Data/prior_psi.rds")

# how many cores can be used
detectCores()

# use 20 cores
cl <- makeCluster(getOption("cl.cores", 20))

# register cores
registerDoParallel(cl)

jabba_information <- foreach(
  x = stocks_list[1:728],
  .combine = "rbind",
  .packages = c("tidyverse", "JABBA")
) %dopar% {
  # run JABBA
  try_JABBA <- try(function_JABBA_200000(
    stock_id = x,
    seed = 1,
    stocks_time_series = stocks_data,
    prior_r = prior_r,
    prior_K = prior_K,
    prior_psi = prior_psi
  ))

  # success or failure
  model_run_cycle <- data.frame(
    stock_id = x,
    success_or_failure = ifelse(is.character(try_JABBA[1]),
      try_JABBA[1], "success"
    )
  )
}

# save information
write_rds(jabba_information, file = "Outputs/JABBA results/0_jabba_information.rds")

# stop cluster
stopCluster(cl)

# check stocks with failure
model_information <- read_rds("Outputs/JABBA results/0_jabba_information.rds")

stock_failure <- model_information %>%
  filter(success_or_failure != "success")

stock_success <- model_information %>%
  filter(success_or_failure == "success")

write_rds(stock_success, file = "Data/stock_success.rds")




# test -------------------------------------------------------------------
load("Outputs/JABBA results/ACMACKSARG_seed1_Fox_jabba.rdata")
jbplot_ppdist(jabba)

