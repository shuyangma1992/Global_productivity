library(tidyverse)
library(JABBA)
library(doParallel)
library(foreach)

# 1 Productivity data -----------------------------------------------
best_model <- read_rds("Outputs/JABBA results/0_best_JABBA_final.rds") %>%
  mutate(file_name = paste0("Outputs/JABBA results/", stockid, "_seed1_", Model, "_jabba.rdata"))

# make best model file name
best_model_file_name <- best_model$file_name

# parallel run
# how many cores can be used
detectCores()

# use 20 cores
cl <- makeCluster(getOption("cl.cores", 20))

# register cores
registerDoParallel(cl)

productivity <- foreach(
  x = best_model_file_name[1:710],
  .combine = "rbind",
  .packages = c("tidyverse", "JABBA")
) %dopar% {
  # x <- best_model_file_name[1]
  load(x) # load jabba result

  # loop get productivity candidate time sereis
  productivity_loop <- NULL
  for (i in 1:6000) {
    data_loop <- filter(as.data.frame(jabba$kbtrj), iter == i) %>%
      arrange(year) # make sure year order
    data_loop <- data_loop %>%
      mutate(Bt0 = B, Bt1 = lead(B, 1))
    data_loop <- data_loop %>%
      mutate(sp = Bt1 + Catch - Bt0)
    data_loop <- data_loop %>%
      mutate(P = sp / Bt0) # calculate productivity
    data_loop <- data_loop %>%
      select(year, P)
    data_loop <- data_loop %>%
      mutate(
        stockid = str_sub(jabba$assessment, 1, -7),
        time_series = i
      ) # add stockid and time series number
    productivity_loop <- bind_rows(productivity_loop, data_loop)
  }

  productivity_loop <- productivity_loop %>%
    pivot_wider(values_from = P, names_from = time_series)
}

# save information
write_rds(productivity, file = "Outputs/Productivity/productivity.rds")

# stop cluster
stopCluster(cl)

# 2 Productivity characteristics ------------------------------------------
# used for Figure 2 stock productivity comparison
# mean, SD, CV and trend
productivity <- read_rds("Outputs/Productivity/productivity.rds")

# #time periods
# a <- productivity %>%
#   select(1, 2)
# a <- a %>%
#   group_by(stockid) %>%
#   summarise(number = n())

# stock information
stock_information <- read_rds("Data/stock_success_full_information_final.rds")
unique(stock_information$scientificname) # 285 species
unique(productivity$stockid) # 710 stocks

# set seed and select 1000 productivity time series for each stock
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

# loop function calculate mean, SD, CV and trend
productivity_characteristics <- NULL
for (i in unique(productivity$stockid)) {
  # i <- unique(productivity$stockid)[1]
  productivity_loop <- filter(productivity, stockid == i) %>%
    drop_na()

  # select 1000 productivity time series
  productivity_loop <- productivity_loop %>%
    select(year, stockid, as.character(resample_number))

  # statistics
  p_mean <- apply(productivity_loop[, 3:1002], MARGIN = 2, FUN = mean) # mean
  p_sd <- apply(productivity_loop[, 3:1002], MARGIN = 2, FUN = sd) # sd
  p_CV <- p_sd / p_mean # CV

  # statistics summary
  p_mean_lci <- quantile(p_mean, 0.025)
  p_mean_mean <- mean(p_mean)
  p_mean_uci <- quantile(p_mean, 0.975)

  p_sd_lci <- quantile(p_sd, 0.025)
  p_sd_mean <- mean(p_sd)
  p_sd_uci <- quantile(p_sd, 0.975)

  p_CV_lci <- quantile(p_CV, 0.025)
  p_CV_mean <- mean(p_CV)
  p_CV_uci <- quantile(p_CV, 0.975)

  # linear regression on year (trend)
  lm_results <- summary(lm(as.matrix(productivity_loop[, 3:1002]) ~ productivity_loop$year))
  p_slope <- NULL
  p_slope_significance <- NULL
  for (j in 1:1000) {
    # j=1
    p_slope <- c(p_slope, lm_results[[j]]$coefficients[2, 1]) # slope
    p_slope_significance <- c(p_slope_significance, lm_results[[j]]$coefficients[2, 4]) # p value
  }

  p_slope_lci <- quantile(p_slope, 0.025)
  p_slope_mean <- mean(p_slope)
  p_slope_uci <- quantile(p_slope, 0.975)

  p_slope_significance_95 <- quantile(p_slope_significance, 0.95)

  # make a data frame
  productivity_characteristics_loop <- data.frame(
    stockid = i,
    p_mean_lci, p_mean_mean, p_mean_uci,
    p_sd_lci, p_sd_mean, p_sd_uci,
    p_CV_lci, p_CV_mean, p_CV_uci,
    p_slope_lci, p_slope_mean, p_slope_uci,
    p_slope_significance_95
  )

  # combind data
  productivity_characteristics <- bind_rows(
    productivity_characteristics,
    productivity_characteristics_loop
  )

  print(i)
}

# save data
write_rds(productivity_characteristics, file = "Outputs/Productivity/productivity_characteristics.rds")

# 3 Productivity trend 1981-2022 ------------------------------------------
# trend 1981-2022
productivity <- read_rds("Outputs/Productivity/productivity.rds")

# stocks
unique(productivity$stockid) # 710 stocks

# set seed and select 1000 productivity time series for each stock
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

productivity_trend <- NULL
for (i in unique(productivity$stockid)) {
  # i <- unique(productivity$stockid)[1]
  productivity_loop <- filter(productivity, stockid == i) %>%
    filter(year %in% c(1981:2022)) %>%
    drop_na()

  # select 1000 productivity time series
  productivity_loop <- productivity_loop %>%
    select(year, stockid, as.character(resample_number))

  # linear regression on year (trend)
  lm_results <- summary(lm(as.matrix(productivity_loop[, 3:1002]) ~ productivity_loop$year))
  p_slope <- NULL
  p_slope_significance <- NULL
  for (j in 1:1000) {
    # j=1
    p_slope <- c(p_slope, lm_results[[j]]$coefficients[2, 1]) # slope
    p_slope_significance <- c(p_slope_significance, lm_results[[j]]$coefficients[2, 4]) # p value
  }

  p_slope_lci <- quantile(p_slope, 0.025)
  p_slope_mean <- mean(p_slope)
  p_slope_uci <- quantile(p_slope, 0.975)

  p_slope_significance_95 <- quantile(p_slope_significance, 0.95)

  # make a data frame
  productivity_trend_loop <- data.frame(
    stockid = i,
    p_slope_lci, p_slope_mean, p_slope_uci,
    p_slope_significance_95
  )

  # combine data
  productivity_trend <- bind_rows(
    productivity_trend,
    productivity_trend_loop
  )

  print(i)
}

# save data
write_rds(productivity_trend, file = "Outputs/Productivity/productivity_trend_1981_2022.rds")


