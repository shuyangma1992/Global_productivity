library(tidyverse)
library(rfishbase)
library(zoo)

# stock data
stock_data <- read_rds("Data/stocks_data.rds")

# stock information
stock_information <- read_rds("Data/stocks_data.rds") %>%
  select(stockid, scientificname) %>%
  distinct()

# species information
species_information <- read_rds("Data/fishbase_information.rds")
# scientificname is the scientific name used in RAM legacy, species is the scientific name used in Fishbase or Sealifebase

# 1 Prior r ---------------------------------------------------------------
prior_r <- NULL
for (i in stock_information$stockid) {
  # i="ACADRED2J3K"
  scientific_name <- filter(stock_information, stockid == i)$scientificname

  ############################################################################# r
  # step 1: r from fishbase or sealifebase
  species_information_cycle <- filter(species_information, scientificname == scientific_name)

  r_lower <- species_information_cycle$lcl_r
  r_upper <- species_information_cycle$ucl_r
  r_mean <- species_information_cycle$prior_r
  r_sd <- (log(r_upper) - log(r_lower)) / 3.92 # lognormal
  r_CV <- sqrt(exp(r_sd^2) - 1)

  # step 2: r transformed from resilience, Froese et al., 2020, ICES JMS, Table 1
  # for stocks without resilience information,
  # r_lower equals to r_lower of "very low",
  # r_upper equals to r_upper of "high", an uninformative prior
  if (is.na(r_lower) | is.na(r_upper)) {
    resilience <- species_information_cycle$Resilience[[1]]
    r_lower <- case_match(
      resilience,
      "Very low" ~ 0.015,
      "Low" ~ 0.05,
      "Medium" ~ 0.2,
      "High" ~ 0.6,
      NA ~ 0.015
    )
    r_upper <- case_match(
      resilience,
      "Very low" ~ 0.1,
      "Low" ~ 0.5,
      "Medium" ~ 0.8,
      "High" ~ 1.5,
      NA ~ 1.5
    )
    r_mean <- exp((log(r_lower) + log(r_upper)) / 2)
    r_sd <- (log(r_upper) - log(r_lower)) / 3.92 # lognormal
    r_CV <- sqrt(exp(r_sd^2) - 1)
  }

  prior_r_cycle <- data.frame(
    stockid = i, scientificname = scientific_name,
    r_lower = r_lower,
    r_mean = r_mean,
    r_upper = r_upper,
    r_sd = r_sd,
    r_CV = r_CV
  )
  prior_r <- bind_rows(prior_r, prior_r_cycle)
  print(i)
}

write_rds(prior_r, file = "Data/prior_r.rds")


# 2 Prior K ---------------------------------------------------------------
prior_r <- read_rds("Data/prior_r.rds")
prior_K <- NULL
for (i in stock_information$stockid) {
  # i = "ACADRED2J3K"
  # i = "YELLGB"
  # i = "DOYSFS"
  # i = "ATBTUNAWATL"
  # i = "SNOSESHARATL"
  # i = "NEPHFU14"
  scientific_name <- filter(stock_information, stockid == i)$scientificname
  stock_data_cycle <- filter(stock_data, stockid == i)

  # give "catch" priority
  if (!is.na(stock_data_cycle$Catch[1])) {
    catch <- stock_data_cycle$Catch
  } else {
    catch <- stock_data_cycle$Landing
  }

  catch <- rollmean(catch, 3)
  max_catch <- max(catch, na.rm = T)

  # biomass at the end of the time series
  # if B/Bmsy available, use B/Bmsy
  # last 3 years' mean, >1 high biomass at the end, <1 low biomass at the end
  judge_B_Bmsy <- mean(tail(na.omit(stock_data_cycle$`B/Bmsy`), n = 3)) > 1

  # if B/Bmsy not available, use all the abundance indices
  # last 3 years' mean comparing to maximum
  # greater than 0.5, high biomass at the end of the time series
  # less than 0.5, high biomass at the end of the time series
  judge_abundance_index <- data.frame(judge = c(
    mean(tail(na.omit(stock_data_cycle$Total_biomass), n = 3)) > max(stock_data_cycle$Total_biomass, na.rm = T) * 0.5,
    mean(tail(na.omit(stock_data_cycle$Total_abundance), n = 3)) > max(stock_data_cycle$Total_abundance, na.rm = T) * 0.5,
    mean(tail(na.omit(stock_data_cycle$SSB), n = 3)) > max(stock_data_cycle$SSB, na.rm = T) * 0.5,
    mean(tail(na.omit(stock_data_cycle$CPUE), n = 3)) > max(stock_data_cycle$CPUE, na.rm = T) * 0.5,
    mean(tail(na.omit(stock_data_cycle$Survey_abundance), n = 3)) > max(stock_data_cycle$Survey_abundance, na.rm = T) * 0.5
  ))
  # the minority obeys the majority
  judge_abundance_index_summary <- judge_abundance_index %>%
    group_by(judge) %>%
    summarize(n = n()) %>%
    drop_na() %>%
    arrange(desc(n))

  if (!is.na(judge_B_Bmsy)) {
    biomass_at_the_end <- ifelse(judge_B_Bmsy == TRUE,
      "High biomass at the end",
      "Low biomass at the end"
    )
  } else {
    biomass_at_the_end <- ifelse(judge_abundance_index_summary[[1, 1]] == TRUE,
      "High biomass at the end",
      "Low biomass at the end"
    )
  }
 
  # prior r
  prior_r_cycle <- filter(prior_r, stockid == i)
  r_lower <- prior_r_cycle$r_lower
  r_upper <- prior_r_cycle$r_upper
  ############################################################################ K
  # Froese et al., 2017, FaF, equations 3 and 4
  if (biomass_at_the_end == "High biomass at the end") {
    K_lower <- 2 * max_catch / r_upper
    K_upper <- 12 * max_catch / r_lower
    K_mean <- exp((log(K_lower) + log(K_upper)) / 2)
    K_sd <- (log(K_upper) - log(K_lower)) / 3.92
    K_CV <- sqrt(exp(K_sd^2) - 1)
  } else {
    K_lower <- max_catch / r_upper
    K_upper <- 4 * max_catch / r_lower
    K_mean <- exp((log(K_lower) + log(K_upper)) / 2)
    K_sd <- (log(K_upper) - log(K_lower)) / 3.92
    K_CV <- sqrt(exp(K_sd^2) - 1)
  }

  prior_K_cycle <- data.frame(
    stockid = i, scientificname = scientific_name,
    K_lower = K_lower,
    K_mean = K_mean,
    K_upper = K_upper,
    K_sd = K_sd,
    K_CV = K_CV
  )
  prior_K <- bind_rows(prior_K, prior_K_cycle)
  print(i)
}

write_rds(prior_K, file = "Data/prior_K.rds")

# 3 Prior psi -------------------------------------------------------------
prior_psi <- NULL
for (i in stock_information$stockid) {
  # i="ACADRED2J3K"
  # i="HERRHG"
  # i="ALBAMED"
  scientific_name <- filter(stock_information, stockid == i)$scientificname
  stock_data_cycle <- filter(stock_data, stockid == i) %>% 
    filter(Catch != 0 | Landing != 0)

  # qualitative stock size information
  # biomass at the start of the time series
  # if B/Bmsy available, use B/Bmsy
  # first 3 years' mean in the first 10 years of the time series
  judge_B_Bmsy <- mean(head(na.omit(stock_data_cycle$`B/Bmsy`[1:10]), n = 3)) 
  
  # if B/Bmsy not available, use other abundance indices
  # still focus on the first 10 years, if not data in the first 10 years, only can use catch
  judge_abundance_index <- data.frame(judge = c(
    mean(head(na.omit(stock_data_cycle$Total_biomass[1:10]), n = 3))/max(stock_data_cycle$Total_biomass, na.rm = T) ,
    mean(head(na.omit(stock_data_cycle$Total_abundance[1:10]), n = 3))/max(stock_data_cycle$Total_abundance, na.rm = T),
    mean(head(na.omit(stock_data_cycle$SSB[1:10]), n = 3))/max(stock_data_cycle$SSB, na.rm = T),
    mean(head(na.omit(stock_data_cycle$CPUE[1:10]), n = 3))/max(stock_data_cycle$CPUE, na.rm = T),
    mean(head(na.omit(stock_data_cycle$Survey_abundance[1:10]), n = 3))/max(stock_data_cycle$Survey_abundance, na.rm = T)
  ))
  judge_abundance_index <- drop_na(judge_abundance_index)
  judge_abundance_index <- mean(judge_abundance_index$judge)
  
  # biomass data always do not started from the start of the time series, so we use catch
  # first 3 years' mean catch comparing to maximum catch
  # >0.8, Close to unexploited
  # 0.6-0.8, More than half
  # 0.4-0.6, About half
  # 0.2-0.4, Small
  # <=0.2, Very small
  
  # give "catch" priority
  if (!is.na(stock_data_cycle$Catch[1])) {
    catch <- stock_data_cycle$Catch
  } else {
    catch <- stock_data_cycle$Landing
  }
  
  catch <- rollmean(catch, 3)
  max_catch <- max(catch, na.rm = T)
  judge_catch <- catch[1] / max_catch
  
  if (!is.na(judge_B_Bmsy)) {
    qualitative_stock_size <- case_when(
      judge_B_Bmsy > 1.75 ~ "Close to unexploited",
      judge_B_Bmsy <= 1.75 & judge_B_Bmsy > 1.25 ~ "More than half",
      judge_B_Bmsy <= 1.25 & judge_B_Bmsy > 0.75 ~ "About half",
      judge_B_Bmsy <= 0.75 & judge_B_Bmsy > 0.25 ~ "Small",
      judge_B_Bmsy <= 0.25 ~ "Very small"
    )
  } else {
    if (!is.na(judge_abundance_index)) {
      qualitative_stock_size <- case_when(
        judge_abundance_index > 0.8 ~ "Close to unexploited",
        judge_abundance_index <= 0.8 & judge_abundance_index > 0.6 ~ "More than half",
        judge_abundance_index <= 0.6 & judge_abundance_index > 0.4 ~ "About half",
        judge_abundance_index <= 0.4 & judge_abundance_index > 0.2 ~ "Small",
        judge_abundance_index <= 0.2 ~ "Very small"
      )
    } else {
      qualitative_stock_size <- case_when(
        judge_catch <= 0.2 ~ "Close to unexploited",
        judge_catch <= 0.4 & judge_catch > 0.2 ~ "More than half",
        judge_catch <= 0.6 & judge_catch > 0.4 ~ "About half",
        judge_catch <= 0.8 & judge_catch > 0.6 ~ "Small",
        judge_catch > 0.8 ~ "Very small"
      )
    }
  }
  
  # psi transformed from qualitative stock size, Froese, et al., 2020, ICES JMS, Table 2
  psi_lower <- case_match(
    qualitative_stock_size,
    "Very small" ~ 0.01,
    "Small" ~ 0.15,
    "About half" ~ 0.35,
    "More than half" ~ 0.5,
    "Close to unexploited" ~ 0.75
  )
  psi_upper <- case_match(
    qualitative_stock_size,
    "Very small" ~ 0.2,
    "Small" ~ 0.4,
    "About half" ~ 0.65,
    "More than half" ~ 0.85,
    "Close to unexploited" ~ 1
  )
  psi_mean <- exp((log(psi_lower) + log(psi_upper)) / 2)
  psi_sd <- (log(psi_upper) - log(psi_lower)) / 3.92 # lognormal
  psi_CV <- sqrt(exp(psi_sd^2) - 1)

  prior_psi_cycle <- data.frame(
    stockid = i, scientificname = scientific_name,
    psi_lower = psi_lower,
    psi_mean = psi_mean,
    psi_upper = psi_upper,
    psi_sd = psi_sd,
    psi_CV = psi_CV,
    judge_B_Bmsy = judge_B_Bmsy,
    judge_abundance_index = judge_abundance_index,
    judge_catch = judge_catch
    
  )
  prior_psi <- bind_rows(prior_psi, prior_psi_cycle)
  print(i)
}

write_rds(prior_psi, file = "Data/prior_psi.rds")






















