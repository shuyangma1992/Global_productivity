library(tidyverse)
library(JABBA)
library(foreach)
library(doParallel)
# Run JABBA with 2000 iterations, focus on stocks with failure, then correct data

# 1 Preliminary run, 2000 iterations, for finding problems ----------------------------------------------------------------
# load JABBA function
load("Functions/function_JABBA_iteration_2000.R")

# stocks data
stocks_data <- read_rds("Data/stocks_data.rds")
stocks_list <- unique(stocks_data$stockid) # 742 stocks

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

# parallel JABBA run
jabba_information <- foreach(
  x = stocks_list[1:742],
  .combine = "rbind",
  .packages = c("tidyverse", "JABBA")
) %dopar% {
  # run JABBA
  try_JABBA <- try(function_JABBA_2000(
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

# 2 Data modification -------------------------------------------------------
# model information
model_information <- read_rds("Outputs/JABBA results/0_jabba_information.rds")

stock_failure <- model_information %>%
  filter(success_or_failure != "success")

stock_success <- read_rds("Data/stock_success.rds")

# stocks data
stocks_data <- read_rds("Data/stocks_data.rds")

# stocks with failure
# 1 ALSKABSAI
a <- filter(stocks_data, stockid == "ALSKABSAI")
stocks_data <- stocks_data %>%
  filter(!(stockid == "ALSKABSAI" & Year < 1965)) # change start time to 1965, rerun

# 2 AMPL3M
a <- filter(stocks_data, stockid == "AMPL3M")
stocks_data <- stocks_data %>%
  filter(!(stockid == "AMPL3M" & Year > 2003)) # change end time to 2003, rerun

# 3 ANCHOBAYB
a <- filter(stocks_data, stockid == "ANCHOBAYB")
b <- mean(a$Catch[c(20, 24)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "ANCHOBAYB" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch (2006, 2010) for 2007, 2008, 2009

# 4 ANCHOSA
a <- filter(stocks_data, stockid == "ANCHOSA")
stocks_data <- stocks_data %>%
  filter(!(stockid == "ANCHOSA" & Year < 1964)) # change start time to 1964, rerun

# 5 ARCSURFBANQ
a <- filter(stocks_data, stockid == "ARCSURFBANQ")
b <- mean(a$Catch[c(5, 7)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "ARCSURFBANQ" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 1992

# 6 BKINGCRABPI
a <- filter(stocks_data, stockid == "BKINGCRABPI") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "BKINGCRABPI")) # delete stock

# 7 BKINGCRABSMI
a <- filter(stocks_data, stockid == "BKINGCRABSMI") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "BKINGCRABSMI")) # delete stock

# 8 BLACKOREOPR
a <- filter(stocks_data, stockid == "BLACKOREOPR") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "BLACKOREOPR")) # delete stock

# 9 BLTILESATLC
a <- filter(stocks_data, stockid == "BLTILESATLC")
stocks_data <- stocks_data %>%
  filter(!(stockid == "BLTILESATLC" & Year < 1962)) # change start time to 1962, rerun

# 10 CAPEIIa-V-XIV
a <- filter(stocks_data, stockid == "CAPEIIa-V-XIV")
b <- mean(a$Catch[c(3, 5)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "CAPEIIa-V-XIV" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch (1981, 1983) for 1982
stocks_data <- stocks_data %>%
  filter(!(stockid == "CAPEIIa-V-XIV" & Year > 2017)) # delete data after 2017

# 11 CAPENOR
a <- filter(stocks_data, stockid == "CAPENOR") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "CAPENOR")) # delete stock

# 12 CHAKESA
a <- filter(stocks_data, stockid == "CHAKESA")
stocks_data <- stocks_data %>%
  filter(!(stockid == "CHAKESA" & Year < 1921)) # change start time to 1921, rerun

# 13 CHROCKCPCOAST
a <- filter(stocks_data, stockid == "CHROCKCPCOAST")
stocks_data <- stocks_data %>%
  filter(!(stockid == "CHROCKCPCOAST" & Year < 1973)) # change start time to 1973, rerun

# 14 CMACKPCOAST
a <- filter(stocks_data, stockid == "CMACKPCOAST")
stocks_data <- stocks_data %>%
  filter(!(stockid == "CMACKPCOAST" & Year < 1983)) # change start time to 1983, rerun

# 15 COD3Ps
a <- filter(stocks_data, stockid == "COD3Ps")
b <- mean(a$Catch[c( 35, 39)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "COD3Ps" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 1994-1996

# 16 COD4TVn
a <- filter(stocks_data, stockid == "COD4TVn")
b <- mean(a$Catch[c(92, 97)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "COD4TVn" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch (2008, 2013) for 2009-2012

# 17 EULAPCOASTCCDU
a <- filter(stocks_data, stockid == "EULAPCOASTCCDU") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "EULAPCOASTCCDU")) # delete stock

# 18 GHAL01ABCDEF
a <- filter(stocks_data, stockid == "GHAL01ABCDEF") # CPUE wrong
stocks_data <- stocks_data %>%
  filter(!(stockid == "GHAL01ABCDEF")) # delete stock

# 19 GSSMELTVIb-VII-VIII-IX-X-XII
a <- filter(stocks_data, stockid == "GSSMELTVIb-VII-VIII-IX-X-XII")
b <- mean(a$Catch[c(19, 21)])
c <- mean(a$Catch[c(32, 34)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "GSSMELTVIb-VII-VIII-IX-X-XII" & Catch == 0 & Year == 1985 ~ b,
    stockid == "GSSMELTVIb-VII-VIII-IX-X-XII" & Catch == 0 & Year == 1998 ~ c,
    TRUE ~ Catch
  )) # use mean catch for 0
stocks_data <- stocks_data %>%
  filter(!(stockid == "GSSMELTVIb-VII-VIII-IX-X-XII" & Year < 1978)) # change start time to 1978, rerun

# 20 GSTRGZRSTA7
a <- filter(stocks_data, stockid == "GSTRGZRSTA7")
stocks_data <- stocks_data %>%
  filter(!(stockid == "GSTRGZRSTA7" & Year < 1967)) # change start time to 1967, rerun

# 21 GURCH4RST
a <- filter(stocks_data, stockid == "GURCH4RST")
stocks_data <- stocks_data %>%
  filter(!(stockid == "GURCH4RST" & Year < 1994)) # change start time to 1994, rerun

# 22 HADGB
a <- filter(stocks_data, stockid == "HADGB")
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "HADGB" & is.na(Catch) ~ Landing,
    TRUE ~ Catch
  )) # use landing as catch for 1989-2010

# 23 HERRCC
a <- filter(stocks_data, stockid == "HERRCC") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "HERRCC" & Year > 2007)) # change end time to 2007, rerun

# 24 HERRHG
a <- filter(stocks_data, stockid == "HERRHG") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "HERRHG")) # delete stock

# 25 HERRPRD
a <- filter(stocks_data, stockid == "HERRPRD")
b <- mean(a$Catch[c(31, 33)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "HERRPRD" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean for 1982 missing value

# 26 HERRPWS
a <- filter(stocks_data, stockid == "HERRPWS") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "HERRPWS")) # delete stock

# 27 HERRSOG
a <- filter(stocks_data, stockid == "HERRSOG")
b <- mean(a$Catch[c(18, 20)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "HERRSOG" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean for 1969 missing value

# 28 HERRWCVANI
a <- filter(stocks_data, stockid == "HERRWCVANI") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "HERRWCVANI")) # delete stock

# 29 ILSCALL3Ps
a <- filter(stocks_data, stockid == "ILSCALL3Ps") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "ILSCALL3Ps")) # delete stock

# 30 LUMP3Pn4RS
a <- filter(stocks_data, stockid == "LUMP3Pn4RS") # too many 0 in catch
b <- mean(a$Catch[c(4, 6)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "LUMP3Pn4RS" & Landing == 0 ~ b,
    TRUE ~ Catch
  )) # use mean landing for 0 landing

# 31 NEPHFU31
a <- filter(stocks_data, stockid == "NEPHFU31") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "NEPHFU31" & Year > 2016)) # change end time to 2016, rerun

# 32 NZLINGLIN6b
a <- filter(stocks_data, stockid == "NZLINGLIN6b") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "NZLINGLIN6b" & Year < 1989)) # change start time to 1989, rerun

# 33 NZLINGWSE
a <- filter(stocks_data, stockid == "NZLINGWSE") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "NZLINGWSE" & Year < 1985)) # change start time to 1985, rerun

# 34 OROUGHYCASCADE
a <- filter(stocks_data, stockid == "OROUGHYCASCADE") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "OROUGHYCASCADE")) # delete stock

# 35 OROUGHYNZ7A
a <- filter(stocks_data, stockid == "OROUGHYNZ7A") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "OROUGHYNZ7A")) # delete stock

# 36 OWSHARCWPAC
a <- filter(stocks_data, stockid == "OWSHARCWPAC")
stocks_data <- stocks_data %>%
  filter(!(stockid == "OWSHARCWPAC" & Year > 2010)) # change end time to 2010, rerun

# 37 PANDALGOM
a <- filter(stocks_data, stockid == "PANDALGOM")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALGOM" & Year > 2017)) # change end time to 2017, rerun

# 38 PANDALNUNE
a <- filter(stocks_data, stockid == "PANDALNUNE")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALNUNE" & Year < 1988)) # change start time to 1988, rerun

# 39 PANDALSFA12
a <- filter(stocks_data, stockid == "PANDALSFA12")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALSFA12" & Year < 1982)) # change start time to 1982, rerun

# 40 PANDALSFA2-3
a <- filter(stocks_data, stockid == "PANDALSFA2-3")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALSFA2-3" & Year < 1988)) # change start time to 1988, rerun

# 41 PANDALSMA16
a <- filter(stocks_data, stockid == "PANDALSMA16")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALSMA16" & Year < 1994)) # change start time to 1994, rerun

# 42 PANDALSMA18-19
a <- filter(stocks_data, stockid == "PANDALSMA18-19")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALSMA18-19" & Year < 1995),
         !(stockid == "PANDALSMA18-19" & Year > 2009)) # change start time to 1995, end year to 2009, rerun

# 43 PANDALSMAGTSE
a <- filter(stocks_data, stockid == "PANDALSMAGTSE")
stocks_data <- stocks_data %>%
  filter(!(stockid == "PANDALSMAGTSE" & Year < 1991)) # change start time to 1994, rerun

# 44 PORSHARATL
a <- filter(stocks_data, stockid == "PORSHARATL")
b <- mean(a$Landing[c(13, 15)])
stocks_data <- stocks_data %>%
  mutate(Landing = case_when(
    stockid == "PORSHARATL" & Landing == 0 ~ b,
    TRUE ~ Landing
  )) # use mean landing for 0 landing

# 45 PTOOTHFISHMI
a <- filter(stocks_data, stockid == "PTOOTHFISHMI")
b <- mean(a$Catch[c(26, 28)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "PTOOTHFISHMI" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 0 catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "PTOOTHFISHMI" & Year < 1994)) # change start time to 1994, rerun

# 46 RKCRABNS
a <- filter(stocks_data, stockid == "RKCRABNS")
b <- mean(a$Catch[c(14, 16)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "RKCRABNS" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 0 catch

# 47 RNGRENIIIa
a <- filter(stocks_data, stockid == "RNGRENIIIa")
stocks_data <- stocks_data %>%
  filter(!(stockid == "RNGRENIIIa" & Year > 2006)) # change end time to 2006, rerun

# 48 RSOLE5AB
a <- filter(stocks_data, stockid == "RSOLE5AB")
stocks_data <- stocks_data %>%
  filter(!(stockid == "RSOLE5AB" & Year < 1947)) # change start time to 1947, rerun

# 49 SBELLYROCKPCOAST
a <- filter(stocks_data, stockid == "SBELLYROCKPCOAST")
b <- mean(a$Catch[c(53, 55)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "SBELLYROCKPCOAST" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch after 2001 for 0 catch

# 50 SCALLNBB
a <- filter(stocks_data, stockid == "SCALLNBB")
b <- mean(a$Catch[c(6, 8)])
c <- mean(a$Catch[c(28, 30)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "SCALLNBB" & Catch == 0 & Year == 1987 ~ b,
    stockid == "SCALLNBB" & Catch == 0 & Year == 2009 ~ c,
    TRUE ~ Catch
  )) # use mean catch after 2001 for 0 catch

# 51 SMOOTHOREOBP
a <- filter(stocks_data, stockid == "SMOOTHOREOBP")
stocks_data <- stocks_data %>%
  filter(!(stockid == "SMOOTHOREOBP" & Year < 1993)) # change start time to 1993, rerun

# 52 SMOOTHOREOEPR
a <- filter(stocks_data, stockid == "SMOOTHOREOEPR") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "SMOOTHOREOEPR")) # delete stock

# 53 SMOOTHOREOSLD
a <- filter(stocks_data, stockid == "SMOOTHOREOSLD")
stocks_data <- stocks_data %>%
  filter(!(stockid == "SMOOTHOREOSLD" & Year < 1984)) # change start time to 1984, rerun

# 54 SNOWCRAB4X
a <- filter(stocks_data, stockid == "SNOWCRAB4X")
b <- mean(a$Catch[c(2, 4)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "SNOWCRAB4X" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean landing for 0 landing 1996

# 55 SSSHRIMPSMA16
a <- filter(stocks_data, stockid == "SSSHRIMPSMA16")
stocks_data <- stocks_data %>%
  filter(!(stockid == "SSSHRIMPSMA16" & Year < 1994)) # change start time to 1994, rerun

# 56 STRSHRIMPNUNE
a <- filter(stocks_data, stockid == "STRSHRIMPNUNE")
stocks_data <- stocks_data %>%
  filter(!(stockid == "STRSHRIMPNUNE" & Year < 1988)) # change start time to 1988, rerun

# 57 STRSHRIMPSFA4
a <- filter(stocks_data, stockid == "STRSHRIMPSFA4")
stocks_data <- stocks_data %>%
  filter(!(stockid == "STRSHRIMPSFA4" & Year < 2004)) # change start time to 2004, rerun

# 58 SWHITSE
a <- filter(stocks_data, stockid == "SWHITSE")
b <- mean(a$Catch[c(2, 4)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "SWHITSE" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean landing for 0 landing 1996

# 59 TANNERCRABBSAI
a <- filter(stocks_data, stockid == "TANNERCRABBSAI") # too many 0 in catch
stocks_data <- stocks_data %>%
  filter(!(stockid == "TANNERCRABBSAI")) # delete stock

# 60 TARAKNZ
a <- filter(stocks_data, stockid == "TARAKNZ")
stocks_data <- stocks_data %>%
  filter(!(stockid == "TARAKNZ" & Year < 1947)) # change start time to 1947, rerun

# 61 TILEGM
a <- filter(stocks_data, stockid == "TILEGM")
b <- mean(a$Catch[c(5, 7)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "TILEGM" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 0 catch 1971

# 62 TILEMATLC
a <- filter(stocks_data, stockid == "TILEMATLC")
stocks_data <- stocks_data %>%
  filter(!(stockid == "TILEMATLC" & Year < 1971)) # change start time to 1971, rerun

# 63 WHAKE4T
a <- filter(stocks_data, stockid == "WHAKE4T") # survey abundance -2000
stocks_data <- stocks_data %>%
  mutate(Survey_abundance = case_when(
    stockid == "WHAKE4T" & Survey_abundance == -2000 ~ NA,
    TRUE ~ Survey_abundance
  )) # change -2000 to NA

# 64 WPOLLBCWS
a <- filter(stocks_data, stockid == "WPOLLBCWS")
b <- mean(a$Catch[c(31, 32, 36, 37)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "WPOLLBCWS" & Catch == 0 ~ b,
    TRUE ~ Catch
  )) # use mean catch for 0 catch 1998-2000 
stocks_data <- stocks_data %>%
  filter(!(stockid == "WPOLLBCWS" & Year < 1989)) # change start time to 1989, rerun

# 65 WPOLLWBS
a <- filter(stocks_data, stockid == "WPOLLWBS")
stocks_data <- stocks_data %>%
  filter(!(stockid == "WPOLLWBS" & Year > 2014)) # change end time to 2014, rerun

# 66 YELL4T
a <- filter(stocks_data, stockid == "YELL4T")
b <- mean(a$Catch[c(13, 15)])
c <- mean(a$Catch[c(15, 17)])
d <- mean(a$Catch[c(18, 20)])
stocks_data <- stocks_data %>%
  mutate(Catch = case_when(
    stockid == "YELL4T" & Catch == 0 & Year == 1973 ~ b,
    stockid == "YELL4T" & Catch == 0 & Year == 1975 ~ c,
    stockid == "YELL4T" & Catch == 0 & Year == 1978 ~ d,
    TRUE ~ Catch
  )) # use mean catch for 0 catch

#save data
write_rds(stocks_data,"Data/stocks_data_for_JABBA.rds")


# 3 Preliminary run, 2000 iterations, for checking revised data ----------------------------------------------------------------
# load JABBA function
load("Functions/function_JABBA_iteration_2000.R")

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

# parallel JABBA run
jabba_information <- foreach(
  x = stocks_list[1:742],
  .combine = "rbind",
  .packages = c("tidyverse", "JABBA")
) %dopar% {
  # run JABBA
  try_JABBA <- try(function_JABBA_2000(
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

write_rds(stock_success, file = "Data/stock_success.rds") # the data are fine with JABBA





# test -------------------------------------------------------------------
load("Outputs/JABBA results/ACMACKSARG_seed1_Fox_jabba.rdata")
jbplot_ppdist(jabba)

