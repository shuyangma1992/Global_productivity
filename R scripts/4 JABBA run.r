library(tidyverse)
library(JABBA)
library(foreach)
library(doParallel)

# 1 single run ------------------------------------------------------------
# load JABBA function
load("Functions/function_JABBA.R")

# stocks data
stocks_data <- read_rds("Data/stocks_data.rds")
stocks_list <- unique(stocks_data$stockid) #745 stocks

# species information
species_information <- read_rds("Data/fishbase_information.rds") 

#prior r
prior_r <- read_rds("Data/prior_r.rds")

#prior K
prior_K <- read_rds("Data/prior_K.rds")

#prior psi
prior_psi <- read_rds("Data/prior_psi.rds")

model_run <- NULL
for (i in stocks_list) {
  
  # i=stocks_list[1]
  # i="ACADRED2J3K"
  # i="HERRHG"
  try_JABBA <- try(function_JABBA(stock_id = i,seed = 1,stocks_time_series = stocks_data,
                                  prior_r = prior_r,prior_K = prior_K, prior_psi = prior_psi))
  model_run_cycle <- data.frame(stock_id=i, 
                                success_or_failure=ifelse(is.character(try_JABBA[1]),
                                                          try_JABBA[1],"success"))
  model_run <- bind_rows(model_run,model_run_cycle)
}

write_rds(model_run,file = "0_model_run.rds")

model_run %>% 
  filter(success_or_failure!="success")

# check stocks with failure
model_run <- read_rds("0_model_run.rds")

stock_failure <- model_run %>% 
  filter(success_or_failure!="success")

stock_success <- model_run %>% 
  filter(success_or_failure=="success")

write_rds(stock_success,file="Data/stock_success.rds")


# 2 parallel run ----------------------------------------------------------------
# load JABBA function
load("Functions/function_JABBA.R")

# stocks data
stocks_data <- read_rds("Data/stocks_data.rds")
stocks_list <- unique(stocks_data$stockid) #751 stocks

# species information
species_information <- read_rds("Data/fishbase_information.rds") 

#prior r
prior_r <- read_rds("Data/prior_r.rds")

#prior K
prior_K <- read_rds("Data/prior_K.rds")

#prior psi
prior_psi <- read_rds("Data/prior_psi.rds")

#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

jabba_information <- foreach(x=stocks_list[1:751],
                             .combine = "rbind",
                             .packages = c("tidyverse","JABBA")) %dopar% {
                               
  #run JABBA
  try_JABBA <- try(function_JABBA(stock_id = x,
                                  seed = 1,
                                  stocks_time_series = stocks_data,
                                  prior_r = prior_r,
                                  prior_K = prior_K, 
                                  prior_psi = prior_psi))
  
  #success or failure
  model_run_cycle <- data.frame(stock_id=x, 
                                success_or_failure=ifelse(is.character(try_JABBA[1]),
                                                          try_JABBA[1],"success"))
  
  }

#save information
write_rds(jabba_information,file="JABBA results/0_jabba_information.rds")

#stop cluster
stopCluster(cl)

# check stocks with failure
model_information <- read_rds("JABBA results/0_jabba_information.rds")

stock_failure <- model_information %>% 
  filter(success_or_failure!="success")

stock_success <- model_information %>% 
  filter(success_or_failure=="success")

write_rds(stock_success,file="Data/stock_success.rds")


# 3 After run check -------------------------------------------------------
#model information
model_information <- read_rds("JABBA results/0_jabba_information.rds")

stock_failure <- model_information %>% 
  filter(success_or_failure!="success")

stock_success <- read_rds("Data/stock_success.rds")

# stocks data
stocks_data <- read_rds("Data/stocks_data.rds")

# stocks with failure
#1 ALSKABSAI
a <- filter(stocks_data,stockid=="ALSKABSAI") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="ALSKABSAI"&Year<1965)) # change start time to 1965, rerun

#2 AMPL3M
a <- filter(stocks_data,stockid=="AMPL3M") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="AMPL3M"&Year>2003)) # change end time to 2003, rerun

#3 AMPL3M
a <- filter(stocks_data,stockid=="ANCHOBAYB") 
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="ANCHOBAYB"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 2007, 2008, 2009
#4 ANCHOSA
a <- filter(stocks_data,stockid=="ANCHOSA") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="ANCHOSA"&Year<1964)) # change start time to 1964, rerun

#5 ARCSURFBANQ
a <- filter(stocks_data,stockid=="ARCSURFBANQ") 
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="ARCSURFBANQ"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 1992

#6 BKINGCRABPI
a <- filter(stocks_data,stockid=="BKINGCRABPI") #too many 0 in catch

#7 BKINGCRABSMI
a <- filter(stocks_data,stockid=="BKINGCRABSMI") #too many 0 in catch

#8 BLACKOREOPR
a <- filter(stocks_data,stockid=="BLACKOREOPR") #too many 0 in catch

#9 BLTILESATLC
a <- filter(stocks_data,stockid=="BLTILESATLC") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="BLTILESATLC"&Year<1962)) # change start time to 1962, rerun

#10 CAPEIIa-V-XIV
a <- filter(stocks_data,stockid=="CAPEIIa-V-XIV") 
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="CAPEIIa-V-XIV"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 1982, 2018, 2019

#11 CAPENOR
a <- filter(stocks_data,stockid=="CAPENOR") #too many 0 in catch

#12 CHAKESA
a <- filter(stocks_data,stockid=="CHAKESA") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="CHAKESA"&Year<1921)) # change start time to 1921, rerun

#13 CHROCKCPCOAST
a <- filter(stocks_data,stockid=="CHROCKCPCOAST") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="CHROCKCPCOAST"&Year<1973)) # change start time to 1973, rerun

#14 CMACKPCOAST
a <- filter(stocks_data,stockid=="CMACKPCOAST") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="CMACKPCOAST"&Year<1983)) # change start time to 1983, rerun

#15 COD3Ps
a <- filter(stocks_data,stockid=="COD3Ps") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="COD3Ps"&Year<1980)) # change start time to 1980, rerun

#16 COD4TVn
a <- filter(stocks_data,stockid=="COD4TVn") 
mean(a$Catch[85:102])
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="COD4TVn"&Catch==0~mean(a$Catch[85:102]),
                         TRUE ~ Catch)) # use mean catch (after 2001) for 2009-2012

#17 EULAPCOASTCCDU
a <- filter(stocks_data,stockid=="EULAPCOASTCCDU") #too many 0 in catch

#18 GHAL01ABCDEF
a <- filter(stocks_data,stockid=="GHAL01ABCDEF") #CPUE wrong

#19 GSSMELTVIb-VII-VIII-IX-X-XII
a <- filter(stocks_data,stockid=="GSSMELTVIb-VII-VIII-IX-X-XII") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="GSSMELTVIb-VII-VIII-IX-X-XII"&Year<1978)) # change start time to 1978, rerun
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="GSSMELTVIb-VII-VIII-IX-X-XII"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 0

#20 GSTRGZRSTA7
a <- filter(stocks_data,stockid=="GSTRGZRSTA7") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="GSTRGZRSTA7"&Year<1967)) # change start time to 1967, rerun

#21 GURCH4RST
a <- filter(stocks_data,stockid=="GURCH4RST") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="GURCH4RST"&Year<1994)) # change start time to 1994, rerun

#22 HADGB
a <- filter(stocks_data,stockid=="HADGB") 
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="HADGB"&is.na(Catch)~Landing,
                         TRUE ~ Catch)) # use landing as catch for 1989-2010

#23 HERRCC
a <- filter(stocks_data,stockid=="HERRCC") #too many 0 in catch

#24 HERRHG
a <- filter(stocks_data,stockid=="HERRHG") #too many 0 in catch

#25 HERRPRD
a <- filter(stocks_data,stockid=="HERRPRD") 
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="HERRPRD"&Catch==0~Landing,
                         TRUE ~ Catch)) # use landing for 0 catch
#26 HERRPWS
a <- filter(stocks_data,stockid=="HERRPWS") #too many 0 in catch

#27 HERRWCVANI
a <- filter(stocks_data,stockid=="HERRWCVANI") #too many 0 in catch

#28 ILSCALL3Ps
a <- filter(stocks_data,stockid=="ILSCALL3Ps") #too many 0 in catch

#29 LUMP3Pn4RS
a <- filter(stocks_data,stockid=="LUMP3Pn4RS") #too many 0 in catch
mean(a$Landing)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="LUMP3Pn4RS"&Landing==0~mean(a$Landing),
                         TRUE ~ Landing)) # use mean landing for 0 landing

#30 NEPHFU31
a <- filter(stocks_data,stockid=="NEPHFU31") #too many 0 in catch
mean(a$Catch[29:36])
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="NEPHFU31"&Catch==0~mean(a$Catch[29:36]),
                         TRUE ~ Catch)) # use mean catch after 2011 for 0 catch 2017

#31 NZLINGLIN6b
a <- filter(stocks_data,stockid=="NZLINGLIN6b") #too many 0 in catch
stocks_data <- stocks_data %>% 
  filter(!(stockid=="NZLINGLIN6b"&Year<1989)) # change start time to 1989, rerun

#32 NZLINGWSE
a <- filter(stocks_data,stockid=="NZLINGWSE") #too many 0 in catch
stocks_data <- stocks_data %>% 
  filter(!(stockid=="NZLINGWSE"&Year<1985)) # change start time to 1985, rerun

#33 OROUGHYCASCADE     
a <- filter(stocks_data,stockid=="OROUGHYCASCADE") #too many 0 in catch

#34 OROUGHYNZ7A
a <- filter(stocks_data,stockid=="OROUGHYNZ7A") #too many 0 in catch

#35 OWSHARCWPAC
a <- filter(stocks_data,stockid=="OWSHARCWPAC") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="OWSHARCWPAC"&Year>2010)) # change end time to 2010, rerun

#36 PANDALGOM
a <- filter(stocks_data,stockid=="PANDALGOM") 
mean(a$Landing[30:37])
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="PANDALGOM"&Landing==0~mean(a$Landing[30:37]),
                         TRUE ~ Landing)) # use mean landing after 2013 for 0 landing 2018 and 2020

#37 PANDALNUNE
a <- filter(stocks_data,stockid=="PANDALNUNE") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALNUNE"&Year<1988)) # change start time to 1988, rerun

#38 PANDALSFA12
a <- filter(stocks_data,stockid=="PANDALSFA12")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALSFA12"&Year<1982)) # change start time to 1982, rerun

#39 PANDALSFA2-3
a <- filter(stocks_data,stockid=="PANDALSFA2-3")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALSFA2-3"&Year<1988)) # change start time to 1988, rerun

#40 PANDALSFA4
a <- filter(stocks_data,stockid=="PANDALSFA4")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALSFA4"&Year<1988)) # change start time to 1988, rerun
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="PANDALSFA4"&is.na(Catch)~Landing,
                           TRUE ~ Catch)) # use landing for catch after 2012

#41 PANDALSMA16
a <- filter(stocks_data,stockid=="PANDALSMA16")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALSMA16"&Year<1994)) # change start time to 1994, rerun

#42 PANDALSMA18-19
a <- filter(stocks_data,stockid=="PANDALSMA18-19")
mean(a$Landing,na.rm = T)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="PANDALSMA18-19"&Landing==0~mean(a$Landing,na.rm = T),
                         TRUE ~ Landing)) # use mean landing for 0 landing

#43 PANDALSMAGTSE
a <- filter(stocks_data,stockid=="PANDALSMAGTSE")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PANDALSMAGTSE"&Year<1991)) # change start time to 1994, rerun

#44 PORSHARATL
a <- filter(stocks_data,stockid=="PORSHARATL")
mean(a$Landing,na.rm = T)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="PORSHARATL"&Landing==0~mean(a$Landing,na.rm = T),
                           TRUE ~ Landing)) # use mean landing for 0 landing

#45 PTOOTHFISHMI
a <- filter(stocks_data,stockid=="PTOOTHFISHMI")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="PTOOTHFISHMI"&Year<1994)) # change start time to 1994, rerun
a <- filter(stocks_data,stockid=="PTOOTHFISHMI")
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="PTOOTHFISHMI"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 0 catch 

#46 RKCRABNS
a <- filter(stocks_data,stockid=="RKCRABNS")
mean(a$Catch)
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="RKCRABNS"&Catch==0~mean(a$Catch),
                         TRUE ~ Catch)) # use mean catch for 0 catch 

#47 RNGRENIIIa
a <- filter(stocks_data,stockid=="RNGRENIIIa")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="RNGRENIIIa"&Year>2006)) # change end time to 2006, rerun

#48 RSOLE5AB
a <- filter(stocks_data,stockid=="RSOLE5AB")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="RSOLE5AB"&Year<1947)) # change start time to 1947, rerun

#49 SBELLYROCKPCOAST
a <- filter(stocks_data,stockid=="SBELLYROCKPCOAST")
mean(a$Catch[52:56])
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="SBELLYROCKPCOAST"&Catch==0~mean(a$Catch[52:56]),
                         TRUE ~ Catch)) # use mean catch after 2001 for 0 catch 

#50 SCALLNBB
a <- filter(stocks_data,stockid=="SCALLNBB")
mean(a$Catch[1:10])
mean(a$Catch[21:30])
stocks_data <- stocks_data %>% 
  mutate(Catch=case_when(stockid=="SCALLNBB"&Catch==0&Year==1987~mean(a$Catch[1:10]),
                         stockid=="SCALLNBB"&Catch==0&Year==2009~mean(a$Catch[21:30]),
                         TRUE ~ Catch)) # use mean catch after 2001 for 0 catch 

#51 SMOOTHOREOBP
a <- filter(stocks_data,stockid=="SMOOTHOREOBP")
stocks_data <- stocks_data %>% 
  filter(!(stockid=="SMOOTHOREOBP"&Year<1993)) # change start time to 1993, rerun

#52 SMOOTHOREOEPR
a <- filter(stocks_data,stockid=="SMOOTHOREOEPR") #too many 0 in catch

#53 SMOOTHOREOSLD
a <- filter(stocks_data,stockid=="SMOOTHOREOSLD") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="SMOOTHOREOSLD"&Year<1984)) # change start time to 1984, rerun

#54 SNOWCRAB4X
a <- filter(stocks_data,stockid=="SNOWCRAB4X") 
mean(a$Landing)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="SNOWCRAB4X"&Landing==0~mean(a$Landing),
                         TRUE ~ Landing)) # use mean landing for 0 landing 1996

#55 SSSHRIMPSMA16
a <- filter(stocks_data,stockid=="SSSHRIMPSMA16") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="SSSHRIMPSMA16"&Year<1994)) # change start time to 1994, rerun

#56 STRSHRIMPNUNE
a <- filter(stocks_data,stockid=="STRSHRIMPNUNE") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="STRSHRIMPNUNE"&Year<1988)) # change start time to 1988, rerun

#57 STRSHRIMPSFA4
a <- filter(stocks_data,stockid=="STRSHRIMPSFA4") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="STRSHRIMPSFA4"&Year<2004)) # change start time to 2004, rerun

#58 SWHITSE
a <- filter(stocks_data,stockid=="SWHITSE") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="SWHITSE"&Year<1945)) # change start time to 1945, rerun

#59 TANNERCRABBSAI
a <- filter(stocks_data,stockid=="TANNERCRABBSAI") #too many 0 in catch

#60 TARAKNZ
a <- filter(stocks_data,stockid=="TARAKNZ") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="TARAKNZ"&Year<1947)) # change start time to 1947, rerun

#61 TILEGM
a <- filter(stocks_data,stockid=="TILEGM") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="TILEGM"&Year<1971)) # change start time to 1971, rerun

#62 TILEMATLC
a <- filter(stocks_data,stockid=="TILEMATLC") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="TILEMATLC"&Year<1971)) # change start time to 1971, rerun

#63 WHAKE4T
a <- filter(stocks_data,stockid=="WHAKE4T") #survey abundance -2000
stocks_data <- stocks_data %>% 
  mutate(Survey_abundance=case_when(stockid=="WHAKE4T"&Survey_abundance==-2000~NA,
                           TRUE ~ Survey_abundance)) # use mean landing for 0 landing 1996

#64 WPOLLBCWS
a <- filter(stocks_data,stockid=="WPOLLBCWS") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="WPOLLBCWS"&Year<1989)) # change start time to 1989, rerun
a <- filter(stocks_data,stockid=="WPOLLBCWS") 
mean(a$Landing)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="WPOLLBCWS"&Landing==0~mean(a$Landing),
                                    TRUE ~ Landing)) # use mean landing for 0 landing 

#65 WPOLLWBS
a <- filter(stocks_data,stockid=="WPOLLWBS") 
stocks_data <- stocks_data %>% 
  filter(!(stockid=="WPOLLWBS"&Year>2014)) # change end time to 2014, rerun

#66 YELL4T
a <- filter(stocks_data,stockid=="YELL4T") 
mean(a$Landing)
stocks_data <- stocks_data %>% 
  mutate(Landing=case_when(stockid=="YELL4T"&Landing==0~mean(a$Landing),
                           TRUE ~ Landing)) # use mean landing for 0 landing 

#save data
write_rds(stocks_data,"Data/stocks_data.rds")



# 4 JABBA rerun for failure stocks ----------------------------------------
#prior r
prior_r <- read_rds("Data/prior_r.rds")

#prior K
prior_K <- read_rds("Data/prior_K.rds")

#prior psi
prior_psi <- read_rds("Data/prior_psi.rds")

#1 ALSKABSAI
function_JABBA(stock_id = "ALSKABSAI",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="ALSKABSAI",success_or_failure="success")

#2 AMPL3M
function_JABBA(stock_id = "AMPL3M",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="AMPL3M",success_or_failure="success")

#3 ANCHOBAYB
function_JABBA(stock_id = "ANCHOBAYB",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="ANCHOBAYB",success_or_failure="success")

#4 ANCHOSA
function_JABBA(stock_id = "ANCHOSA",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="ANCHOSA",success_or_failure="success")

#5 ARCSURFBANQ
function_JABBA(stock_id = "ARCSURFBANQ",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="ARCSURFBANQ",success_or_failure="success")

#9 BLTILESATLC
function_JABBA(stock_id = "BLTILESATLC",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="BLTILESATLC",success_or_failure="success")

#10 CAPEIIa-V-XIV
function_JABBA(stock_id = "CAPEIIa-V-XIV",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="CAPEIIa-V-XIV",success_or_failure="success")

#12 CHAKESA
function_JABBA(stock_id = "CHAKESA",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="CHAKESA",success_or_failure="success")

#13 CHROCKCPCOAST
function_JABBA(stock_id = "CHROCKCPCOAST",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="CHROCKCPCOAST",success_or_failure="success")

#14 CMACKPCOAST
function_JABBA(stock_id = "CMACKPCOAST",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="CMACKPCOAST",success_or_failure="success")

#15 COD3Ps
function_JABBA(stock_id = "COD3Ps",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="COD3Ps",success_or_failure="success")

#16 COD4TVn
function_JABBA(stock_id = "COD4TVn",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="COD4TVn",success_or_failure="success")

#19 GSSMELTVIb-VII-VIII-IX-X-XII
function_JABBA(stock_id = "GSSMELTVIb-VII-VIII-IX-X-XII",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="GSSMELTVIb-VII-VIII-IX-X-XII",success_or_failure="success")

#20 GSTRGZRSTA7
function_JABBA(stock_id = "GSTRGZRSTA7",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="GSTRGZRSTA7",success_or_failure="success")

#21 GURCH4RST
function_JABBA(stock_id = "GURCH4RST",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="GURCH4RST",success_or_failure="success")

#22 HADGB
function_JABBA(stock_id = "HADGB",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="HADGB",success_or_failure="success")

#25 HERRPRD
function_JABBA(stock_id = "HERRPRD",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="HERRPRD",success_or_failure="success")

#29 LUMP3Pn4RS
function_JABBA(stock_id = "LUMP3Pn4RS",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="LUMP3Pn4RS",success_or_failure="success")

#30 NEPHFU31
function_JABBA(stock_id = "NEPHFU31",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="NEPHFU31",success_or_failure="success")

#31 NZLINGLIN6b
function_JABBA(stock_id = "NZLINGLIN6b",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="NZLINGLIN6b",success_or_failure="success")

#32 NZLINGWSE
function_JABBA(stock_id = "NZLINGWSE",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="NZLINGWSE",success_or_failure="success")

#35 OWSHARCWPAC
function_JABBA(stock_id = "OWSHARCWPAC",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="OWSHARCWPAC",success_or_failure="success")

#36 PANDALGOM
function_JABBA(stock_id = "PANDALGOM",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALGOM",success_or_failure="success")

#37 PANDALNUNE
function_JABBA(stock_id = "PANDALNUNE",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALNUNE",success_or_failure="success")

#38 PANDALSFA12
function_JABBA(stock_id = "PANDALSFA12",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSFA12",success_or_failure="success")

#39 PANDALSFA2-3
function_JABBA(stock_id = "PANDALSFA2-3",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSFA2-3",success_or_failure="success")

#40 PANDALSFA4
function_JABBA(stock_id = "PANDALSFA4",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSFA4",success_or_failure="success")

#41 PANDALSMA16
function_JABBA(stock_id = "PANDALSMA16",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSMA16",success_or_failure="success")

#42 PANDALSMA18-19
function_JABBA(stock_id = "PANDALSMA18-19",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSMA18-19",success_or_failure="success")

#43 PANDALSMAGTSE
function_JABBA(stock_id = "PANDALSMAGTSE",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PANDALSMAGTSE",success_or_failure="success")

#44 PORSHARATL
function_JABBA(stock_id = "PORSHARATL",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PORSHARATL",success_or_failure="success")


#45 PTOOTHFISHMI
function_JABBA(stock_id = "PTOOTHFISHMI",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="PTOOTHFISHMI",success_or_failure="success")

#46 RKCRABNS
function_JABBA(stock_id = "RKCRABNS",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="RKCRABNS",success_or_failure="success")

#47 RNGRENIIIa
function_JABBA(stock_id = "RNGRENIIIa",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="RNGRENIIIa",success_or_failure="success")

#48 RSOLE5AB
function_JABBA(stock_id = "RSOLE5AB",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="RSOLE5AB",success_or_failure="success")

#49 SBELLYROCKPCOAST
function_JABBA(stock_id = "SBELLYROCKPCOAST",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SBELLYROCKPCOAST",success_or_failure="success")

#50 SCALLNBB
function_JABBA(stock_id = "SCALLNBB",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SCALLNBB",success_or_failure="success")

#51 SMOOTHOREOBP
function_JABBA(stock_id = "SMOOTHOREOBP",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SMOOTHOREOBP",success_or_failure="success")

#53 SMOOTHOREOSLD
function_JABBA(stock_id = "SMOOTHOREOSLD",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SMOOTHOREOSLD",success_or_failure="success")

#54 SNOWCRAB4X
function_JABBA(stock_id = "SNOWCRAB4X",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SNOWCRAB4X",success_or_failure="success")

#55 SSSHRIMPSMA16
function_JABBA(stock_id = "SSSHRIMPSMA16",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SSSHRIMPSMA16",success_or_failure="success")

#56 STRSHRIMPNUNE
function_JABBA(stock_id = "STRSHRIMPNUNE",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="STRSHRIMPNUNE",success_or_failure="success")

#57 STRSHRIMPSFA4
function_JABBA(stock_id = "STRSHRIMPSFA4",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="STRSHRIMPSFA4",success_or_failure="success")

#58 SWHITSE
function_JABBA(stock_id = "SWHITSE",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="SWHITSE",success_or_failure="success")

#60 TARAKNZ
function_JABBA(stock_id = "TARAKNZ",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="TARAKNZ",success_or_failure="success")

#61 TILEGM
function_JABBA(stock_id = "TILEGM",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="TILEGM",success_or_failure="success")

#62 TILEMATLC
function_JABBA(stock_id = "TILEMATLC",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="TILEMATLC",success_or_failure="success")

#63 WHAKE4T
function_JABBA(stock_id = "WHAKE4T",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="WHAKE4T",success_or_failure="success")

#64 WPOLLBCWS
function_JABBA(stock_id = "WPOLLBCWS",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="WPOLLBCWS",success_or_failure="success")

#65 WPOLLWBS
function_JABBA(stock_id = "WPOLLWBS",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="WPOLLWBS",success_or_failure="success")

#66 YELL4T
function_JABBA(stock_id = "YELL4T",
               seed = 1,
               stocks_time_series = stocks_data,
               prior_r = prior_r,
               prior_K = prior_K, 
               prior_psi = prior_psi)

stock_success <- stock_success %>% 
  add_row(stock_id="YELL4T",success_or_failure="success")

#save data
write_rds(stock_success,"Data/stock_success.rds")

