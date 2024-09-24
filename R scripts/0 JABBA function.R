library(tidyverse)
library(JABBA)
library(readxl)

# the function to 1) make JABBA data, 2) fit JABBA model, and 3) return results
# including 4 functions with increasing iterations, for model convergence

# 1 JABBA function 20000 iterations ---------------------------------------
#stocks time series
stocks_time_series <- read_rds("Data/stocks_data.rds")

a <- filter(stocks_time_series,Catch==0)
#prior r
prior_r <- read_rds("Data/prior_r.rds")

#prior K
prior_K <- read_rds("Data/prior_K.rds")

#prior psi
prior_psi <- read_rds("Data/prior_psi.rds")

#test data
stock_id=unique(stocks_time_series$stockid)[4]
stock_id="ALBAMED"
seed=1
stock_id="HERRHG"
stock_id="CMACKPCOAST"
stock_id="SPHAKECH"
stock_id="ACMACKSARG"


#function code
function_JABBA <- function(stock_id,seed,stocks_time_series,prior_r,prior_K,prior_psi){
  
  #-----------------------------------------------------------------------1 data
  time_series_stock <- filter(stocks_time_series,stockid==stock_id)
  prior_r_stock <- filter(prior_r,stockid==stock_id)
  prior_K_stock <- filter(prior_K,stockid==stock_id)
  prior_psi_stock <- filter(prior_psi,stockid==stock_id)
  data <- time_series_stock %>% 
    select(stockid,Year,Catch,Landing,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance)

  #data part 1 - catch data
  #give priority to catch, if not have, use landing
  catch <- data %>% 
    select(Year,Catch) %>% 
    mutate(Catch=na_if(Catch,0)) %>% 
    drop_na()
  
  landing <- data %>% 
    select(Year,Landing) %>% 
    mutate(Landing=na_if(Landing,0)) %>% 
    drop_na()
  
  if(length(catch$Year)>=length(landing$Year)){
    
    catch <- catch
    catch_information <- "Catch" 
    
  } else {
    
    catch <- landing
    catch_information <- "Landing"
    
  }
  colnames(catch)[-1] <-paste0("catch_index_",colnames(catch)[-1])
  
  #data part 2 - cpue data
  #use all we have: total biomass, total abundance, SSB, CPUE and survey abundance
  not_all_na <- function(x) any(!is.na(x))
  cpue <- data %>% 
    select(Year,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance) %>% 
    select(where(not_all_na))
  cpue_information <- paste(colnames(cpue)[2],colnames(cpue)[3],colnames(cpue)[4],colnames(cpue)[5],colnames(cpue)[6])
  colnames(cpue)[-1] <-paste0("cpue_index_",colnames(cpue)[-1])
  
  #data part 3 - cpue se
  #no se data
  cpue_SE_information <- "None"
  
  #unify data length
  data_unify_length <- left_join(catch,cpue) %>% 
    mutate(Year=as.numeric(Year))
  catch <- data_unify_length %>% 
    select(Year,starts_with("catch_index"))
  catch <- as.data.frame(catch)

  cpue <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) %>% 
    mutate(across(starts_with("cpue_index"),~na_if(.,0))) #change 0 to NA
  cpue <- as.data.frame(cpue)
  
  cpue_se <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) 
  cpue_se[,-1] <- NA
  cpue_se <- as.data.frame(cpue_se)
  
  #----------------------------------------------------------------------2 prior
  #prior part 1 - r prior: mean and logsd
  r_mean <- prior_r_stock$r_mean
  r_sd <- prior_r_stock$r_sd
  
  #prior part 2 - K prior: mean and CV
  K_mean <- prior_K_stock$K_mean
  K_CV <- prior_K_stock$K_CV
  
  #prior part 3 - psi prior: mean and CV
  psi_mean <- prior_psi_stock$psi_mean
  psi_CV <- prior_psi_stock$psi_CV
  
  #-----------------------------------------------------------3 build JABBA data
  #Schaefer
  # seed=1
  jbinput_Schaefer <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                                  assessment = paste0(stock_id,"_seed",seed),scenario = "Schaefer",
                                  model.type = "Schaefer",
                                  r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                                  K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                                  psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                                  sigma.est = TRUE, # estimate additional observation error
                                  fixed.obsE = 0.1, # mimum observation error
                                  igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                                  verbose=F)
  #Fox
  jbinput_Fox <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                             assessment = paste0(stock_id,"_seed",seed),scenario = "Fox",
                             model.type = "Fox",
                             r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                             K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                             psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                             sigma.est = TRUE, # estimate additional observation error
                             fixed.obsE = 0.1, # mimum observation error
                             igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                             verbose=F)
  #Pella-Tomlinson
  jbinput_Pella <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                               assessment =paste0(stock_id,"_seed",seed),scenario = "Pella",
                               model.type = "Pella",
                               r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                               K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                               psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                               sigma.est = TRUE, # estimate additional observation error
                               fixed.obsE = 0.1, # mimum observation error
                               igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                               verbose=F)

  #------------------------------------------------------------------4 fit JABBA
  #output directory
  work_dir <- getwd()
  output_dir <- paste0(work_dir,"/JABBA results/")
  fit_Schaefer <- fit_jabba(jbinput_Schaefer,save.jabba=TRUE,output.dir=output_dir,
                            ni=20000,nt=5,nb=10000,nc=3,seed=seed)
  fit_Fox <- fit_jabba(jbinput_Fox,save.jabba=TRUE,output.dir=output_dir,
                       ni=20000,nt=5,nb=10000,nc=3,seed=seed)
  fit_Pella <- fit_jabba(jbinput_Pella,save.jabba=TRUE,output.dir=output_dir,
                         ni=20000,nt=5,nb=10000,nc=3,seed=seed)

  #############################################################################5 save results
  #data_information
  data_information <- data.frame(Stockname=stock_id,
                                 Year_start=min(catch$Year),
                                 Year_end=max(catch$Year),
                                 catch_information=catch_information,
                                 cpue_information=cpue_information,
                                 cpue_SE_information=cpue_SE_information)
  
  #model information
  model_information_Schaefer <- fit_Schaefer$stats %>% 
    mutate(Model="Schaefer")
  model_information_Fox <- fit_Fox$stats %>% 
    mutate(Model="Fox")
  model_information_Pella <- fit_Pella$stats %>% 
    mutate(Model="Pella")
  model_information <- bind_rows(model_information_Schaefer,
                                 model_information_Fox,
                                 model_information_Pella)
  
  #combine the two results
  results <- list(data_information,model_information)
  
  #save as rds file
  file_name <- paste0(output_dir,stock_id,"seed_",seed,"_JABBA information.rds")
  write_rds(results,file=file_name)
 
}
  
#save function
save(function_JABBA,file="Functions/function_JABBA.R")

#test
function_JABBA(stock_id,seed,stocks_time_series,prior_r,prior_K,prior_psi)









# 2 JABBA function 60000 iterations, rerun function for stocks wit --------
#function code
function_JABBA_60000 <- function(stock_id,seed,stocks_time_series,prior_r,prior_K,prior_psi){
  
  #-----------------------------------------------------------------------1 data
  time_series_stock <- filter(stocks_time_series,stockid==stock_id)
  prior_r_stock <- filter(prior_r,stockid==stock_id)
  prior_K_stock <- filter(prior_K,stockid==stock_id)
  prior_psi_stock <- filter(prior_psi,stockid==stock_id)
  data <- time_series_stock %>% 
    select(stockid,Year,Catch,Landing,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance)
  
  #data part 1 - catch data
  #give priority to catch, if not have, use landing
  catch <- data %>% 
    select(Year,Catch) %>% 
    mutate(Catch=na_if(Catch,0)) %>% 
    drop_na()
  
  landing <- data %>% 
    select(Year,Landing) %>% 
    mutate(Landing=na_if(Landing,0)) %>% 
    drop_na()
  
  if(length(catch$Year)>=length(landing$Year)){
    
    catch <- catch
    catch_information <- "Catch" 
    
  } else {
    
    catch <- landing
    catch_information <- "Landing"
    
  }
  colnames(catch)[-1] <-paste0("catch_index_",colnames(catch)[-1])
  
  #data part 2 - cpue data
  #use all we have: total biomass, total abundance, SSB, CPUE and survey abundance
  not_all_na <- function(x) any(!is.na(x))
  cpue <- data %>% 
    select(Year,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance) %>% 
    select(where(not_all_na))
  cpue_information <- paste(colnames(cpue)[2],colnames(cpue)[3],colnames(cpue)[4],colnames(cpue)[5],colnames(cpue)[6])
  colnames(cpue)[-1] <-paste0("cpue_index_",colnames(cpue)[-1])
  
  #data part 3 - cpue se
  #no se data
  cpue_SE_information <- "None"
  
  #unify data length
  data_unify_length <- left_join(catch,cpue) %>% 
    mutate(Year=as.numeric(Year))
  catch <- data_unify_length %>% 
    select(Year,starts_with("catch_index"))
  catch <- as.data.frame(catch)
  
  cpue <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) %>% 
    mutate(across(starts_with("cpue_index"),~na_if(.,0))) #change 0 to NA
  cpue <- as.data.frame(cpue)
  
  cpue_se <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) 
  cpue_se[,-1] <- NA
  cpue_se <- as.data.frame(cpue_se)
  
  #----------------------------------------------------------------------2 prior
  #prior part 1 - r prior: mean and logsd
  r_mean <- prior_r_stock$r_mean
  r_sd <- prior_r_stock$r_sd
  
  #prior part 2 - K prior: mean and CV
  K_mean <- prior_K_stock$K_mean
  K_CV <- prior_K_stock$K_CV
  
  #prior part 3 - psi prior: mean and CV
  psi_mean <- prior_psi_stock$psi_mean
  psi_CV <- prior_psi_stock$psi_CV
  
  #-----------------------------------------------------------3 build JABBA data
  #Schaefer
  # seed=1
  jbinput_Schaefer <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                                  assessment = paste0(stock_id,"_seed",seed),scenario = "Schaefer",
                                  model.type = "Schaefer",
                                  r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                                  K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                                  psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                                  sigma.est = TRUE, # estimate additional observation error
                                  fixed.obsE = 0.1, # mimum observation error
                                  igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                                  verbose=F)
  #Fox
  jbinput_Fox <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                             assessment = paste0(stock_id,"_seed",seed),scenario = "Fox",
                             model.type = "Fox",
                             r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                             K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                             psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                             sigma.est = TRUE, # estimate additional observation error
                             fixed.obsE = 0.1, # mimum observation error
                             igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                             verbose=F)
  #Pella-Tomlinson
  jbinput_Pella <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                               assessment =paste0(stock_id,"_seed",seed),scenario = "Pella",
                               model.type = "Pella",
                               r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                               K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                               psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                               sigma.est = TRUE, # estimate additional observation error
                               fixed.obsE = 0.1, # mimum observation error
                               igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                               verbose=F)
  
  #------------------------------------------------------------------4 fit JABBA
  #output directory
  work_dir <- getwd()
  output_dir <- paste0(work_dir,"/JABBA results/")
  fit_Schaefer <- fit_jabba(jbinput_Schaefer,save.jabba=TRUE,output.dir=output_dir,
                            ni=60000,nt=15,nb=30000,nc=3,seed=seed)
  fit_Fox <- fit_jabba(jbinput_Fox,save.jabba=TRUE,output.dir=output_dir,
                       ni=60000,nt=15,nb=30000,nc=3,seed=seed)
  fit_Pella <- fit_jabba(jbinput_Pella,save.jabba=TRUE,output.dir=output_dir,
                         ni=60000,nt=15,nb=30000,nc=3,seed=seed)
  
  #############################################################################5 save results
  #data_information
  data_information <- data.frame(Stockname=stock_id,
                                 Year_start=min(catch$Year),
                                 Year_end=max(catch$Year),
                                 catch_information=catch_information,
                                 cpue_information=cpue_information,
                                 cpue_SE_information=cpue_SE_information)
  
  #model information
  model_information_Schaefer <- fit_Schaefer$stats %>% 
    mutate(Model="Schaefer")
  model_information_Fox <- fit_Fox$stats %>% 
    mutate(Model="Fox")
  model_information_Pella <- fit_Pella$stats %>% 
    mutate(Model="Pella")
  model_information <- bind_rows(model_information_Schaefer,
                                 model_information_Fox,
                                 model_information_Pella)
  
  #combine the two results
  results <- list(data_information,model_information)
  
  #save as rds file
  file_name <- paste0(output_dir,stock_id,"seed_",seed,"_JABBA information.rds")
  write_rds(results,file=file_name)
  
}

#save function
save(function_JABBA_60000,file="Functions/function_JABBA_iteration_60000.R")


# 3 JABBA function 100000 iterations, rerun function for stocks wit --------
#function code
function_JABBA_100000 <- function(stock_id,seed,stocks_time_series,prior_r,prior_K,prior_psi){
  
  #-----------------------------------------------------------------------1 data
  time_series_stock <- filter(stocks_time_series,stockid==stock_id)
  prior_r_stock <- filter(prior_r,stockid==stock_id)
  prior_K_stock <- filter(prior_K,stockid==stock_id)
  prior_psi_stock <- filter(prior_psi,stockid==stock_id)
  data <- time_series_stock %>% 
    select(stockid,Year,Catch,Landing,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance)
  
  #data part 1 - catch data
  #give priority to catch, if not have, use landing
  catch <- data %>% 
    select(Year,Catch) %>% 
    mutate(Catch=na_if(Catch,0)) %>% 
    drop_na()
  
  landing <- data %>% 
    select(Year,Landing) %>% 
    mutate(Landing=na_if(Landing,0)) %>% 
    drop_na()
  
  if(length(catch$Year)>=length(landing$Year)){
    
    catch <- catch
    catch_information <- "Catch" 
    
  } else {
    
    catch <- landing
    catch_information <- "Landing"
    
  }
  colnames(catch)[-1] <-paste0("catch_index_",colnames(catch)[-1])
  
  #data part 2 - cpue data
  #use all we have: total biomass, total abundance, SSB, CPUE and survey abundance
  not_all_na <- function(x) any(!is.na(x))
  cpue <- data %>% 
    select(Year,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance) %>% 
    select(where(not_all_na))
  cpue_information <- paste(colnames(cpue)[2],colnames(cpue)[3],colnames(cpue)[4],colnames(cpue)[5],colnames(cpue)[6])
  colnames(cpue)[-1] <-paste0("cpue_index_",colnames(cpue)[-1])
  
  #data part 3 - cpue se
  #no se data
  cpue_SE_information <- "None"
  
  #unify data length
  data_unify_length <- left_join(catch,cpue) %>% 
    mutate(Year=as.numeric(Year))
  catch <- data_unify_length %>% 
    select(Year,starts_with("catch_index"))
  catch <- as.data.frame(catch)
  
  cpue <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) %>% 
    mutate(across(starts_with("cpue_index"),~na_if(.,0))) #change 0 to NA
  cpue <- as.data.frame(cpue)
  
  cpue_se <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) 
  cpue_se[,-1] <- NA
  cpue_se <- as.data.frame(cpue_se)
  
  #----------------------------------------------------------------------2 prior
  #prior part 1 - r prior: mean and logsd
  r_mean <- prior_r_stock$r_mean
  r_sd <- prior_r_stock$r_sd
  
  #prior part 2 - K prior: mean and CV
  K_mean <- prior_K_stock$K_mean
  K_CV <- prior_K_stock$K_CV
  
  #prior part 3 - psi prior: mean and CV
  psi_mean <- prior_psi_stock$psi_mean
  psi_CV <- prior_psi_stock$psi_CV
  
  #-----------------------------------------------------------3 build JABBA data
  #Schaefer
  # seed=1
  jbinput_Schaefer <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                                  assessment = paste0(stock_id,"_seed",seed),scenario = "Schaefer",
                                  model.type = "Schaefer",
                                  r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                                  K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                                  psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                                  sigma.est = TRUE, # estimate additional observation error
                                  fixed.obsE = 0.1, # mimum observation error
                                  igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                                  verbose=F)
  #Fox
  jbinput_Fox <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                             assessment = paste0(stock_id,"_seed",seed),scenario = "Fox",
                             model.type = "Fox",
                             r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                             K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                             psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                             sigma.est = TRUE, # estimate additional observation error
                             fixed.obsE = 0.1, # mimum observation error
                             igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                             verbose=F)
  #Pella-Tomlinson
  jbinput_Pella <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                               assessment =paste0(stock_id,"_seed",seed),scenario = "Pella",
                               model.type = "Pella",
                               r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                               K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                               psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                               sigma.est = TRUE, # estimate additional observation error
                               fixed.obsE = 0.1, # mimum observation error
                               igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                               verbose=F)
  
  #------------------------------------------------------------------4 fit JABBA
  #output directory
  work_dir <- getwd()
  output_dir <- paste0(work_dir,"/JABBA results/")
  fit_Schaefer <- fit_jabba(jbinput_Schaefer,save.jabba=TRUE,output.dir=output_dir,
                            ni=100000,nt=25,nb=50000,nc=3,seed=seed)
  fit_Fox <- fit_jabba(jbinput_Fox,save.jabba=TRUE,output.dir=output_dir,
                       ni=100000,nt=25,nb=50000,nc=3,seed=seed)
  fit_Pella <- fit_jabba(jbinput_Pella,save.jabba=TRUE,output.dir=output_dir,
                         ni=100000,nt=25,nb=50000,nc=3,seed=seed)
  
  #############################################################################5 save results
  #data_information
  data_information <- data.frame(Stockname=stock_id,
                                 Year_start=min(catch$Year),
                                 Year_end=max(catch$Year),
                                 catch_information=catch_information,
                                 cpue_information=cpue_information,
                                 cpue_SE_information=cpue_SE_information)
  
  #model information
  model_information_Schaefer <- fit_Schaefer$stats %>% 
    mutate(Model="Schaefer")
  model_information_Fox <- fit_Fox$stats %>% 
    mutate(Model="Fox")
  model_information_Pella <- fit_Pella$stats %>% 
    mutate(Model="Pella")
  model_information <- bind_rows(model_information_Schaefer,
                                 model_information_Fox,
                                 model_information_Pella)
  
  #combine the two results
  results <- list(data_information,model_information)
  
  #save as rds file
  file_name <- paste0(output_dir,stock_id,"seed_",seed,"_JABBA information.rds")
  write_rds(results,file=file_name)
  
}

#save function
save(function_JABBA_100000,file="Functions/function_JABBA_iteration_100000.R")


# 4 JABBA function 200000 iterations, rerun function for stocks wit --------
#function code
function_JABBA_200000 <- function(stock_id,seed,stocks_time_series,prior_r,prior_K,prior_psi){
  
  #-----------------------------------------------------------------------1 data
  time_series_stock <- filter(stocks_time_series,stockid==stock_id)
  prior_r_stock <- filter(prior_r,stockid==stock_id)
  prior_K_stock <- filter(prior_K,stockid==stock_id)
  prior_psi_stock <- filter(prior_psi,stockid==stock_id)
  data <- time_series_stock %>% 
    select(stockid,Year,Catch,Landing,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance)
  
  #data part 1 - catch data
  #give priority to catch, if not have, use landing
  catch <- data %>% 
    select(Year,Catch) %>% 
    mutate(Catch=na_if(Catch,0)) %>% 
    drop_na()
  
  landing <- data %>% 
    select(Year,Landing) %>% 
    mutate(Landing=na_if(Landing,0)) %>% 
    drop_na()
  
  if(length(catch$Year)>=length(landing$Year)){
    
    catch <- catch
    catch_information <- "Catch" 
    
  } else {
    
    catch <- landing
    catch_information <- "Landing"
    
  }
  colnames(catch)[-1] <-paste0("catch_index_",colnames(catch)[-1])
  
  #data part 2 - cpue data
  #use all we have: total biomass, total abundance, SSB, CPUE and survey abundance
  not_all_na <- function(x) any(!is.na(x))
  cpue <- data %>% 
    select(Year,Total_biomass,Total_abundance,SSB,CPUE,Survey_abundance) %>% 
    select(where(not_all_na))
  cpue_information <- paste(colnames(cpue)[2],colnames(cpue)[3],colnames(cpue)[4],colnames(cpue)[5],colnames(cpue)[6])
  colnames(cpue)[-1] <-paste0("cpue_index_",colnames(cpue)[-1])
  
  #data part 3 - cpue se
  #no se data
  cpue_SE_information <- "None"
  
  #unify data length
  data_unify_length <- left_join(catch,cpue) %>% 
    mutate(Year=as.numeric(Year))
  catch <- data_unify_length %>% 
    select(Year,starts_with("catch_index"))
  catch <- as.data.frame(catch)
  
  cpue <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) %>% 
    mutate(across(starts_with("cpue_index"),~na_if(.,0))) #change 0 to NA
  cpue <- as.data.frame(cpue)
  
  cpue_se <- data_unify_length %>% 
    select(Year,starts_with("cpue_index")) 
  cpue_se[,-1] <- NA
  cpue_se <- as.data.frame(cpue_se)
  
  #----------------------------------------------------------------------2 prior
  #prior part 1 - r prior: mean and logsd
  r_mean <- prior_r_stock$r_mean
  r_sd <- prior_r_stock$r_sd
  
  #prior part 2 - K prior: mean and CV
  K_mean <- prior_K_stock$K_mean
  K_CV <- prior_K_stock$K_CV
  
  #prior part 3 - psi prior: mean and CV
  psi_mean <- prior_psi_stock$psi_mean
  psi_CV <- prior_psi_stock$psi_CV
  
  #-----------------------------------------------------------3 build JABBA data
  #Schaefer
  # seed=1
  jbinput_Schaefer <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                                  assessment = paste0(stock_id,"_seed",seed),scenario = "Schaefer",
                                  model.type = "Schaefer",
                                  r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                                  K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                                  psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                                  sigma.est = TRUE, # estimate additional observation error
                                  fixed.obsE = 0.1, # mimum observation error
                                  igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                                  verbose=F)
  #Fox
  jbinput_Fox <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                             assessment = paste0(stock_id,"_seed",seed),scenario = "Fox",
                             model.type = "Fox",
                             r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                             K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                             psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                             sigma.est = TRUE, # estimate additional observation error
                             fixed.obsE = 0.1, # mimum observation error
                             igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                             verbose=F)
  #Pella-Tomlinson
  jbinput_Pella <- build_jabba(catch = catch,cpue = cpue,se = cpue_se,
                               assessment =paste0(stock_id,"_seed",seed),scenario = "Pella",
                               model.type = "Pella",
                               r.dist = c("lnorm","range")[1],r.prior = c(r_mean,r_sd),
                               K.dist = c("lnorm","range")[1],K.prior = c(K_mean,K_CV),
                               psi.dist = c("lnorm","beta")[1],psi.prior = c(psi_mean,psi_CV), # Initial depletion B/K
                               sigma.est = TRUE, # estimate additional observation error
                               fixed.obsE = 0.1, # mimum observation error
                               igamma = c(0.001,0.001), # uninformative inv-gamma for process error
                               verbose=F)
  
  #------------------------------------------------------------------4 fit JABBA
  #output directory
  work_dir <- getwd()
  output_dir <- paste0(work_dir,"/JABBA results/")
  fit_Schaefer <- fit_jabba(jbinput_Schaefer,save.jabba=TRUE,output.dir=output_dir,
                            ni=200000,nt=50,nb=100000,nc=3,seed=seed)
  fit_Fox <- fit_jabba(jbinput_Fox,save.jabba=TRUE,output.dir=output_dir,
                       ni=200000,nt=50,nb=100000,nc=3,seed=seed)
  fit_Pella <- fit_jabba(jbinput_Pella,save.jabba=TRUE,output.dir=output_dir,
                         ni=200000,nt=50,nb=100000,nc=3,seed=seed)
  
  #############################################################################5 save results
  #data_information
  data_information <- data.frame(Stockname=stock_id,
                                 Year_start=min(catch$Year),
                                 Year_end=max(catch$Year),
                                 catch_information=catch_information,
                                 cpue_information=cpue_information,
                                 cpue_SE_information=cpue_SE_information)
  
  #model information
  model_information_Schaefer <- fit_Schaefer$stats %>% 
    mutate(Model="Schaefer")
  model_information_Fox <- fit_Fox$stats %>% 
    mutate(Model="Fox")
  model_information_Pella <- fit_Pella$stats %>% 
    mutate(Model="Pella")
  model_information <- bind_rows(model_information_Schaefer,
                                 model_information_Fox,
                                 model_information_Pella)
  
  #combine the two results
  results <- list(data_information,model_information)
  
  #save as rds file
  file_name <- paste0(output_dir,stock_id,"seed_",seed,"_JABBA information.rds")
  write_rds(results,file=file_name)
  
}

#save function
save(function_JABBA_200000,file="Functions/function_JABBA_iteration_200000.R")

