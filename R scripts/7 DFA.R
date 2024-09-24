library(tidyverse)
library(MARSS)
library(doParallel)
library(foreach)

# 1 Data processing -------------------------------------------------------
#load data
#period
DFA_period <- read_rds("DFA results/0_DFA_period.rds")

#productivity
productivity <- read_rds("Data/productivity_summary.rds") %>% 
  select(1,2,3,4) %>% 
  drop_na() %>% 
  arrange(year) 

#stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") 

#stock FAO area
stock_FAOarea <- stock_success %>% 
  group_by(primary_FAOarea) %>% 
  summarise(n=n())

#delete area 51, 58, as less than 5 stocks in each area
stock_FAOarea <- stock_success %>% 
  group_by(primary_FAOarea) %>% 
  summarise(n=n()) %>% 
  filter(n>=5)

# 2 DFA run parallel ------------------------------------------------------
#control list
cntl_list <-  list(minit=200, maxit=1000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

#variance-covariance matrix
# levels_R <-  c("diagonal and equal",
#                "diagonal and unequal",
#                "equalvarcov",
#                "unconstrained")
levels_R <-  c("diagonal and equal",
               "diagonal and unequal",
               "equalvarcov",
               "unconstrained")[2]

# levels_R <-  c("diagonal and equal")
#trend number
trend_number <- c(1,2)

#FAO area
FAOarea <- stock_FAOarea$primary_FAOarea

#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 16));

#register cores
registerDoParallel(cl)   

#run parallel
DFA_selection_results <- foreach(R=levels_R) %do% {
  
  foreach(m=trend_number) %do% {
    
    foreach(area=FAOarea,.combine = "rbind",.packages = c("MARSS","tidyverse")) %dopar% {
      
      # area="71"
      # R=levels_R[1]
      # m=trend_number[1]
      
      #loop data
      stock_loop <- stock_success %>%
        filter(primary_FAOarea==area)
      period_loop <- DFA_period %>% 
        filter(FAOarea==area)
      data_loop <- productivity %>% 
        filter(year %in% c(period_loop$start_year:period_loop$end_year)) %>% 
        filter(stockid %in% stock_loop$stockid) %>% 
        select(1,2,3) %>% 
        pivot_wider(names_from = stockid,values_from = p_mean)
      data_loop <- data_loop[,-1] #delete year
      data_loop <- t(scale(data_loop)) #scale and transpose data for DFA
      
      #DFA run
      dfa.model = list(A="zero", R=R, m=m)
      kemz = MARSS(data_loop, model=dfa.model, control=cntl_list, form="dfa", z.score=TRUE)
      dfa_CI <- MARSSparamCIs(kemz)
      
      #save results
      write_rds(dfa_CI,file=paste0("DFA results/kemz","_FAOarea_",area,"_trend_",m,"_structure_",R,".rds"))
      model_data_loop <- data.frame(FAOarea=area,
                                     R=R,
                                     m=m,
                                     logLik=kemz$logLik,
                                     K=kemz$num.params,
                                     AICc=kemz$AICc,
                                     stringsAsFactors=FALSE)
      
      
    }
      
  }
    
}

#save information
write_rds(DFA_selection_results,file="DFA results/0_DFA_selection.rds")

#stop cluster
stopCluster(cl)


# 3 best DFA model --------------------------------------------------------
DFA_files <- list.files("DFA results/")[startsWith(list.files("DFA results/"),"kemz")]

#all DFA model information
DFA_model_information <- NULL
for (i in DFA_files) {
  
  # i <- DFA_files[1]
  DFA_results_cycle <- read_rds(paste0("DFA results/",i))
  DFA_model_information_cycle <- data.frame(file_name=i,
                                            logLik=ifelse(is.null(DFA_results_cycle$logLik),NA,DFA_results_cycle$logLik),
                                            AICc=ifelse(is.null(DFA_results_cycle$AICc),NA,DFA_results_cycle$AICc),
                                            K=ifelse(is.null(DFA_results_cycle$num.params),NA,DFA_results_cycle$num.params))
  
  DFA_model_information <- bind_rows(DFA_model_information,DFA_model_information_cycle)
  
}

best_DFA_model <- DFA_model_information %>% 
  separate(file_name, sep = "_", 
           into = c("model","area1","area2","trend1","trend2","structure1","structure2"),
           remove = F) %>% 
  mutate(area=paste(area1,area2),
         trend=paste(trend2,trend1),
         structure=str_sub(structure2,end=-5)) %>% 
  select(file_name,area,trend,structure,logLik,AICc,K)

write.csv(best_DFA_model,"DFA models.csv")
# #focus on only 1 trend
# best_DFA_model <- best_DFA_model %>%
#   filter(trend=="1 trend")

#based on AICc, select the best DFA model
best_DFA_model <- best_DFA_model %>% 
  group_by(area) %>% 
  filter(AICc==min(AICc, na.rm = T)) #all two trends model

#save data
write_rds(best_DFA_model,file="DFA results/0_best_DFA_model.rds")

# 4 best DFA model results ------------------------------------------------
best_DFA_model <- read_rds("DFA results/0_best_DFA_model.rds")
DFA_period <- read_rds("DFA results/0_DFA_period.rds")

#-----------------------------------------------------------------------function
extract_DFA_results <- function(file_name=i){
  
  # i=best_DFA_model$file_name[1]
  area <- str_sub(i, start = 6, end = 15)
  DFA_period_loop <- filter(DFA_period,FAOarea==str_sub(area,-2,-1))
  best_DFA_model_loop <- read_rds(paste0("DFA results/",i))
  
  # parameter confidence interval
  # dfa_CI <- MARSSparamCIs(best_DFA_model_loop)
  dfa_CI <- best_DFA_model_loop
  
  #1 or 2 trends
  if(dim(dfa_CI$states)[1]==1) { #1 trend, no rotate
    
    #-----------------------------------------------------------------loading, Z
    #get the Z_mean, Z_up and Z_low
    Z_mean <- as.data.frame(coef(dfa_CI, type = "matrix")$Z) %>% 
      rename("trend1"=1) 
    Z_mean <- Z_mean %>% 
      mutate(names=str_sub(rownames(Z_mean), end = -7),
             type="mean") 
    Z_low <- as.data.frame(coef(dfa_CI, type="matrix", what="par.lowCI")$Z) %>% 
      rename("trend1"=1) 
    Z_low <- Z_low %>% 
      mutate(names=str_sub(rownames(Z_low), end = -7),
             type="low") 
    Z_up <- as.data.frame(coef(dfa_CI, type="matrix", what="par.upCI")$Z) %>% 
      rename("trend1"=1)
    Z_up <- Z_up %>% 
      mutate(names=str_sub(rownames(Z_up), end = -7),
             type="up") 
    
    #combine data
    Z <- bind_rows(Z_mean,Z_up,Z_low) %>% 
      mutate(area=area)
    
    #----------------------------------------------------------------------trend
    #get the trend_mean, trend_up and trend_low
    trend_mean <- as.data.frame(t(best_DFA_model_loop$states)) %>% 
      rename("trend1"=1) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="mean")
    trend_up <- as.data.frame(t(best_DFA_model_loop$states+1.96*best_DFA_model_loop$states.se)) %>% 
      rename("trend1"=1) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="up")
    trend_low <- as.data.frame(t(best_DFA_model_loop$states-1.96*best_DFA_model_loop$states.se)) %>% 
      rename("trend1"=1) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="low")
    
    #combine data
    trend <- bind_rows(trend_mean,trend_up,trend_low) %>% 
      mutate(area=area)
    
    #------------------------------------------------------------------residuals
    #residuals
    res <- residuals(best_DFA_model_loop)
    alpha <- 0.05
    res <- res %>% 
      mutate(up=qnorm(1 - alpha / 2) * .sigma + res$.fitted,
             low=qnorm(alpha / 2) * res$.sigma + res$.fitted)
    res$Year <- c(DFA_period_loop$start_year:DFA_period_loop$end_year)
    
    #------------------------------------------------------combine data and save 
    DFA_results <- list(Z,trend,res)
    write_rds(DFA_results,file=paste0("DFA results/0_best_DFA_model_results_",area,".rds"))
    
  } else { # 2 trends
    
    #-----------------------------------------------------------------loading, Z
    #get the Z_mean, Z_up and Z_low
    Z_mean <- coef(dfa_CI, type = "matrix")$Z
    Z_low <- coef(dfa_CI, type="matrix", what="par.lowCI")$Z
    Z_up <- coef(dfa_CI, type="matrix", what="par.upCI")$Z
    
    #get the rotation matrix for the Z_mean
    H_inv <- varimax(Z_mean)$rotmat
    
    #rotate the loading
    Z_rot_mean <- as.data.frame(Z_mean %*% H_inv) %>%
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(names=rownames(Z_mean),
             type="mean") 
    Z_rot_up <- as.data.frame(Z_up %*% H_inv) %>% 
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(names=rownames(Z_up),
             type="up")
    Z_rot_low <- as.data.frame(Z_low %*% H_inv) %>% 
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(names=rownames(Z_low),
             type="low")
    
    #combine data
    Z_rot <- bind_rows(Z_rot_mean,Z_rot_up,Z_rot_low)
    
    #------------------------------------------------------------------------trend
    #get the trend_mean, trend_up and trend_low
    trend_mean <- best_DFA_model_loop$states
    trend_up <- best_DFA_model_loop$states+1.96*best_DFA_model_loop$states.se
    trend_low <- best_DFA_model_loop$states-1.96*best_DFA_model_loop$states.se
    
    #Rotate the trend
    trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>% 
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="mean")
    trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>% 
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="up")
    trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
      rename("trend1"=1,"trend2"=2) %>% 
      mutate(year=DFA_period_loop$start_year:DFA_period_loop$end_year,
             type="low")
    
    #combine data
    trend_rot <- bind_rows(trend_rot_mean,trend_rot_up,trend_rot_low)
    
    #--------------------------------------------------------------------residuals
    #residuals
    res <- residuals(best_DFA_model_loop)
    alpha <- 0.05
    res <- res %>% 
      mutate(up=qnorm(1 - alpha / 2) * .sigma + res$.fitted, #same as 1.96*se
             low=qnorm(alpha / 2) * res$.sigma + res$.fitted) #same as 1.96*se
    res$Year <- c(DFA_period_loop$start_year:DFA_period_loop$end_year)
    
    #------------------------------------------------------combine data and save 
    DFA_results <- list(Z_rot,trend_rot,res)
    write_rds(DFA_results,file=paste0("DFA results/0_best_DFA_model_results_",area,".rds"))
    
  }
  
}

#-------------------------------------------------------------------run parallel
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

foreach(i = best_DFA_model$file_name, .packages = c("MARSS","tidyverse")) %dopar% {
  
  extract_DFA_results(i)
  
}
 
#stop cluster
stopCluster(cl)


# 5 best DFA model results compile----------------------------------------------
file_name <- list.files("DFA results/")[startsWith(list.files("DFA results/"),"0_best_DFA_model_results")]

#loop extract trend and loading (and residual)
DFA_trend <- NULL
DFA_loading <- NULL
for (i in file_name){
  
  # i <- file_name[1]
  
  data_loop <- read_rds(paste0("DFA results/",i))
  
  data_loop[[1]]$area <- str_sub(i, start = -6, end = -5)
  data_loop[[2]]$area <- str_sub(i, start = -6, end = -5)
  
  #loading
  DFA_loading_loop <- data_loop[[1]] %>% 
    pivot_longer(-c(area,names,type),names_to = "trend",values_to = "value") %>% 
    pivot_wider(names_from = type,values_from = value)
  
  #trend
  DFA_trend_loop <- data_loop[[2]] %>% 
    pivot_longer(-c(area,year,type),names_to = "trend",values_to = "value") %>% 
    pivot_wider(names_from = type,values_from = value)
  
  #combine data
  DFA_loading <- bind_rows(DFA_loading,DFA_loading_loop)
  DFA_trend <- bind_rows(DFA_trend,DFA_trend_loop)
  
}

write_rds(DFA_loading,file="DFA results/0_best_DFA_loading.rds")
write_rds(DFA_trend,file="DFA results/0_best_DFA_trend.rds")


a <- DFA_loading %>% 
  filter(area=="21")



