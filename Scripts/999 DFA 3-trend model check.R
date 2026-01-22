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

# 2 DFA 3-trend model test ------------------------------------------------------
#control list
cntl_list <-  list(minit=200, maxit=1000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)

#variance-covariance matrix
# levels_R <-  c("diagonal and equal",
#                "diagonal and unequal",
#                "equalvarcov",
#                "unconstrained")
levels_R <-  c("diagonal and equal",
               "diagonal and unequal",
               "equalvarcov")

# levels_R <-  c("diagonal and equal")
#trend number
trend_number <- c(3)

#FAO area area 27 the most stocks, area 71 the least stocks
FAOarea <- stock_FAOarea$primary_FAOarea[c(2,11)]

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
      
      # area="27"
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
write_rds(DFA_selection_results,file="DFA results/0_DFA_selection_test_27_71.rds")

#stop cluster
stopCluster(cl)

# 3 best 3-trend DFA model --------------------------------------------------------

DFA_files_27 <- list.files("DFA results/")[startsWith(list.files("DFA results/"),"kemz_FAOarea_27_trend_3")]
DFA_files_71 <- list.files("DFA results/")[startsWith(list.files("DFA results/"),"kemz_FAOarea_71_trend_3")]

#all DFA model information in area 27
DFA_model_information <- NULL
for (i in DFA_files_27) {
  
  # i <- DFA_files[1]
  DFA_results_cycle <- read_rds(paste0("DFA results/",i))
  DFA_model_information_cycle <- data.frame(file_name=i,
                                            logLik=ifelse(is.null(DFA_results_cycle$logLik),NA,DFA_results_cycle$logLik),
                                            AICc=ifelse(is.null(DFA_results_cycle$AICc),NA,DFA_results_cycle$AICc),
                                            K=ifelse(is.null(DFA_results_cycle$num.params),NA,DFA_results_cycle$num.params))
  
  DFA_model_information <- bind_rows(DFA_model_information,DFA_model_information_cycle)
  
}

#all DFA model information in area 71
DFA_model_information <- NULL
for (i in DFA_files_71) {
  
  # i <- DFA_files[1]
  DFA_results_cycle <- read_rds(paste0("DFA results/",i))
  DFA_model_information_cycle <- data.frame(file_name=i,
                                            logLik=ifelse(is.null(DFA_results_cycle$logLik),NA,DFA_results_cycle$logLik),
                                            AICc=ifelse(is.null(DFA_results_cycle$AICc),NA,DFA_results_cycle$AICc),
                                            K=ifelse(is.null(DFA_results_cycle$num.params),NA,DFA_results_cycle$num.params))
  
  DFA_model_information <- bind_rows(DFA_model_information,DFA_model_information_cycle)
  
}



# 4 best DFA model results 27------------------------------------------------
#extract DFA results
dfa_CI <- read_rds("DFA results/kemz_FAOarea_27_trend_3_structure_diagonal and unequal.rds")

Z_mean <- coef(dfa_CI, type = "matrix")$Z
Z_low <- coef(dfa_CI, type="matrix", what="par.lowCI")$Z
Z_up <- coef(dfa_CI, type="matrix", what="par.upCI")$Z

#get the rotation matrix for the Z_mean
H_inv <- varimax(Z_mean)$rotmat

#rotate the loading
Z_rot_mean <- as.data.frame(Z_mean %*% H_inv) %>%
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_mean),
         type="mean") 
Z_rot_up <- as.data.frame(Z_up %*% H_inv) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_up),
         type="up")
Z_rot_low <- as.data.frame(Z_low %*% H_inv) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_low),
         type="low")

#combine data
Z_rot <- bind_rows(Z_rot_mean,Z_rot_up,Z_rot_low)

#------------------------------------------------------------------------trend
#get the trend_mean, trend_up and trend_low
trend_mean <- dfa_CI$states
trend_up <- dfa_CI$states+1.96*dfa_CI$states.se
trend_low <- dfa_CI$states-1.96*dfa_CI$states.se

#Rotate the trend
trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1984:2019,
         type="mean")
trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1984:2019,
         type="up")
trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1984:2019,
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

#plot
ggplot(filter(trend_rot, type == "mean"))+
  geom_line(aes(x = year, y = trend1), color = "blue")+
  geom_line(aes(x = year, y = trend2), color = "red")+
  geom_line(aes(x = year, y = trend3), color = "darkgreen")






# 5 best DFA model results 71------------------------------------------------
#extract DFA results
dfa_CI <- read_rds("DFA results/kemz_FAOarea_71_trend_3_structure_diagonal and unequal.rds")

Z_mean <- coef(dfa_CI, type = "matrix")$Z
Z_low <- coef(dfa_CI, type="matrix", what="par.lowCI")$Z
Z_up <- coef(dfa_CI, type="matrix", what="par.upCI")$Z

#get the rotation matrix for the Z_mean
H_inv <- varimax(Z_mean)$rotmat

#rotate the loading
Z_rot_mean <- as.data.frame(Z_mean %*% H_inv) %>%
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_mean),
         type="mean") 
Z_rot_up <- as.data.frame(Z_up %*% H_inv) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_up),
         type="up")
Z_rot_low <- as.data.frame(Z_low %*% H_inv) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(names=rownames(Z_low),
         type="low")

#combine data
Z_rot <- bind_rows(Z_rot_mean,Z_rot_up,Z_rot_low)

#------------------------------------------------------------------------trend
#get the trend_mean, trend_up and trend_low
trend_mean <- dfa_CI$states
trend_up <- dfa_CI$states+1.96*dfa_CI$states.se
trend_low <- dfa_CI$states-1.96*dfa_CI$states.se

#Rotate the trend
trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1952:2014,
         type="mean")
trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>% 
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1952:2014,
         type="up")
trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
  rename("trend1"=1,"trend2"=2,"trend3"=3) %>% 
  mutate(year=1952:2014,
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

#plot
ggplot(filter(trend_rot, type == "mean"))+
  geom_line(aes(x = year, y = trend1), color = "blue")+
  geom_line(aes(x = year, y = trend2), color = "red")+
  geom_line(aes(x = year, y = trend3), color = "darkgreen")





