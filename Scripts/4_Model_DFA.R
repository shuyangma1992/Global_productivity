library(tidyverse)
library(MARSS)
library(doParallel)
library(foreach)

# 1 Data processing -------------------------------------------------------
# load data
# period
DFA_period <- read_rds("Outputs/Productivity/productivity_period_for_DFA.rds")

# productivity
productivity <- read_rds("Outputs/Productivity/productivity_for_DFA.rds") %>%
  select(1, 2, 3, 4) %>%
  drop_na() %>%
  arrange(year)

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds")

# stock FAO area
stock_FAOarea <- stock_success %>%
  group_by(primary_FAOarea) %>%
  summarise(n = n())

# delete area 51, 58, as less than 5 stocks in each area
stock_FAOarea <- stock_success %>%
  group_by(primary_FAOarea) %>%
  summarise(n = n()) %>%
  filter(n >= 5)

# 2 DFA run parallel ------------------------------------------------------
# control list
cntl_list <- list(minit = 200, maxit = 1000, allow.degen = FALSE, conv.test.slope.tol = 0.1, abstol = 0.0001)

# variance-covariance matrix
# do not use "unconstrained" because model cannot be converged
levels_R <- c(
  "diagonal and equal",
  "diagonal and unequal",
  "equalvarcov",
  "unconstrained"
)[2]

# trend number
trend_number <- c(1, 2)

# FAO area
FAOarea <- stock_FAOarea$primary_FAOarea

# how many cores can be used
detectCores()

# use 16 cores
cl <- makeCluster(getOption("cl.cores", 16))

# register cores
registerDoParallel(cl)

# run parallel
DFA_selection_results <- foreach(R = levels_R) %do% {
  foreach(m = trend_number) %do% {
    foreach(area = FAOarea, .combine = "rbind", .packages = c("MARSS", "tidyverse")) %dopar% {
      # area="71"
      # R=levels_R[1]
      # m=trend_number[1]

      # loop data
      stock_loop <- stock_success %>%
        filter(primary_FAOarea == area)
      period_loop <- DFA_period %>%
        filter(FAOarea == area)
      data_loop <- productivity %>%
        filter(year %in% c(period_loop$start_year:period_loop$end_year)) %>%
        filter(stockid %in% stock_loop$stockid) %>%
        select(1, 2, 3) %>%
        pivot_wider(names_from = stockid, values_from = p_mean)
      data_loop <- data_loop[, -1] # delete year
      data_loop <- t(scale(data_loop)) # scale and transpose data for DFA

      # DFA run
      dfa.model <- list(A = "zero", R = R, m = m)
      kemz <- MARSS(data_loop, model = dfa.model, control = cntl_list, form = "dfa", z.score = TRUE)
      dfa_CI <- MARSSparamCIs(kemz)

      # save results
      write_rds(dfa_CI, file = paste0("Outputs/DFA results/kemz", "_FAOarea_", area, "_trend_", m, "_structure_", R, ".rds"))
      model_data_loop <- data.frame(
        FAOarea = area,
        R = R,
        m = m,
        logLik = kemz$logLik,
        K = kemz$num.params,
        AICc = kemz$AICc,
        stringsAsFactors = FALSE
      )
    }
  }
}

# save information
write_rds(DFA_selection_results, file = "Outputs/DFA results/0_DFA_selection.rds")

# stop cluster
stopCluster(cl)

# 3 DFA model comparison --------------------------------------------------------
DFA_files <- list.files("Outputs/DFA results/")[startsWith(list.files("Outputs/DFA results/"), "kemz")]

# all DFA model information
DFA_model_information <- NULL
for (i in DFA_files) {
  # i <- DFA_files[1]
  DFA_results_cycle <- read_rds(paste0("Outputs/DFA results/", i))
  DFA_model_information_cycle <- data.frame(
    file_name = i,
    logLik = ifelse(is.null(DFA_results_cycle$logLik), NA, DFA_results_cycle$logLik),
    AICc = ifelse(is.null(DFA_results_cycle$AICc), NA, DFA_results_cycle$AICc),
    K = ifelse(is.null(DFA_results_cycle$num.params), NA, DFA_results_cycle$num.params)
  )

  DFA_model_information <- bind_rows(DFA_model_information, DFA_model_information_cycle)
}

best_DFA_model <- DFA_model_information %>%
  separate(file_name,
    sep = "_",
    into = c("model", "area1", "area2", "trend1", "trend2", "structure1", "structure2"),
    remove = F
  ) %>%
  mutate(
    area = paste(area1, area2),
    trend = paste(trend2, trend1),
    structure = str_sub(structure2, end = -5)
  ) %>%
  select(file_name, area, trend, structure, logLik, AICc, K)

# # save results, Table SX
# write.csv(best_DFA_model, "Outputs/DFA results/DFA_models.csv")

# based on AICc, select the best DFA model
best_DFA_model <- best_DFA_model %>%
  group_by(area) %>%
  filter(AICc == min(AICc, na.rm = T)) # all two trends model

# save data
write_rds(best_DFA_model, file = "Outputs/DFA results/0_best_DFA_model.rds")

# 4 best DFA model results ------------------------------------------------
best_DFA_model <- read_rds("Outputs/DFA results/0_best_DFA_model.rds")
DFA_period <- read_rds("Outputs/Productivity/productivity_period_for_DFA.rds")

#-----------------------------------------------------------------------function
extract_DFA_results <- function(file_name = i) {
  # i=best_DFA_model$file_name[1]
  area <- str_sub(i, start = 6, end = 15)
  DFA_period_loop <- filter(DFA_period, FAOarea == str_sub(area, -2, -1))
  best_DFA_model_loop <- read_rds(paste0("Outputs/DFA results/", i))

  # parameter confidence interval
  # dfa_CI <- MARSSparamCIs(best_DFA_model_loop)
  dfa_CI <- best_DFA_model_loop

  # 1 or 2 trends
  if (dim(dfa_CI$states)[1] == 1) { # 1 trend, no rotate

    #-----------------------------------------------------------------loading, Z
    # get the Z_mean, Z_up and Z_low
    Z_mean <- as.data.frame(coef(dfa_CI, type = "matrix")$Z) %>%
      rename("trend1" = 1)
    Z_mean <- Z_mean %>%
      mutate(
        names = str_sub(rownames(Z_mean), end = -7),
        type = "mean"
      )
    Z_low <- as.data.frame(coef(dfa_CI, type = "matrix", what = "par.lowCI")$Z) %>%
      rename("trend1" = 1)
    Z_low <- Z_low %>%
      mutate(
        names = str_sub(rownames(Z_low), end = -7),
        type = "low"
      )
    Z_up <- as.data.frame(coef(dfa_CI, type = "matrix", what = "par.upCI")$Z) %>%
      rename("trend1" = 1)
    Z_up <- Z_up %>%
      mutate(
        names = str_sub(rownames(Z_up), end = -7),
        type = "up"
      )

    # combine data
    Z <- bind_rows(Z_mean, Z_up, Z_low) %>%
      mutate(area = area)

    #----------------------------------------------------------------------trend
    # get the trend_mean, trend_up and trend_low
    trend_mean <- as.data.frame(t(best_DFA_model_loop$states)) %>%
      rename("trend1" = 1) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "mean"
      )
    trend_up <- as.data.frame(t(best_DFA_model_loop$states + 1.96 * best_DFA_model_loop$states.se)) %>%
      rename("trend1" = 1) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "up"
      )
    trend_low <- as.data.frame(t(best_DFA_model_loop$states - 1.96 * best_DFA_model_loop$states.se)) %>%
      rename("trend1" = 1) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "low"
      )

    # combine data
    trend <- bind_rows(trend_mean, trend_up, trend_low) %>%
      mutate(area = area)

    #------------------------------------------------------------------residuals
    # residuals
    res <- residuals(best_DFA_model_loop)
    alpha <- 0.05
    res <- res %>%
      mutate(
        up = qnorm(1 - alpha / 2) * .sigma + res$.fitted,
        low = qnorm(alpha / 2) * res$.sigma + res$.fitted
      )
    res$Year <- c(DFA_period_loop$start_year:DFA_period_loop$end_year)

    #------------------------------------------------------combine data and save 
    DFA_results <- list(Z, trend, res)
    write_rds(DFA_results, file = paste0("DFA results/0_best_DFA_model_results_", area, ".rds"))
  } else { # 2 trends

    #-----------------------------------------------------------------loading, Z
    # get the Z_mean, Z_up and Z_low
    Z_mean <- coef(dfa_CI, type = "matrix")$Z
    Z_low <- coef(dfa_CI, type = "matrix", what = "par.lowCI")$Z
    Z_up <- coef(dfa_CI, type = "matrix", what = "par.upCI")$Z

    # get the rotation matrix for the Z_mean
    H_inv <- varimax(Z_mean)$rotmat

    # rotate the loading
    Z_rot_mean <- as.data.frame(Z_mean %*% H_inv) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        names = rownames(Z_mean),
        type = "mean"
      )
    Z_rot_up <- as.data.frame(Z_up %*% H_inv) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        names = rownames(Z_up),
        type = "up"
      )
    Z_rot_low <- as.data.frame(Z_low %*% H_inv) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        names = rownames(Z_low),
        type = "low"
      )

    # combine data
    Z_rot <- bind_rows(Z_rot_mean, Z_rot_up, Z_rot_low)

    #------------------------------------------------------------------------trend
    # get the trend_mean, trend_up and trend_low
    trend_mean <- best_DFA_model_loop$states
    trend_up <- best_DFA_model_loop$states + 1.96 * best_DFA_model_loop$states.se
    trend_low <- best_DFA_model_loop$states - 1.96 * best_DFA_model_loop$states.se

    # Rotate the trend
    trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "mean"
      )
    trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "up"
      )
    trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
      rename("trend1" = 1, "trend2" = 2) %>%
      mutate(
        year = DFA_period_loop$start_year:DFA_period_loop$end_year,
        type = "low"
      )

    # combine data
    trend_rot <- bind_rows(trend_rot_mean, trend_rot_up, trend_rot_low)

    #--------------------------------------------------------------------residuals
    # residuals
    res <- residuals(best_DFA_model_loop)
    alpha <- 0.05
    res <- res %>%
      mutate(
        up = qnorm(1 - alpha / 2) * .sigma + res$.fitted, # same as 1.96*se
        low = qnorm(alpha / 2) * res$.sigma + res$.fitted
      ) # same as 1.96*se
    res$Year <- c(DFA_period_loop$start_year:DFA_period_loop$end_year)

    #------------------------------------------------------combine data and save 
    DFA_results <- list(Z_rot, trend_rot, res)
    write_rds(DFA_results, file = paste0("Outputs/DFA results/0_best_DFA_model_results_", area, ".rds"))
  }
}

#-------------------------------------------------------------------run parallel
#how many cores can be used
detectCores() 

#use 14 cores
cl <- makeCluster(getOption("cl.cores", 14));

#register cores
registerDoParallel(cl)   

foreach(i = best_DFA_model$file_name, .packages = c("MARSS","tidyverse")) %dopar% {
  
  extract_DFA_results(i)
  
}
 
#stop cluster
stopCluster(cl)


# 5 best DFA model results compile----------------------------------------------
file_name <- list.files("Outputs/DFA results/")[startsWith(list.files("Outputs/DFA results/"), "0_best_DFA_model_results")]

# loop extract trend and loading (and residual)
DFA_trend <- NULL
DFA_loading <- NULL
for (i in file_name) {
  # i <- file_name[1]

  data_loop <- read_rds(paste0("Outputs/DFA results/", i))

  data_loop[[1]]$area <- str_sub(i, start = -6, end = -5)
  data_loop[[2]]$area <- str_sub(i, start = -6, end = -5)

  # loading
  DFA_loading_loop <- data_loop[[1]] %>%
    pivot_longer(-c(area, names, type), names_to = "trend", values_to = "value") %>%
    pivot_wider(names_from = type, values_from = value)

  # trend
  DFA_trend_loop <- data_loop[[2]] %>%
    pivot_longer(-c(area, year, type), names_to = "trend", values_to = "value") %>%
    pivot_wider(names_from = type, values_from = value)

  # combine data
  DFA_loading <- bind_rows(DFA_loading, DFA_loading_loop)
  DFA_trend <- bind_rows(DFA_trend, DFA_trend_loop)
}

write_rds(DFA_loading, file = "Outputs/DFA results/0_best_DFA_loading.rds")
write_rds(DFA_trend, file = "Outputs/DFA results/0_best_DFA_trend.rds")


# 0 R square --------------------------------------------------------------

file_name <- list.files("DFA results/")[startsWith(list.files("DFA results/"),"0_best_DFA_model_results")]

r_square <- NULL
for (i in file_name) {
  
  # i <- file_name[3]

  #read dfa results
  data_loop <- read_rds(paste0("DFA results/",i))
  
  #extract values and residuals
  fitted_results <- data_loop[[3]] 
  
  for (j in unique(fitted_results$.rownames)) {
    
    # j <- unique(fitted_results$.rownames)[1]
    j <- "YEGROUPGM"
    fitted_results_stock <- fitted_results %>% 
      filter(.rownames == j) %>% 
      drop_na()
    
    r_square_loop <- data.frame(stockid = j,
                                r2 = 1 - (sd(fitted_results_stock$.resids)^2)/(sd(fitted_results_stock$value)^2),
                                model_file = i)
 
    r_square <- bind_rows(r_square, r_square_loop)
    
  }
 
}

























# 0 Bayesian DFA on DFA trends --------------------------------------------
DFA_trend <- read_rds("DFA results/0_best_DFA_trend.rds")

#calculate SE
DFA_trend <- DFA_trend %>% 
  rowwise() %>% 
  mutate(se=(up-low)/1.96/2)

#select year, area, mean, se
DFA_trend <- DFA_trend %>% 
  select(year,area,mean,se) %>% 
  rename("time"=year,"ts"=area,"obs"=mean,"n"=se)

fit <- fit_dfa(y = DFA_trend, 
               num_trends = 1,
               estimate_nu = FALSE, #normal distribution
               # varIndx = rep(1DFA_trend,dim(DFA_trend)[1]), #equal variance structure
               weights = "n",
               iter = 2000, 
               chains = 3,
               data_shape="long")

rot <- rotate_trends(fit)
plot_trends(rot)
plot_loadings(rot)
















# 0 best DFA model test ---------------------------------------------------
#---------------------------------------------------------------------best model
best_model <- read_rds("DFA results/kemz_FAOarea_71_trend_2_structure_diagonal and unequal.rds")

# Rotate the loading
# Z_est <-  coef(best_model, type="matrix")$Z
# H_inv <-  varimax(coef(best_model, type="matrix")$Z)$rotmat
# Z_rot <-  as.data.frame(Z.est %*% H.inv)
# Z_rot$names <- rownames(Z.rot)

#parameter confidence interval
dfa_CI <- best_model

#---------------------------------------------------------------------loading, Z
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

#--------------------------------------------------------------------------trend
#get the trend_mean, trend_up and trend_low
trend_mean <- best_model$states
trend_up <- best_model$states+1.96*best_model$states.se
trend_low <- best_model$states-1.96*best_model$states.se

#Rotate the trend
trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>% 
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1952:2014,type="mean")
trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>% 
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1952:2014,type="up")
trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1952:2014,type="low")

#combine data
trend_rot <- bind_rows(trend_rot_mean,trend_rot_up,trend_rot_low)

#----------------------------------------------------------------------residuals
#residuals
res <- residuals(best_model)
alpha <- 0.05
res$up <- qnorm(1 - alpha / 2) * res$.sigma + res$.fitted
res$lo <- qnorm(alpha / 2) * res$.sigma + res$.fitted
res$Year <- c(1952:2014)

#---------------------------------------------------------------------------plot
#loading
fig_loading <- Z_rot %>% 
  pivot_longer(-c(names,type),names_to = "trend",values_to = "loading") %>% 
  pivot_wider(names_from = type,values_from = loading)

f_loading <- ggplot(fig_loading)+
  geom_point(aes(x=names,y=mean,color=trend),show.legend = F)+
  geom_errorbar(aes(x=names,ymax=up,ymin=low,color=trend),width=0,show.legend = F)+
  scale_fill_brewer(palette="Accent")+
  scale_x_discrete("Stocks")+
  scale_y_continuous("Loadings",minor_breaks = NULL)+
  facet_wrap(~trend,nrow = 2)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold"),
        legend.text = element_text(face="bold"),
        legend.background = element_rect(linetype = "solid",colour = "black"))

#trend
fig_trend <- trend_rot %>% 
  pivot_longer(-c(year,type), names_to = "trend", values_to = "value") %>% 
  pivot_wider(names_from = type,values_from = value)

f_trend <- ggplot(fig_trend)+
  geom_ribbon(aes(x=year,ymax=up,ymin=low,fill=trend),show.legend = F)+
  geom_line(aes(x=year,y=mean,color=trend),show.legend = F)+
  scale_fill_brewer(palette="Accent")+
  scale_color_brewer(palette="Accent")+
  scale_x_continuous("Year",minor_breaks = NULL)+
  scale_y_continuous("Fitted values",minor_breaks = NULL)+
  facet_wrap(~trend,nrow=2)+
  theme(axis.text.x = element_text(face="bold",family = "A"),
        axis.text.y = element_text(face="bold",family = "A"),
        axis.title.x = element_text(face="bold",family = "A"),
        axis.title.y = element_text(face="bold",family = "A"),
        strip.text = element_text(face="bold",family = "A"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold",family = "A"),
        legend.text = element_text(face="bold",family = "A"),
        legend.background = element_rect(linetype = "solid",colour = "black"))

#Residuals
f_residual <- ggplot(filter(res,name=="model"))+
  geom_point(aes(x=Year,y=value),color="blue")+
  geom_line(aes(x=Year,y=.fitted))+
  geom_ribbon(aes(x=Year,ymin=lo,ymax=up),linetype=3,alpha=0.2)+
  scale_x_continuous("Year",minor_breaks = NULL)+
  scale_y_continuous("Fitted values",minor_breaks = NULL)+
  facet_wrap(~.rownames,ncol=4)+
  theme(axis.text.x = element_text(face="bold",family = "A"),
        axis.text.y = element_text(face="bold",family = "A"),
        axis.title.x = element_text(face="bold",family = "A"),
        axis.title.y = element_text(face="bold",family = "A"),
        strip.text = element_text(face="bold",family = "A"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold",family = "A"),
        legend.text = element_text(face="bold",family = "A"),
        legend.background = element_rect(linetype = "solid",colour = "black"))


# 0 DFA setting-----------------------------------------------------------
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
trend_number <- c(1,2)[2]
# trend_number <- c(1)

#FAO area
FAOarea <- stock_FAOarea$primary_FAOarea

# 0 DFA run ---------------------------------------------------------------
#model information
model_data <- NULL
for (area in FAOarea) {
  
  # area <- FAOarea[1]
  stock_loop <- stock_success %>%
    filter(primary_FAOarea==area)
  
  period_loop <- DFA_period %>% 
    filter(FAOarea==area)
  
  data_loop <- productivity %>% 
    filter(year %in% c(period_loop$start_year:period_loop$end_year)) %>% 
    filter(stockid %in% stock_loop$stockid) %>% 
    select(1,2,3) %>% 
    pivot_wider(names_from = stockid,values_from = p_mean)
    
  data_loop <- data_loop[,-1]
  
  data_loop <- t(scale(data_loop)) #scale and transpose data for DFA
  
  for (R in levels_R) {
    
    for (m in trend_number) {  
      
      # R="diagonal and equal"
      # m=2
      dfa.model = list(A="zero", R=R, m=m)
      kemz = MARSS(data_loop, model=dfa.model, control=cntl_list,form="dfa", z.score=TRUE)
      model_data_loop <- data.frame(FAOarea=area,
                                     R=R,
                                     m=m,
                                     logLik=kemz$logLik,
                                     K=kemz$num.params,
                                     AICc=kemz$AICc,
                                     stringsAsFactors=FALSE)
      model_data <- bind_rows(model_data,model_data_loop)
      write_rds(kemz,file=paste0("DFA results/kemz","_FAOarea_",area,"_trend_",m,"_structure_",R,".rds"))
      # assign(paste("kemz", m, R, sep="_"), kemz)
      print(paste(area,R,m))
    }
    
  } 
  
}


# 999 test ----------------------------------------------------------------
# FAO area 31
stock_27 <- stock_success %>% 
  filter(primary_FAOarea=="61")

#time period
period_27 <- DFA_period %>% 
  filter(FAOarea=="61")

#test data
data_test <- productivity %>% 
  filter(stockid %in% stock_27$stockid)

#stock productivity data matrix
y_ts <- data_test %>% 
  select(1,2,3) %>% 
  # mutate(p_mean=p_mean*100) %>% 
  pivot_wider(names_from = stockid, values_from = p_mean) %>% 
  filter(year %in% c(period_27$start_year:period_27$end_year)) %>% 
  ungroup()

#stock missing value greater than 40%
stock_missing <- y_ts %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  t() %>% 
  as.data.frame()
stock_missing <- stock_missing %>% 
  mutate(stockid=rownames(stock_missing)) %>% 
  filter(V1>=dim(y_ts)[1]*0.4)

#remove these "missing stocks"
y_ts <- y_ts %>% 
  select(!stock_missing$stockid)

#change it to a matrix 
y_ts_mat <- t(y_ts)
colnames(y_ts_mat) <- y_ts_mat[1,] #colnames year
y_ts_mat <- y_ts_mat[-1,]

#stock productivity uncertainty matrix
y_uncert_ts <- data_test %>% 
  select(1,2,4) %>% 
  # mutate(p_sd=p_sd*100) %>% 
  pivot_wider(names_from = stockid, values_from = p_sd) %>% 
  filter(year %in% c(period_27$start_year:period_27$end_year)) %>% 
  ungroup()

#remove these "missing stocks"
y_uncert_ts <- y_uncert_ts %>% 
  select(!stock_missing$stockid)

#change it to a matrix 
y_uncert_ts_mat <- t(y_uncert_ts)
colnames(y_uncert_ts_mat) <- y_uncert_ts_mat[1,] #colnames year
y_uncert_ts_mat <- y_uncert_ts_mat[-1,]
y_uncert_ts_mat <- log(y_uncert_ts_mat)

#species names and codes
species_name_ex <- data.frame(name_long=colnames(y_ts)[-1],
                              code_sp=colnames(y_ts)[-1])

# write_rds(list(y_ts_mat,y_uncert_ts_mat,species_name_ex),
#                file="data for DFAclust test.rds")
# a <- read_rds("data for DFAclust test.rds")

#prepare data for DFAclust
data_ready_dfa <- prepare_data(data_ts = y_ts_mat,data_ts_se = y_uncert_ts_mat, se_log = F,
                               perc_replace = 0.01)

#run model
#dfa model
#data_ready_dfa$data_ts-min(data_ready_dfa$data_ts,na.rm=T)+0.000001
dfa_result <- fit_dfa(data_ts = data_ready_dfa$data_ts-min(data_ready_dfa$data_ts,na.rm = T)+0.0000001,
                      data_ts_se = data_ready_dfa$data_ts_se,
                      min_year = data_ready_dfa$min_year, 
                      max_year = data_ready_dfa$max_year, 
                      species_name_ordre = data_ready_dfa$species_name_ordre,
                      species_sub = species_name_ex, 
                      nfac = 0, mintrend = 1, maxtrend = 3, 
                      AIC = TRUE, center_option = 0, silent = T, control = list())


# dfa_result$x_hat #trend
# dfa_result$x_hat_se # trend se
# dfa_result$Z_hat #loading
# dfa_result$Z_hat_se #loading se

#cluster model (do not need)
cluster_result <- cluster_dfa(data_dfa = dfa_result, species_sub = species_name_ex, nboot = 10)

#plot
dfa_result_plot <- plot_dfa_result(data_dfa = dfa_result,
                                   sdRep = cluster_result$sdRep,
                                   species_sub = species_name_ex,
                                   group_dfa = cluster_result$group_dfa, 
                                   min_year = data_ready_dfa$min_year,
                                   species_name_ordre = data_ready_dfa$species_name_ordre)

dfa_result_plot$plot_sp
head(dfa_result_plot$data_to_plot_sp)

dfa_result_plot$plot_tr
head(dfa_result_plot$data_to_plot_tr)

dfa_result_plot$plot_ld
head(dfa_result_plot$data_loadings)

dfa_result_plot$plot_perc_var
head(dfa_result_plot$exp_var_lt)


# 999 DFAclust test -------------------------------------------------------
data(species_ts_mat)
data(species_uncert_ts_mat)

data_ready_dfa <- prepare_data(data_ts = species_ts_mat,data_ts_se = species_uncert_ts_mat,
                               se_log = TRUE, perc_replace = 0.01)

dfa_result <- fit_dfa(data_ts = data_ready_dfa$data_ts,data_ts_se = data_ready_dfa$data_ts_se,
                      min_year = data_ready_dfa$min_year, max_year = data_ready_dfa$max_year, species_name_ordre = data_ready_dfa$species_name_ordre,
                      species_sub = species_name, nfac = 0, mintrend = 1, maxtrend = 5, AIC = TRUE,
                      center_option = 1, silent = TRUE, control = list())


data(ts_bird_se_allcountry)
data(species_data)

species_sub <- species_farm <- droplevels(species_data[species_data$code_sp %in% c(
  "FALTIN","VANVAN","ALAARV","HIRRUS","CORFRU",
  "SAXRUB","SYLCOM","ANTPRA","MOTFLA","LANCOL",
  "STUVUL","LINCAN","EMBCIT","EMBHOR","PASMON"),])


Obs <- ts_bird_se_allcountry[ts_bird_se_allcountry$code_sp %in% species_sub$code_sp,]

library(reshape2)
y_farm <- dcast(Obs[,c("code_sp","relative_abundance_m0","year")],
                code_sp~year, fun.aggregate = sum, value.var = "relative_abundance_m0")
obs_se_farm <- dcast(Obs[,c("code_sp","Log_SE_m0","year")],
                     code_sp~year, fun.aggregate = sum, value.var = "Log_SE_m0")
y_farm_ts <- as.matrix(y_farm[,2:ncol(y_farm)]) # species time series
y_uncert_ts <- as.matrix(obs_se_farm[,2:ncol(obs_se_farm)]) # standard error on time series
rownames(y_farm_ts) <- rownames(y_uncert_ts) <- y_farm$code_sp # add species names as row names

data_ready_dfa <- prepare_data(data_ts = y_farm_ts,data_ts_se = y_uncert_ts, se_log = TRUE,
                               perc_replace = 0.01)

dfa_result <- fit_dfa(data_ts = data_ready_dfa$data_ts,data_ts_se = data_ready_dfa$data_ts_se,
                      min_year = data_ready_dfa$min_year, max_year = data_ready_dfa$max_year,
                      species_name_ordre = data_ready_dfa$species_name_ordre, species_sub = species_farm,
                      nfac = 1, mintrend = 1, maxtrend = 5, AIC = TRUE, center_option = 1, silent = TRUE,
                      control = list())

cluster_result <- cluster_dfa(data_dfa = dfa_result, species_sub = species_farm, nboot = 10)


dfa_result_plot_farm <- plot_dfa_result(data_dfa = dfa_result, sdRep = cluster_result$sdRep,
                                        species_sub = species_farm, group_dfa = cluster_result$group_dfa, min_year =
                                          data_ready_dfa$min_year, species_name_ordre = data_ready_dfa$species_name_ordre)

dfa_result_plot_farm$plot_tr

dfa_result_plot_farm$plot_ld
head(dfa_result_plot$data_loadings)

dfa_result_plot_farm$plot_perc_var
head(dfa_result_plot$exp_var_lt)




# 999 DFA test ------------------------------------------------------------
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
               "unconstrained")

# levels_R <-  c("diagonal and equal")
#trend number
trend_number <- c(1,2)
# trend_number <- c(1)

#FAO area
FAOarea <- stock_FAOarea$primary_FAOarea[1]


#test
area <- FAOarea[1]
stock_loop <- stock_success %>%
  filter(primary_FAOarea==area)

period_loop <- DFA_period %>% 
  filter(FAOarea==area)

data_loop <- productivity %>% 
  filter(year %in% c(period_loop$start_year:period_loop$end_year)) %>% 
  filter(stockid %in% stock_loop$stockid) %>% 
  select(1,2,3) %>% 
  pivot_wider(names_from = stockid,values_from = p_mean)

data_loop <- data_loop[,-1]

data_loop <- t(scale(data_loop)) #scale and transpose data for DFA









R="unconstrained"
m=2
dfa.model = list(A="zero", R=R, m=m)
kemz = MARSS(data_loop, model=dfa.model, control=cntl_list,form="dfa", z.score=TRUE)
write_rds(kemz,file=paste0("DFA results/kemz","_FAOarea_",area,"_trend_",m,"_structure_",R,".rds"))



# Rotate the loading
Z_est <-  coef(best_model, type="matrix")$Z
H_inv <-  varimax(coef(best_model, type="matrix")$Z)$rotmat
Z_rot <-  as.data.frame(Z.est %*% H.inv)
Z_rot$names <- rownames(Z.rot)

#parameter confidence interval
dfa_CI <- MARSSparamCIs(best_model)

#---------------------------------------------------------------------loading, Z
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

#--------------------------------------------------------------------------trend
#get the trend_mean, trend_up and trend_low
trend_mean <- best_model$states
trend_up <- best_model$states+1.96*best_model$states.se
trend_low <- best_model$states-1.96*best_model$states.se

#Rotate the trend
trend_rot_mean <- as.data.frame(t(solve(H_inv) %*% trend_mean)) %>% 
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1950:2020,type="mean")
trend_rot_up <- as.data.frame(t(solve(H_inv) %*% trend_up)) %>% 
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1950:2020,type="up")
trend_rot_low <- as.data.frame(t(solve(H_inv) %*% trend_low)) %>%
  rename("trend1"=1,"trend2"=2) %>% 
  mutate(year=1950:2020,type="low")

#combine data
trend_rot <- bind_rows(trend_rot_mean,trend_rot_up,trend_rot_low)

#----------------------------------------------------------------------residuals
#residuals
res <- residuals(best_model)
alpha <- 0.05
res$up <- qnorm(1 - alpha / 2) * res$.sigma + res$.fitted
res$lo <- qnorm(alpha / 2) * res$.sigma + res$.fitted
res$Year <- c(1950:2020)

#---------------------------------------------------------------------------plot
#loading
fig_loading <- Z_rot %>% 
  pivot_longer(-c(names,type),names_to = "trend",values_to = "loading") %>% 
  pivot_wider(names_from = type,values_from = loading)

f_loading <- ggplot(fig_loading)+
  geom_point(aes(x=names,y=mean,color=trend),show.legend = F)+
  geom_errorbar(aes(x=names,ymax=up,ymin=low,color=trend),width=0,show.legend = F)+
  scale_fill_brewer(palette="Accent")+
  scale_x_discrete("Stocks")+
  scale_y_continuous("Loadings",minor_breaks = NULL)+
  facet_wrap(~trend,nrow = 2)+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(face="bold"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold"),
        legend.text = element_text(face="bold"),
        legend.background = element_rect(linetype = "solid",colour = "black"))

#trend
fig_trend <- trend_rot %>% 
  pivot_longer(-c(year,type), names_to = "trend", values_to = "value") %>% 
  pivot_wider(names_from = type,values_from = value)

f_trend <- ggplot(fig_trend)+
  geom_ribbon(aes(x=year,ymax=up,ymin=low,fill=trend),show.legend = F)+
  geom_line(aes(x=year,y=mean,color=trend),show.legend = F)+
  scale_fill_brewer(palette="Accent")+
  scale_color_brewer(palette="Accent")+
  scale_x_continuous("Year",minor_breaks = NULL)+
  scale_y_continuous("Fitted values",minor_breaks = NULL)+
  facet_wrap(~trend,nrow=2)+
  theme(axis.text.x = element_text(face="bold",family = "A"),
        axis.text.y = element_text(face="bold",family = "A"),
        axis.title.x = element_text(face="bold",family = "A"),
        axis.title.y = element_text(face="bold",family = "A"),
        strip.text = element_text(face="bold",family = "A"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold",family = "A"),
        legend.text = element_text(face="bold",family = "A"),
        legend.background = element_rect(linetype = "solid",colour = "black"))

#Residuals
f_residual <- ggplot(filter(res,name=="model"))+
  geom_point(aes(x=Year,y=value),color="blue")+
  geom_line(aes(x=Year,y=.fitted))+
  geom_ribbon(aes(x=Year,ymin=lo,ymax=up),linetype=3,alpha=0.2)+
  scale_x_continuous("Year",minor_breaks = NULL)+
  scale_y_continuous("Fitted values",minor_breaks = NULL)+
  facet_wrap(~.rownames,ncol=4)+
  theme(axis.text.x = element_text(face="bold",family = "A"),
        axis.text.y = element_text(face="bold",family = "A"),
        axis.title.x = element_text(face="bold",family = "A"),
        axis.title.y = element_text(face="bold",family = "A"),
        strip.text = element_text(face="bold",family = "A"),
        axis.ticks = element_blank(),
        legend.position = c(0.75,0.25),
        legend.title = element_text(face="bold",family = "A"),
        legend.text = element_text(face="bold",family = "A"),
        legend.background = element_rect(linetype = "solid",colour = "black"))















# 9999 DFA 3-trend model test ------------------------------------------------------
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

#FAO area
FAOarea <- stock_FAOarea$primary_FAOarea[2]

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
      
      area="27"
      R=levels_R[1]
      m=trend_number[1]
      
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
write_rds(DFA_selection_results,file="DFA results/0_DFA_selection_test_27.rds")

#stop cluster
stopCluster(cl)

#extract DFA results
dfa_CI <- read_rds("DFA results/kemz_FAOarea_27_trend_3_structure_diagonal and equal.rds")

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

ggplot(filter(trend_rot, type == "mean"))+
  geom_line(aes(x = year, y = trend1), color = "blue")+
  geom_line(aes(x = year, y = trend2), color = "red")+
  geom_line(aes(x = year, y = trend3), color = "darkgreen")
  













