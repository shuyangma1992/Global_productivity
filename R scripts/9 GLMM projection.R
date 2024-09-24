library(tidyverse)
library(RColorBrewer)
library(ggdist)
library(glmmTMB)
library(performance)
library(psych)
library(DHARMa) #glmm dignose
library(broom.mixed)
library(glmtoolbox)
library(foreach)
library(doParallel)
library(lubridate)

#Font
windowsFonts(B=windowsFont("Calibri"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_npg()(10))
# show_col(pal_npg()(10))
mypal <- pal_npg()(10)

# 1 Productivity data compile ----------------------------------------------------------
#stock productivity
productivity <- read_rds("Data/productivity.rds")

#stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,GRSF_uuid,primary_FAOarea)

#combine data
productivity <- left_join(productivity,stock_success)

# 2 CIMP6 stock-specific NorESM2-LM data compile --------------------------------------------
#data file
biophysics_file_name <- list.files("Data/CMIP6 stocks/NorESM2-LM/") 
file_name_split <- matrix(unlist(str_split(biophysics_file_name,"_")),nrow = 2) #use uuid
biophysics_file_name <- unique(file_name_split[1,])

#loop
biophysics <- NULL
for (i in biophysics_file_name) {
  
  # i <- biophysics_file_name[1]
  
  #read data
  chl_loop <- read_rds(paste0("Data/CMIP6 stocks/NorESM2-LM/",i,"_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6 stocks/NorESM2-LM/",i,"_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6 stocks/NorESM2-LM/",i,"_mlotst.rds"))
  
  #GRSF_uuid
  uuid <- i
  
  #combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop,thetao_loop) %>%
    left_join(mlotst_loop) %>% 
    mutate(year = year(time),
           month = month(time))
  
  #calculate annual averages
  biophysics_loop <- biophysics_loop %>% 
    group_by(year, ssp) %>% 
    summarise(chl = mean(chl),
              thetao = mean(thetao),
              mlotst = mean(mlotst)) %>% 
    ungroup()
  
  #mutate uuid
  biophysics_loop <- biophysics_loop %>% 
    mutate(GRSF_uuid=uuid)
  
  #combine data
  biophysics <- bind_rows(biophysics,biophysics_loop)
  
  print(i)
  
}

biophysics <- biophysics %>% 
  mutate(ESM = "NorESM2-LM")

write_rds(biophysics, file = "Data/CMIP6_NorESM2-LM_for_glmm_projection.rds")




# 3 CIMP6 stock-specific MPI-ESM1-2-LR data compile --------------------------------------------
#data file
biophysics_file_name <- list.files("Data/CMIP6 stocks/MPI-ESM1-2-LR/") 
file_name_split <- matrix(unlist(str_split(biophysics_file_name,"_")),nrow = 2) #use uuid
biophysics_file_name <- unique(file_name_split[1,])

#loop
biophysics <- NULL
for (i in biophysics_file_name) {
  
  # i <- biophysics_file_name[1]
  
  #read data
  chl_loop <- read_rds(paste0("Data/CMIP6 stocks/MPI-ESM1-2-LR/",i,"_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6 stocks/MPI-ESM1-2-LR/",i,"_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6 stocks/MPI-ESM1-2-LR/",i,"_mlotst.rds"))
  
  #GRSF_uuid
  uuid <- i
  
  #combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop,thetao_loop) %>%
    left_join(mlotst_loop) %>% 
    mutate(year = year(time),
           month = month(time))
  
  #calculate annual averages
  biophysics_loop <- biophysics_loop %>% 
    group_by(year, ssp) %>% 
    summarise(chl = mean(chl),
              thetao = mean(thetao),
              mlotst = mean(mlotst)) %>% 
    ungroup()
  
  #mutate uuid
  biophysics_loop <- biophysics_loop %>% 
    mutate(GRSF_uuid=uuid)
  
  #combine data
  biophysics <- bind_rows(biophysics,biophysics_loop)
  
  print(i)
  
}

biophysics <- biophysics %>% 
  mutate(ESM = "MPI-ESM1-2-LR")

write_rds(biophysics, file = "Data/CMIP6_MPI-ESM1-2-LR_for_glmm_projection.rds")







# 4 CIMP6 stock-specific IPSL-CM6A-LR data compile --------------------------------------------
#data file
biophysics_file_name <- list.files("Data/CMIP6 stocks/IPSL-CM6A-LR/") 
file_name_split <- matrix(unlist(str_split(biophysics_file_name,"_")),nrow = 2) #use uuid
biophysics_file_name <- unique(file_name_split[1,])

#loop
biophysics <- NULL
for (i in biophysics_file_name) {
  
  # i <- biophysics_file_name[1]
  
  #read data
  chl_loop <- read_rds(paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/",i,"_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/",i,"_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/",i,"_mlotst.rds"))
  
  #GRSF_uuid
  uuid <- i
  
  #combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop,thetao_loop) %>%
    left_join(mlotst_loop) %>% 
    mutate(year = year(time),
           month = month(time))
  
  #calculate annual averages
  biophysics_loop <- biophysics_loop %>% 
    group_by(year, ssp) %>% 
    summarise(chl = mean(chl),
              thetao = mean(thetao),
              mlotst = mean(mlotst)) %>% 
    ungroup()
  
  #mutate uuid
  biophysics_loop <- biophysics_loop %>% 
    mutate(GRSF_uuid=uuid)
  
  #combine data
  biophysics <- bind_rows(biophysics,biophysics_loop)
  
  print(i)
  
}

biophysics <- biophysics %>% 
  mutate(ESM = "IPSL-CM6A-LR")

write_rds(biophysics, file = "Data/CMIP6_IPSL-CM6A-LR_for_glmm_projection.rds")






# 5 Data combination and clean --------------------------------------------
#species name
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(GRSF_uuid,stockid,scientificname)

#CMIP6 data change chl
CMIP6_Nor <- read_rds("Data/CMIP6_NorESM2-LM_for_glmm_projection.rds") %>% 
  left_join(species_information) %>% 
  mutate(chl = chl*1000*1000) %>% #kg to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)
CMIP6_MPI <- read_rds("Data/CMIP6_MPI-ESM1-2-LR_for_glmm_projection.rds") %>% 
  left_join(species_information)%>% 
  mutate(chl = chl*1000*1000) %>% #kg to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)
CMIP6_IPSL <- read_rds("Data/CMIP6_IPSL-CM6A-LR_for_glmm_projection.rds") %>% 
  left_join(species_information)%>% 
  mutate(chl = chl*1000) %>% #g to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)


CMIP6_Nor <- CMIP6_Nor %>% 
  drop_na() 
unique(CMIP6_Nor$stockid)
unique(CMIP6_Nor$scientificname)

CMIP6_MPI <- CMIP6_MPI %>% 
  drop_na() 
unique(CMIP6_MPI$scientificname)
unique(CMIP6_MPI$stockid)

CMIP6_IPSL <- CMIP6_IPSL %>% 
  drop_na() 
unique(CMIP6_IPSL$stockid)
unique(CMIP6_IPSL$scientificname) # one new species 

# 6 glmmTMB projection ---------------------------------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds")
productivity_for_glmm <- productivity_for_glmm %>% 
  drop_na()
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm,species_information)
unique(productivity_for_glmm$scientificname)

#adjust data ipsl as a new species in it
CMIP6_IPSL <- CMIP6_IPSL %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_IPSL$stockid)
unique(CMIP6_IPSL$scientificname)

CMIP6_MPI <- CMIP6_MPI %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_MPI$stockid)
unique(CMIP6_MPI$scientificname)

CMIP6_Nor <- CMIP6_Nor %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_Nor$stockid)
unique(CMIP6_Nor$scientificname)

#set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000,size=1000,replace = F)

#loop
productivity_projection_Nor <- CMIP6_Nor
productivity_projection_MPI <- CMIP6_MPI
productivity_projection_IPSL <- CMIP6_IPSL 
for (i in 1:3) {
  
  # i=1
  
  #data for loop
  data_loop <- productivity_for_glmm %>% 
    select(year,stockid,primary_FAOarea,scientificname, #basic information
           temperature,chlorophyll,mixedlayerthickness, #environment drivers
           as.character(resample_number[i])) %>% 
    rename(productivity=8)
  
  # #3 year moving avearage
  # data_loop <- data_loop %>% 
  #   group_by(stockid) %>% 
  #   mutate(temperature=zoo::rollmean(temperature,3,fill=NA),
  #          chlorophyll=zoo::rollmean(chlorophyll,3,fill=NA),
  #          mixedlayerthickness=zoo::rollmean(mixedlayerthickness,3,fill=NA),
  #          productivity=zoo::rollmean(productivity,3,fill=NA)) %>% 
  #   ungroup() %>% 
  #   drop_na()
  
  #best model
  #model 6, random slope (two variables, temperature+chlorophyll), random intercept
  model6 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature+chlorophyll|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  #NorESM-LM
  productivity_projection_Nor_loop <- CMIP6_Nor %>% 
    mutate(predict(model6, newdata = CMIP6_Nor, allow.new.levels=FALSE)) 
  colnames(productivity_projection_Nor_loop)[10] <- i #rename with number
  productivity_projection_Nor <- left_join(productivity_projection_Nor, productivity_projection_Nor_loop)
    
  #MPI-ESM1-2-LR
  productivity_projection_MPI_loop <- CMIP6_MPI %>% 
    mutate(predict(model6, newdata = CMIP6_MPI, allow.new.levels=FALSE)) 
  colnames(productivity_projection_MPI_loop)[10] <- i #rename with number
  productivity_projection_MPI <- left_join(productivity_projection_MPI, productivity_projection_MPI_loop)
  
  #IPSL-CM6A-LR
  productivity_projection_IPSL_loop <- CMIP6_IPSL %>% 
    mutate(predict(model6, newdata = CMIP6_IPSL, allow.new.levels=FALSE)) 
  colnames(productivity_projection_IPSL_loop)[10] <- i #rename with number
  productivity_projection_IPSL <- left_join(productivity_projection_IPSL, productivity_projection_IPSL_loop)
  print(i)

}

write_rds(productivity_projection_Nor, file = "productivity_projection_NorESM2-LM.rds")
write_rds(productivity_projection_MPI, file = "productivity_projection_MPI-ESM1-2-LR.rds")
write_rds(productivity_projection_IPSL, file = "productivity_projection_IPSL-CM6A-LR.rds")

a <- simulateResiduals(model6)

plot(a)

r2_nakagawa(model6)


# 7 Results inspection ----------------------------------------------------
productivity_projection_Nor <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM.rds") 
productivity_projection_MPI <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")
productivity_projection_IPSL <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")
productivity_projection_Nor <- productivity_projection_Nor %>% 
  drop_na()
unique(productivity_projection_Nor$stockid) #633 stocks
unique(productivity_projection_Nor$scientificname) #264 species

productivity_projection_MPI <- productivity_projection_MPI %>% 
  drop_na()
unique(productivity_projection_MPI$stockid) #627 stocks
unique(productivity_projection_MPI$scientificname) #263 species

productivity_projection_IPSL <- productivity_projection_IPSL %>% 
  drop_na()
unique(productivity_projection_IPSL$stockid) #678 stocks
unique(productivity_projection_IPSL$scientificname) #262 species

atlantic_cod <- filter(productivity_projection_Nor, scientificname == "Gadus morhua")

# 8 Productivity projection trend 2021-2100 ------------------------------------------
#productivity projection 2021-2100
productivity_projection_Nor <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM.rds")
productivity_projection_MPI <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")
productivity_projection_IPSL <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")

#stocks
unique(productivity_projection_Nor$stockid) #633 stocks
unique(productivity_projection_MPI$stockid) #627 stocks
unique(productivity_projection_IPSL$stockid) #678 stocks

#SSP1-2.6
productivity_projection_trend_ssp126 <- NULL
for (i in unique(productivity_projection_Nor$stockid)) {
  
  # i <- unique(productivity_projection_Nor$stockid)[1]
  
  #SSP1-2.6
  productivity_loop_ssp126_Nor <- filter(productivity_projection_Nor,stockid==i&ssp=="ssp126") %>% 
    filter(year > 2020)
  productivity_loop_ssp126_MPI <- filter(productivity_projection_MPI,stockid==i&ssp=="ssp126") %>% 
    filter(year > 2020)
  productivity_loop_ssp126_IPSL <- filter(productivity_projection_IPSL,stockid==i&ssp=="ssp126") %>% 
    filter(year > 2020)
  
  if(is.na(productivity_loop_ssp126_Nor[10,10])|is.na(productivity_loop_ssp126_MPI[10,10])|is.na(productivity_loop_ssp126_IPSL[10,10]))
  {next} else {
    
  #linear regression on year (trend), Nor
  lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp126_Nor[,10:1009])~productivity_loop_ssp126_Nor$year))
  p_slope_Nor <- NULL
  p_slope_significance_Nor <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_Nor <- c(p_slope_Nor,lm_results_Nor[[j]]$coefficients[2,1]) #slope
    p_slope_significance_Nor <- c(p_slope_significance_Nor,lm_results_Nor[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_Nor <- quantile(p_slope_Nor,0.025)
  p_slope_mean_Nor <- mean(p_slope_Nor)
  p_slope_uci_Nor <- quantile(p_slope_Nor,0.975)
  
  p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp126_loop_Nor <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_Nor,
                                                              p_slope_mean = p_slope_mean_Nor,
                                                              p_slope_uci = p_slope_uci_Nor,
                                                              p_slope_significance_95 = p_slope_significance_95_Nor,
                                                              ESM = "NorESM2-LM",
                                                              ssp = "ssp126")
  
  #linear regression on year (trend), MPI
  lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp126_MPI[,10:1009])~productivity_loop_ssp126_MPI$year))
  p_slope_MPI <- NULL
  p_slope_significance_MPI <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_MPI <- c(p_slope_MPI,lm_results_MPI[[j]]$coefficients[2,1]) #slope
    p_slope_significance_MPI <- c(p_slope_significance_MPI,lm_results_MPI[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_MPI <- quantile(p_slope_MPI,0.025)
  p_slope_mean_MPI <- mean(p_slope_MPI)
  p_slope_uci_MPI <- quantile(p_slope_MPI,0.975)
  
  p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp126_loop_MPI <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_MPI,
                                                              p_slope_mean = p_slope_mean_MPI,
                                                              p_slope_uci = p_slope_uci_MPI,
                                                              p_slope_significance_95 = p_slope_significance_95_MPI,
                                                              ESM = "MPI-ESM1-2-LR",
                                                              ssp = "ssp126")
  
  #linear regression on year (trend), IPSL
  lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp126_IPSL[,10:1009])~productivity_loop_ssp126_IPSL$year))
  p_slope_IPSL <- NULL
  p_slope_significance_IPSL <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_IPSL <- c(p_slope_IPSL,lm_results_IPSL[[j]]$coefficients[2,1]) #slope
    p_slope_significance_IPSL <- c(p_slope_significance_IPSL,lm_results_IPSL[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_IPSL <- quantile(p_slope_IPSL,0.025)
  p_slope_mean_IPSL <- mean(p_slope_IPSL)
  p_slope_uci_IPSL <- quantile(p_slope_IPSL,0.975)
  
  p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp126_loop_IPSL <- data.frame(stockid = i,
                                                               p_slope_lci = p_slope_lci_IPSL,
                                                               p_slope_mean = p_slope_mean_IPSL,
                                                               p_slope_uci = p_slope_uci_IPSL,
                                                               p_slope_significance_95 = p_slope_significance_95_IPSL,
                                                               ESM = "IPSL-CM6A-LR",
                                                               ssp = "ssp126")
  
  #combind data
  productivity_projection_trend_ssp126 <- bind_rows(productivity_projection_trend_ssp126,
                                                    productivity_projection_trend_ssp126_loop_Nor,
                                                    productivity_projection_trend_ssp126_loop_MPI,
                                                    productivity_projection_trend_ssp126_loop_IPSL)
  }
  
  print(i)
  
}

#save data
write_rds(productivity_projection_trend_ssp126, 
          file="Data/productivity_projection_trend_ssp126_2021_2100.rds")

#SSP2-4.5
productivity_projection_trend_ssp245 <- NULL
for (i in unique(productivity_projection_Nor$stockid)) {
  
  # i <- unique(productivity_projection_Nor$stockid)[1]
  
  #SSP1-2.6
  productivity_loop_ssp245_Nor <- filter(productivity_projection_Nor,stockid==i&ssp=="ssp245") %>% 
    filter(year > 2020)
  productivity_loop_ssp245_MPI <- filter(productivity_projection_MPI,stockid==i&ssp=="ssp245") %>% 
    filter(year > 2020)
  productivity_loop_ssp245_IPSL <- filter(productivity_projection_IPSL,stockid==i&ssp=="ssp245") %>% 
    filter(year > 2020)
  
  if(is.na(productivity_loop_ssp245_Nor[10,10])|is.na(productivity_loop_ssp245_MPI[10,10])|is.na(productivity_loop_ssp245_IPSL[10,10]))
  {next} else {
    
  #linear regression on year (trend), Nor
  lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp245_Nor[,10:1009])~productivity_loop_ssp245_Nor$year))
  p_slope_Nor <- NULL
  p_slope_significance_Nor <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_Nor <- c(p_slope_Nor,lm_results_Nor[[j]]$coefficients[2,1]) #slope
    p_slope_significance_Nor <- c(p_slope_significance_Nor,lm_results_Nor[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_Nor <- quantile(p_slope_Nor,0.025)
  p_slope_mean_Nor <- mean(p_slope_Nor)
  p_slope_uci_Nor <- quantile(p_slope_Nor,0.975)
  
  p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp245_loop_Nor <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_Nor,
                                                              p_slope_mean = p_slope_mean_Nor,
                                                              p_slope_uci = p_slope_uci_Nor,
                                                              p_slope_significance_95 = p_slope_significance_95_Nor,
                                                              ESM = "NorESM2-LM",
                                                              ssp = "ssp245")
  
  #linear regression on year (trend), MPI
  lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp245_MPI[,10:1009])~productivity_loop_ssp245_MPI$year))
  p_slope_MPI <- NULL
  p_slope_significance_MPI <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_MPI <- c(p_slope_MPI,lm_results_MPI[[j]]$coefficients[2,1]) #slope
    p_slope_significance_MPI <- c(p_slope_significance_MPI,lm_results_MPI[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_MPI <- quantile(p_slope_MPI,0.025)
  p_slope_mean_MPI <- mean(p_slope_MPI)
  p_slope_uci_MPI <- quantile(p_slope_MPI,0.975)
  
  p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp245_loop_MPI <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_MPI,
                                                              p_slope_mean = p_slope_mean_MPI,
                                                              p_slope_uci = p_slope_uci_MPI,
                                                              p_slope_significance_95 = p_slope_significance_95_MPI,
                                                              ESM = "MPI-ESM1-2-LR",
                                                              ssp = "ssp245")
  
  #linear regression on year (trend), IPSL
  lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp245_IPSL[,10:1009])~productivity_loop_ssp245_IPSL$year))
  p_slope_IPSL <- NULL
  p_slope_significance_IPSL <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_IPSL <- c(p_slope_IPSL,lm_results_IPSL[[j]]$coefficients[2,1]) #slope
    p_slope_significance_IPSL <- c(p_slope_significance_IPSL,lm_results_IPSL[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_IPSL <- quantile(p_slope_IPSL,0.025)
  p_slope_mean_IPSL <- mean(p_slope_IPSL)
  p_slope_uci_IPSL <- quantile(p_slope_IPSL,0.975)
  
  p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp245_loop_IPSL <- data.frame(stockid = i,
                                                               p_slope_lci = p_slope_lci_IPSL,
                                                               p_slope_mean = p_slope_mean_IPSL,
                                                               p_slope_uci = p_slope_uci_IPSL,
                                                               p_slope_significance_95 = p_slope_significance_95_IPSL,
                                                               ESM = "IPSL-CM6A-LR",
                                                               ssp = "ssp245")
  
  #combind data
  productivity_projection_trend_ssp245 <- bind_rows(productivity_projection_trend_ssp245,
                                                    productivity_projection_trend_ssp245_loop_Nor,
                                                    productivity_projection_trend_ssp245_loop_MPI,
                                                    productivity_projection_trend_ssp245_loop_IPSL)
  
  }
  
  print(i)
  
}

#save data
write_rds(productivity_projection_trend_ssp245, 
          file="Data/productivity_projection_trend_ssp245_2021_2100.rds")



#SSP5-8.5
productivity_projection_trend_ssp585 <- NULL
for (i in unique(productivity_projection_Nor$stockid)) {
  
  # i <- unique(productivity_projection_Nor$stockid)[1]
  
  #SSP1-2.6
  productivity_loop_ssp585_Nor <- filter(productivity_projection_Nor,stockid==i&ssp=="ssp585") %>% 
    filter(year > 2020)
  productivity_loop_ssp585_MPI <- filter(productivity_projection_MPI,stockid==i&ssp=="ssp585") %>% 
    filter(year > 2020)
  productivity_loop_ssp585_IPSL <- filter(productivity_projection_IPSL,stockid==i&ssp=="ssp585") %>% 
    filter(year > 2020)
  
  if(is.na(productivity_loop_ssp585_Nor[10,10])|is.na(productivity_loop_ssp585_MPI[10,10])|is.na(productivity_loop_ssp585_IPSL[10,10]))
  {next} else {
    
  #linear regression on year (trend), Nor
  lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp585_Nor[,10:1009])~productivity_loop_ssp585_Nor$year))
  p_slope_Nor <- NULL
  p_slope_significance_Nor <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_Nor <- c(p_slope_Nor,lm_results_Nor[[j]]$coefficients[2,1]) #slope
    p_slope_significance_Nor <- c(p_slope_significance_Nor,lm_results_Nor[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_Nor <- quantile(p_slope_Nor,0.025)
  p_slope_mean_Nor <- mean(p_slope_Nor)
  p_slope_uci_Nor <- quantile(p_slope_Nor,0.975)
  
  p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp585_loop_Nor <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_Nor,
                                                              p_slope_mean = p_slope_mean_Nor,
                                                              p_slope_uci = p_slope_uci_Nor,
                                                              p_slope_significance_95 = p_slope_significance_95_Nor,
                                                              ESM = "NorESM2-LM",
                                                              ssp = "ssp585")
  
  #linear regression on year (trend), MPI
  lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp585_MPI[,10:1009])~productivity_loop_ssp585_MPI$year))
  p_slope_MPI <- NULL
  p_slope_significance_MPI <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_MPI <- c(p_slope_MPI,lm_results_MPI[[j]]$coefficients[2,1]) #slope
    p_slope_significance_MPI <- c(p_slope_significance_MPI,lm_results_MPI[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_MPI <- quantile(p_slope_MPI,0.025)
  p_slope_mean_MPI <- mean(p_slope_MPI)
  p_slope_uci_MPI <- quantile(p_slope_MPI,0.975)
  
  p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp585_loop_MPI <- data.frame(stockid = i,
                                                              p_slope_lci = p_slope_lci_MPI,
                                                              p_slope_mean = p_slope_mean_MPI,
                                                              p_slope_uci = p_slope_uci_MPI,
                                                              p_slope_significance_95 = p_slope_significance_95_MPI,
                                                              ESM = "MPI-ESM1-2-LR",
                                                              ssp = "ssp585")
  
  #linear regression on year (trend), IPSL
  lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp585_IPSL[,10:1009])~productivity_loop_ssp585_IPSL$year))
  p_slope_IPSL <- NULL
  p_slope_significance_IPSL <- NULL
  for (j in 1:1000) {
    
    # j=1
    p_slope_IPSL <- c(p_slope_IPSL,lm_results_IPSL[[j]]$coefficients[2,1]) #slope
    p_slope_significance_IPSL <- c(p_slope_significance_IPSL,lm_results_IPSL[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci_IPSL <- quantile(p_slope_IPSL,0.025)
  p_slope_mean_IPSL <- mean(p_slope_IPSL)
  p_slope_uci_IPSL <- quantile(p_slope_IPSL,0.975)
  
  p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL,0.95)
  
  #make a data frame
  productivity_projection_trend_ssp585_loop_IPSL <- data.frame(stockid = i,
                                                               p_slope_lci = p_slope_lci_IPSL,
                                                               p_slope_mean = p_slope_mean_IPSL,
                                                               p_slope_uci = p_slope_uci_IPSL,
                                                               p_slope_significance_95 = p_slope_significance_95_IPSL,
                                                               ESM = "IPSL-CM6A-LR",
                                                               ssp = "ssp585")
  
  #combind data
  productivity_projection_trend_ssp585 <- bind_rows(productivity_projection_trend_ssp585,
                                                    productivity_projection_trend_ssp585_loop_Nor,
                                                    productivity_projection_trend_ssp585_loop_MPI,
                                                    productivity_projection_trend_ssp585_loop_IPSL)
  }
  
  print(i)
  
}

#save data
write_rds(productivity_projection_trend_ssp585, 
          file="Data/productivity_projection_trend_ssp585_2021_2100.rds")



