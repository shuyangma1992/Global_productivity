library(tidyverse)
library(tidyterra)
library(terra)
library(lubridate)

# 1 Global Ocean Ensemble Physics Reanalysis ------------------------------
#data files
GOEPR_files <- list.files("D:/Global Ocean Ensemble Physics Reanalysis/")

#loop
GOEPR_temperature <- NULL
GOEPR_salinity <- NULL
GOEPR_seasurfaceheight <- NULL
GOEPR_mixedlayerthickness <- NULL
for (i in 1:length(GOEPR_files)) {
  
  # i=319
  #open file
  GOEPR_loop <- sds(paste0("D:/Global Ocean Ensemble Physics Reanalysis/",GOEPR_files[i]))
  
  #extract data
  # GOEPR_loop_temperature <- mean(select(GOEPR_loop["thetao_mean"],1:18),na.rm=T) #0-50m mean
  GOEPR_loop_salinity <-  mean(select(GOEPR_loop["so_mean"],1:18),na.rm=T)#0-50m mean
  # GOEPR_loop_seasurfaceheight <- GOEPR_loop["zos_mean"]
  # GOEPR_loop_mixedlayerthickness <- GOEPR_loop["mlotst_mean"]
  
  #change to data frame
  time_loop <- str_sub(GOEPR_files[i],start = -9,end = -4)
  # GOEPR_loop_temperature <- as.data.frame(GOEPR_loop_temperature,xy=T) %>% 
  #   mutate(time=time_loop) %>% 
  #   rename(t=3)
  GOEPR_loop_salinity <- as.data.frame(GOEPR_loop_salinity,xy=T) %>%
    mutate(time=time_loop) %>%
    rename(s=3)
  # GOEPR_loop_seasurfaceheight <- as.data.frame(GOEPR_loop_seasurfaceheight,xy=T) %>% 
  #   mutate(time=time_loop) %>% 
  #   rename(ssh=3)
  # GOEPR_loop_mixedlayerthickness <- as.data.frame(GOEPR_loop_mixedlayerthickness,xy=T) %>% 
  #   mutate(time=time_loop) %>% 
  #   rename(mld=3)
  
 #combine data
  # GOEPR_temperature <- bind_rows(GOEPR_temperature,GOEPR_loop_temperature)
  GOEPR_salinity <- bind_rows(GOEPR_salinity,GOEPR_loop_salinity)
  # GOEPR_seasurfaceheight <- bind_rows(GOEPR_seasurfaceheight,GOEPR_loop_seasurfaceheight)
  # GOEPR_mixedlayerthickness <- bind_rows(GOEPR_mixedlayerthickness,GOEPR_loop_mixedlayerthickness)
  
  print(i)
  
}

write_rds(GOEPR_temperature,file = "global temperature.rds")
write_rds(GOEPR_salinity,file = "global salinity.rds")
head(GOEPR_salinity)
tail(GOEPR_salinity)
write_rds(GOEPR_seasurfaceheight,file = "global sea surface height.rds")
write_rds(GOEPR_mixedlayerthickness,file = "global mixed layer thickness.rds")

# 2 Global Ocean Biogeochemistry Hindcast ------------------------------
#data files
GOBH_files <- list.files("D:/Global Ocean Biogeochemistry Hindcast/")

#loop
GOBH_chlorophyll <- NULL
GOBH_dissolvedoxygen <- NULL
GOBH_primaryproduction <- NULL
GOBH_ph <- NULL
for (i in 1:length(GOBH_files)) {
  
  
  #open file
  GOBH_loop <- sds(paste0("D:/Global Ocean Biogeochemistry Hindcast/",GOBH_files[i]))
  
  #extract data
  #chlorophyll, dissolved oxygen, primary production, ph
  GOBH_loop_chlorophyll <- mean(select(GOBH_loop["chl"],1:18)) #0-50m
  GOBH_loop_dissolvedoxygen <- mean(select(GOBH_loop["o2"],1:18)) #0-50m
  GOBH_loop_primaryproduction <- mean(select(GOBH_loop["nppv"],1:18)) #0-50m
  GOBH_loop_ph <- mean(select(GOBH_loop["ph"],1:18)) #0-50m
  
  #change to data frame
  time_loop <- unique(time(GOBH_loop["chl"]))
  GOBH_loop_chlorophyll <- as.data.frame(GOBH_loop_chlorophyll,xy=T) %>% 
    mutate(time=time_loop) %>% 
    rename(chl=3)
  GOBH_loop_dissolvedoxygen <- as.data.frame(GOBH_loop_dissolvedoxygen,xy=T) %>% 
    mutate(time=time_loop) %>% 
    rename(do=3)
  GOBH_loop_primaryproduction <- as.data.frame(GOBH_loop_primaryproduction,xy=T) %>% 
    mutate(time=time_loop) %>% 
    rename(npp=3)
  GOBH_loop_ph <- as.data.frame(GOBH_loop_ph,xy=T) %>% 
    mutate(time=time_loop) %>% 
    rename(ph=3)
  
  #combine data
  GOBH_chlorophyll <- bind_rows(GOBH_chlorophyll,GOBH_loop_chlorophyll)
  GOBH_dissolvedoxygen <- bind_rows(GOBH_dissolvedoxygen,GOBH_loop_dissolvedoxygen)
  GOBH_primaryproduction <- bind_rows(GOBH_primaryproduction,GOBH_loop_primaryproduction)
  GOBH_ph <- bind_rows(GOBH_ph,GOBH_loop_ph)
  
  print(i)
  
}

write_rds(GOBH_chlorophyll,file = "global chlorophyll.rds")
write_rds(GOBH_dissolvedoxygen,file = "global dissolved oxygen.rds")
write_rds(GOBH_primaryproduction,file = "global primary production.rds")
write_rds(GOBH_ph,file = "global ph.rds")









