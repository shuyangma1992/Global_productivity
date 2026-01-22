library(tidyverse)
library(tidyterra)
library(terra)
library(geojsonR)
library(lubridate)
library(foreach)
library(doParallel)

# 1 IPSL-CM6A-LR chl------------------------------------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#stock full information
stock_full_information <- read_rds("Data/stock_success_full_information_final.rds") 

#stock
stock <- left_join(stock_full_information,stock_distribution)

#data files
var_files <- list.files("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/")
var_files <- var_files[startsWith(var_files,"chl")]

#-------------------------------------------------------------------run parallel 
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 4));

#register cores
registerDoParallel(cl)   

#run parallel
env_data_run_parallel <- foreach(i=1:nrow(stock), .combine = "rbind",.packages = c("tidyverse","geojsonR","terra","tidyterra")) %do% {
  
  # i=1
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  env_dat_stock <- foreach(j=var_files, .combine = "rbind", .packages = c("tidyverse","geojsonR","terra","tidyterra")) %dopar% {
    
    # j=var_files[1]
    
    #stock distribution loop
    #geojson data
    char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
    
    crdref <- "+proj=longlat +datum=WGS84"
    
    #change it to shapefile
    stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
    # plot(stock_distribution_loop)
    
    #crop (window, more effective but only useful when it is rectangle) GOEPR data set by stock distribution
    var_loop <- rast(readRDS(paste0("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/",j)))
    # plot(var_loop,1)
    var_loop <- crop(var_loop,stock_distribution_loop,mask=T)
    # plot(var_loop,1)
    
    #variable mean
    var_mean_loop <- global(var_loop,"mean",na.rm=T)
  
    #make data
    env_dat_loop <- var_mean_loop %>% 
      rownames_to_column(var = "time") %>% 
      rename("chl"=mean) %>% 
      mutate(uuid=stock_GRSF_uuid_loop) %>% 
      mutate(ssp=str_sub(j,23,28))
      
  }
  
  write_rds(env_dat_stock,file=paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/", stock_GRSF_uuid_loop,"_chl.rds"))
                                            
}

#save information
# write_rds(env_data_run_parallel,file="env_data_run_parallel.rds")

#stop cluster
stopCluster(cl)





# 2 IPSL-CM6A-LR thetao------------------------------------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#stock full information
stock_full_information <- read_rds("Data/stock_success_full_information_final.rds") 

#stock
stock <- left_join(stock_full_information,stock_distribution)

#data files
var_files <- list.files("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/")
var_files <- var_files[startsWith(var_files,"thetao")]

#-------------------------------------------------------------------run parallel 
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 4));

#register cores
registerDoParallel(cl)   

#run parallel
env_data_run_parallel <- foreach(i=1:nrow(stock), .combine = "rbind",.packages = c("tidyverse","geojsonR","terra","tidyterra")) %do% {
  
  # i=1
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  env_dat_stock <- foreach(j=var_files, .combine = "rbind", .packages = c("tidyverse","geojsonR","terra","tidyterra")) %dopar% {
    
    # j=var_files[1]
    
    #stock distribution loop
    #geojson data
    char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
    
    crdref <- "+proj=longlat +datum=WGS84"
    
    #change it to shapefile
    stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
    # plot(stock_distribution_loop)
    
    #crop (window, more effective but only useful when it is rectangle) GOEPR data set by stock distribution
    var_loop <- rast(readRDS(paste0("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/",j)))
    # plot(var_loop,1)
    var_loop <- crop(var_loop,stock_distribution_loop,mask=T)
    # plot(var_loop,1)
    
    #variable mean
    var_mean_loop <- global(var_loop,"mean",na.rm=T)
    
    #make data
    env_dat_loop <- var_mean_loop %>% 
      rownames_to_column(var = "time") %>% 
      rename("thetao"=mean) %>% 
      mutate(uuid=stock_GRSF_uuid_loop) %>% 
      mutate(ssp=str_sub(j,26,31))
    
  }
  
  write_rds(env_dat_stock,file=paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/", stock_GRSF_uuid_loop,"_thetao.rds"))
  
}

#save information
# write_rds(env_data_run_parallel,file="env_data_run_parallel.rds")

#stop cluster
stopCluster(cl)







# 3 IPSL-CM6A-LR mlotst------------------------------------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#stock full information
stock_full_information <- read_rds("Data/stock_success_full_information_final.rds") 

#stock
stock <- left_join(stock_full_information,stock_distribution)

#data files
var_files <- list.files("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/")
var_files <- var_files[startsWith(var_files,"mlotst")]

#-------------------------------------------------------------------run parallel 
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 4));

#register cores
registerDoParallel(cl)   

#run parallel
env_data_run_parallel <- foreach(i=1:nrow(stock), .combine = "rbind",.packages = c("tidyverse","geojsonR","terra","tidyterra")) %do% {
  
  # i=1
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  env_dat_stock <- foreach(j=var_files, .combine = "rbind", .packages = c("tidyverse","geojsonR","terra","tidyterra")) %dopar% {
    
    # j=var_files[1]
    
    #stock distribution loop
    #geojson data
    char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
    
    crdref <- "+proj=longlat +datum=WGS84"
    
    #change it to shapefile
    stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
    # plot(stock_distribution_loop)
    
    #crop (window, more effective but only useful when it is rectangle) GOEPR data set by stock distribution
    var_loop <- rast(readRDS(paste0("D:/CMIP6/Compiled/IPSL-CM6A-LR compiled/",j)))
    # plot(var_loop,1)
    var_loop <- crop(var_loop,stock_distribution_loop,mask=T)
    # plot(var_loop,1)
    
    #variable mean
    var_mean_loop <- global(var_loop,"mean",na.rm=T)
    
    #make data
    env_dat_loop <- var_mean_loop %>% 
      rownames_to_column(var = "time") %>% 
      rename("mlotst"=mean) %>% 
      mutate(uuid=stock_GRSF_uuid_loop) %>% 
      mutate(ssp=str_sub(j,26,31))
    
  }
  
  write_rds(env_dat_stock,file=paste0("Data/CMIP6 stocks/IPSL-CM6A-LR/", stock_GRSF_uuid_loop,"_mlotst.rds"))
  
}

#save information
# write_rds(env_data_run_parallel,file="env_data_run_parallel.rds")

#stop cluster
stopCluster(cl)







# test --------------------------------------------------------------------
a <- read_rds("Data/CMIP6 stocks/IPSL-CM6A-LR/fe0c21c8-8872-36bc-8063-d3fc3fbe9dff_chl.rds")








