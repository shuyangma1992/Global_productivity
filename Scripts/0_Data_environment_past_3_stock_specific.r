library(tidyverse)
library(tidyterra)
library(terra)
library(geojsonR)
library(lubridate)
library(foreach)
library(doParallel)

# 1 Global Ocean Ensemble Physics Reanalysis ------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#stock full information
stock_full_information <- read_rds("Data/stock_success_full_information_final.rds") 

#stock
stock <- left_join(stock_full_information,stock_distribution)

#data files
GOEPR_files <- list.files("D:/Global Ocean Ensemble Physics Reanalysis/")

#-------------------------------------------------------------------run parallel 
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

#run parallel
env_data_run_parallel <- foreach(i=1:nrow(stock), .combine = "rbind",.packages = c("tidyverse","geojsonR","terra","tidyterra")) %do% {
  
  # i=2
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  env_dat_stock <- foreach(j=GOEPR_files, .combine = "rbind", .packages = c("tidyverse","geojsonR","terra","tidyterra")) %dopar% {
    
    # j=GOEPR_files[1]
    
    #stock distribution loop
    #geojson data
    char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
    
    crdref <- "+proj=longlat +datum=WGS84"
    
    #change it to shapefile
    stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
    # plot(stock_distribution_loop)
    
    #crop (window, more effective but only useful when it is rectangle) GOEPR data set by stock distribution
    GOEPR_loop <- sds(paste0("D:/Global Ocean Ensemble Physics Reanalysis/",j))
    # GOEPR_loop <- crop(GOEPR_loop,stock_distribution_loop,mask=T)
   
    #temperature, salinity, sea surface height, mixed layer thickness
    GOEPR_loop_temperature <- select(GOEPR_loop["thetao_mean"],1:18) #0-50m
    GOEPR_loop_salinity <- select(GOEPR_loop["so_mean"],1:18) #0-50m
    GOEPR_loop_seasurfaceheight <- GOEPR_loop["zos_mean"]
    GOEPR_loop_mixedlayerthickness <- GOEPR_loop["mlotst_mean"]
    
    # #put windows on them (wrong, should use crop with mask=T)
    # window(GOEPR_loop_temperature) <- ext(stock_distribution_loop)
    # window(GOEPR_loop_salinity) <- ext(stock_distribution_loop)
    # window(GOEPR_loop_seasurfaceheight) <- ext(stock_distribution_loop)
    # window(GOEPR_loop_mixedlayerthickness) <- ext(stock_distribution_loop)
    
    #crop with mask=T
    GOEPR_loop_temperature <- crop(GOEPR_loop_temperature,stock_distribution_loop,mask=T)
    GOEPR_loop_salinity <- crop(GOEPR_loop_salinity,stock_distribution_loop,mask=T)
    GOEPR_loop_seasurfaceheight <- crop(GOEPR_loop_seasurfaceheight,stock_distribution_loop,mask=T)
    GOEPR_loop_mixedlayerthickness <- crop(GOEPR_loop_mixedlayerthickness,stock_distribution_loop,mask=T)
    
    #temperature 0-50m
    temperature_0_50_mean_loop <- global(GOEPR_loop_temperature,"mean",na.rm=T)
    # temperature_0_50_mean_loop <- temperature_0_50_mean_loop %>% 
    #   mutate(depth=as.numeric(str_split_fixed(rownames(temperature_0_50_mean_loop), "=",n=2)[,2])) %>% 
    #   filter(depth<=50)
    temperature_mean_loop <- mean(temperature_0_50_mean_loop$mean)
    
    #salinity 0-50m
    salinity_0_50_mean_loop <- global(GOEPR_loop_salinity,"mean",na.rm=T)
    # salinity_0_50_mean_loop <- salinity_0_50_mean_loop %>% 
    #   mutate(depth=as.numeric(str_split_fixed(rownames(salinity_0_50_mean_loop), "=",n=2)[,2])) %>% 
    #   filter(depth<=50)
    salinity_mean_loop <- mean(salinity_0_50_mean_loop$mean)
    
    #sea surface height
    seasurfaceheight_mean_loop <- global(GOEPR_loop_seasurfaceheight,"mean",na.rm=T)[1,1]
    
    #mixed layer thickness
    mixedlayerthickness_mean_loop <- global(GOEPR_loop_mixedlayerthickness,"mean",na.rm=T)[1,1]
    
    #make data
    env_dat_loop <- data.frame(temperature=temperature_mean_loop,
                                salinity=salinity_mean_loop,
                                seasurfaceheight=seasurfaceheight_mean_loop,
                                mixedlayerthickness=mixedlayerthickness_mean_loop,
                                time=time(GOEPR_loop["thetao_mean"])[1],
                                uuid=stock_GRSF_uuid_loop)
    
    
  }
  
  write_rds(env_dat_stock,file=paste0("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/", stock_GRSF_uuid_loop,".rds"))
                                            
}

#save information
# write_rds(env_data_run_parallel,file="env_data_run_parallel.rds")

#stop cluster
stopCluster(cl)


# 2 Global Ocean Biogeochemistry Hindcast ------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#stock full information
stock_full_information <- read_rds("Data/stock_success_full_information_final.rds") 

#stock
stock <- left_join(stock_full_information,stock_distribution)

#data files
GOBH_files <- list.files("D:/Global Ocean Biogeochemistry Hindcast/")

#-------------------------------------------------------------------run parallel 
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

#run parallel
env_data_run_parallel <- foreach(i=1:nrow(stock), .combine = "rbind",.packages = c("tidyverse","geojsonR","terra","tidyterra")) %do% {
  
  # i=2
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  env_dat_stock <- foreach(j=GOBH_files, .combine = "rbind", .packages = c("tidyverse","geojsonR","terra","tidyterra")) %dopar% {
    
    # j=GOBH_files[1]
    
    #stock distribution loop
    #geojson data
    char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
    
    crdref <- "+proj=longlat +datum=WGS84"
    
    #change it to shapefile
    stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
    # plot(stock_distribution_loop)
    
    #crop (window, more effective) GOBH data set by stock distribution
    GOBH_loop <- sds(paste0("D:/Global Ocean Biogeochemistry Hindcast/",j))
    # GOBH_loop <- crop(GOBH_loop,stock_distribution_loop)
    
    #chlorophyll, dissolved oxygen, primary production, ph
    GOBH_loop_chlorophyll <- select(GOBH_loop["chl"],1:18) #0-50m
    GOBH_loop_dissolvedoxygen <- select(GOBH_loop["o2"],1:18) #0-50m
    GOBH_loop_primaryproduction <- select(GOBH_loop["nppv"],1:18) #0-50m
    GOBH_loop_ph <- select(GOBH_loop["ph"],1:18) #0-50m
    
    # #put windows on them
    # window(GOBH_loop_chlorophyll) <- ext(stock_distribution_loop)
    # window(GOBH_loop_dissolvedoxygen) <- ext(stock_distribution_loop)
    # window(GOBH_loop_primaryproduction) <- ext(stock_distribution_loop)
    # window(GOBH_loop_ph) <- ext(stock_distribution_loop)
    
    #crop it with mask=T
    GOBH_loop_chlorophyll <- crop(GOBH_loop_chlorophyll,stock_distribution_loop,mask=T)
    GOBH_loop_dissolvedoxygen <- crop(GOBH_loop_dissolvedoxygen,stock_distribution_loop,mask=T)
    GOBH_loop_primaryproduction <- crop(GOBH_loop_primaryproduction,stock_distribution_loop,mask=T)
    GOBH_loop_ph <- crop(GOBH_loop_ph,stock_distribution_loop,mask=T)
    
    #chlorophyll 0-50m
    chlorophyll_0_50_mean_loop <- global(GOBH_loop_chlorophyll,"mean",na.rm=T)
    chlorophyll_mean_loop <- mean(chlorophyll_0_50_mean_loop$mean)
    
    #dissolved oxygen 0-50m
    dissolvedoxygen_0_50_mean_loop <- global(GOBH_loop_dissolvedoxygen,"mean",na.rm=T)
    dissolvedoxygen_mean_loop <- mean(dissolvedoxygen_0_50_mean_loop$mean)
    
    #primary production 0-50m
    primaryproduction_0_50_mean_loop <- global(GOBH_loop_primaryproduction,"mean",na.rm=T)
    primaryproduction_mean_loop <- mean(primaryproduction_0_50_mean_loop$mean)
    
    #ph 0-50m
    ph_0_50_mean_loop <- global(GOBH_loop_ph,"mean",na.rm=T)
    ph_mean_loop <- mean(ph_0_50_mean_loop$mean)
    
    #make data
    env_dat_loop <- data.frame(chlorophyll=chlorophyll_mean_loop,
                                dissolvedoxygen=dissolvedoxygen_mean_loop,
                                primaryproduction=primaryproduction_mean_loop,
                                ph_mean_loop=ph_mean_loop,
                                time=time(GOBH_loop["chl"])[1],
                                uuid=stock_GRSF_uuid_loop)
    
    
  }
  
  write_rds(env_dat_stock,file=paste0("Data/Copernicus/Global Ocean Biogeochemistry Hindcast/", stock_GRSF_uuid_loop,".rds"))
  
}

#save information
# write_rds(env_data_run_parallel,file="env_data_run_parallel.rds")


#stop cluster
stopCluster(cl)

#test
# read_rds("Data/Copernicus/Global Ocean Biogeochemistry Hindcast/6ef91e5a-4a51-3ff8-bfc9-065cba1a5f79.rds")



# test --------------------------------------------------------------------
#--------------------------------------------------------read environmental data
#spatrasterdataset
test_environment <- sds("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/grepv2_monthly_mnstd_199301.nc") 
test_environment <- sds("Data/Copernicus/test/mercatorfreebiorys2v4_global_mean_199301.nc") 

names(test_environment)
#temperature as an example
test_temperature <- test_environment["thetao_mean"]
test_chlorophyll <- test_environment["chl"]

#---------------------------------------------------read stock distribution data
stock_distribution <- read_rds("Data/stock_distribution.rds")

#geojson data
char_js <-  FROM_GeoJson(url_file_string = stock_distribution[88,2])

crdref <- "+proj=longlat +datum=WGS84"

#change it to shapefile
test_stock_distribution <- vect(char_js$coordinates,type="polygons", crs=crdref)

#plot
plot(test_stock_distribution)

#----------------------------------crop environmental data by stock distribution
#crop
stock_distribution_temperature <- crop(test_temperature,test_stock_distribution)
stock_distribution_chlorophyll <- crop(test_chlorophyll,test_stock_distribution)

#plot
plot(stock_distribution_temperature[[1]])
plot(stock_distribution_chlorophyll[[1]])

#calculate mean of each layer (depth)
stock_distribution_temperature_mean <- global(stock_distribution_temperature,"mean",na.rm=T) %>% 
  mutate(depth=as.numeric(str_split_fixed(rownames(stock_distribution_temperature_mean), "=",n=2)[,2])) 

#filter depth below 50m
stock_distribution_temperature_mean_0_50m <- stock_distribution_temperature_mean %>% 
  filter(depth<=50) 

#calculate 0-50m mean
mean(stock_distribution_temperature_mean_0_50m$mean)





# test loop --------------------------------------------------------------
#stock distribution 
stock_distribution <- read_rds("Data/stock_distribution.rds") %>% 
  drop_na()

#data files
GOEPR_files <- list.files("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/")

#loop make data
env_list <- list()
for (i in 1:3) {
  
  # i <- 1
  stock_GRSF_uuid_loop <- stock_distribution[i,1]
  
  #stock distribution loop
  #geojson data
  char_js <- FROM_GeoJson(url_file_string = stock_distribution[i,2])
  
  crdref <- "+proj=longlat +datum=WGS84"
  
  #change it to shapefile
  stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
  
  # plot(stock_distribution)
  
  #environmental data
  env_dat <- NULL
  for (j in GOEPR_files) {
    
    # j <- GOEPR_files[1]
    
    #crop GOEPR data set by stock distribution
    GOEPR_loop <- sds(paste0("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/",j))
    # GOEPR_loop <- crop(GOEPR_loop,stock_distribution_loop)
    
    #temperature, salinity, sea surface height, mixed layer thickness
    GOEPR_loop_temperature <- select(GOEPR_loop["thetao_mean"],1:18) #0-50m
    GOEPR_loop_salinity <- select(GOEPR_loop["so_mean"],1:18) #0-50m
    GOEPR_loop_seasurfaceheight <- GOEPR_loop["zos_mean"]
    GOEPR_loop_mixedlayerthickness <- GOEPR_loop["mlotst_mean"]
    
    #put windows on them
    window(GOEPR_loop_temperature) <- ext(stock_distribution_loop)
    window(GOEPR_loop_salinity) <- ext(stock_distribution_loop)
    window(GOEPR_loop_seasurfaceheight) <- ext(stock_distribution_loop)
    window(GOEPR_loop_mixedlayerthickness) <- ext(stock_distribution_loop)

    #temperature 0-50m
    temperature_0_50_mean_loop <- global(GOEPR_loop_temperature,"mean",na.rm=T)
    # temperature_0_50_mean_loop <- temperature_0_50_mean_loop %>% 
    #   mutate(depth=as.numeric(str_split_fixed(rownames(temperature_0_50_mean_loop), "=",n=2)[,2])) %>% 
    #   filter(depth<=50)
    temperature_mean_loop <- mean(temperature_0_50_mean_loop$mean)
    
    #salinity 0-50m
    salinity_0_50_mean_loop <- global(GOEPR_loop_salinity,"mean",na.rm=T)
    # salinity_0_50_mean_loop <- salinity_0_50_mean_loop %>% 
    #   mutate(depth=as.numeric(str_split_fixed(rownames(salinity_0_50_mean_loop), "=",n=2)[,2])) %>% 
    #   filter(depth<=50)
    salinity_mean_loop <- mean(salinity_0_50_mean_loop$mean)
    
    #sea surface height
    seasurfaceheight_mean_loop <- global(GOEPR_loop_seasurfaceheight,"mean",na.rm=T)[1,1]
    
    #mixed layer thickness
    mixedlayerthickness_mean_loop <- global(GOEPR_loop_mixedlayerthickness,"mean",na.rm=T)[1,1]
    
    #make data
    env_dat_loop <- data.frame(temperature=temperature_mean_loop,
                                salinity=salinity_mean_loop,
                                seasurfaceheight=seasurfaceheight_mean_loop,
                                mixedlayerthickness=mixedlayerthickness_mean_loop,
                                time=time(GOEPR_loop["thetao_mean"])[1],
                                uuid=stock_GRSF_uuid_loop)
    
    #combine data
    env_dat <- bind_rows(env_dat,env_dat_loop)
    
    print(j)
  }
  
  #combine data
  env_list[[i]] <- env_dat
  
}
