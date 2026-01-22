library(tidyverse)
library(tidyterra)
library(terra)
library(lubridate)
library(tidync)
library(ncdf4)
library(geojsonR)

# nc <- nc_open("thetao_Omon_NorESM2-LM_ssp126_r1i1p1f1_gr_209101-210012.nc")
# # nc <- nc_open("thetao_Omon_NorESM2-LM_ssp126_r1i1p1f1_gr_208101-209012.nc")
# lon <- ncvar_get(nc, "longitude")
# lat <- ncvar_get(nc, "latitude")
# time <- ncvar_get(nc, "time")
# as.Date(time, origin = as.Date('1800-03-10 00:00'))


#1 IPSL-CM6A-LR chl ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/IPSL-CM6A-LR/")
file_name <- file_name[startsWith(file_name,"chl")]

for(i in 1:length(file_name)) {
  
  # i=1
  # nc <- nc_open(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D1,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D5") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "2015-01-01 00:00")) #make a little change
  
  time_label <- time
  
  for (k in 1:6) {
    
    # k=2
    #variable five time period
    var <- data %>% 
      activate("D1,D2,D4,D5") %>% 
      hyper_filter(olevel = olevel <= 50,
                   time = as_date(time, origin = "2015-01-01 00:00") <= time_label[k*172,1]&
                     as_date(time, origin = "2015-01-01 00:00") >= time_label[k*172-171,1]) %>% 
      hyper_tibble(force = T)
    
    #0-50m var average
    var <- var %>% 
      group_by(time, x, y) %>% 
      summarise(var = mean(chl, na.rm = T)) %>% 
      ungroup()
    
    #combine data
    var <- left_join(var, lon_lat) %>% 
      mutate(time = as_date(time, origin = "2015-01-01 00:00"))
    
    var <- var %>% 
      # mutate(longitude = longitude-180) %>% #change longitude
      select(nav_lon, nav_lat, time, var)
    
    #make an even grid
    even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
    
    #loop
    var_even_grid <- rast()
    for (j in 1:172) { #120 months
      
      # j=1
      time_loop <- time[j+(k-1)*172,1]
      var_loop <- var %>% 
        filter(time == time_loop$time)
      
      #original coordinates
      uneven_grid <- as.matrix(var_loop[,1:2]) 
      
      #make it an even grid
      var_even_grid_loop <- rasterize(uneven_grid,even_grid,value=var_loop$var,fun=mean) #get even grid
      # plot(var_even_grid_loop)
      names(var_even_grid_loop) <- time_loop$time #change the name of layer as time
      var_even_grid <- c(var_even_grid,var_even_grid_loop) #combine the grids
      print(paste(i,k,j))
    }
    
    #now we get a raster file with even grid, it has many layers, standing for months.
    # plot(var_even_grid,10)
    # names(var_even_grid)
    saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_period",k,"_compiled.rds"))
    
  }
  
  
}



#2 IPSL-CM6A-LR thetao ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/IPSL-CM6A-LR/")
file_name <- file_name[startsWith(file_name,"thetao")]

for(i in 1:length(file_name)) {
  
  # i=1
  # nc <- nc_open(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D1,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D5") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "2015-01-01 00:00")) #make a little change
  
  time_label <- time
  
  for (k in 1:6) {
    
    # k=2
    #variable five time period
    var <- data %>% 
      activate("D1,D2,D4,D5") %>% 
      hyper_filter(olevel = olevel <= 50,
                   time = as_date(time, origin = "2015-01-01 00:00") <= time_label[k*172,1]&
                     as_date(time, origin = "2015-01-01 00:00") >= time_label[k*172-171,1]) %>% 
      hyper_tibble(force = T)
    
    #0-50m var average
    var <- var %>% 
      group_by(time, x, y) %>% 
      summarise(var = mean(thetao, na.rm = T)) %>% 
      ungroup()
    
    #combine data
    var <- left_join(var, lon_lat) %>% 
      mutate(time = as_date(time, origin = "2015-01-01 00:00"))
    
    var <- var %>% 
      # mutate(longitude = longitude-180) %>% #change longitude
      select(nav_lon, nav_lat, time, var)
    
    #make an even grid
    even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
    
    #loop
    var_even_grid <- rast()
    for (j in 1:172) { #120 months
      
      # j=1
      time_loop <- time[j+(k-1)*172,1]
      var_loop <- var %>% 
        filter(time == time_loop$time)
      
      #original coordinates
      uneven_grid <- as.matrix(var_loop[,1:2]) 
      
      #make it an even grid
      var_even_grid_loop <- rasterize(uneven_grid,even_grid,value=var_loop$var,fun=mean) #get even grid
      # plot(var_even_grid_loop)
      names(var_even_grid_loop) <- time_loop$time #change the name of layer as time
      var_even_grid <- c(var_even_grid,var_even_grid_loop) #combine the grids
      print(paste(i,j))
    }
    
    #now we get a raster file with even grid, it has many layers, standing for months.
    # plot(var_even_grid,10)
    # names(var_even_grid)
    saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_period",k,"_compiled.rds"))
    
  }
  
  
}



#3 IPSL-CM6A-LR mlotst ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/IPSL-CM6A-LR/")
file_name <- file_name[startsWith(file_name,"mlotst")]

for(i in 1:length(file_name)) {
  
  # i=1
  # nc <- nc_open(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/IPSL-CM6A-LR/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D1,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D4") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "2015-01-01 00:00")) #make a little change
  
  time_label <- time
  
  for (k in 1:6) {
    
    # k=1
    #variable five time period
    var <- data %>% 
      activate("D1,D2,D4") %>% 
      hyper_filter(time = as_date(time, origin = "2015-01-01 00:00") <= time_label[k*172,1]&
                     as_date(time, origin = "2015-01-01 00:00") >= time_label[k*172-171,1]) %>% 
      hyper_tibble(force = T)
    
    #0-50m var average
    var <- var %>% 
      group_by(time, x, y) %>% 
      summarise(var = mean(mlotst, na.rm = T)) %>% 
      ungroup()
    
    #combine data
    var <- left_join(var, lon_lat) %>% 
      mutate(time = as_date(time, origin = "2015-01-01 00:00"))
    
    var <- var %>% 
      # mutate(longitude = longitude-180) %>% #change longitude
      select(nav_lon, nav_lat, time, var)
    
    #make an even grid
    even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
    
    #loop
    var_even_grid <- rast()
    for (j in 1:172) { #120 months
      
      # j=1
      time_loop <- time[j+(k-1)*172,1]
      var_loop <- var %>% 
        filter(time == time_loop$time)
      
      #original coordinates
      uneven_grid <- as.matrix(var_loop[,1:2]) 
      
      #make it an even grid
      var_even_grid_loop <- rasterize(uneven_grid,even_grid,value=var_loop$var,fun=mean) #get even grid
      # plot(var_even_grid_loop)
      names(var_even_grid_loop) <- time_loop$time #change the name of layer as time
      var_even_grid <- c(var_even_grid,var_even_grid_loop) #combine the grids
      print(paste(i,j))
    }
    
    #now we get a raster file with even grid, it has many layers, standing for months.
    # plot(var_even_grid,10)
    # names(var_even_grid)
    saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_period",k,"_compiled.rds"))
    
  }
  
  
}



# terra -------------------------------------------------------------------
data <- read_rds("ssp126.rds")
data <- read_rds("ssp126_MPI.rds")
data <- rast(data)
plot(data)




names(data)
data <- select(data,"thetao_lev=0_1")

data <- rotate(data)

plot(data,1)


data_1 <- select(data,"thetao_lev=0_8")


plot(data_1)
time(data_1)
stock_distribution <- read_rds("stock_distribution.rds")

#stock distribution loop
#geojson data
char_js <- FROM_GeoJson(url_file_string = stock_distribution[200,2])

crdref <- "+proj=longlat +datum=WGS84"

#change it to shapefile
stock_distribution_loop <- vect(char_js$coordinates,type="polygons", crs=crdref)
plot(stock_distribution_loop)

loop_temperature <- crop(data,stock_distribution_loop,mask=T)

plot(loop_temperature)
global(loop_temperature,"mean",na.rm=T)






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









