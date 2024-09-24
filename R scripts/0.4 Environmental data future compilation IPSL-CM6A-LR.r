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



