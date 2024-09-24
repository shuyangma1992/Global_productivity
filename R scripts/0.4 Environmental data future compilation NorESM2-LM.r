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


#1 NorESM2-LM SSP1-2.6 chl ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 chl/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 chl/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 chl/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(chl, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}

#2 NorESM2-LM SSP2-4.5 chl ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 chl/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 chl/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 chl/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(chl, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}









#3 NorESM2-LM SSP5-8.5 chl ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 chl/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 chl/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 chl/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(chl, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}


#4 NorESM2-LM SSP1-2.6 thetao ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 thetao/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 thetao/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 thetao/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(thetao, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}

#5 NorESM2-LM SSP2-4.5 thetao ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 thetao/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 thetao/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 thetao/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(thetao, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}


#6 NorESM2-LM SSP5-8.5 thetao ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 thetao/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 thetao/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 thetao/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D3,D2") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D3,D2,D1,D0") %>% 
    hyper_filter(lev = lev <= 50) %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(thetao, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}










#7 NorESM2-LM SSP1-2.6 mlotst ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 mlotst/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 mlotst/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 mlotst/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D2,D1") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D2,D1,D0") %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(mlotst, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}

#8 NorESM2-LM SSP2-4.5 mlotst ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 mlotst/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 mlotst/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP2-4.5 mlotst/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D2,D1") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D2,D1,D0") %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(mlotst, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}


#9 NorESM2-LM SSP5-8.5 mlotst ------------------------------------------------------------------
file_name <- list.files("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 mlotst/")

for(i in 1:length(file_name)) {
  
  # i=9
  # nc <- nc_open(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP1-2.6 mlotst/", file_name[1]))
  data <- tidync(paste0("D:/CMIP6/NorESM2-LM/NorESM2-LM SSP5-8.5 mlotst/", file_name[i]))
  
  #coordinate
  lon_lat <- data %>% 
    activate("D2,D1") %>% 
    hyper_tibble()
  
  #time
  time <- data %>% 
    activate("D0") %>% 
    hyper_tibble() %>% 
    mutate(time = as_date(time, origin = "1800-01-01 00:00")) #make a little change
  
  #variable
  var <- data %>% 
    activate("D2,D1,D0") %>% 
    hyper_tibble()
  
  #0-50m var average
  var <- var %>% 
    group_by(time, i, j) %>% 
    summarise(var = mean(mlotst, na.rm = T)) %>% 
    ungroup()
  
  #combine data
  var <- left_join(var, lon_lat) %>% 
    mutate(time = as_date(time, origin = "1800-01-01"))
  
  var <- var %>% 
    mutate(longitude = longitude-180) %>% #change longitude
    select(longitude, latitude, time, var)
  
  #make an even grid
  even_grid <- rast(xmin=-180,xmax=180,ymin=-90,ymax=90,ncols=360,nrows=180) #1*1 grid
  
  #loop
  var_even_grid <- rast()
  for (j in 1:nrow(time)) { #120 months
    
    # j=1
    time_loop <- time[j,1]
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
  # plot(var_even_grid,1) 
  # names(var_even_grid)
  saveRDS(var_even_grid,paste0("CMIP6/",file_name[i],"_compiled.rds"))
  
}

















# 10 Time adjustment ------------------------------------------------------
file_name <- list.files("D:/CMIP6/Compiled/NorESM2-LM compiled")

for (i in 1:length(file_name)) {
  
  # i=1
  #read data
  data_loop <- rast(readRDS(paste0("D:/CMIP6/Compiled/NorESM2-LM compiled/",file_name[i])))
 
  #find time label
  time_label <- str_sub(file_name[i],-29,-17)
  time_start <- str_sub(time_label,1,4)
  time_end <- str_sub(time_label,-6,-3)
  
  #rename time
  names(data_loop) <- seq(as_date(paste0(time_start,"-01-15")),
                          as_date(paste0(time_end,"-12-15")),
                          "mon")
  
  saveRDS(data_loop,paste0("CMIP6/",file_name[i]))
  print(i)
  
}






