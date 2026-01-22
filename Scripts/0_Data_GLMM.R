library(tidyverse)

# 1 Productivity data compile ----------------------------------------------------------
# stock productivity
productivity <- read_rds("Outputs/Productivity/productivity.rds")

# set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

# select 1000 productivity time series
productivity <- productivity %>%
  select(year, stockid, as.character(resample_number))

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, GRSF_uuid, primary_FAOarea)

# combine data
productivity <- left_join(productivity, stock_success)

# 2 Physical data compile --------------------------------------------
# physical driver
physics_file_name <- list.files("Data/Copernicus_stock/Global Ocean Ensemble Physics Reanalysis/")

physics <- NULL
for (i in physics_file_name) {
  # i <- physics_file_name[1]
  
  # read data
  physics_loop <- read_rds(paste0("Data/Copernicus_stock/Global Ocean Ensemble Physics Reanalysis/", i))
  
  # GRSF_uuid
  uuid <- unique(physics_loop$uuid)
  
  # mutate year and month (problems with the last 18 data points)
  physics_loop <- physics_loop %>%
    mutate(
      year = rep(c(1993:2020), each = 12),
      month = rep(c(1:12), times = 28)
    )
  
  # calculate annual averages
  physics_loop <- physics_loop %>%
    group_by(year) %>%
    summarise(
      temperature = mean(temperature),
      salinity = mean(salinity),
      seasurfaceheight = mean(seasurfaceheight),
      mixedlayerthickness = mean(mixedlayerthickness)
    ) %>%
    ungroup()
  
  # mutate uuid
  physics_loop <- physics_loop %>%
    mutate(GRSF_uuid = uuid)
  
  # combine data
  physics <- bind_rows(physics, physics_loop)
}


# 3 Biogeochemical data compile --------------------------------------------
# biogeochemical driver
biogeochemistry_file_name <- list.files("Data/Copernicus_stock/Global Ocean Biogeochemistry Hindcast/")

biogeochemistry <- NULL
for (i in biogeochemistry_file_name) {
  # i <- biogeochemistry_file_name[1]
  
  # read data
  biogeochemistry_loop <- read_rds(paste0("Data/Copernicus_stock/Global Ocean Biogeochemistry Hindcast/", i))
  
  # GRSF_uuid
  uuid <- unique(biogeochemistry_loop$uuid)
  
  # mutate year and month (problems with the last 18 data points)
  biogeochemistry_loop <- biogeochemistry_loop %>%
    mutate(
      year = rep(c(1993:2020), each = 12),
      month = rep(c(1:12), times = 28)
    )
  
  # calculate annual averages
  biogeochemistry_loop <- biogeochemistry_loop %>%
    group_by(year) %>%
    summarise(
      chlorophyll = mean(chlorophyll),
      dissolvedoxygen = mean(dissolvedoxygen),
      primaryproduction = mean(primaryproduction),
      ph = mean(ph_mean_loop)
    ) %>%
    ungroup()
  
  # mutate uuid
  biogeochemistry_loop <- biogeochemistry_loop %>%
    mutate(GRSF_uuid = uuid)
  
  # combine data
  biogeochemistry <- bind_rows(biogeochemistry, biogeochemistry_loop)
}

# 4 Data combination and clean --------------------------------------------
# combine data, delete NAs
productivity_for_glmm <- left_join(productivity, physics) %>%
  left_join(biogeochemistry) %>%
  drop_na()

unique(productivity_for_glmm$stockid) # 652 stocks

# save data
write_rds(productivity_for_glmm, file = "Outputs/Productivity/productivity_for_glmm.rds")
