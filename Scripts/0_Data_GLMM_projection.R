library(tidyverse)

# 1 Productivity data compile ----------------------------------------------------------
# stock productivity
productivity <- read_rds("Outputs/Productivity/productivity.rds")

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, GRSF_uuid, primary_FAOarea)

# combine data
productivity <- left_join(productivity, stock_success)

# 2 CIMP6 stock-specific NorESM2-LM data compile --------------------------------------------
# data file
biophysics_file_name <- list.files("Data/CMIP6_stock/NorESM2-LM/")
file_name_split <- matrix(unlist(str_split(biophysics_file_name, "_")), nrow = 2) # use uuid
biophysics_file_name <- unique(file_name_split[1, ])

# loop
biophysics <- NULL
for (i in biophysics_file_name) {
  # i <- biophysics_file_name[1]

  # read data
  chl_loop <- read_rds(paste0("Data/CMIP6_stock/NorESM2-LM/", i, "_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6_stock/NorESM2-LM/", i, "_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6_stock/NorESM2-LM/", i, "_mlotst.rds"))

  # GRSF_uuid
  uuid <- i

  # combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop, thetao_loop) %>%
    left_join(mlotst_loop) %>%
    mutate(
      year = year(time),
      month = month(time)
    )

  # calculate annual averages
  biophysics_loop <- biophysics_loop %>%
    group_by(year, ssp) %>%
    summarise(
      chl = mean(chl),
      thetao = mean(thetao),
      mlotst = mean(mlotst)
    ) %>%
    ungroup()

  # mutate uuid
  biophysics_loop <- biophysics_loop %>%
    mutate(GRSF_uuid = uuid)

  # combine data
  biophysics <- bind_rows(biophysics, biophysics_loop)

  print(i)
}

biophysics <- biophysics %>%
  mutate(ESM = "NorESM2-LM")

write_rds(biophysics, file = "Data/CMIP6_NorESM2-LM_for_glmm_projection.rds")


# 3 CIMP6 stock-specific MPI-ESM1-2-LR data compile --------------------------------------------
# data file
biophysics_file_name <- list.files("Data/CMIP6_stock/MPI-ESM1-2-LR/")
file_name_split <- matrix(unlist(str_split(biophysics_file_name, "_")), nrow = 2) # use uuid
biophysics_file_name <- unique(file_name_split[1, ])

# loop
biophysics <- NULL
for (i in biophysics_file_name) {
  # i <- biophysics_file_name[1]

  # read data
  chl_loop <- read_rds(paste0("Data/CMIP6_stock/MPI-ESM1-2-LR/", i, "_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6_stock/MPI-ESM1-2-LR/", i, "_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6_stock/MPI-ESM1-2-LR/", i, "_mlotst.rds"))

  # GRSF_uuid
  uuid <- i

  # combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop, thetao_loop) %>%
    left_join(mlotst_loop) %>%
    mutate(
      year = year(time),
      month = month(time)
    )

  # calculate annual averages
  biophysics_loop <- biophysics_loop %>%
    group_by(year, ssp) %>%
    summarise(
      chl = mean(chl),
      thetao = mean(thetao),
      mlotst = mean(mlotst)
    ) %>%
    ungroup()

  # mutate uuid
  biophysics_loop <- biophysics_loop %>%
    mutate(GRSF_uuid = uuid)

  # combine data
  biophysics <- bind_rows(biophysics, biophysics_loop)

  print(i)
}

biophysics <- biophysics %>%
  mutate(ESM = "MPI-ESM1-2-LR")

write_rds(biophysics, file = "Data/CMIP6_MPI-ESM1-2-LR_for_glmm_projection.rds")

# 4 CIMP6 stock-specific IPSL-CM6A-LR data compile --------------------------------------------
# data file
biophysics_file_name <- list.files("Data/CMIP6_stock/IPSL-CM6A-LR/")
file_name_split <- matrix(unlist(str_split(biophysics_file_name, "_")), nrow = 2) # use uuid
biophysics_file_name <- unique(file_name_split[1, ])

# loop
biophysics <- NULL
for (i in biophysics_file_name) {
  # i <- biophysics_file_name[1]

  # read data
  chl_loop <- read_rds(paste0("Data/CMIP6_stock/IPSL-CM6A-LR/", i, "_chl.rds"))
  thetao_loop <- read_rds(paste0("Data/CMIP6_stock/IPSL-CM6A-LR/", i, "_thetao.rds"))
  mlotst_loop <- read_rds(paste0("Data/CMIP6_stock/IPSL-CM6A-LR/", i, "_mlotst.rds"))

  # GRSF_uuid
  uuid <- i

  # combine data and mutate year and month
  biophysics_loop <- left_join(chl_loop, thetao_loop) %>%
    left_join(mlotst_loop) %>%
    mutate(
      year = year(time),
      month = month(time)
    )

  # calculate annual averages
  biophysics_loop <- biophysics_loop %>%
    group_by(year, ssp) %>%
    summarise(
      chl = mean(chl),
      thetao = mean(thetao),
      mlotst = mean(mlotst)
    ) %>%
    ungroup()

  # mutate uuid
  biophysics_loop <- biophysics_loop %>%
    mutate(GRSF_uuid = uuid)

  # combine data
  biophysics <- bind_rows(biophysics, biophysics_loop)

  print(i)
}

biophysics <- biophysics %>%
  mutate(ESM = "IPSL-CM6A-LR")

write_rds(biophysics, file = "Data/CMIP6_IPSL-CM6A-LR_for_glmm_projection.rds")
