library(tidyverse)
library(lubridate)
library(broom)

# 1 Annual mean ------------------------------------------------------
#temperature
temp <- read_rds(file = "Data/Copernicus global/global temperature.rds")
temp <- temp %>% 
  mutate(year=str_sub(time,1,4)) #add year
temp_annual <- temp %>% 
  group_by(x,y,year) %>% 
  summarise(t=mean(t)) %>% #annual temperature
  ungroup()
write_rds(temp_annual,"Data/Copernicus global/global temperature annual.rds")

#salinity
sali <- read_rds(file = "Data/Copernicus global/global salinity.rds")
sali <- sali %>% 
  mutate(year=str_sub(time,1,4)) #add year
sali_annual <- sali %>% 
  group_by(x,y,year) %>% 
  summarise(s=mean(s)) %>% #annual temperature
  ungroup()
write_rds(sali_annual,"Data/Copernicus global/global salinity annual.rds")

#sea surface height
ssh <- read_rds(file = "Data/Copernicus global/global sea surface height.rds")
ssh <- ssh %>% 
  mutate(year=str_sub(time,1,4)) #add year
ssh_annual <- ssh %>% 
  group_by(x,y,year) %>% 
  summarise(ssh=mean(ssh)) %>% 
  ungroup()
write_rds(ssh_annual,"Data/Copernicus global/global sea surface height annual.rds")

#mixed layer thickness
mld <- read_rds(file = "Data/Copernicus global/global mixed layer thickness.rds")
mld <- mld %>% 
  mutate(year=str_sub(time,1,4)) #add year
mld_annual <- mld %>% 
  group_by(x,y,year) %>% 
  summarise(mld=mean(mld)) %>% 
  ungroup()
write_rds(mld_annual,"Data/Copernicus global/global mixed layer thickness annual.rds")

#chlorophyll
chl <- read_rds(file = "Data/Copernicus global/global chlorophyll.rds")
chl <- chl %>% 
  mutate(year=year(time)) #add year
chl_annual <- chl %>% 
  group_by(x,y,year) %>% 
  summarise(chl=mean(chl)) %>% 
  ungroup()
write_rds(chl_annual,"Data/Copernicus global/global chlorophyll annual.rds")

#dissolved oxygen
do <- read_rds(file = "Data/Copernicus global/global dissolved oxygen.rds")
do <- do %>% 
  mutate(year=year(time)) #add year
do_annual <- do %>% 
  group_by(x,y,year) %>% 
  summarise(do=mean(do)) %>% 
  ungroup()
write_rds(do_annual,"Data/Copernicus global/global dissolved oxygen annual.rds")

#net primary production
npp <- read_rds(file = "Data/Copernicus global/global primary production.rds")
npp <- npp %>% 
  mutate(year=year(time)) #add year
npp_annual <- npp %>% 
  group_by(x,y,year) %>% 
  summarise(npp=mean(npp)) %>% 
  ungroup()
write_rds(npp_annual,"Data/Copernicus global/global primary production annual.rds")

#ph
ph <- read_rds(file = "Data/Copernicus global/global ph.rds")
ph <- ph %>% 
  mutate(year=year(time)) #add year
ph_annual <- ph %>% 
  group_by(x,y,year) %>% 
  summarise(ph=mean(ph)) %>% 
  ungroup()
write_rds(ph_annual,"Data/Copernicus global/global ph annual.rds")

















# 2 Trend detection ------------------------------------------------------
#temperature
temp <- read_rds("Data/Copernicus global/global temperature annual.rds")
temp_trend <- temp %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(t ~ as.numeric(year), data = .x)))
write_rds(temp_trend,file = "Environment results/temperature trend.rds")

#salinity
sali <- read_rds("Data/Copernicus global/global salinity annual.rds")
sali_trend <- sali %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(s ~ as.numeric(year), data = .x)))
write_rds(sali_trend,file = "Environment results/salinity trend.rds")

#sea surface height
ssh <- read_rds("Data/Copernicus global/global sea surface height annual.rds")
ssh_trend <- ssh %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(ssh ~ as.numeric(year), data = .x)))
write_rds(ssh_trend,file = "Environment results/sea surface height trend.rds")

#mixed layer thickness
mld <- read_rds("Data/Copernicus global/global mixed layer thickness annual.rds")
mld_trend <- mld %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(mld ~ as.numeric(year), data = .x)))
write_rds(mld_trend,file = "Environment results/mixed layer thickness trend.rds")

#chlorophyll
chl <- read_rds("Data/Copernicus global/global chlorophyll annual.rds")
chl_trend <- chl %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(chl ~ as.numeric(year), data = .x)))
write_rds(chl_trend,file = "Environment results/chlorophyll trend.rds")

#dissolved oxygen
do <- read_rds("Data/Copernicus global/global dissolved oxygen annual.rds")
do_trend <- do %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(do ~ as.numeric(year), data = .x)))
write_rds(do_trend,file = "Environment results/dissolved oxygen trend.rds")

#primary production
npp <- read_rds("Data/Copernicus global/global primary production annual.rds")
npp_trend <- npp %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(npp ~ as.numeric(year), data = .x)))
write_rds(npp_trend,file = "Environment results/primary production trend.rds")

#ph
ph <- read_rds("Data/Copernicus global/global ph annual.rds")
ph_trend <- ph %>% 
  group_by(x,y) %>%
  group_modify(~tidy(lm(ph ~ as.numeric(year), data = .x)))
write_rds(ph_trend,file = "Environment results/ph trend.rds")





