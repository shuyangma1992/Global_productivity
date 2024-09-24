library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(ggsci)
library(scales)
library(ggpubr)
library(viridis)

#world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")
#Font
windowsFonts(B=windowsFont("Calibri"),C=windowsFont("Arial"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_npg()(10))
# show_col(pal_aaas()(10))
show_col(viridis(10))
# display.brewer.all()
mypal <- pal_npg()(10)
mypal <- viridis(10)
#world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

# 1 temperature -----------------------------------------------------------
#read data
temp_trend <- read_rds("Environment results/temperature trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(temp_trend$estimate)

#adjust for plotting
thre_95 <- quantile(temp_trend$estimate,0.95)
thre_05 <- quantile(temp_trend$estimate,0.001) #0.05 is not enough
temp_trend <- temp_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))
#make it a raster
temp_trend_raster <- rast(select(temp_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_temp <- ggplot()+ 
  geom_spatraster(data=temp_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Temperature (\u00B0C)")+
  # ggtitle("Temperature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))


# 2 salinity -----------------------------------------------------------
#read data
sali_trend <- read_rds("Environment results/salinity trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(sali_trend$estimate)

#adjust for plotting
thre_95 <- quantile(sali_trend$estimate,0.95)
thre_05 <- quantile(sali_trend$estimate,0.05) 
sali_trend <- sali_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))
#make it a raster
sali_trend_raster <- rast(select(sali_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_sali <- ggplot()+ 
  geom_spatraster(data=sali_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Salinity (psu)")+
  # ggtitle("salierature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))

# 3 sea surface height -----------------------------------------------------------
#read data
ssh_trend <- read_rds("Environment results/sea surface height trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(ssh_trend$estimate)

#adjust for plotting
thre_95 <- quantile(ssh_trend$estimate,0.95)
thre_05 <- quantile(ssh_trend$estimate,0.0001) #0.05 is not enough
ssh_trend <- ssh_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))
#make it a raster
ssh_trend_raster <- rast(select(ssh_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_ssh <- ggplot()+ 
  geom_spatraster(data=ssh_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Sea surface height (m)")+
  # ggtitle("ssherature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))



# 4 mixed layer thickness -----------------------------------------------------------
#read data
mld_trend <- read_rds("Environment results/mixed layer thickness trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(mld_trend$estimate)

#adjust for plotting
thre_95 <- quantile(mld_trend$estimate,0.95)
thre_05 <- quantile(mld_trend$estimate,0.05) 
mld_trend <- mld_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))
#make it a raster
mld_trend_raster <- rast(select(mld_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_mld <- ggplot()+ 
  geom_spatraster(data=mld_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Mixed layer thickness (m)")+
  # ggtitle("mlderature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))

# 5 chlorophyll -----------------------------------------------------------
#read data
chl_trend <- read_rds("Environment results/chlorophyll trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(chl_trend$estimate)

#adjust for plotting
thre_95 <- quantile(chl_trend$estimate,0.95)
thre_05 <- quantile(chl_trend$estimate,0.05) 
chl_trend <- chl_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))

#make it a raster
chl_trend_raster <- rast(select(chl_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_chl <- ggplot()+ 
  geom_spatraster(data=chl_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Chlorophyll (mg/m3)")+
  # ggtitle("chlerature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))



# 6 dissolved oxygen -----------------------------------------------------------
#read data
do_trend <- read_rds("Environment results/dissolved oxygen trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(do_trend$estimate)

#adjust for plotting
thre_95 <- quantile(do_trend$estimate,0.95)
thre_05 <- quantile(do_trend$estimate,0.05) 
do_trend <- do_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))

#make it a raster
do_trend_raster <- rast(select(do_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_do <- ggplot()+ 
  geom_spatraster(data=do_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Dissolved oxygen (mmol/m3)")+
  # ggtitle("doerature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))


# 7 primary production -----------------------------------------------------------
#read data
npp_trend <- read_rds("Environment results/primary production trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(npp_trend$estimate)

#adjust for plotting
thre_95 <- quantile(npp_trend$estimate,0.95)
thre_05 <- quantile(npp_trend$estimate,0.05) 
npp_trend <- npp_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))

#make it a raster
npp_trend_raster <- rast(select(npp_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_npp <- ggplot()+ 
  geom_spatraster(data=npp_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="Total primary production (mg/m3/day)")+
  # ggtitle("npperature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))

# 8 ph -----------------------------------------------------------
#read data
ph_trend <- read_rds("Environment results/ph trend.rds") %>% 
  filter(term=="as.numeric(year)") %>% 
  filter(p.value<=0.05) # only significant grids
hist(ph_trend$estimate)

#adjust for plotting
thre_95 <- quantile(ph_trend$estimate,0.999)
thre_05 <- quantile(ph_trend$estimate,0.05) 
ph_trend <- ph_trend %>% 
  mutate(estimate = case_when(estimate>thre_95 ~ thre_95,
                              estimate<thre_05 ~ thre_05,
                              .default = estimate))

#make it a raster
ph_trend_raster <- rast(select(ph_trend,c(1,2,4)),crs="+init=epsg:4326")

#plot
f_ph <- ggplot()+ 
  geom_spatraster(data=ph_trend_raster,aes(fill=estimate),show.legend = T)+
  geom_sf(data=world,fill="black",color="black")+
  scale_fill_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                       mid="white",
                       high=brewer_pal(type="div",palette = "PuOr")(9)[1],
                       midpoint = 0,
                       na.value = "transparent",name="pH")+
  # ggtitle("pherature (0-50 m)")+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = c(0.25,0.5),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Arial"),
        legend.text = element_text(family="Arial"),
        legend.key.height= unit(0.25,'cm'))+
  coord_sf(crs = st_crs("ESRI:54030"))







# 9 combination -----------------------------------------------------------

ggarrange(f_temp,f_sali,
          f_ssh,f_chl,
          f_npp,f_do,
          f_ph,f_mld,
          ncol = 2,nrow = 4)

ggsave("Figures/global environment change.pdf",device = cairo_pdf,width = 200,height = 200,units = "mm")




