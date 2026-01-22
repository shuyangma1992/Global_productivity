library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(rnaturalearth)
library(RColorBrewer)
library(ggsci)
library(scales)
library(ggpubr)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Arial")) # Font
scales::show_col(pal_lancet()(9))
mypal <- pal_lancet()(9) # Color
mytheme <- theme(
  # title
  plot.title = element_text(
    family = "Arial", face = 2, size = 10,
    color = mypal[2], vjust = 0
  ),
  plot.subtitle = element_text(family = "Arial", size = 11, margin = margin(b = 0)),

  # axis
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),

  # grid
  panel.grid.major = element_line(linetype = "dashed", linewidth = 0.5),

  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial", margin = margin(b = 0.25)),

  # legend
  legend.title = element_blank(),
  legend.text = element_text(family = "Arial"),
  legend.position = "right",
  legend.direction = "vertical",
  legend.background = element_blank(),
  legend.key.width = unit(0.25, "cm")
)


# 0 World map data --------------------------------------------------------
# world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

# world map border
n <- 200
lat <- seq(-90, 90, length.out = n)
lon_left <- rep(-180, n)
lon_right <- rep(180, n)
# left
left_line <- st_sfc(st_linestring(cbind(lon_left, lat)), crs = 4326)
# right
right_line <- st_sfc(st_linestring(cbind(lon_right, lat)), crs = 4326)
# up
top_line <- st_sfc(st_linestring(cbind(seq(-180, 180, length.out = n), rep(90, n))), crs = 4326)
# down
bottom_line <- st_sfc(st_linestring(cbind(seq(-180, 180, length.out = n), rep(-90, n))), crs = 4326)
# combine lines
border_sf <- c(left_line, right_line, top_line, bottom_line)


# 1 temperature -----------------------------------------------------------
# read data
temp_trend <- read_rds("Outputs/Environment results/temperature trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
# temp_trend_not_significant <- temp_trend %>%
#   filter(p.value > 0.05)
hist(temp_trend$estimate)

# adjust for plotting
thre_95 <- quantile(temp_trend$estimate, 0.95)
thre_05 <- quantile(temp_trend$estimate, 0.001) # 0.05 is not enough
temp_trend <- temp_trend %>%
  mutate(estimate = case_when(
    estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
temp_trend_raster <- rast(select(temp_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# temp_trend_not_significant_sf <- st_as_sf(temp_trend_not_significant,
#   coords = c("x", "y"),
#   crs = 4326
# )
# plot
f_temp <- ggplot() +
  labs(subtitle = "Temperature (\u00B0C/year)") +
  geom_spatraster(data = temp_trend_raster, aes(fill = estimate), show.legend = T) +
  # geom_sf(data = temp_trend_not_significant_sf, shape = 0, size = 0.01, color = "grey75", alpha = 0.01) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Temperature (\u00B0C)"
  ) +
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))


# 2 salinity -----------------------------------------------------------
# read data
sali_trend <- read_rds("Outputs/Environment results/salinity trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(sali_trend$estimate)

# adjust for plotting
thre_95 <- quantile(sali_trend$estimate, 0.95)
thre_05 <- quantile(sali_trend$estimate, 0.05)
sali_trend <- sali_trend %>%
  mutate(estimate = case_when(
    estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
sali_trend_raster <- rast(select(sali_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_sali <- ggplot() +
  labs(subtitle = "Salinity (psu/year)") +
  geom_spatraster(data = sali_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Salinity (psu)"
  ) +
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))

# 3 sea surface height -----------------------------------------------------------
# read data
ssh_trend <- read_rds("Outputs/Environment results/sea surface height trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value <= 0.05) # only significant grids
hist(ssh_trend$estimate)

# adjust for plotting
thre_95 <- quantile(ssh_trend$estimate, 0.95)
thre_05 <- quantile(ssh_trend$estimate, 0.0001) # 0.05 is not enough
ssh_trend <- ssh_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
ssh_trend_raster <- rast(select(ssh_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_ssh <- ggplot() +
  labs(subtitle = "Sea surface height (m/year)") +
  geom_spatraster(data = ssh_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Sea surface height (m)"
  ) +
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))



# 4 mixed layer thickness -----------------------------------------------------------
# read data
mld_trend <- read_rds("Outputs/Environment results/mixed layer thickness trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(mld_trend$estimate)

# adjust for plotting
thre_95 <- quantile(mld_trend$estimate, 0.95)
thre_05 <- quantile(mld_trend$estimate, 0.05)
mld_trend <- mld_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))
# make it a raster
mld_trend_raster <- rast(select(mld_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_mld <- ggplot() +
  labs(subtitle = "Mixed layer depth (m/year)") +
  geom_spatraster(data = mld_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Mixed layer thickness (m)"
  ) +
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))

# 5 chlorophyll -----------------------------------------------------------
# read data
chl_trend <- read_rds("Outputs/Environment results/chlorophyll trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(chl_trend$estimate)

# adjust for plotting
thre_95 <- quantile(chl_trend$estimate, 0.95)
thre_05 <- quantile(chl_trend$estimate, 0.05)
chl_trend <- chl_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
chl_trend_raster <- rast(select(chl_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_chl <- ggplot() +
  labs(subtitle = "Chlorophyll (mg/m3/year)") +
  geom_spatraster(data = chl_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Chlorophyll (mg/m3)"
  ) +
  # ggtitle("chlerature (0-50 m)")+
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))



# 6 dissolved oxygen -----------------------------------------------------------
# read data
do_trend <- read_rds("Outputs/Environment results/dissolved oxygen trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(do_trend$estimate)

# adjust for plotting
thre_95 <- quantile(do_trend$estimate, 0.95)
thre_05 <- quantile(do_trend$estimate, 0.05)
do_trend <- do_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
do_trend_raster <- rast(select(do_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_do <- ggplot() +
  labs(subtitle = "Dissolved oxygen (mmol/m3/year)") +
  geom_spatraster(data = do_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Dissolved oxygen (mmol/m3)"
  ) +
  # ggtitle("doerature (0-50 m)")+
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))


# 7 primary production -----------------------------------------------------------
# read data
npp_trend <- read_rds("Outputs/Environment results/primary production trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(npp_trend$estimate)

# adjust for plotting
thre_95 <- quantile(npp_trend$estimate, 0.95)
thre_05 <- quantile(npp_trend$estimate, 0.05)
npp_trend <- npp_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
npp_trend_raster <- rast(select(npp_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_npp <- ggplot() +
  labs(subtitle = "Primary production (mg/m3/day/year)") +
  geom_spatraster(data = npp_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "Total primary production (mg/m3/day)"
  ) +
  # ggtitle("npperature (0-50 m)")+
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))

# 8 ph -----------------------------------------------------------
# read data
ph_trend <- read_rds("Outputs/Environment results/ph trend.rds") %>%
  filter(term == "as.numeric(year)")
# filter(p.value<=0.05) # only significant grids
hist(ph_trend$estimate)

# adjust for plotting
thre_95 <- quantile(ph_trend$estimate, 0.999)
thre_05 <- quantile(ph_trend$estimate, 0.05)
ph_trend <- ph_trend %>%
  mutate(estimate = case_when(estimate > thre_95 ~ thre_95,
    estimate < thre_05 ~ thre_05,
    .default = estimate
  ))

# make it a raster
ph_trend_raster <- rast(select(ph_trend, c(1, 2, 4)), crs = "+init=epsg:4326")

# plot
f_ph <- ggplot() +
  labs(subtitle = "pH (/year)") +
  geom_spatraster(data = ph_trend_raster, aes(fill = estimate), show.legend = T) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  scale_fill_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    mid = "white",
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    midpoint = 0,
    na.value = "transparent", name = "pH"
  ) +
  # ggtitle("pherature (0-50 m)")+
  mytheme +
  coord_sf(crs = st_crs("ESRI:54030"))

# 9 combination -----------------------------------------------------------
ggarrange(
  f_temp, f_chl,
  f_mld, f_sali,
  f_ssh, f_npp,
  f_do, f_ph,
  ncol = 2, nrow = 4
)

ggsave("Figures/global_environment_change.pdf", device = cairo_pdf, width = 10, height = 10)



