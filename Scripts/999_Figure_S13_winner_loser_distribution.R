library(tidyverse)
library(terra)
library(tidyterra)
library(ggsci)
library(rnaturalearth)
library(sf)
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
  plot.subtitle = element_text(
    family = "Arial", size = c(10, 8), face = c(2, 1),
    color = c(mypal[2], "black"),
    hjust = c(0, 1), vjust = 0, margin = margin(b = 1.25)
  ),

  # axis
  axis.text = element_text(family = "Arial"),
  axis.title = element_blank(),
  axis.ticks = element_blank(),

  # grid
  panel.grid.major = element_line(linetype = "dashed", linewidth = 0.25, color = "grey75"),

  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial", margin = margin(b = 0.25)),

  # legend
  legend.title = element_text(family = "Arial", angle = 90),
  legend.text = element_text(family = "Arial"),
  legend.position = c(0.15, 0.5)
)


# 1 Data compile ----------------------------------------------------------
# stock distribution centroid
stock_distribution <- read_rds("Data/stock_distribution_area_centroid.rds")
stock_information <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, GRSF_uuid)
stock_distribution <- left_join(stock_distribution, stock_information)

# 2050s - 2020s
productivity_change_50s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_50s_20s.rds")
productivity_change_50s_20s <- productivity_change_50s_20s %>%
  drop_na()
unique(productivity_change_50s_20s$stockid) # 657 stocks
productivity_change_50s_20s <- left_join(productivity_change_50s_20s, stock_distribution) %>%
  mutate(reference = "2050s - 2020s")

# 2090s - 2020s
productivity_change_90s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_90s_20s.rds")
productivity_change_90s_20s <- productivity_change_90s_20s %>%
  drop_na()
unique(productivity_change_90s_20s$stockid) # 657 stocks
productivity_change_90s_20s <- left_join(productivity_change_90s_20s, stock_distribution) %>%
  mutate(reference = "2090s - 2020s")

productivity_change <- bind_rows(productivity_change_50s_20s, productivity_change_90s_20s)

# change values over 0.15 and -0.15 to 0.15 and -0.15
# productivity_change <- productivity_change %>%
#   mutate(p_change_mean = case_when(p_change_mean > 0.10 ~ 0.10,
#                                    p_change_mean < -0.10 ~ -0.10,
#                                    TRUE ~ p_change_mean))


# 2 World map -------------------------------------------------------------
# world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")
# world <- map_data("world")

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

# # world map inside grid
# n <- 200
# lat <- seq(-90, 90, length.out = n)
# lon_left <- rep(-180, n)
# lon_right <- rep(180, n)
# # longitude
# lon_w120 <- rep(-120, n)
# w120_line <- st_sfc(st_linestring(cbind(lon_w120, lat)), crs = 4326)
#
# # latitude
# n60_line <- st_sfc(st_linestring(cbind(seq(-180, 180, length.out = n), rep(60, n))), crs = 4326)
#
# inside_sf <- c(w120_line, n60_line)



# 3 Figure a, 2090s -2020s, SSP1-2.6 ---------------------------------------
data_a <- productivity_change %>%
  filter(ssp == "ssp126" & reference == "2090s - 2020s")
hist(data_a$p_change_mean)
data_a <- data_a %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_a <- st_as_sf(drop_na(select(data_a, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_a <- ggplot(data_a) +
  geom_sf(data = border_sf, color = "black", size = 0.25) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_a, aes(color = p_change_mean), size = 0.5) +
  facet_wrap(~ESM) + 
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme


# 4 Figure b, 2090s - 2020s, SSP2-4.5 -------------------------------------
data_b <- productivity_change %>%
  filter(ssp == "ssp245" & reference == "2090s - 2020s")
hist(data_b$p_change_mean)
data_b <- data_b %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_b <- st_as_sf(drop_na(select(data_b, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_b <- ggplot(data_b) +
  geom_sf(data = border_sf, color = "black", size = 0.25) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_b, aes(color = p_change_mean), size = 0.5) +
  facet_wrap(~ESM) + 
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme

# 5 Figure c, 2090s - 2020s, SSP5-8.5 -------------------------------------
data_c <- productivity_change %>%
  filter(ssp == "ssp585" & reference == "2090s - 2020s")
hist(data_f$p_change_mean)
data_c <- data_c %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_c <- st_as_sf(drop_na(select(data_c, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_c <- ggplot(data_c) +
  geom_sf(data = border_sf, color = "black", size = 0.25) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_c, aes(color = p_change_mean), size = 0.5) +
  facet_wrap(~ESM) + 
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme

# 6 Combine figures -------------------------------------------------------
ggarrange(
  figure_a, 
  figure_b, 
  figure_c,
  ncol = 1, nrow = 3, labels = c("(a)", "(b)", "(c)"),
  font.label = "Arial"
)

ggsave("Figures/winner_loser_distribution.PDF", device = cairo_pdf, width = 10, height = 6)


# XXX Figure, 2050s - 2020s, SSP1-2.6----------------------------------------------------------------
data_a <- productivity_change %>%
  filter(ssp == "ssp126" & reference == "2050s - 2020s")
hist(data_a$p_change_mean)
data_a <- data_a %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_a <- st_as_sf(drop_na(select(data_a, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_a <- ggplot(data_a) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_a, aes(color = p_change_mean), size = 0.5) +
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme

# XXX Figure, 2050s - 2020s, SSP2-4.5 -------------------------------------
data_c <- productivity_change %>%
  filter(ssp == "ssp245" & reference == "2050s - 2020s")
hist(data_c$p_change_mean)
data_c <- data_c %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_c <- st_as_sf(drop_na(select(data_c, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_c <- ggplot(data_c) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_c, aes(color = p_change_mean), size = 0.5) +
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme

# XXX Figure, 2050s - 2020s, SSP5-8.5 -------------------------------------
data_e <- productivity_change %>%
  filter(ssp == "ssp585" & reference == "2050s - 2020s")
hist(data_e$p_change_mean)
data_e <- data_e %>%
  mutate(p_change_mean = case_when(
    p_change_mean > 0.06 ~ 0.06,
    p_change_mean < -0.06 ~ -0.06,
    TRUE ~ p_change_mean
  ))

# change to sf
data_e <- st_as_sf(drop_na(select(data_e, centroid_lon, centroid_lat, p_change_mean)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

# figure
figure_e <- ggplot(data_e) +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  # geom_sf(data = inside_sf, color = "black", linewidth = 0.25, color = "grey", linetype = "dashed") +
  geom_sf(data = data_e, aes(color = p_change_mean), size = 0.5) +
  scale_color_gradient2(
    name = "Productivity change",
    low = scales::brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = scales::brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-0.06, 0.06),
    guide = guide_colorbar(title.position = "left")
  ) +
  scale_x_continuous("Longtitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030")) +
  mytheme
