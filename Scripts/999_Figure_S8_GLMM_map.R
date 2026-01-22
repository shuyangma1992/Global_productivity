library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggsci)
library(ggpubr)
library(RColorBrewer)
library(scales)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Arial")) # Font
scales::show_col(pal_npg()(9))
mypal <- pal_npg()(9) # Color
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
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial", margin = margin(b = 0.25)),
  # legend
  legend.text = element_text(family = "Arial"),
  legend.position = c(0.1, 0.5)
)

# 1 Data compile ----------------------------------------------------------
# distribution data
stock_distribution <- read_rds("Data/stock_distribution_area_centroid.rds")

# stock information
stock_information <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(scientificname, GRSF_uuid)

# random effects
random_effects <- read_rds("Outputs/GLMM results/random_effects.rds") %>%
  select(1, 5, 6)

# calculate median
random_effects_summary <- random_effects %>%
  group_by(scientificname) %>%
  summarise(
    median_T = median(temperature_adjusted),
    low_T = quantile(temperature_adjusted, 0.025),
    high_T = quantile(temperature_adjusted, 0.975),
    median_CHL = median(chlorophyll_adjusted),
    low_CHL = quantile(chlorophyll_adjusted, 0.025),
    high_CHL = quantile(chlorophyll_adjusted, 0.975)
  ) %>%
  ungroup()

stock <- left_join(stock_distribution, stock_information) %>%
  left_join(random_effects_summary)

# T slope adjustment
hist(stock$median_T)
stock <- stock %>%
  mutate(median_T = case_when(
    median_T > 0.02 ~ 0.02,
    median_T < -0.02 ~ -0.02,
    TRUE ~ median_T
  ))

# CHL slope adjustment
hist(stock$median_CHL)
stock <- stock %>%
  mutate(median_CHL = case_when(
    median_CHL > 1 ~ 1,
    median_CHL < -1 ~ -1,
    TRUE ~ median_CHL
  ))

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


# 3 Figure T ----------------------------------------------------------------
# change to sf
slope_T <- st_as_sf(
  drop_na(select(stock, centroid_lon, centroid_lat, median_T)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

T_map <- ggplot() +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  geom_sf(data = slope_T, aes(color = median_T * 100)) +
  scale_color_gradient2(
    name = "Temperature effect",
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1]
  ) +
  scale_x_continuous("Longitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030"), expand = FALSE) +
  mytheme

T_x <- ggplot(stock) +
  geom_point(aes(x = centroid_lon, y = median_T * 100, color = median_T * 100), show.legend = F, size = 0.75) +
  geom_smooth(aes(x = centroid_lon, y = median_T * 100),
    method = "loess", color = "black", fill = "gray", linewidth = 0.5
  ) +
  scale_color_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-2, 2)
  ) +
  scale_x_continuous("Longitude", limits = c(-180, 180), breaks = seq(-180, 180, 60), expand = c(0, 0)) +
  scale_y_continuous("Temperature effect") +
  mytheme +
  theme(plot.margin = margin(l = 12, r = 12))

T_y <- ggplot(stock) +
  geom_point(aes(x = centroid_lat, y = median_T * 100, color = median_T * 100), show.legend = F, size = 0.75) +
  geom_smooth(aes(x = centroid_lat, y = median_T * 100),
    method = "loess", color = "black", fill = "gray", linewidth = 0.5
  ) +
  scale_color_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-2, 2)
  ) +
  scale_x_continuous("Latitude", limits = c(-90, 90), breaks = seq(-90, 90, 30), expand = c(0, 0)) +
  scale_y_continuous("Temperature effect") +
  mytheme +
  coord_flip()

# combine
layout_matrix <- rbind(
  c(1, 2),
  c(3, 0)
)

f_T <- ggarrange(T_map, T_y, T_x,
  layout = layout_matrix,
  widths = c(1.9717, 0.5), heights = c(1, 0.5), align = "hv"
)

# ggsave("Figures/GLMM_map_T.PDF", device = cairo_pdf, width = (1.9717+0.5)*3, height = 1.5*3)

summary(lm(median_T ~ centroid_lon, data = stock))
summary(lm(median_T ~ centroid_lat, data = stock))


# 4 Figure CHL ------------------------------------------------------------
# change to sf
slope_CHL <- st_as_sf(
  drop_na(select(stock, centroid_lon, centroid_lat, median_CHL)),
  coords = c("centroid_lon", "centroid_lat"),
  crs = st_crs(4326)
)

CHL_map <- ggplot() +
  geom_sf(data = border_sf, color = "black", size = 0.8) +
  geom_sf(data = world, fill = "black", color = "black") +
  geom_sf(data = slope_CHL, aes(color = median_CHL)) +
  scale_color_gradient2(
    name = "Chlorophyll effect",
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9],
    high = brewer_pal(type = "div", palette = "PuOr")(9)[1]
  ) +
  scale_x_continuous("Longitude", breaks = seq(-180, 180, 60)) +
  scale_y_continuous("Latitude", breaks = seq(-90, 90, 30)) +
  coord_sf(crs = st_crs("ESRI:54030"), expand = FALSE) +
  mytheme

CHL_x <- ggplot(stock) +
  geom_point(aes(x = centroid_lon, y = median_CHL, color = median_CHL), show.legend = F, size = 0.75) +
  geom_smooth(aes(x = centroid_lon, y = median_CHL),
    method = "loess", color = "black", fill = "gray", linewidth = 0.5
  ) +
  scale_color_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9], high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-1, 1)
  ) +
  scale_x_continuous("Longitude", limits = c(-180, 180), breaks = seq(-180, 180, 60), expand = c(0, 0)) +
  scale_y_continuous("Chlorophyll effect") +
  mytheme +
  theme(plot.margin = margin(l = 12, r = 12))

CHL_y <- ggplot(stock) +
  geom_point(aes(x = centroid_lat, y = median_CHL, color = median_CHL), show.legend = F, size = 0.75) +
  geom_smooth(aes(x = centroid_lat, y = median_CHL),
    method = "loess", color = "black", fill = "gray", linewidth = 0.5
  ) +
  scale_color_gradient2(
    low = brewer_pal(type = "div", palette = "PuOr")(9)[9], high = brewer_pal(type = "div", palette = "PuOr")(9)[1],
    limits = c(-1, 1)
  ) +
  scale_x_continuous("Latitude", limits = c(-90, 90), breaks = seq(-90, 90, 30), expand = c(0, 0)) +
  scale_y_continuous("Chlorophyll effect") +
  mytheme +
  coord_flip()

# combine
layout_matrix <- rbind(
  c(1, 2),
  c(3, 0)
)

f_CHL <- ggarrange(CHL_map, CHL_y, CHL_x,
  layout = layout_matrix,
  widths = c(1.9717, 0.5), heights = c(1, 0.5), align = "hv"
)

# ggsave("Figures/GLMM results CHL map.PDF",device = cairo_pdf, width = 8, height = 4)

summary(lm(median_CHL ~ centroid_lon, data = stock))
summary(lm(median_CHL ~ centroid_lat, data = stock))


# 5 Figure combine --------------------------------------------------------
ggarrange(f_T, f_CHL,
  nrow = 2, align = "hv",
  labels = c("(a)", "(b)"), font.label = "Airl"
)

ggsave("Figures/GLMM_map.PDF",device = cairo_pdf, width = (1.9717+0.5)*3, height = 1.5*3*2)








