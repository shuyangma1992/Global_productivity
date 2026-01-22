library(terra)
library(tidyverse)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(tidyterra)
library(scales)
library(ggsci)
library(RColorBrewer)

display.brewer.all()
mypal <- c(
  brewer_pal(type = "div", palette = "PuOr")(9)[9],
  brewer_pal(type = "div", palette = "PuOr")(9)[1]
)
show_col(mypal)

# 1 LME map ---------------------------------------------------------------
# data information
lme_shp <- vect("Data/Map_LME/LME66/LMEs66.shp")
lme_dat <- as.data.frame(lme_shp)

world <- map_data("world")
shp <- readOGR("Data/Map_LME/LME66/LMEs66.shp", stringsAsFactors = F)

map <- ggplot() +
  geom_map(map = world, data = world, aes(x = long, y = lat, map_id = region), fill = "black", color = "black") +
  geom_polygon(data = shp, aes(x = long, y = lat, group = group), colour = "black", fill = "lightblue") +
  # coord_map("moll")+
  theme_map()

# 2 FAO map -------------------------------------------------------------------
# data information
FAO_shp <- vect("Data/FAO_AREAS_CWP/FAO_AREAS_CWP.shp")

# attributes
FAO_shp_attribute <- as.data.frame(FAO_shp)

# geometry
FAO_shp_geometry <- geom(FAO_shp)

# major area
FAO_shp_major <- FAO_shp %>%
  filter(F_LEVEL == "MAJOR")

# attributes
FAO_shp_major_attribute <- as.data.frame(FAO_shp_major)

# world data (ggplot2 package)
# world <- map_data("world")

# world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

# plot world map
map <- ggplot(data = world) +
  # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  geom_spatvector(data = FAO_shp_major, fill = "white", color = "black") +
  geom_sf(fill = "black", color = "black") +
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  # color = mypal[2], family = "Arial", fontface = 2, size = 5)+
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

ggsave("Figures/map.png", width = 8, height = 6)
# ggsave("Figures/map.PDF",width = 8,height = 6)


# 3 FAO map color ---------------------------------------------------------
# data information
FAO_shp <- vect("Data/FAO_AREAS_CWP/FAO_AREAS_CWP.shp")

# attributes
FAO_shp_attribute <- as.data.frame(FAO_shp)

# geometry
FAO_shp_geometry <- geom(FAO_shp)

# major area
FAO_shp_major <- FAO_shp %>%
  filter(F_LEVEL == "MAJOR")

# attributes
FAO_shp_major_attribute <- as.data.frame(FAO_shp_major)

# world data (ggplot2 package)
# world <- map_data("world")

# world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

# plot world map 37 61 34 red 71 67 77 blue
fill_pal <- c(
  "white", alpha(mypal[1], 0.5), alpha(mypal[2], 0.5), alpha(mypal[1], 0.5), "white",
  "white", alpha(mypal[1], 0.5), "white", "white", alpha(mypal[2], 0.5),
  "white", "white", "white", "white", "white",
  "white", alpha(mypal[2], 0.5), "white", "white"
)

map_highlight <- ggplot(data = world) +
  # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  geom_spatvector(data = FAO_shp_major, fill = fill_pal, color = "black") +
  geom_sf(fill = "black", color = "black") +
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 5)+
  coord_sf(crs = st_crs("ESRI:54030")) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(0.1, 0.1, 0.1, 0.1)
  )

ggsave("Figures/map_highlight.png", width = 4, height = 2)



# #plot world map yellow
# map <- ggplot(data = world)+
#   # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
#   geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
#   geom_sf(fill="black",color="black")+
#   geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
#                        color = mypal[5], family = "Calibri", fontface = 2, size = 10)+
#   coord_sf(crs = st_crs("ESRI:54030"))+
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         plot.margin = margin(0.1,0.1,0.1,0.1))
#
# ggsave("Figures/map.png",width = 8,height = 6)
# # ggsave("Figures/map.PDF",device = cairo_pdf,width = 8,height = 6)
