library(tidyverse)
library(ggsci)

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
  axis.title = element_text(family = "Arial"),
  axis.ticks = element_blank(),
  # grid
  panel.grid.major = element_line(linetype = "dashed", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# Productivity hindcast trend by FAO area 

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds")

# productivity_trend
productivity_trend <- read_rds("Outputs/Productivity/productivity_trend_1981_2022.rds")

# stock information
productivity_trend <- left_join(productivity_trend, stock_success)

# total number
productivity_total_number <- productivity_trend %>%
  group_by(primary_FAOarea) %>%
  summarize(total_number = n()) %>%
  ungroup()

# positive trend
productivity_positive_trend_number <- productivity_trend %>%
  filter(p_slope_lci > 0) %>%
  group_by(primary_FAOarea) %>%
  summarize(positive_trend_number = n()) %>%
  ungroup()

# negative trend
productivity_negative_trend_number <- productivity_trend %>%
  filter(p_slope_uci < 0) %>%
  group_by(primary_FAOarea) %>%
  summarize(negative_trend_number = n()) %>%
  ungroup()

# combine data
productivity_trend_number <- left_join(productivity_total_number, productivity_positive_trend_number) %>%
  left_join(productivity_negative_trend_number)

# statistics
sum(productivity_trend_number$total_number) # 710 stocks
sum(productivity_trend_number$positive_trend_number, na.rm = TRUE) # 199 stocks
sum(productivity_trend_number$negative_trend_number, na.rm = TRUE) # 182 stocks


# calculate proportion
productivity_trend_number <- productivity_trend_number %>%
  filter(!primary_FAOarea %in% c("51", "58")) %>%
  mutate(
    positive_trend_proportion = positive_trend_number / total_number,
    negative_trend_proportion = negative_trend_number / total_number,
    no_trend_proportion = (total_number - positive_trend_number - negative_trend_number) / total_number
  )

# select
productivity_trend_number <- productivity_trend_number %>%
  select(1, 2, 5, 6, 7)

productivity_trend_number <- productivity_trend_number %>%
  pivot_longer(-c(1, 2), names_to = "trend", values_to = "proportion")

# compute the cumulative percentages (top of each rectangle)
productivity_trend_number <- productivity_trend_number %>%
  group_by(primary_FAOarea) %>%
  mutate(ymax = cumsum(proportion)) %>%
  ungroup()

# compute the bottom of each rectangle
productivity_trend_number <- productivity_trend_number %>%
  group_by(primary_FAOarea) %>%
  mutate(ymin = c(0, head(ymax, n = -1)))

# make the plot
f <- ggplot(productivity_trend_number) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = trend), show.legend = F) +
  scale_fill_manual(values = c(
    "positive_trend_proportion" = mypal[1],
    "negative_trend_proportion" = mypal[2],
    "no_trend_proportion" = "grey"
  )) +
  facet_wrap(. ~ primary_FAOarea + total_number) +
  coord_polar(theta = "y") + # Try to remove that to understand how the chart is built initially
  xlim(c(1, 4)) + # Try to remove that to see how to make a pie chart
  theme(axis.text = element_blank())

ggsave("Figures/productivity_hindcast_trend.PDF", device = cairo_pdf, width = 6, height = 6)











#coordinates
productivity_trend_number$x <- c(-50,-30,-70,-20,10,-40,0,100,160,-150,160,-145,160,-100)
productivity_trend_number$y <- c(50,50,20,20,35,-40,-30,-35,40,50,5,10,-45,-40)

#world map
#data information
FAO_shp <- vect("Data/FAO_AREAS_CWP/FAO_AREAS_CWP.shp")

#attributes
FAO_shp_attribute <- as.data.frame(FAO_shp)

#geometry
FAO_shp_geometry <- geom(FAO_shp)

#major area
FAO_shp_major <- FAO_shp %>%
  filter(F_LEVEL=="MAJOR")

#world data (ggplot2 package)
# world <- map_data("world")

#world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

#plot world map
map <- ggplot(data = world)+
  # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(fill="black",color="black")+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),color=mypal[2],family="Calibri",fontface=2,size=4)+
  geom_scatterpie(data=productivity_trend_number,cols=c("positive_trend_proportion",
                                                        "negative_trend_proportion",
                                                        "no_trend_proportion"),
                  aes(x=x,y=y,group=primary_FAOarea,r=log(total_number)+8),show.legend=F)+
  scale_fill_manual(values = c("positive_trend_proportion"=mypal[3],
                                "negative_trend_proportion"=mypal[1],
                                "no_trend_proportion"="grey"))+
  geom_scatterpie_legend(log(productivity_trend_number$total_number)+8, x=-160, y=-50)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1))

# ggsave("Figures/map.png",width = 8,height = 6)
ggsave("Figures/trend map.PDF",device = cairo_pdf,width = 8,height = 3)