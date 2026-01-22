library(tidyverse)
library(readxl)
library(ggsci)
library(ggh4x) # different strip background colors

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
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_rect(linewidth = 0.25),
  strip.text = element_text(family = "Arial", color = "white", margin = margin(b = 0,t = 0)),
  # legend
  legend.text = element_text(family = "Arial")
)


# load RAM legacy database
load("Data/DBdata[asmt][v4.61].RData")

# stocks
stocks <- read_rds("Data/stock_success_full_information_final.rds")
stocks <- unique(stocks$stockid) # 710 stocks

# total catch data for these 710 stocks
catch <- tcbest.data %>%
  rownames_to_column(var = "Year")
catch <- catch %>%
  pivot_longer(-Year, names_to = "stockid", values_to = "catch")

# 2001 to 2020
catch <- catch %>%
  filter(Year %in% c(2001:2020))

# successful stocks
catch <- catch %>%
  filter(stockid %in% stocks)

# change 0 to NA
catch <- catch %>%
  mutate(catch = na_if(catch, 0))

# area information
stocks <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, primary_FAOarea)

catch <- left_join(catch, stocks) %>%
  drop_na()

# calculate number of stocks and total catch in each year and FAO area
catch <- catch %>%
  group_by(Year, primary_FAOarea) %>%
  summarise(
    stock_number = n(),
    total_catch = sum(catch)
  )

# FAO catch data
FAO_catch <- read_excel("Data/FAOarea catch.xlsx", sheet = "FAO area catch") %>%
  pivot_longer(-c(1, 2), names_to = "Year", values_to = "area_total_catch")
FAO_catch <- FAO_catch %>%
  mutate(primary_FAOarea = as.character(primary_FAOarea))

# catch proportion
catch <- left_join(catch, FAO_catch)
catch <- catch %>%
  mutate(proportion = total_catch / area_total_catch) %>%
  mutate(Year = as.numeric(Year))

######################################################################### figure
strip_color <- strip_themed(background_x = elem_list_rect(fill = c(
  alpha(mypal[3], 1),
  "darkgreen",
  alpha(mypal[3], 0.75),
  alpha(mypal[3], 1),
  alpha(mypal[3], 0.75),
  alpha(mypal[3], 0.75),
  alpha(mypal[3], 0.75),
  "black",
  alpha(mypal[3], 0.5),
  "black",  
  alpha(mypal[3], 0.5),
  "darkgreen",
  alpha(mypal[3], 0.5),
  alpha(mypal[3], 0.75),
  alpha(mypal[3], 1),
  alpha(mypal[3], 1)
)))

# facet_wrap2(.~stock, strip = strip_color, labeller = label_wrap_gen(width = 22)) +
f <- ggplot(catch) +
  geom_line(aes(x = Year, y = proportion), linewidth = 0.5) +
  facet_wrap2(~primary_FAOarea,
    labeller = labeller(primary_FAOarea = c(
      `21` = "21 Northwest Atlantic",
      `27` = "27 Northeast Atlantic",
      `31` = "31 Western Central\nAtlantic",
      `34` = "34 Eastern Central\nAtlantic",
      `37` = "37 Mediterranean and\nBlack Sea",
      `41` = "41 Southwest Atlantic",
      `47` = "47 Southeast Atlantic",
      `51` = "51 Western Indian\nOcean",
      `57` = "57 Eastern Indian\nOcean",
      `58` = "58 Antarctic, Southern\nIndian Ocean",
      `61` = "61 Northwest Pacific",
      `67` = "67 Northeast Pacific",
      `71` = "71 Western Central\nPacific",
      `77` = "77 Eastern Central\nPacific",
      `81` = "81 Southwest Pacific",
      `87` = "87 Southeast Pacific"
      )),
    strip = strip_color,
  ) +
  scale_x_continuous("Year") +
  scale_y_continuous("Catch proportion", limits = c(0, 1), labels = scales::percent) +
  mytheme

ggsave("Figures/catch_proportion.PDF", device = cairo_pdf, width = 6, height = 6)




