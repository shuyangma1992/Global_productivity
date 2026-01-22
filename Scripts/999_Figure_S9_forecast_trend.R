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
    color = c(mypal[4], "black"),
    hjust = c(0, 1), vjust = 0, margin = margin(b = 1.25)
  ),
  # axis
  axis.text = element_text(family = "Arial"),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dashed", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial", margin = margin(b = 0.25)),
  # legend
  legend.text = element_text(family = "Arial")
)

# 1 data compile -----------------------------------------------
############################################################## stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname, primary_FAOarea)
unique(stock_success$stockid) # 710 stocks
unique(stock_success$scientificname) # 285 species
unique(stock_success$primary_FAOarea) # 16 FAO areas

###################################################### Projection trend SSP1-2.6
# read data
projection_trend_ssp126 <- read_rds("Outputs/GLMM projection results/productivity_projection_trend_ssp126_2021_2100.rds")
projection_trend_ssp126 <- left_join(
  projection_trend_ssp126,
  stock_success
)

# total number
projection_total_number_ssp126 <- projection_trend_ssp126 %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(total_number = n()) %>%
  ungroup()

# positive trend
projection_positive_trend_number_ssp126 <- projection_trend_ssp126 %>%
  filter(p_slope_lci > 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(positive_trend_number = n()) %>%
  ungroup()

# negative trend
projection_negative_trend_number_ssp126 <- projection_trend_ssp126 %>%
  filter(p_slope_uci < 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(negative_trend_number = n()) %>%
  ungroup()

# combine data
projection_trend_number_ssp126 <- left_join(
  projection_total_number_ssp126,
  projection_positive_trend_number_ssp126
) %>%
  left_join(projection_negative_trend_number_ssp126)

# change NA to 0
projection_trend_number_ssp126 <- replace_na(
  projection_trend_number_ssp126,
  list(
    positive_trend_number = 0,
    negative_trend_number = 0
  )
)

# calculate proportion
projection_trend_number_ssp126 <- projection_trend_number_ssp126 %>%
  filter(!primary_FAOarea %in% c("51", "58")) %>%
  mutate(
    positive_trend_proportion = positive_trend_number / total_number,
    negative_trend_proportion = negative_trend_number / total_number,
    no_trend_proportion = (total_number - positive_trend_number - negative_trend_number) / total_number
  )

# select
projection_trend_number_ssp126 <- projection_trend_number_ssp126 %>%
  mutate(ssp = "SSP1-2.6")


###################################################### Projection trend SSP2-4.5
# read data
projection_trend_ssp245 <- read_rds("Outputs/GLMM projection results/productivity_projection_trend_ssp245_2021_2100.rds")
projection_trend_ssp245 <- left_join(
  projection_trend_ssp245,
  stock_success
)

# total number
projection_total_number_ssp245 <- projection_trend_ssp245 %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(total_number = n()) %>%
  ungroup()

# positive trend
projection_positive_trend_number_ssp245 <- projection_trend_ssp245 %>%
  filter(p_slope_lci > 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(positive_trend_number = n()) %>%
  ungroup()

# negative trend
projection_negative_trend_number_ssp245 <- projection_trend_ssp245 %>%
  filter(p_slope_uci < 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(negative_trend_number = n()) %>%
  ungroup()

# combine data
projection_trend_number_ssp245 <- left_join(
  projection_total_number_ssp245,
  projection_positive_trend_number_ssp245
) %>%
  left_join(projection_negative_trend_number_ssp245)

# change NA to 0
projection_trend_number_ssp245 <- replace_na(
  projection_trend_number_ssp245,
  list(
    positive_trend_number = 0,
    negative_trend_number = 0
  )
)

# calculate proportion
projection_trend_number_ssp245 <- projection_trend_number_ssp245 %>%
  filter(!primary_FAOarea %in% c("51", "58")) %>%
  mutate(
    positive_trend_proportion = positive_trend_number / total_number,
    negative_trend_proportion = negative_trend_number / total_number,
    no_trend_proportion = (total_number - positive_trend_number - negative_trend_number) / total_number
  )

# select
projection_trend_number_ssp245 <- projection_trend_number_ssp245 %>%
  mutate(ssp = "SSP2-4.5")

###################################################### Projection trend SSP5-8.5 
# SSP5-8.5
# read data
projection_trend_ssp585 <- read_rds("Outputs/GLMM projection results/productivity_projection_trend_ssp585_2021_2100.rds")
projection_trend_ssp585 <- left_join(
  projection_trend_ssp585,
  stock_success
)

# total number
projection_total_number_ssp585 <- projection_trend_ssp585 %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(total_number = n()) %>%
  ungroup()

# positive trend
projection_positive_trend_number_ssp585 <- projection_trend_ssp585 %>%
  filter(p_slope_lci > 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(positive_trend_number = n()) %>%
  ungroup()

# negative trend
projection_negative_trend_number_ssp585 <- projection_trend_ssp585 %>%
  filter(p_slope_uci < 0) %>%
  group_by(primary_FAOarea, ESM) %>%
  summarize(negative_trend_number = n()) %>%
  ungroup()

# combine data
projection_trend_number_ssp585 <- left_join(
  projection_total_number_ssp585,
  projection_positive_trend_number_ssp585
) %>%
  left_join(projection_negative_trend_number_ssp585)

# change NA to 0
projection_trend_number_ssp585 <- replace_na(
  projection_trend_number_ssp585,
  list(
    positive_trend_number = 0,
    negative_trend_number = 0
  )
)

# calculate proportion
projection_trend_number_ssp585 <- projection_trend_number_ssp585 %>%
  filter(!primary_FAOarea %in% c("51", "58")) %>%
  mutate(
    positive_trend_proportion = positive_trend_number / total_number,
    negative_trend_proportion = negative_trend_number / total_number,
    no_trend_proportion = (total_number - positive_trend_number - negative_trend_number) / total_number
  )

# select
projection_trend_number_ssp585 <- projection_trend_number_ssp585 %>%
  mutate(ssp = "SSP5-8.5")

################################################################### combine data
projection_trend_number <- bind_rows(
  projection_trend_number_ssp126,
  projection_trend_number_ssp245,
  projection_trend_number_ssp585
)


# 2 Figure global pattern ---------------------------------------------------------------------
# global pattern
projection_trend_number_global <- projection_trend_number %>%
  group_by(ESM, ssp) %>%
  summarise(
    total_number = sum(total_number),
    positive_trend_number = sum(positive_trend_number),
    negative_trend_number = sum(negative_trend_number)
  ) %>%
  ungroup()

projection_trend_number_global <- projection_trend_number_global %>%
  mutate(
    positive_trend_proportion = positive_trend_number / total_number,
    negative_trend_proportion = negative_trend_number / total_number,
    no_trend_proportion = (total_number - positive_trend_number - negative_trend_number) / total_number
  )

projection_trend_number_global <- projection_trend_number_global %>%
  pivot_longer(-c(ESM, ssp, total_number, positive_trend_number, negative_trend_number), names_to = "trend", values_to = "proportion") %>%
  mutate(x_label = paste(ESM, ssp)) %>%
  mutate(trend = factor(trend, levels = c(
    "positive_trend_proportion",
    "no_trend_proportion",
    "negative_trend_proportion"
  )))

f_global_trend <- ggplot(projection_trend_number_global) +
  geom_bar(aes(x = x_label, y = proportion, fill = trend),
    stat = "identity", position = "fill"
  ) +
  geom_hline(yintercept = c(0.25, 0.50, 0.75), color = "white", 
             linetype = "dotted", size = 0.25) +
  facet_wrap(. ~ ssp, scale = "free_x") +
  labs(subtitle = c("Global", "N = 604")) +
  scale_fill_manual(
    name = "Directional effect",
    values = c(
      "positive_trend_proportion" = mypal[1],
      "negative_trend_proportion" = mypal[2],
      "no_trend_proportion" = "grey"
    ),
    labels = c(
      "positive_trend_proportion" = "Positive",
      "negative_trend_proportion" = "Negative",
      "no_trend_proportion" = "Neutral"
    )
  ) +
  scale_x_discrete("Earth system model (ESM)", expand = c(0, 0), labels = c(
    "IPSL", "MPI", "NOR", "IPSL", "MPI", "NOR", "IPSL", "MPI", "NOR"
  )) +
  scale_y_continuous("Proportion", expand = c(0, 0), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
  mytheme +
  theme(panel.spacing = unit(1, "lines"),
        plot.subtitle = element_text(
          family = "Arial", size = c(12, 10), face = c(2, 1),
          color = c(mypal[4], "black"),
          hjust = c(0, 1), vjust = 0, margin = margin(b = 1.25)
        ),)


# 3 Figure area pattern ---------------------------------------------------
# area pattern
for (i in unique(projection_trend_number$primary_FAOarea)) {
  # i="34"
  projection_trend_loop <- projection_trend_number %>%
    filter(primary_FAOarea == i)

  total_stock_number <- projection_trend_loop$total_number[1]

  # change to long data
  projection_trend_loop <- projection_trend_loop %>%
    pivot_longer(-c(primary_FAOarea, ESM, ssp, total_number, positive_trend_number, negative_trend_number), names_to = "trend", values_to = "proportion") %>%
    mutate(x_label = paste(ESM, ssp)) %>%
    mutate(trend = factor(trend, levels = c(
      "positive_trend_proportion",
      "no_trend_proportion",
      "negative_trend_proportion"
    )))

  # plot
  f_trend <- ggplot(projection_trend_loop) +
    geom_bar(aes(x = x_label, y = proportion, fill = trend),
             stat = "identity", position = "fill", show.legend = FALSE) +
    geom_hline(yintercept = c(0.25, 0.50, 0.75), color = "white", 
               linetype = "dotted", size = 0.25) +
    facet_wrap(. ~ ssp, scale = "free_x") +
    labs(subtitle = c(paste0("Area ", i), paste0("N = ", total_stock_number))) +
    scale_fill_manual(
      name = "Directional effect",
      values = c(
        "positive_trend_proportion" = mypal[1],
        "negative_trend_proportion" = mypal[2],
        "no_trend_proportion" = "grey"
      ),
      labels = c(
        "positive_trend_proportion" = "Positive",
        "negative_trend_proportion" = "Negative",
        "no_trend_proportion" = "Neutral"
      )
    ) +
    scale_x_discrete("Earth system model (ESM)", expand = c(0, 0), labels = c(
      "IPSL", "MPI", "NOR", "IPSL", "MPI", "NOR", "IPSL", "MPI", "NOR"
    )) +
    scale_y_continuous("Proportion", expand = c(0, 0), breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
    mytheme +
    theme(axis.text = element_blank(),
          strip.text = element_blank())
  
  assign(paste0("f_trend_", i), f_trend)
}


# 4 Figure combination -----------------------------------------------
# layout
f_null <- ggplot() +
  theme(axis.line = element_blank())

f_top <- ggarrange(f_trend_67, f_trend_21, f_trend_31, f_trend_34, f_trend_27, f_trend_37, nrow = 1)
f_middle1 <- ggarrange(f_trend_77, f_null, f_null, f_null, f_null, f_trend_61, nrow = 1)
f_middle2 <- ggarrange(f_trend_81, f_null, f_null, f_null, f_null, f_trend_71, nrow = 1)
f_bottom <- ggarrange(f_trend_87, f_trend_41, f_trend_47, f_trend_57, f_null, f_null, nrow = 1)
f <- ggarrange(f_top, f_middle1, f_middle2, f_bottom, nrow = 4)

g <- ggplotGrob(f_global_trend)
f_final <- f + annotation_custom(g, xmin = 0.175, xmax = 0.8375, ymin = 0.25, ymax = 0.75)

ggsave("Figures/projection_trend.PDF", device = cairo_pdf, width = 9, height = 6)

# 9999 Trend summary ---------------------------------------------------------
projection_trend_number_summary <- projection_trend_number %>%
  group_by(primary_FAOarea, ssp) %>%
  summarize(
    positive = mean(positive_trend_proportion),
    negative = mean(negative_trend_proportion),
    none = mean(no_trend_proportion)
  )


