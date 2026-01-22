library(tidyverse)
library(ggsci)
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
  axis.text.x = element_text(family = "Arial", size = 8, margin = margin(t = 0)),
  axis.text.y = element_text(family = "Arial", size = 8, margin = margin(r = 0)),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# identify period of DFA, using period with more than 60% of data availability


# 1 Data compile ----------------------------------------------------------
# read data
productivity <- read_rds("Outputs/Productivity/productivity_for_DFA.rds") %>%
  select(year, stockid, p_mean) %>%
  drop_na() %>%
  arrange(year) %>%
  filter(year >= 1950)

productivity_period_DFA <- read_rds("Outputs/Productivity/productivity_period_for_DFA.rds") 

# stock information
# delete FAO area 51, 58, less than 5 stocks
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  filter(!primary_FAOarea %in% c("51", "58"))

productivity_stock_number <- NULL
for (i in unique(stock_success$primary_FAOarea)) {
  # i="21"
  stock_success_loop <- stock_success %>%
    filter(primary_FAOarea == i)
  stock_number <- nrow(stock_success_loop)
  
  productivity_loop <- productivity %>%
    filter(stockid %in% stock_success_loop$stockid) %>%
    drop_na()
  
  productivity_loop <- productivity_loop %>%
    group_by(year) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(
      FAOarea = i,
      stock_number = stock_number
    )
  
  productivity_stock_number <- bind_rows(productivity_stock_number, productivity_loop)
}

# stock number proportion
productivity_stock_number <- productivity_stock_number %>%
  rowwise() %>%
  mutate(stock_number_proportion = n / stock_number)


# 2 Figure ----------------------------------------------------------------
# loop make figure
for (i in unique(productivity_period_DFA$FAOarea)) {
  # i=21
  productivity_stock_number_loop <- productivity_stock_number %>%
    filter(FAOarea == i)
  
  stock_number  <- unique(productivity_stock_number_loop$stock_number) 
  
  period <- productivity_period_DFA %>%
    filter(FAOarea == i)

  f <- ggplot(productivity_stock_number_loop) +
    annotate("rect",
      xmin = period$start_year, xmax = period$end_year, ymin = -Inf, ymax = Inf,
      fill = scales::alpha(mypal[1], 0.1)
    ) +
    annotate("text",
      x = 1952, y = 0.95, label = paste0("FAO Area ", i),
      hjust = 0, color = mypal[1], family = "Arial", fontface = 2, size = 3
    ) +
    annotate("text",
      x = 1952, y = 0.8, label = paste0("N = ", stock_number),
      hjust = 0, color = "black", family = "Arial", size = 3
    ) +
    annotate("text",
      x = 2020, y = -Inf, label = paste0(period$start_year, "-", period$end_year),
      vjust = -0.5, hjust = 1, color = mypal[1], family = "Calibri", fontface = 2
    ) +
    geom_line(aes(x = year, y = stock_number_proportion), linewidth = 0.25) +
    geom_hline(yintercept = 0.6, linetype = "dashed", linewidth = 0.25) +
    scale_x_continuous("Year", limits = c(1950, 2020), expand = c(0, 0), breaks = seq(1960, 2000, 20)) +
    scale_y_continuous("Proportion", limits = c(0, 1), expand = c(0, 0), breaks = seq(0, 1, 0.5)) +
    mytheme

  assign(paste0("f_", i), f)
}

# layout
f_null <- ggplot() +
  theme(axis.line = element_blank())
f_top <- ggarrange(f_67, f_21, f_31, f_34, f_27, f_37, nrow = 1)
f_middle1 <- ggarrange(f_77, f_null, f_null, f_null, f_null, f_61, nrow = 1)
f_middle2 <- ggarrange(f_81, f_null, f_null, f_null, f_null, f_71, nrow = 1)
f_bottom <- ggarrange(f_87, f_41, f_47, f_57, f_null, f_null, nrow = 1)
f <- ggarrange(f_top, f_middle1, f_middle2, f_bottom, nrow = 4)

ggsave("Figures/DFA_period.PDF", device = cairo_pdf, width = 9, height = 6)

