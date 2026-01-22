library(tidyverse)
library(ggthemes)
library(scales)
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
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# 0 Stock proportion, needed for DFA trend figure -------------------------
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

productivity_stock_number <- rename(productivity_stock_number, area = 3)


# 1 DFA trend -------------------------------------------------------------
# loading
DFA_loading <- read_rds("Outputs/DFA results/0_best_DFA_loading.rds")

# # change sign of fao area 21 trend 1
# DFA_loading <- DFA_loading %>%
#   mutate(mean = case_when(
#     area == "21" & trend == "trend1" ~ -mean,
#     .default = mean
#   )) %>%
#   mutate(up = case_when(
#     area == "21" & trend == "trend1" ~ -up,
#     .default = up
#   )) %>%
#   mutate(low = case_when(
#     area == "21" & trend == "trend1" ~ -low,
#     .default = low
#   ))

# trend
DFA_trend <- read_rds("Outputs/DFA results/0_best_DFA_trend.rds")

# # change sign of fao area 21 trend 1
# DFA_trend <- DFA_trend %>%
#   mutate(mean = case_when(
#     area == "21" & trend == "trend1" ~ -mean,
#     .default = mean
#   )) %>%
#   mutate(up = case_when(
#     area == "21" & trend == "trend1" ~ -up,
#     .default = up
#   )) %>%
#   mutate(low = case_when(
#     area == "21" & trend == "trend1" ~ -low,
#     .default = low
#   ))
for (i in unique(DFA_trend$area)) {
  # i="34"
  # get stock loading results
  DFA_loading_loop <- DFA_loading %>%
    filter(area == i)
  DFA_loading_loop <- DFA_loading_loop %>%
    mutate(names = factor(names, levels = arrange(filter(DFA_loading_loop, trend == "trend1"), mean)$names)) %>%
    rowwise() %>%
    mutate(significance = ifelse((mean > 0) & (quantile(range(low, up), 0.05) > 0),
      "significant",
      ifelse((mean < 0) & (quantile(range(low, up), 0.95) < 0),
        "significant",
        "not significant"
      )
    ))
  # stock number information
  stock_number <- length(unique(DFA_loading_loop$names))
  stock_number_trend1_positive <- dim(filter(DFA_loading_loop, (mean > 0) & significance == "significant" & trend == "trend1"))[1]
  stock_number_trend1_negative <- dim(filter(DFA_loading_loop, (mean < 0) & significance == "significant" & trend == "trend1"))[1]
  stock_number_trend2_positive <- dim(filter(DFA_loading_loop, (mean > 0) & significance == "significant" & trend == "trend2"))[1]
  stock_number_trend2_negative <- dim(filter(DFA_loading_loop, (mean < 0) & significance == "significant" & trend == "trend2"))[1]

  # plot trend
  DFA_trend_loop <- DFA_trend %>%
    filter(area == i)
  DFA_trend_loop <- left_join(DFA_trend_loop, productivity_stock_number)
  f_trend <- ggplot(DFA_trend_loop) +
    geom_vline(xintercept = c("1950", "1960", "1970", "1980", "1990", "2000", "2010"), 
               linetype = "dotted", color = "gray75", linewidth = 0.25) +
    geom_boxplot(
      data = filter(DFA_trend_loop, trend == "trend1"),
      aes(
        x = as.factor(year), ymin = low, lower = low, middle = mean, upper = up, ymax = up,
        alpha = stock_number_proportion, fill = "trend1"
      ),
      stat = "identity", show.legend = F, color = NA
    ) +
    geom_boxplot(
      data = filter(DFA_trend_loop, trend == "trend2"),
      aes(
        x = as.factor(year), ymin = low, lower = low, middle = mean, upper = up, ymax = up,
        alpha = stock_number_proportion, fill = "trend2"
      ),
      stat = "identity", show.legend = F, color = NA
    ) +
    geom_line(
      data = filter(DFA_trend_loop, trend == "trend1"),
      aes(x = as.factor(year), y = mean, group = 1, color = "trend1"), linewidth = 0.25, show.legend = F
    ) +
    geom_line(
      data = filter(DFA_trend_loop, trend == "trend2"),
      aes(x = as.factor(year), y = mean, group = 1, color = "trend2"), linewidth = 0.25, show.legend = F
    ) +
    scale_alpha_continuous(range = c(0.1, 0.5)) +
    scale_fill_manual(values = c("trend1" = mypal[1], "trend2" = mypal[2])) +
    scale_color_manual(values = c("trend1" = mypal[1], "trend2" = mypal[2])) +
    annotate("text",
      x = 2, y = Inf, label = i,
      vjust = 1, hjust = 0, color = "black", family = "Arial", fontface = 1
    ) +
    annotate("text",
      x = 10, y = Inf, label = paste0(min(DFA_trend_loop$year), "-", max(DFA_trend_loop$year)),
      vjust = 1, hjust = 0, color = "black", family = "Arial", fontface = 1, size = 3
    ) +
    annotate("text",
      x = 2, y = -Inf, label = paste(paste0("Total: ", stock_number),
        paste0("Trend 1+: ", stock_number_trend1_positive),
        paste0("Trend 1-: ", stock_number_trend1_negative),
        paste0("Trend 2+: ", stock_number_trend2_positive),
        paste0("Trend 2-: ", stock_number_trend2_negative),
        sep = "\n"
      ),
      vjust = -0.1, hjust = 0, color = "black", family = "Arial", fontface = 1, size = 2,
      lineheight = 1
    ) +
    scale_x_discrete("Year",
      limits = factor(c(1950:2020)),
      breaks = factor(seq(1950, 2020, 10)),
      labels = c("", "1960", "", "1980", "", "2000", "", ""),
      expand = c(0, 0)
    ) +
    scale_y_continuous("Value") +
    mytheme

  assign(paste0("f_trend_", i), f_trend)
}

# layout
f_null <- ggplot() +
  theme(axis.line = element_blank())

f_top <- ggarrange(f_trend_67, f_trend_21, f_trend_31, f_trend_34, f_trend_27, f_trend_37, nrow = 1)
f_middle1 <- ggarrange(f_trend_77, f_null, f_null, f_null, f_null, f_trend_61, nrow = 1)
f_middle2 <- ggarrange(f_trend_81, f_null, f_null, f_null, f_null, f_trend_71, nrow = 1)
f_bottom <- ggarrange(f_trend_87, f_trend_41, f_trend_47, f_trend_57, f_null, f_null, nrow = 1)
f <- ggarrange(f_top, f_middle1, f_middle2, f_bottom, nrow = 4)

ggsave("Figures/DFA_trend.PDF", device = cairo_pdf, width = 9, height = 6)



