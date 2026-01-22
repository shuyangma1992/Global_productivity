library(tidyverse)
library(ggsci)
library(scales)
library(ggpubr)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Arial")) # Font
scales::show_col(pal_lancet()(9))
mypal <- pal_lancet()(9) # Color
mytheme <- theme(
  # axis
  axis.title.x = element_blank(),
  axis.title.y = element_text(family = "Arial"),
  axis.text.x = element_text(family = "Arial"),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major.x = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  panel.grid.major.y = element_blank(),
  # legend
  legend.text = element_text(family = "Arial")
)

# Figure 1 By FAO area -----------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# FAO area
productivity_characteristics <- productivity_characteristics %>%
  group_by(primary_FAOarea) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  ) 

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%  
  filter(!primary_FAOarea %in% c("51", "58")) %>% # too few stocks
  mutate(primary_FAOarea = factor(primary_FAOarea, levels = primary_FAOarea)) 

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# # save table
# write.csv(productivity_characteristics, "Tables/Table_S1_FAO_area.csv")

f1 <- ggplot(productivity_characteristics) +
  geom_point(aes(x = p_mean, y = primary_FAOarea), colour = mypal[1], size = 1) +
  geom_errorbar(
    aes(xmin = p_mean - 1.96 * p_sd, xmax = p_mean + 1.96 * p_sd, y = primary_FAOarea),
    colour = alpha(mypal[1], 0.25), width = 0, linewidth = 0.25
  ) +
  geom_text(
    aes(x = p_mean + 0.05, y = primary_FAOarea, label = primary_FAOarea),
    colour = "black", family = "Arial", hjust = 0, size = 3
  ) +
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity", breaks = c(0, 0.2, 0.4, 0.6)) +
  scale_y_discrete("FAO major fishing area") +
  mytheme

# Figure 2 By major taxa ---------------------------------------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# fishery type
productivity_characteristics <- productivity_characteristics %>%
  group_by(FisheryType) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  )

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%
  mutate(FisheryType = factor(FisheryType, levels = FisheryType))

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# # save table
# write.csv(productivity_characteristics, "Tables/Table_S1_major_taxa.csv")

f2 <- ggplot(productivity_characteristics) +
  geom_point(aes(x = p_mean, y = FisheryType), colour = mypal[4], size = 1) +
  geom_errorbar(aes(xmin = p_mean - 1.96 * p_sd, xmax = p_mean + 1.96 * p_sd, y = FisheryType),
    colour = alpha(mypal[4], 0.25), width = 0, linewidth = 0.25
  ) +
  geom_text(aes(x = p_mean + 0.1, y = FisheryType, label = FisheryType),
    colour = "black", family = "Arial", hjust = 0, size = 3
  ) +
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity") +
  scale_y_discrete("Major taxa") +
  mytheme

# Figure 3 By family -----------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

# stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

# combine data
productivity_characteristics <- productivity_characteristics %>%
  left_join(stock_group_information)

# family
productivity_characteristics <- productivity_characteristics %>%
  group_by(family) %>%
  summarise(
    p_mean = mean(p_mean_mean),
    p_sd = sd(p_mean_mean),
    n = n()
  )

productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean) %>%
  mutate(family = factor(family, levels = family))

productivity_characteristics <- productivity_characteristics %>% 
  mutate(mean_and_CI = paste0(
    round(p_mean, 3), " (", round(p_mean - 1.96 * p_sd, 3), "-", round(p_mean + 1.96 * p_sd, 3), ")"))

# # save table
# write.csv(productivity_characteristics, "Tables/Table_S1_family.csv")

top_5 <- tail(productivity_characteristics, 5)
bottom_5 <- head(productivity_characteristics, 5)

f3 <- ggplot(productivity_characteristics) +
  geom_point(aes(x = p_mean, y = family), colour = mypal[3], size = 1) +
  geom_errorbar(aes(xmin = p_mean - 1.96 * p_sd, xmax = p_mean + 1.96 * p_sd, y = family),
    colour = alpha(mypal[3], 0.25), width = 0, linewidth = 0.25
  ) +
  annotate("text", x = 0.75, y = 70, label = paste(
    "Top 5",
    top_5$family[5],
    top_5$family[4],
    top_5$family[3],
    top_5$family[2],
    top_5$family[1],
    sep = "\n"
    ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  annotate("text", x = 0.75, y = 20, label = paste(
    "Bottom 5",
    bottom_5$family[1],
    bottom_5$family[2],
    bottom_5$family[3],
    bottom_5$family[4],
    bottom_5$family[5],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  scale_x_continuous("Productivity") +
  scale_y_discrete("Family") +
  mytheme

# Figure 4 By stock -----------------------------------------------------------------
# productivity_characteristics
productivity_characteristics <- read_rds("Outputs/Productivity/productivity_characteristics.rds")

nrow(filter(productivity_characteristics, p_mean_mean <= 0.50))/710
nrow(filter(productivity_characteristics, p_mean_mean <= 0.25))/710


# reorder by mean productivity
productivity_characteristics <- productivity_characteristics %>%
  arrange(p_mean_mean) %>%
  mutate(stockid = factor(stockid, levels = stockid))

# save table
write.csv(productivity_characteristics, "Tables/Table_S1_stock.csv")

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds")

top_10 <- tail(productivity_characteristics, 10) %>% 
  left_join(stock_success)
bottom_10 <- head(productivity_characteristics, 10) %>% 
  left_join(stock_success)

f4 <- ggplot(productivity_characteristics) +
  geom_point(aes(x = p_mean_mean, y = stockid), colour = mypal[2], size = 1) +
  geom_errorbar(aes(xmin = p_mean_lci, xmax = p_mean_uci, y = stockid),
    colour = alpha(mypal[2], 0.25), width = 0, linewidth = 0.25
  ) +
  annotate("text", x = 0.3, y = 600, label = paste(
    "Top 10",
    top_10$stocklong[10],
    top_10$stocklong[9],
    top_10$stocklong[8],
    top_10$stocklong[7],
    top_10$stocklong[6],
    top_10$stocklong[5],
    top_10$stocklong[4],
    top_10$stocklong[3],
    top_10$stocklong[2],
    top_10$stocklong[1],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  annotate("text", x = 0.3, y = 200, label = paste(
    "Bottom 10",
    bottom_10$stocklong[1],
    bottom_10$stocklong[2],
    bottom_10$stocklong[3],
    bottom_10$stocklong[4],
    bottom_10$stocklong[5],
    bottom_10$stocklong[6],
    bottom_10$stocklong[7],
    bottom_10$stocklong[8],
    bottom_10$stocklong[9],
    bottom_10$stocklong[10],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  scale_x_continuous("Productivity") +
  scale_y_discrete("Stock") +
  mytheme

# Figure combination ------------------------------------------------------
f_left <- ggarrange(f1, f2, f3, ncol = 1, heights = c(2.5, 2, 3.5))
f_right <- f4
f <- ggarrange(f_left, f_right, ncol = 2)

ggsave("Figures/productivity_comparison.pdf", device = cairo_pdf, width = 6, height = 6)















