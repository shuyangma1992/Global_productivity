library(tidyverse)
library(ggsci)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(ggnewscale)

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
  axis.text.x = element_blank(),
  axis.text.y = element_text(family = "Arial"),
  axis.title = element_text(family = "Arial"),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# Figure about future winner and loser, use other ESMs results


# 1 2090s - 2020s MPI---------------------------------------------------------
# 2090s - 2020s
productivity_change_90s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_90s_20s.rds")
productivity_change_90s_20s <- productivity_change_90s_20s %>%
  filter(ESM == "MPI-ESM1-2-LR") %>%
  drop_na()
unique(productivity_change_90s_20s$stockid) # 608 stocks

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds")

# combine data
productivity_change_90s_20s <- left_join(productivity_change_90s_20s, stock_success)

# number of stocks
numbers <- productivity_change_90s_20s %>%
  mutate(significance = ifelse(p_change_mean > 0 & p_change_2.5 > 0,
    1,
    ifelse(p_change_mean < 0 & p_change_97.5 < 0,
      -1,
      0
    )
  ))

numbers <- numbers %>%
  ungroup() %>%
  select(-1) %>%
  count(ssp, significance)

# order based on ssp585 from low to high
order <- productivity_change_90s_20s %>%
  filter(ssp == "ssp585") %>%
  arrange(p_change_mean)

# winner and loser
loser_5 <- head(order, 5)
winner_5 <- tail(order, 5)

order <- as.character(order$stockid)

# stockid order
productivity_change_90s_20s <- productivity_change_90s_20s %>%
  mutate(stockid = factor(stockid, levels = order))

f_90s_MPI <- ggplot(productivity_change_90s_20s) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = stockid, y = p_change_mean, color = ssp), show.legend = F, size = 0.25) +
  scale_color_manual(values = c(
    "ssp126" = mypal[3],
    "ssp245" = mypal[1],
    "ssp585" = mypal[2]
  )) +
  new_scale_color() +
  geom_errorbar(aes(x = stockid, y = p_change_mean, color = ssp, ymin = p_change_2.5, ymax = p_change_97.5),
    width = 0, show.legend = F, linewidth = 0.25
  ) +
  scale_color_manual(values = c(
    "ssp126" = alpha(mypal[3], 0.25),
    "ssp245" = alpha(mypal[1], 0.25),
    "ssp585" = alpha(mypal[2], 0.25)
  )) +
  annotate("text",
    x = 25, y = -0.175,
    label = paste0(numbers$n[1], ", ", numbers$n[4], ", ", numbers$n[7]),
    family = "Arial", hjust = 0, size = 3
  ) +
  annotate("text",
    x = 375, y = 0.175,
    label = paste0(numbers$n[3], ", ", numbers$n[6], ", ", numbers$n[9]),
    family = "Arial", hjust = 0, size = 3
  ) +
  annotate("text", x = 450, y = 0.175, label = paste(
    "Top 5 winners",
    winner_5$stocklong[5],
    winner_5$stocklong[4],
    winner_5$stocklong[3],
    winner_5$stocklong[2],
    winner_5$stocklong[1],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  annotate("text", x = 100, y = -0.175, label = paste(
    "Top 5 losers",
    loser_5$stocklong[1],
    loser_5$stocklong[2],
    loser_5$stocklong[3],
    loser_5$stocklong[4],
    loser_5$stocklong[5],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  scale_x_discrete("Stock (arranged by productivity change under SSP 5-8.5)", expand = c(0, 0)) +
  scale_y_continuous("Productivity change", limits = c(-0.25, 0.25), expand = c(0, 0)) +
  mytheme

# ggsave("Figures/winner_loser_2090s_MPI.pdf", device = cairo_pdf, width = 6, height = 3)

# 2 2090s - 2020s Nor ---------------------------------------------------------
# 2090s - 2020s
productivity_change_90s_20s <- read_rds("Outputs/GLMM projection results/productivity_projection_change_90s_20s.rds")
productivity_change_90s_20s <- productivity_change_90s_20s %>%
  filter(ESM == "NorESM2-LM") %>%
  drop_na()
unique(productivity_change_90s_20s$stockid) # 614 stocks

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds")

# combine data
productivity_change_90s_20s <- left_join(productivity_change_90s_20s, stock_success)

# number of stocks
numbers <- productivity_change_90s_20s %>%
  mutate(significance = ifelse(p_change_mean > 0 & p_change_2.5 > 0,
    1,
    ifelse(p_change_mean < 0 & p_change_97.5 < 0,
      -1,
      0
    )
  ))

numbers <- numbers %>%
  ungroup() %>%
  select(-1) %>%
  count(ssp, significance)

# order based on ssp585 from low to high
order <- productivity_change_90s_20s %>%
  filter(ssp == "ssp585") %>%
  arrange(p_change_mean)

# winner and loser
loser_5 <- head(order, 5)
winner_5 <- tail(order, 5)

order <- as.character(order$stockid)

# stockid order
productivity_change_90s_20s <- productivity_change_90s_20s %>%
  mutate(stockid = factor(stockid, levels = order))

f_90s_Nor <- ggplot(productivity_change_90s_20s) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = stockid, y = p_change_mean, color = ssp), show.legend = F, size = 0.25) +
  scale_color_manual(values = c(
    "ssp126" = mypal[3],
    "ssp245" = mypal[1],
    "ssp585" = mypal[2]
  )) +
  new_scale_color() +
  geom_errorbar(aes(x = stockid, y = p_change_mean, color = ssp, ymin = p_change_2.5, ymax = p_change_97.5),
    width = 0, show.legend = F, linewidth = 0.25
  ) +
  scale_color_manual(values = c(
    "ssp126" = alpha(mypal[3], 0.25),
    "ssp245" = alpha(mypal[1], 0.25),
    "ssp585" = alpha(mypal[2], 0.25)
  )) +
  annotate("text",
    x = 25, y = -0.175,
    label = paste0(numbers$n[1], ", ", numbers$n[4], ", ", numbers$n[7]),
    family = "Arial", hjust = 0, size = 3
  ) +
  annotate("text",
    x = 375, y = 0.175,
    label = paste0(numbers$n[3], ", ", numbers$n[6], ", ", numbers$n[9]),
    family = "Arial", hjust = 0, size = 3
  ) +
  annotate("text", x = 450, y = 0.175, label = paste(
    "Top 5 winners",
    winner_5$stocklong[5],
    winner_5$stocklong[4],
    winner_5$stocklong[3],
    winner_5$stocklong[2],
    winner_5$stocklong[1],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  annotate("text", x = 100, y = -0.175, label = paste(
    "Top 5 losers",
    loser_5$stocklong[1],
    loser_5$stocklong[2],
    loser_5$stocklong[3],
    loser_5$stocklong[4],
    loser_5$stocklong[5],
    sep = "\n"
  ), family = "Arial", hjust = 0, size = 3, lineheight = 0.9) +
  scale_x_discrete("Stock (arranged by productivity change under SSP 5-8.5)", expand = c(0, 0)) +
  scale_y_continuous("Productivity change", limits = c(-0.25, 0.25), expand = c(0, 0)) +
  mytheme

# ggsave("Figures/winner_loser_2090s_Nor.pdf", device = cairo_pdf, width = 6, height = 3)


# 3 Combine figure --------------------------------------------------------
ggarrange(f_90s_MPI, f_90s_Nor,
  nrow = 2, align = "hv",
  labels = c("(a)", "(b)"), font.label = list(family = "Arial")
)

ggsave("Figures/winner_loser_2090s_MPI_Nor.pdf", device = cairo_pdf, width = 6, height = 6)