library(tidyverse)
library(ggdist)
library(ggsci)
library(ggpubr)
library(scales)
library(broom)
library(RColorBrewer)

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
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# 1 Fixed effects ---------------------------------------------------------
fixed_effects <- read_rds("Outputs/GLMM results/fixed_effects.rds") %>%
  filter(term %in% c("temperature", "chlorophyll", "mixedlayerthickness"))

# median and 95% confidence interval
fixed_effects_summary <- fixed_effects %>%
  group_by(term) %>%
  summarise(
    median = median(estimate),
    Q2.5 = quantile(estimate, 0.025),
    Q17 = quantile(estimate, 0.17),
    Q83 = quantile(estimate, 0.83),
    Q97.5 = quantile(estimate, 0.975)
  ) %>%
  mutate(term = factor(term, levels = c("temperature", "chlorophyll", "mixedlayerthickness")))

# temperature and mixlayerthickness *100
fixed_effects_summary[2:3, 2:6] <- fixed_effects_summary[2:3, 2:6] * 100

# plot
f_fixed_effects <- ggplot(fixed_effects_summary) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_errorbar(aes(x = term, ymin = Q2.5, ymax = Q97.5, color = term),
    width = 0, show.legend = F
  ) +
  geom_errorbar(aes(x = term, ymin = Q17, ymax = Q83, color = term),
    width = 0, show.legend = F, linewidth = 1
  ) +
  geom_point(aes(x = term, y = median, color = term), show.legend = F) +
  scale_color_manual(values = c("temperature" = mypal[2], "chlorophyll" = mypal[3], "mixedlayerthickness" = mypal[1])) +
  scale_x_discrete("Variable", labels = c("Temperature (x100)", "Chlorophyll", "Mixed layer depth (x100)")) +
  scale_y_continuous("Slope") +
  mytheme +
  theme(axis.title.x = element_blank())

# 2 Random effects --------------------------------------------------------
random_effects <- read_rds("Outputs/GLMM results/random_effects.rds") %>%
  select(1, 5, 6)

random_effects_summary <- random_effects %>%
  group_by(scientificname) %>%
  summarise(
    median_T = median(temperature_adjusted),
    low_T = quantile(temperature_adjusted, 0.025),
    high_T = quantile(temperature_adjusted, 0.975),
    median_CHL = median(chlorophyll_adjusted),
    low_CHL = quantile(chlorophyll_adjusted, 0.025),
    high_CHL = quantile(chlorophyll_adjusted, 0.975)
  )

# significance 95% confidence interval does not include 0
random_effects_summary <- random_effects_summary %>%
  mutate(
    significance_T = ifelse(median_T < 0 & high_T < 0, 1, ifelse(median_T > 0 & low_T > 0, 1, 0.15)),
    significance_CHL = ifelse(median_CHL < 0 & high_CHL < 0, 1, ifelse(median_CHL > 0 & low_CHL > 0, 1, 0.15))
  )

# T figure
random_effects_summary_T <- random_effects_summary %>%
  select(1, 2, 3, 4, 8) %>%
  arrange(median_T) %>%
  mutate(scientificname = factor(scientificname, levels = scientificname))

# random_effects_summary_T %>%
#   filter(scientificname == "Macruronus novaezelandiae")

# negative and positive effects
T_negative_effect <- filter(random_effects_summary_T, significance_T == 1 & median_T < 0) %>%
  arrange(median_T)
a <- nrow(T_negative_effect) # 49 species negative effects
bottom_5 <- head(T_negative_effect, 5)

T_positive_effect <- filter(random_effects_summary_T, significance_T == 1 & median_T > 0)
b <- nrow(T_positive_effect) # 57 species positive effects
top_5 <- tail(T_positive_effect, 5)

f_random_effects_T <- ggplot(random_effects_summary_T) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_point(aes(x = scientificname, y = median_T * 100),
    color = mypal[2], alpha = random_effects_summary_T$significance_T, size = 0.25
  ) +
  geom_errorbar(aes(ymin = low_T * 100, ymax = high_T * 100, x = scientificname),
    width = 0, color = mypal[2], linewidth = 0.1,
    alpha = random_effects_summary_T$significance_T
  ) +
  annotate("text",
    label = paste0("N = ", a), family = "Arial",
    x = -Inf, y = -Inf, hjust = 0, vjust = 0
  ) +
  annotate("text",
    label = paste0("N = ", b), family = "Arial",
    x = Inf, y = Inf, hjust = 1, vjust = 1
  ) +
  annotate("text",
    label = paste("Top 5",
      top_5$scientificname[5],
      top_5$scientificname[4],
      top_5$scientificname[3],
      top_5$scientificname[2],
      top_5$scientificname[1],
      sep = "\n"
    ),
    family = "Arial", x = 100, y = 4, hjust = 0,
    lineheight = 0.8, size = 3
  ) +
  annotate("text",
    label = paste("Bottom 5",
      bottom_5$scientificname[1],
      bottom_5$scientificname[2],
      bottom_5$scientificname[3],
      bottom_5$scientificname[4],
      bottom_5$scientificname[5],
      sep = "\n"
    ),
    family = "Arial", x = 100, y = -4, hjust = 0,
    lineheight = 0.8, size = 3
  ) +
  scale_x_discrete("Species (arranged by slope of T)") +
  scale_y_continuous("Temperature effect") +
  mytheme +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )

# CHL figure
random_effects_summary_CHL <- random_effects_summary %>%
  select(1, 5, 6, 7, 9) %>%
  arrange(median_CHL) %>%
  mutate(scientificname = factor(scientificname, levels = scientificname))

# random_effects_summary_CHL %>%
#   filter(scientificname == "Macruronus novaezelandiae")

# negative and positive effects
CHL_negative_effect <- filter(random_effects_summary_CHL, significance_CHL == 1 & median_CHL < 0) %>%
  arrange(median_CHL)
c <- nrow(CHL_negative_effect) # 37 species negative effects
bottom_5 <- head(CHL_negative_effect, 5)

CHL_positive_effect <- filter(random_effects_summary_CHL, significance_CHL == 1 & median_CHL > 0)
d <- nrow(CHL_positive_effect) # 57 species positive effects
top_5 <- tail(CHL_positive_effect, 5)

f_random_effects_CHL <- ggplot(random_effects_summary_CHL) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_point(aes(x = scientificname, y = median_CHL), color = mypal[3], alpha = random_effects_summary_CHL$significance_CHL, size = 0.25) +
  geom_errorbar(aes(ymin = low_CHL, ymax = high_CHL, x = scientificname),
    width = 0, color = mypal[3], linewidth = 0.1,
    alpha = random_effects_summary_CHL$significance_CHL
  ) +
  annotate("text",
    label = paste0("N = ", c), family = "Arial",
    x = -Inf, y = -Inf, hjust = 0, vjust = 0
  ) +
  annotate("text",
    label = paste0("N = ", d), family = "Arial",
    x = Inf, y = Inf, hjust = 1, vjust = 1
  ) +
  annotate("text",
    label = paste("Top 5",
      top_5$scientificname[5],
      top_5$scientificname[4],
      top_5$scientificname[3],
      top_5$scientificname[2],
      top_5$scientificname[1],
      sep = "\n"
    ),
    family = "Arial", x = 100, y = 2, hjust = 0,
    lineheight = 0.8, size = 3
  ) +
  annotate("text",
    label = paste("Bottom 5",
      bottom_5$scientificname[1],
      bottom_5$scientificname[2],
      bottom_5$scientificname[3],
      bottom_5$scientificname[4],
      bottom_5$scientificname[5],
      sep = "\n"
    ),
    family = "Arial", x = 100, y = -1, hjust = 0,
    lineheight = 0.8, size = 3
  ) +
  scale_x_discrete("Species (arranged by slope of CHL)") +
  scale_y_continuous("Chlorophyll effect") +
  mytheme +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
  )



# 3 Figure combination ----------------------------------------------------
f <- ggarrange(f_fixed_effects, f_random_effects_T, f_random_effects_CHL,
  labels = c("(a)", "(b)", "(c)"), ncol = 1, heights = c(1, 1.25, 1.25),
  font.label = list(family = "Arial"), align = "v"
)

ggsave("Figures/GLMM_results.PDF", device = cairo_pdf, width = 5, height = 6)




# 4 Information -----------------------------------------------------------
# data used for analysis
productivity_for_glmm <- read_rds("Outputs/Productivity/productivity_for_glmm.rds")
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm, species_information)

# significant effects of T
a <- random_effects_summary_T %>%
  filter(significance_T == 1) # 106 species
a_positive <- filter(a, median_T > 0) # 57 species positive effects of T
a_positive_stock <- productivity_for_glmm %>% 
  filter(scientificname %in% a_positive$scientificname)
unique(a_positive_stock$stockid) # 129 stocks positive effects of T

a_negative <- filter(a, median_T < 0) # 49 species negative effects of T
a_negative_stock <- productivity_for_glmm %>% 
  filter(scientificname %in% a_negative$scientificname)
unique(a_negative_stock$stockid) # 133 stocks negative effects of T


# significant effects of CHL
b <- random_effects_summary_CHL %>%
  filter(significance_CHL == 1)
b_positive <- filter(b, median_CHL > 0) # 71 species positive effects of CHL
b_positive_stock <- productivity_for_glmm %>% 
  filter(scientificname %in% b_positive$scientificname)
unique(b_positive_stock$stockid) # 129 stocks positive effects of CHL

b_negative <- filter(b, median_CHL < 0) # 37 species negative effects of CHL
b_negative_stock <- productivity_for_glmm %>% 
  filter(scientificname %in% b_negative$scientificname)
unique(b_negative_stock$stockid) # 105 stocks negative effects of CHL

# significant effects in total
c <- bind_rows(a, b)

unique(c$scientificname) # 139 out of 265 species

d <- productivity_for_glmm %>% 
  filter(scientificname %in% unique(c$scientificname))
unique(d$stockid) # 354 stocks with significant effects

unique(productivity_for_glmm$stockid) # in total 652 stocks





# 999 Stock-specific effects and area (mianji) relationship --------------------------------------------
stock_distribution <- read_rds("Data/stock_distribution_area_centroid.rds")

stock_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(scientificname,GRSF_uuid)

stock <- left_join(stock_distribution,stock_information) %>% 
  left_join(random_effects_summary_T) %>% 
  left_join(random_effects_summary_CHL)

f1 <- ggplot(stock)+
  geom_point(aes(x = area, y = median_T))

f2 <- ggplot(stock)+
  geom_point(aes(x = area, y = median_CHL))

glance(lm(median_T~area, data = stock))
glance(lm(median_CHL~area, data = stock))
