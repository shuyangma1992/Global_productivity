library(tidyverse)
library(RColorBrewer)
library(ggdist)
library(ggsci)
library(psych)

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
  axis.text.y = element_text(family = "Arial", size = 8, margin = margin(t = 0)),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.line = element_line(linewidth = 0.25),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_rect(fill = "white", linewidth = 0.25),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# Biophysical driver collinearity
productivity_for_glmm <- read_rds("Outputs/Productivity/productivity_for_glmm.rds") %>%
  select(c(1, 2, 1005:1012))

r <- NULL
p <- NULL
for (i in unique(productivity_for_glmm$stockid)) {
  # i <- unique(productivity_for_glmm$stockid)[1]
  data_loop <- filter(productivity_for_glmm, stockid == i)
  r_loop <- as.data.frame(corr.test(data_loop[, -c(1, 2)])$r)
  r_loop <- r_loop %>%
    mutate(variable1 = rownames(r_loop))
  p_loop <- as.data.frame(corr.test(data_loop[, -c(1, 2)])$p)
  p_loop <- p_loop %>%
    mutate(variable1 = rownames(p_loop))
  r <- bind_rows(r, r_loop)
  p <- bind_rows(p, p_loop)
}

# data processing
r <- r %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "r") %>%
  mutate(
    variable1 = factor(variable1, levels = unique(variable1)),
    variable2 = factor(variable2, levels = unique(variable2))
  )

r_statistics <- r %>%
  group_by(variable1, variable2) %>%
  summarise(
    r_2.5 = quantile(r, 0.025),
    r_97.5 = quantile(r, 0.975),
    r_median = quantile(r, 0.5)
  )

p <- p %>%
  pivot_longer(-variable1, names_to = "variable2", values_to = "p") %>%
  mutate(
    variable1 = factor(variable1, levels = unique(variable1)),
    variable2 = factor(variable2, levels = unique(variable2))
  )

# in total 652 stocks
p %>% filter(variable1 == "temperature", variable2 == "chlorophyll") %>% 
  filter(p >= 0.05) # 461 stocks
p %>% filter(variable1 == "temperature", variable2 == "mixedlayerthickness") %>% 
  filter(p >= 0.05) # 613 stocks
p %>% filter(variable1 == "chlorophyll", variable2 == "mixedlayerthickness") %>% 
  filter(p >= 0.05) # 470 stocks

# for all paired biophysical variables
n_no_correlation <- NULL
for (i in unique(p$variable1)) {
  
  # i = "temperature"
  for (j in unique(p$variable2)) {
    
    # j = "chlorophyll"
    data_loop <- p %>% 
      filter(variable1 == i, variable2 == j) %>% 
      filter(p > 0.05)
    
    n_no_correlation_loop <- data.frame(variable1 = i,
                                        variable2 = j,
                                        n = nrow(data_loop))
    
    n_no_correlation <- bind_rows(n_no_correlation, n_no_correlation_loop)
  
  }
  
}
n_no_correlation <- n_no_correlation %>% 
  pivot_wider(names_from = variable2, values_from = n)

# facet labels
driver.labs <- c("T", "S", "SSH", "MLD", "CHL", "DO", "TPP", "PH")
names(driver.labs) <- c(
  "temperature", "salinity", "seasurfaceheight", "mixedlayerthickness",
  "chlorophyll", "dissolvedoxygen", "primaryproduction", "ph"
)

# violin figure
f_r <- ggplot(r) +
  stat_halfeye(aes(x = "a", y = r),
    .width = c(0.66, 0, 95),
    scale = 5, point_size = 1, interval_size_range = c(0.25, 0.5)
  ) +
  geom_hline(yintercept = c(-0.374, 0.374), color = "pink", linewidth = 0.25) +
  geom_hline(yintercept = c(-0.478, 0.478), color = "red", linewidth = 0.25) +
  scale_x_discrete() +
  scale_y_continuous("Correlation coefficients") +
  facet_grid(variable1 ~ variable2, labeller = labeller(
    variable1 = driver.labs,
    variable2 = driver.labs
  )) +
  mytheme

ggsave("Figures/biophysical_collinearity.PDF", device = cairo_pdf, width = 6, height = 6)
