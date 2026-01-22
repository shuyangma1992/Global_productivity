library(tidyverse)
library(ggdist)
library(ggsci)
library(ggpubr)
library(scales)
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
  axis.text.x = element_text(family = "Arial"),
  axis.text.y = element_text(family = "Arial"),
  axis.title = element_text(family = "Arial"),
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

# read data
glmm_information <- read_rds("Outputs/GLMM results/glmm_information.rds") %>%
  mutate(model = paste0("Model ", model)) %>%
  pivot_longer(c(AIC, BIC), names_to = "information_criterion", values_to = "value")
glmm_information <- glmm_information %>% 
  mutate(model = factor(model, levels = rev(unique(model))))

# min information criterion
min_information_criterion <- glmm_information %>%
  group_by(loop, information_criterion) %>%
  summarize(min_information_criterion = min(value)) %>%
  ungroup()

# combine data and calculate delta AIC and delta BIC
glmm_information <- left_join(glmm_information, min_information_criterion)
glmm_information <- glmm_information %>%
  mutate(delta_value = value - min_information_criterion)

# data without model 6
glmm_information_without_model6 <- glmm_information %>%
  mutate(delta_value = case_when(
    model == "Model 6" ~ NA,
    .default = delta_value
  ))

# data with only model 6
glmm_information_with_model6 <- glmm_information %>%
  filter(model == "Model 6")

f <- ggplot() +
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, fill = "grey90") +
  stat_dotsinterval(
    data = glmm_information_without_model6,
    aes(x = delta_value, y = model, slab_fill = information_criterion),
    .width = c(0.66, 0, 95), point_size = 0.5, interval_size_range = c(0.25, 0.5),
    position = "dodge", show.legend = FALSE, slab_color = NA, scale = 0.8
  ) +
  stat_dotsinterval(
    data = glmm_information_with_model6,
    aes(x = delta_value, y = model, slab_fill = information_criterion),
    .width = c(0.66, 0, 95), point_size = 0.5, interval_size_range = c(0.25, 0.5),
    position = "dodge", show.legend = FALSE, slab_color = NA, scale = 0.8
  ) +
  scale_color_manual(
    values = c("AIC" = alpha(mypal[1], 0.5), "BIC" = alpha(mypal[2], 0.5)),
    aesthetics = "slab_fill"
  ) +
  scale_x_continuous("Information Criteria value (delta)") +
  scale_y_discrete("Generalized linear mixed model (GLMM)") +
  mytheme

ggsave("Figures/GLMM_comparison.pdf", device = cairo_pdf, width = 4, height = 6)


# inside figure
glmm_information_without_model6_model1 <- glmm_information %>%
  mutate(delta_value = case_when(
    model == "Model 6" ~ NA,
    model == "Model 1" ~ NA,
    .default = delta_value
  ))

f <- ggplot() +
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = 3.5, fill = "grey90") +
  stat_dotsinterval(
    data = glmm_information_without_model6_model1,
    aes(x = delta_value, y = model, slab_fill = information_criterion),
    .width = c(0.66, 0, 95), point_size = 0.5, interval_size_range = c(0.25, 0.5),
    position = "dodge", show.legend = FALSE, slab_color = NA, scale = 0.8
  ) +
  stat_dotsinterval(
    data = glmm_information_with_model6,
    aes(x = delta_value, y = model, slab_fill = information_criterion),
    .width = c(0.66, 0, 95), point_size = 0.5, interval_size_range = c(0.25, 0.5),
    position = "dodge", show.legend = FALSE, slab_color = NA, scale = 0.8
  ) +
  scale_color_manual(
    values = c("AIC" = alpha(mypal[1], 0.5), "BIC" = alpha(mypal[2], 0.5)),
    aesthetics = "slab_fill"
  ) +
  scale_x_continuous("Information Criteria value (delta)") +
  scale_y_discrete("Generalized linear mixed model (GLMM)") +
  mytheme

ggsave("Figures/GLMM_comparison_inside.pdf", device = cairo_pdf, width = 3, height = 6)





