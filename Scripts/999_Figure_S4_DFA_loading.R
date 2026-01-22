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
  panel.grid.major = element_blank(),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# loading
DFA_loading <- read_rds("Outputs/DFA results/0_best_DFA_loading.rds")

for (i in unique(DFA_loading$area)) {
  # i="47"
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

  f_loading <- ggplot(DFA_loading_loop) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.25) +
    geom_errorbar(aes(xmin = low, xmax = up, y = names, alpha = significance, color = trend), show.legend = F, position = position_dodge(width = 1), width = 0, linewidth = 0.1) +
    geom_point(aes(x = mean, y = names, alpha = significance, color = trend), show.legend = F, position = position_dodge(width = 1), size = 0.25) +
    scale_color_manual(values = c("trend1" = mypal[1], "trend2" = mypal[2])) +
    scale_alpha_manual(values = c("significant" = 1, "not significant" = 0.1)) +
    annotate("text",
      x = -Inf, y = Inf, label = paste0(i),
      vjust = 1, hjust = -0.05, color = "black", family = "Arial", fontface = 2
    ) +
    annotate("text",
             x = Inf, y = -Inf, label = paste(paste0("Total: ", stock_number),
                                            paste0("Trend 1+: ", stock_number_trend1_positive),
                                            paste0("Trend 1-: ", stock_number_trend1_negative),
                                            paste0("Trend 2+: ", stock_number_trend2_positive),
                                            paste0("Trend 2-: ", stock_number_trend2_negative),
                                            sep = "\n"
             ),
             vjust = -0.1, hjust = 1, color = "black", family = "Arial", fontface = 1, size = 2,
             lineheight = 1
    ) +
    scale_x_continuous("Loading", expand = c(0, 0), 
                       limits = c(quantile(DFA_loading_loop$low, 0.025), quantile(DFA_loading_loop$up, 0.975)), 
                       breaks = c(0)) +
    scale_y_discrete("Stock", expand = c(0, 0)) +
    mytheme

  assign(paste0("f_loading_", i), f_loading)
}

# layout
f_null <- ggplot() +
  theme(axis.line = element_blank())

f_top <- ggarrange(f_loading_67, f_loading_21, f_loading_31, f_loading_34, f_loading_27, f_loading_37, nrow = 1)
f_middle1 <- ggarrange(f_loading_77, f_null, f_null, f_null, f_null, f_loading_61, nrow = 1)
f_middle2 <- ggarrange(f_loading_81, f_null, f_null, f_null, f_null, f_loading_71, nrow = 1)
f_bottom <- ggarrange(f_loading_87, f_loading_41, f_loading_47, f_loading_57, f_null, f_null, nrow = 1)
f <- ggarrange(f_top, f_middle1, f_middle2, f_bottom, nrow = 4)

ggsave("Figures/DFA_loading.PDF", device = cairo_pdf, width = 9, height = 6)








