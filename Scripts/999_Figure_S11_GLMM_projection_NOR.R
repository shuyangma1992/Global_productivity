library(tidyverse)
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
  axis.text.x = element_blank(),
  axis.text.y = element_text(family = "Arial", angle = 90, hjust = 0.5, margin = margin(r = 0)),
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


# 1 Productivity projection global  ---------------------------------
#read data
productivity_summary <- read_rds("Outputs/GLMM projection results/productivity_projection_summary_global.rds")

# 2021-2100
productivity_summary <- productivity_summary %>% 
  filter(year %in% seq(2021,2100,1))

# a <- projection_all %>% 
#   filter(year %in% seq(2021,2029,1)) %>% 
#   filter(ssp == "ssp126")
# mean(a$productivity_mean)
# 
# b <- projection_all %>% 
#   filter(year %in% seq(2090,2099,1)) %>% 
#   filter(ssp == "ssp126")
# mean(b$productivity_mean)

summary(lm(productivity_mean~year, data = filter(productivity_summary, ssp == "ssp126")))
summary(lm(productivity_mean~year, data = filter(productivity_summary, ssp == "ssp245")))
summary(lm(productivity_mean~year, data = filter(productivity_summary, ssp == "ssp585")))

# global figure, use IPSL
f_global <- ggplot(filter(productivity_summary, ESM == "NorESM2-LM")) +
  geom_vline(xintercept = seq(2020, 2100, 10), linetype = "dotted", color = "gray50", linewidth = 0.25) +
  geom_ribbon(aes(x = year, ymin = productivity_2.5, ymax = productivity_97.5, fill = ssp), alpha = 0.1, show.legend = F) +
  geom_line(aes(x = year, y = productivity_mean, color = ssp), show.legend = F, linewidth = 0.5) +
  geom_smooth(aes(x = year, y = productivity_mean, color = ssp), method = "lm", show.legend = F, se = FALSE, linetype = "dashed", linewidth = 0.5) +
  # geom_line(aes(x = year, y = productivity_25, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
  # geom_line(aes(x = year, y = productivity_75, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
  scale_color_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[1], "ssp585" = mypal[2])) +
  scale_fill_manual(values = c("ssp126" = mypal[3], "ssp245" = mypal[1], "ssp585" = mypal[2])) +
  annotate("text",
    x = 2022, y = Inf, label = "Global",
    vjust = 1, hjust = 0, color = "black", family = "Arial", fontface = 1
  ) +
  annotate("text",
    x = 2030, y = Inf, label = "N = 614",
    vjust = 1, hjust = 0, color = "black", family = "Arial", fontface = 1, size = 3
  ) +
  annotate("text",
    x = seq(2030, 2090, 10), y = -Inf, label = seq(2030, 2090, 10),
    vjust = -0.5, hjust = 0.5, color = "gray50", family = "Arial", fontface = 1, size = 3
  ) +
  scale_x_continuous("Year", limits = c(2020, 2100), expand = c(0, 0)) +
  scale_y_continuous("Productivity",
    expand = c(0, 0), n.breaks = 4,
    labels = label_number(accuracy = 0.001)
  ) + 
  mytheme


# 2 Productivity projection by FAO area ----------------------------------
#read data
projection_summary <- read_rds("Outputs/GLMM projection results/productivity_projection_summary_FAOarea.rds") %>% 
  filter(!primary_FAOarea %in% c("51", "58"))

# 2021-2100
projection_summary <- projection_summary %>% 
  filter(year %in% seq(2021,2100,1))
# projection_summary_2020s <- projection_summary %>%
#   filter(year %in% c(2020:2029)) %>%
#   group_by(ssp, primary_FAOarea) %>%
#   summarise(p_mean_2020s = mean(productivity_mean))
# 
# projection_summary_2050s <- projection_summary %>%
#   filter(year %in% c(2050:2059)) %>%
#   group_by(ssp, primary_FAOarea) %>%
#   summarise(p_mean_2050s = mean(productivity_mean))
# 
# projection_summary_2090s <- projection_summary %>%
#   filter(year %in% c(2090:2099)) %>%
#   group_by(ssp, primary_FAOarea) %>%
#   summarise(p_mean_2090s = mean(productivity_mean))
# 
# projection_summary_change <- left_join(projection_summary_2020s, projection_summary_2050s) %>%
#   left_join(projection_summary_2090s) %>%
#   mutate(change1 = (p_mean_2050s - p_mean_2020s)/p_mean_2020s,
#          change2 = (p_mean_2090s - p_mean_2020s)/p_mean_2020s)
# 
# write.csv(projection_summary_change,"projection_summary_change.csv")

# FAO area figure, use IPSL
#loop
p <- NULL
for (i in unique(projection_summary$primary_FAOarea)) {
  
  # i="27"
  projection_summary_loop <- projection_summary %>% 
    filter(primary_FAOarea==i,
           ESM == "NorESM2-LM") 
  # pivot_longer(-c(1,2,3), names_to = "quantile", values_to = "productivity")
  
  a <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp126")))
  b <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp245")))
  c <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp585")))
  
  p_loop <- data.frame(area = i, 
                       p_ssp1 = a$coefficients[2,4], 
                       p_ssp2 = b$coefficients[2,4],
                       p_ssp5 = c$coefficients[2,4])
  p <- bind_rows(p, p_loop)
  # projection_summary_loop <- projection_summary_loop %>% 
  #   mutate(quantile = factor(quantile, levels = c("productivity_75", "productivity_median", "productivity_25"))) %>% 
  #   filter(year %in% c(2021:2100))
  # 
  # productivity_labs <- c("Q3", "Q2", "Q1")
  # names(productivity_labs) <- c("productivity_75", "productivity_median", "productivity_25")
  stock_number <- projection_summary_loop$total_stock[1]
  
  #plot
  f_summary <- ggplot(projection_summary_loop)+
    geom_vline(xintercept = seq(2020,2100,10),linetype="dotted",color="gray50",linewidth=0.25)+
    geom_ribbon(aes(x = year, ymin = productivity_2.5,  ymax = productivity_97.5, fill = ssp), alpha = 0.1, show.legend = F)+
    geom_line(aes(x = year, y = productivity_mean, color = ssp), show.legend = F, linewidth=0.25)+
    geom_smooth(aes(x = year, y = productivity_mean, color = ssp), method = "lm", show.legend = F, se = FALSE, linetype = "dashed", linewidth = 0.5)+
    # geom_line(aes(x = year, y = productivity_25, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
    # geom_line(aes(x = year, y = productivity_75, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
    scale_color_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[1], "ssp585"=mypal[2]))+
    scale_fill_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[1], "ssp585"=mypal[2]))+
    annotate("text",x = 2022,y=Inf,label=i,
             vjust=1,hjust=0,color="black",family="Arial",fontface=1)+
    annotate("text",x = 2030,y=Inf,label=paste0("N = ", stock_number),
             vjust=1,hjust=0,color="black",family="Arial",fontface=1,size = 3)+
    # facet_grid(quantile~., scales = "free_y", labeller = labeller(quantile = productivity_labs))+
    # ggtitle(label = paste0("FAO area ",i))+
    scale_x_continuous("Year", limits = c(2020,2100),expand = c(0,0))+
    scale_y_continuous("Productivity", expand = c(0,0), n.breaks = 4,
                       labels = label_number(accuracy = 0.01)) +
    mytheme
  
  assign(paste0("f_summary_",i),f_summary)
  
}

#layout
f_null <- ggplot()+
  theme(axis.line = element_blank())

f_top <- ggarrange(f_summary_67,f_summary_21,f_summary_31,f_summary_34,f_summary_27,f_summary_37,nrow = 1)
f_middle1 <- ggarrange(f_summary_77,f_null,f_null,f_null,f_null,f_summary_61,nrow = 1)
f_middle2 <- ggarrange(f_summary_81,f_null,f_null,f_null,f_null,f_summary_71,nrow = 1)
f_bottom <- ggarrange(f_summary_87,f_summary_41,f_summary_47,f_summary_57,f_null,nrow = 1,widths = c(1,1,1,1,2))
f <- ggarrange(f_top,f_middle1,f_middle2,f_bottom,nrow = 4)

g <- ggplotGrob(f_global)
f_final <- f+annotation_custom(g,xmin=0.165,xmax=0.835,ymin=0.25,ymax=0.75)

ggsave("Figures/GLMM_projection_Nor.PDF",device = cairo_pdf,width = 9,height = 6)



















