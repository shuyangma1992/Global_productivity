library(tidyverse)
library(ggdist)
library(ggsci)
library(ggpubr)
library(scales)
library(rnaturalearth)
library(sf)
library(broom)
library(RColorBrewer)

#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"),C=windowsFont("Arial"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_aaas()(10))
show_col(pal_d3()(10))
show_col(pal_lancet()(9))
show_col(pal_npg()(10))
# show_col(pal_npg()(10))
mypal <- pal_npg()(10)


# 1 Model comparison ------------------------------------------------------
glmm_information <- read_rds("GLMM results/glmm_information.rds") %>% 
  mutate(model = paste0("Model ", model)) %>% 
  pivot_longer(c(AIC,BIC), names_to = "information criterion", values_to = "value")

fa <- ggplot(glmm_information)+
  stat_dotsinterval(aes(x = model, y = value, slab_fill = `information criterion`),
                    position = "dodge", show.legend = T, slab_color=NA)+
  scale_color_manual(values = c("AIC" = alpha(mypal[1], 0.5), "BIC" = alpha(mypal[2], 0.5)),
                     aesthetics = "slab_fill")+
  scale_x_discrete("Model")+
  scale_y_continuous("Information Criteria value")+
  theme(axis.text = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        legend.position = c(0.75,0.75),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_text(family="Calibri"),
        legend.text = element_text(family="Calibri"),
        legend.key.height= unit(0.25,'cm'))

ggsave("Figures/AIC BIC.pdf", device = cairo_pdf, width = 5, height = 3)


# 2 Fixed effects ---------------------------------------------------------
fixed_effects <- read_rds("GLMM results/fixed_effects.rds") %>% 
  filter(term %in% c("temperature","chlorophyll","mixedlayerthickness"))

#median and 95% confidence interval
fixed_effects_summary <- fixed_effects %>% 
  group_by(term) %>% 
  summarise(median = median(estimate),
            Q2.5 = quantile(estimate, 0.025),
            Q17 = quantile(estimate, 0.17),
            Q83 = quantile(estimate, 0.83),
            Q97.5 = quantile(estimate, 0.975)) %>% 
  mutate(term = factor(term, levels = c("temperature","chlorophyll","mixedlayerthickness")))

#temperature and mixlayerthickness *100
fixed_effects_summary[2:3,2:6] <- fixed_effects_summary[2:3,2:6]*100

#plot
f_fixed_effects <- ggplot(fixed_effects_summary)+
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(x = term, ymin = Q2.5, ymax = Q97.5, color = term), width = 0,  show.legend = F)+
  geom_errorbar(aes(x = term, ymin = Q17, ymax = Q83, color = term), width = 0,  show.legend = F, linewidth = 1)+
  geom_point(aes(x = term, y = median, color= term), show.legend = F)+
  scale_color_manual(values = c("temperature" = mypal[2], "chlorophyll" = mypal[3], "mixedlayerthickness" = mypal[1]))+
  scale_x_discrete("Variable", labels = c("T (x100)","CHL","MLD (x100)"))+
  scale_y_continuous("Slope")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.text = element_text(family="Calibri"),
        axis.title.x = element_blank())

# 3 Random effects --------------------------------------------------------
random_effects <- read_rds("GLMM results/random_effects.rds") %>% 
  select(1,5,6) 

random_effects_summary <- random_effects %>% 
  group_by(scientificname) %>% 
  summarise(median_T = median(temperature_adjusted),
            low_T = quantile(temperature_adjusted, 0.025),
            high_T = quantile(temperature_adjusted, 0.975),
            median_CHL = median(chlorophyll_adjusted),
            low_CHL = quantile(chlorophyll_adjusted, 0.025),
            high_CHL = quantile(chlorophyll_adjusted, 0.975)) 


#significance 95% confidence interval does not include 0
random_effects_summary <- random_effects_summary %>% 
  mutate(significance_T = ifelse(median_T<0&high_T<0, 1, ifelse(median_T>0&low_T>0, 1, 0.15)),
         significance_CHL = ifelse(median_CHL<0&high_CHL<0, 1, ifelse(median_CHL>0&low_CHL>0, 1, 0.15)))

#T figure
random_effects_summary_T <- random_effects_summary %>% 
  select(1,2,3,4,8) %>% 
  arrange(median_T) %>% 
  mutate(scientificname = factor(scientificname, levels = scientificname))

a <- filter(random_effects_summary_T,significance_T==1&median_T<0) #47 species negative effects
b <- filter(random_effects_summary_T,significance_T==1&median_T>0) #66 species positive effects

f_random_effects_T <- ggplot(random_effects_summary_T)+
  geom_vline(xintercept = 0)+
  geom_point(aes(x = median_T, y = scientificname), color = mypal[2], alpha = random_effects_summary_T$significance_T, size = 0.25)+
  geom_errorbar(aes(xmin = low_T, xmax = high_T, y = scientificname), width = 0, color = mypal[2], linewidth = 0.1,
                alpha = random_effects_summary_T$significance_T)+
  annotate("text", label = "Negative\neffects\nn = 47", family = "Calibri", x = -Inf, y = -Inf, hjust = 0, vjust = -1)+
  annotate("text", label = "Positive\neffects\nn = 66", family = "Calibri", x = Inf, y = Inf, hjust = 1, vjust = 1)+
  scale_x_continuous("Slope of T")+
  scale_y_discrete("Species (arranged by slope of T)")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#CHL figure
random_effects_summary_CHL <- random_effects_summary %>% 
  select(1,5,6,7,9) %>% 
  arrange(median_CHL) %>% 
  mutate(scientificname = factor(scientificname, levels = scientificname))

c <- filter(random_effects_summary_CHL,significance_CHL==1&median_CHL<0) #38 species negative effects
d <- filter(random_effects_summary_CHL,significance_CHL==1&median_CHL>0) #75 species positive effects

f_random_effects_CHL <- ggplot(random_effects_summary_CHL)+
  geom_vline(xintercept = 0)+
  geom_point(aes(x = median_CHL, y = scientificname), color = mypal[3], alpha = random_effects_summary_CHL$significance_CHL, size = 0.25)+
  geom_errorbar(aes(xmin = low_CHL, xmax = high_CHL, y = scientificname), width = 0, color = mypal[3], linewidth = 0.1,
                alpha = random_effects_summary_CHL$significance_CHL)+
  annotate("text", label = "Negative\neffects\nn = 38", family = "Calibri", x = -Inf, y = -Inf, hjust = 0, vjust = -1)+
  annotate("text", label = "Positive\neffects\nn = 75", family = "Calibri", x = Inf, y = Inf, hjust = 1, vjust = 1)+
  scale_x_continuous("Slope of CHL")+
  scale_y_discrete("Species (arranged by slope of CHL)")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())





a <- random_effects_summary_T %>% 
  filter(significance_T == 1)

b <- random_effects_summary_CHL %>% 
  filter(significance_CHL == 1)

c <- bind_rows(a,b)

unique(c$scientificname)






# 4 Figure combination ----------------------------------------------------

f <- ggarrange(f_fixed_effects, ggarrange(f_random_effects_T,f_random_effects_CHL,labels = c("b","c")),
               labels = c("a"),nrow = 2, heights = c(1,3), font.label = list(family = "Calibri")) 

ggsave("Figures/GLMM results.PDF",device = cairo_pdf,width = 5,height = 6)















# 5 Stock-specific effects map --------------------------------------------
stock_distribution <- read_rds("Data/stock_distribution_area_centroid.rds")

stock_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(scientificname,GRSF_uuid)

stock <- left_join(stock_distribution,stock_information) %>% 
  left_join(random_effects_summary_T) %>% 
  left_join(random_effects_summary_CHL)

#world data (rnaturalearth package)
# world <- ne_countries(scale = "small", returnclass = "sf")

world <- map_data("world")

#plot T map
T_map <- ggplot()+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(data = stock, aes(x = centroid_lon, y = centroid_lat, color = median_T*100), size = 0.75)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        axis.ticks = element_blank(),
        legend.position = c(0.15,0.30),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, "cm"))

T_x <- ggplot(stock)+
  geom_point(aes(x = centroid_lon, y = median_T*100, color = median_T*100), show.legend = F, size = 0.75)+
  geom_smooth(aes(x = centroid_lon, y = median_T*100), 
              method = "loess", color = "black", fill = "gray", linewidth = 0.5)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Slope of T (x100)")+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        legend.position = c(0.1,0.30),
        legend.background = element_blank())

T_y <- ggplot(stock)+
  geom_point(aes(x = centroid_lat, y = median_T*100, color = median_T*100), show.legend = F, size = 0.75)+
  geom_smooth(aes(x = centroid_lat, y = median_T*100), 
              method = "loess", color = "black", fill = "gray", linewidth = 0.5)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  scale_y_continuous("Slope of T (x100)")+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        legend.position = c(0.1,0.30),
        legend.background = element_blank())+
  coord_flip()

#combine
T_null <- NULL
T_f1 <- ggarrange(T_map, T_y, widths = c(2.5,1), align = "hv",labels = c("a","b"))
T_f2 <- ggarrange(T_x, T_null, widths = c(2.5,1), align = "hv",labels = c("c"))
T_f <- ggarrange(T_f1, T_f2, heights = c(1.5,1), ncol = 1, align = "hv")

ggsave("Figures/GLMM results T map.PDF",device = cairo_pdf, width = 6,height = 4)

summary(lm(median_T~centroid_lon, data = stock))
summary(lm(median_T~centroid_lat, data = stock))

#plot CHL world map
CHL_map <- ggplot()+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(data = stock, aes(x = centroid_lon, y = centroid_lat, color = median_CHL), size = 0.75)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        axis.ticks = element_blank(),
        legend.position = c(0.15,0.30),
        legend.background = element_blank(),
        legend.direction = "horizontal",
        legend.key.height = unit(0.5, "cm"))

CHL_x <- ggplot(stock)+
  geom_point(aes(x = centroid_lon, y = median_CHL, color = median_CHL), show.legend = F, size = 0.75)+
  geom_smooth(aes(x = centroid_lon, y = median_CHL), 
              method = "loess", color = "black", fill = "gray", linewidth = 0.5)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Slope of CHL")+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        legend.position = c(0.1,0.30),
        legend.background = element_blank())

CHL_y <- ggplot(stock)+
  geom_point(aes(x = centroid_lat, y = median_CHL, color = median_CHL), show.legend = F, size = 0.75)+
  geom_smooth(aes(x = centroid_lat, y = median_CHL), 
              method = "loess", color = "black", fill = "gray", linewidth = 0.5)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  scale_x_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  scale_y_continuous("Slope of CHL")+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        legend.position = c(0.1,0.30),
        legend.background = element_blank())+
  coord_flip()

#combine
CHL_null <- NULL
CHL_f1 <- ggarrange(CHL_map, CHL_y, widths = c(2.5,1), align = "hv",labels = c("a","b"))
CHL_f2 <- ggarrange(CHL_x, CHL_null, widths = c(2.5,1), align = "hv",labels = c("c"))
CHL_f <- ggarrange(CHL_f1, CHL_f2, heights = c(1.5,1), ncol = 1, align = "hv")

ggsave("Figures/GLMM results CHL map.PDF",device = cairo_pdf, width = 6,height = 4)

summary(lm(median_CHL~centroid_lon, data = stock))
summary(lm(median_CHL~centroid_lat, data = stock))

# 6 Stock-specific effects and area (mianji) relationship --------------------------------------------
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
