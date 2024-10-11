library(tidyverse)
library(ggdist)
library(ggsci)
library(ggpubr)
library(scales)
library(ggpattern)
library(terra)
library(rgdal)
library(tidyterra)
library(ggthemes)
require(rgdal)
library(sf)
library(rnaturalearth)
library(ggnewscale)
library(viridis)

#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"),C=windowsFont("Arial"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_aaas()(10))
show_col(pal_npg()(9))
show_col(pal_lancet()(9))
# show_col(pal_npg()(10))
mypal <- pal_npg()(9)

# 0 world map -------------------------------------------------------------------
#data information
FAO_shp <- vect("Data/FAO_AREAS_CWP/FAO_AREAS_CWP.shp")

#attributes
FAO_shp_attribute <- as.data.frame(FAO_shp)

#geometry
FAO_shp_geometry <- geom(FAO_shp)

#major area
FAO_shp_major <- FAO_shp %>% 
  filter(F_LEVEL=="MAJOR")

#world data (ggplot2 package)
# world <- map_data("world")

#world data (rnaturalearth package)
world <- ne_countries(scale = "small", returnclass = "sf")

#plot world map
map <- ggplot(data = world)+
  # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  geom_sf(fill="black",color="black")+
  geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),color="black",family="Calibri",fontface=2,size=4)+
  coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1))

# ggsave("Figures/map.png",width = 8,height = 6)
# ggsave("Figures/map.PDF",device = cairo_pdf,width = 8,height = 6)


# 0 stock group information -----------------------------------------------
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname,primary_FAOarea)
unique(stock_success$stockid) #725 stocks
unique(stock_success$scientificname) #290 species
unique(stock_success$primary_FAOarea) #16 FAO areas



# 1 Productivity projection trend -------------------------------------------
#SSP1-2.6
# read data
projection_trend_ssp126 <- read_rds("Data/productivity_projection_trend_ssp126_2021_2100.rds")
projection_trend_ssp126 <- left_join(projection_trend_ssp126,
                                     stock_success)
projection_trend_ssp126 %>% filter(ESM == "NorESM2-LM")
projection_trend_ssp126 %>% filter(ESM == "MPI-ESM1-2-LR")
projection_trend_ssp126 %>% filter(ESM == "IPSL-CM6A-LR")

#total number
projection_total_number_ssp126 <- projection_trend_ssp126 %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(total_number=n()) %>% 
  ungroup()

#positive trend
projection_positive_trend_number_ssp126 <- projection_trend_ssp126 %>% 
  filter(p_slope_lci>0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(positive_trend_number=n()) %>% 
  ungroup()

#negative trend
projection_negative_trend_number_ssp126 <- projection_trend_ssp126 %>% 
  filter(p_slope_uci<0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(negative_trend_number=n()) %>% 
  ungroup()

#combine data
projection_trend_number_ssp126 <- left_join(projection_total_number_ssp126,
                                            projection_positive_trend_number_ssp126) %>% 
  left_join(projection_negative_trend_number_ssp126)

#change NA to 0
projection_trend_number_ssp126 <- replace_na(projection_trend_number_ssp126,
                                             list(positive_trend_number = 0,
                                                  negative_trend_number = 0))

#calculate proportion
projection_trend_number_ssp126 <- projection_trend_number_ssp126 %>% 
  filter(!primary_FAOarea %in% c("51","58")) %>% 
  mutate(positive_trend_proportion=positive_trend_number/total_number,
         negative_trend_proportion=negative_trend_number/total_number,
         no_trend_proportion=(total_number-positive_trend_number-negative_trend_number)/total_number)

#select
projection_trend_number_ssp126 <- projection_trend_number_ssp126 %>% 
  mutate(ssp = "SSP1-2.6")

#SSP2-4.5
# read data
projection_trend_ssp245 <- read_rds("Data/productivity_projection_trend_ssp245_2021_2100.rds")
projection_trend_ssp245 <- left_join(projection_trend_ssp245,
                                     stock_success)

#total number
projection_total_number_ssp245 <- projection_trend_ssp245 %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(total_number=n()) %>% 
  ungroup()

#positive trend
projection_positive_trend_number_ssp245 <- projection_trend_ssp245 %>% 
  filter(p_slope_lci>0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(positive_trend_number=n()) %>% 
  ungroup()

#negative trend
projection_negative_trend_number_ssp245 <- projection_trend_ssp245 %>% 
  filter(p_slope_uci<0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(negative_trend_number=n()) %>% 
  ungroup()

#combine data
projection_trend_number_ssp245 <- left_join(projection_total_number_ssp245,
                                            projection_positive_trend_number_ssp245) %>% 
  left_join(projection_negative_trend_number_ssp245)

#change NA to 0
projection_trend_number_ssp245 <- replace_na(projection_trend_number_ssp245,
                                             list(positive_trend_number = 0,
                                                  negative_trend_number = 0))

#calculate proportion
projection_trend_number_ssp245 <- projection_trend_number_ssp245 %>% 
  filter(!primary_FAOarea %in% c("51","58")) %>% 
  mutate(positive_trend_proportion=positive_trend_number/total_number,
         negative_trend_proportion=negative_trend_number/total_number,
         no_trend_proportion=(total_number-positive_trend_number-negative_trend_number)/total_number)

#select
projection_trend_number_ssp245 <- projection_trend_number_ssp245 %>% 
  mutate(ssp = "SSP2-4.5")


#SSP5-8.5
# read data
projection_trend_ssp585 <- read_rds("Data/productivity_projection_trend_ssp585_2021_2100.rds")
projection_trend_ssp585 <- left_join(projection_trend_ssp585,
                                     stock_success)

#total number
projection_total_number_ssp585 <- projection_trend_ssp585 %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(total_number=n()) %>% 
  ungroup()

#positive trend
projection_positive_trend_number_ssp585 <- projection_trend_ssp585 %>% 
  filter(p_slope_lci>0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(positive_trend_number=n()) %>% 
  ungroup()

#negative trend
projection_negative_trend_number_ssp585 <- projection_trend_ssp585 %>% 
  filter(p_slope_uci<0) %>% 
  group_by(primary_FAOarea,ESM) %>% 
  summarize(negative_trend_number=n()) %>% 
  ungroup()

#combine data
projection_trend_number_ssp585 <- left_join(projection_total_number_ssp585,
                                            projection_positive_trend_number_ssp585) %>% 
  left_join(projection_negative_trend_number_ssp585)

#change NA to 0
projection_trend_number_ssp585 <- replace_na(projection_trend_number_ssp585,
                                             list(positive_trend_number = 0,
                                                  negative_trend_number = 0))

#calculate proportion
projection_trend_number_ssp585 <- projection_trend_number_ssp585 %>% 
  filter(!primary_FAOarea %in% c("51","58")) %>% 
  mutate(positive_trend_proportion=positive_trend_number/total_number,
         negative_trend_proportion=negative_trend_number/total_number,
         no_trend_proportion=(total_number-positive_trend_number-negative_trend_number)/total_number)

#select
projection_trend_number_ssp585 <- projection_trend_number_ssp585 %>% 
  mutate(ssp = "SSP5-8.5")

#combine data
projection_trend_number <- bind_rows(projection_trend_number_ssp126,
                                     projection_trend_number_ssp245,
                                     projection_trend_number_ssp585)

#global situation
projection_trend_number_global <- projection_trend_number %>% 
  group_by(ESM,ssp) %>% 
  summarise(total_number=sum(total_number),
            positive_trend_number=sum(positive_trend_number),
            negative_trend_number=sum(negative_trend_number)) %>% 
  ungroup()

projection_trend_number_global <- projection_trend_number_global %>% 
  mutate(positive_trend_proportion=positive_trend_number/total_number,
         negative_trend_proportion=negative_trend_number/total_number,
         no_trend_proportion=(total_number-positive_trend_number-negative_trend_number)/total_number)

projection_trend_number_global <- projection_trend_number_global %>% 
  pivot_longer(-c(ESM,ssp,total_number,positive_trend_number,negative_trend_number),names_to = "trend", values_to = "proportion") %>% 
  mutate(x_label = paste(ESM, ssp)) %>% 
  mutate(trend = factor(trend, levels = c("positive_trend_proportion",
                                          "no_trend_proportion",
                                          "negative_trend_proportion")))

f_global_trend <- ggplot(projection_trend_number_global)+
  # geom_bar(aes(x = x_label, y = proportion, fill = trend), stat="identity", position="fill")+
  geom_bar_pattern(aes(x = x_label, y = proportion, fill = trend, pattern = ssp), 
                   stat="identity", position="fill", color = "black", show.legend = F, 
                   linewidth = 0.25, pattern_density = 0.1)+
  # ggtitle(label = paste0("FAO area ",i),
  #         subtitle = paste0("Total stock: ",total_stock_number))+
  annotate("text",x = 0.75,y=Inf,label="Global",
           vjust=1.25,hjust=0,color="yellow",family="Calibri",fontface=2)+
  annotate("text",x = 0.75,y=Inf,label="Total stock number: 623",
           vjust=3,hjust=0,color="yellow",family="Calibri",fontface=2,size=3)+
  scale_fill_manual(values = c("positive_trend_proportion"=mypal[3],
                               "negative_trend_proportion"=mypal[1],
                               "no_trend_proportion"="grey"))+
  scale_pattern_manual(values = c("SSP1-2.6" = "none","SSP2-4.5" = "circle","SSP5-8.5" = "stripe"))+
  scale_x_discrete("Earth system model (ESM)", expand = c(0,0),labels = c("","IPSL","",
                                                                          "","MPI","",
                                                                          "","NOR",""))+
  scale_y_continuous("Proportion", expand = c(0,0), breaks = c(0.25, 0.50, 0.75, 1.00))+
  theme(plot.title = element_text(family = "Calibri", face = 2, size = 8, color = mypal[2]),
        plot.subtitle = element_text(family = "Calibri", size = 6),
        axis.text = element_text(family = "Calibri"),
        axis.text.x = element_text(margin = margin(t=-1.5)),
        axis.text.y = element_text(angle = 90, margin = margin(r=-1.5)),
        axis.title = element_blank(),
        axis.ticks = element_blank())


#loop
for (i in unique(projection_trend_number$primary_FAOarea)) {
  
  # i="34"
  projection_trend_loop <- projection_trend_number %>% 
    filter(primary_FAOarea==i)
  
  total_stock_number <- projection_trend_loop$total_number[1]
  
  #change to long data
  projection_trend_loop <- projection_trend_loop %>% 
    pivot_longer(-c(primary_FAOarea,ESM,ssp,total_number,positive_trend_number,negative_trend_number),names_to = "trend", values_to = "proportion") %>% 
    mutate(x_label = paste(ESM, ssp)) %>% 
    mutate(trend = factor(trend, levels = c("positive_trend_proportion",
                                            "no_trend_proportion",
                                            "negative_trend_proportion")))
  
  #plot
  f_trend <- ggplot(projection_trend_loop)+
    # geom_bar(aes(x = x_label, y = proportion, fill = trend), stat="identity", position="fill")+
    geom_bar_pattern(aes(x = x_label, y = proportion, fill = trend, pattern = ssp), 
                     stat="identity", position="fill", color = "black", show.legend = F, 
                     linewidth = 0.25, pattern_density = 0.1)+
    # ggtitle(label = paste0("FAO area ",i),
    #         subtitle = paste0("Total stock: ",total_stock_number))+
    annotate("text",x = 0.75,y=Inf,label=paste0("FAO area ",i),
             vjust=1.25,hjust=0,color="yellow",family="Calibri",fontface=2)+
    annotate("text",x = 0.75,y=Inf,label=paste0("Total stock number: ",total_stock_number),
             vjust=3,hjust=0,color="yellow",family="Calibri",fontface=2,size=3)+
    scale_fill_manual(values = c("positive_trend_proportion"=mypal[3],
                                 "negative_trend_proportion"=mypal[1],
                                 "no_trend_proportion"="grey"))+
    scale_pattern_manual(values = c("SSP1-2.6" = "none","SSP2-4.5" = "circle","SSP5-8.5" = "stripe"))+
    scale_x_discrete("Earth system model (ESM)", expand = c(0,0),labels = c("","IPSL","",
                                                                            "","MPI","",
                                                                            "","NOR",""))+
    scale_y_continuous("Proportion", expand = c(0,0), breaks = c(0.25, 0.50, 0.75, 1.00))+
    theme(plot.title = element_text(family = "Calibri", face = 2, size = 8, color = mypal[2]),
          plot.subtitle = element_text(family = "Calibri", size = 6),
          axis.text = element_text(family = "Calibri"),
          axis.text.x = element_text(margin = margin(t=-1.5)),
          axis.text.y = element_text(angle = 90, margin = margin(r=-1.5)),
          axis.title = element_blank(),
          axis.ticks = element_blank())
 
  assign(paste0("f_trend_",i),f_trend)
  
}

#layout
f_null <- ggplot()+
  theme(axis.line = element_blank())

f_top <- ggarrange(f_trend_67,f_trend_21,f_trend_31,f_trend_34,f_trend_27,f_trend_37,nrow = 1)
f_middle1 <- ggarrange(f_trend_77,f_null,f_null,f_null,f_null,f_trend_61,nrow = 1)
f_middle2 <- ggarrange(f_trend_81,f_null,f_null,f_null,f_null,f_trend_71,nrow = 1)
f_bottom <- ggarrange(f_trend_87,f_trend_41,f_trend_47,f_trend_57,f_null,f_null,nrow = 1)
f <- ggarrange(f_top,f_middle1,f_middle2,f_bottom,nrow = 4)

g <- ggplotGrob(f_global_trend)
f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.6625,ymin=0.25,ymax=0.75)

ggsave("Figures/projection_trend.png",width = 9,height = 6)
ggsave("Figures/projection_trend.PDF",device = cairo_pdf,width = 9,height = 6)





# 2 Productivity projection summary global ESM aggregated --------------------------------
projection_IPSL <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")

projection_IPSL <- left_join(projection_IPSL,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1009) %>% 
  pivot_longer(-c(year,ssp,ESM,stockid), names_to = "number", values_to = "productivity") 

projection_MPI <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")

projection_MPI <- left_join(projection_MPI,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1009) %>% 
  pivot_longer(-c(year,ssp,ESM,stockid), names_to = "number", values_to = "productivity") 

projection_Nor <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM.rds")

projection_Nor <- left_join(projection_Nor,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1009) %>% 
  pivot_longer(-c(year,ssp,ESM,stockid), names_to = "number", values_to = "productivity") 

#combine
projection_all <- bind_rows(projection_IPSL, projection_MPI, projection_Nor)

projection_all <- projection_all %>% 
  group_by(year,ssp,number) %>% 
  summarise(productivity = mean(productivity, na.rm = T),
            total_stock = n())

projection_all <- projection_all %>% 
  group_by(year,ssp,total_stock) %>% 
  summarise(productivity_mean = mean(productivity, na.rm = T),
            productivity_sd = sd(productivity, na.rm = T),
            productivity_CV = sd(productivity, na.rm = T)/mean(productivity, na.rm = T),
            productivity_2.5 = quantile(productivity, 0.025, na.rm = T),
            productivity_25 = quantile(productivity, 0.25, na.rm = T),
            productivity_50 = quantile(productivity, 0.20, na.rm = T),
            productivity_75 = quantile(productivity, 0.75, na.rm = T),
            productivity_97.5 = quantile(productivity, 0.975, na.rm = T))

write_rds(projection_all,"GLMM projection results/productivity_projection_summary_global_ESM_aggregated.rds")


# 3 Productivity projection summary FAO area ESM aggregated----------------------------------
projection_IPSL <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")

projection_IPSL <- left_join(projection_IPSL,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1010) %>% 
  pivot_longer(-c(year,ssp,primary_FAOarea,ESM,stockid), names_to = "number", values_to = "productivity") 

projection_MPI <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")

projection_MPI <- left_join(projection_MPI,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1010) %>% 
  pivot_longer(-c(year,ssp,primary_FAOarea,ESM,stockid), names_to = "number", values_to = "productivity") 

projection_Nor <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM.rds")

projection_Nor <- left_join(projection_Nor,stock_success) %>% 
  select(year,ssp,ESM,stockid,10:1010) %>% 
  pivot_longer(-c(year,ssp,primary_FAOarea,ESM,stockid), names_to = "number", values_to = "productivity") 

#combine
projection_all <- bind_rows(projection_IPSL, projection_MPI, projection_Nor)

projection_all <- projection_all %>% 
  group_by(year,ssp,primary_FAOarea,number) %>% 
  summarise(productivity = mean(productivity, na.rm = T),
            total_stock = n())

projection_all <- projection_all %>% 
  group_by(year,ssp,primary_FAOarea,total_stock) %>% 
  summarise(productivity_mean = mean(productivity, na.rm = T),
            productivity_sd = sd(productivity, na.rm = T),
            productivity_CV = sd(productivity, na.rm = T)/mean(productivity, na.rm = T),
            productivity_2.5 = quantile(productivity, 0.025, na.rm = T),
            productivity_25 = quantile(productivity, 0.25, na.rm = T),
            productivity_50 = quantile(productivity, 0.20, na.rm = T),
            productivity_75 = quantile(productivity, 0.75, na.rm = T),
            productivity_97.5 = quantile(productivity, 0.975, na.rm = T))

write_rds(projection_all,"GLMM projection results/productivity_projection_summary_FAOarea_ESM_aggregated.rds")

# 4 Figure Productivity projection global  ---------------------------------
#read data
projection_all <- read_rds("GLMM projection results/productivity_projection_summary_global_ESM_aggregated.rds")

projection_all <- projection_all %>% 
  filter(year %in% seq(2021,2100,1))

a <- projection_all %>% 
  filter(year %in% seq(2021,2029,1)) %>% 
  filter(ssp == "ssp126")
mean(a$productivity_mean)

b <- projection_all %>% 
  filter(year %in% seq(2090,2099,1)) %>% 
  filter(ssp == "ssp126")
mean(b$productivity_mean)

summary(lm(productivity_mean~year, data = filter(projection_all, ssp == "ssp126")))
summary(lm(productivity_mean~year, data = filter(projection_all, ssp == "ssp245")))
summary(lm(productivity_mean~year, data = filter(projection_all, ssp == "ssp585")))

f_global <- ggplot(projection_all)+
  geom_vline(xintercept = seq(2020,2100,10),linetype="dotted",color="gray50",linewidth=0.25)+
  geom_ribbon(aes(x = year, ymin = productivity_25,  ymax = productivity_75, fill = ssp), alpha = 0.1, show.legend = F)+
  geom_line(aes(x = year, y = productivity_mean, color = ssp), show.legend = F, linewidth = 0.5)+
  geom_smooth(aes(x = year, y = productivity_mean, color = ssp), method = "lm", show.legend = F, se = FALSE, linetype = "dashed", linewidth = 0.5)+
  # geom_line(aes(x = year, y = productivity_25, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
  # geom_line(aes(x = year, y = productivity_75, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
  scale_color_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[2], "ssp585"=mypal[1]))+
  scale_fill_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[2], "ssp585"=mypal[1]))+
  annotate("text",x = 2022,y=Inf,label="Global",
           vjust=1,hjust=0,color="black",family="Calibri",fontface=1)+
  annotate("text",x = 2022,y=Inf,label="Number of stocks: 678",
           vjust=2.5,hjust=0,color="black",family="Calibri",fontface=1,size=3)+
  annotate("text",x = seq(2030,2090,10), y=-Inf,label=seq(2030,2090,10),
           vjust=-0.5,hjust=0.5,color="gray50",family="Calibri",fontface=1,size=3)+
  scale_x_continuous("Year", limits = c(2020,2100),expand = c(0,0))+
  scale_y_continuous("Productivity", expand = c(0,0), n.breaks = 4,
                     labels = label_number(accuracy = 0.001))+
  theme(plot.title = element_text(family = "Calibri", face = 2, size = 8, color = mypal[2]),
        plot.subtitle = element_text(family = "Calibri", size = 6),
        axis.text.y = element_text(family = "Calibri", angle = 90, hjust = 0.5),
        axis.text.x =  element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.length=unit(-0.1, "cm"),
        strip.background = element_blank(),
        strip.text = element_blank())


# 5 Figure Productivity projection by FAO area ----------------------------------
#read data
# projection_Nor_summary <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM_summary.rds")
# projection_MPI_summary <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR_summary.rds")
# projection_IPSL_summary <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR_summary.rds")

projection_summary <- read_rds("GLMM projection results/productivity_projection_summary_FAOarea_ESM_aggregated.rds") %>% 
  filter(!primary_FAOarea %in% c("51", "58"))

projection_summary <- projection_summary %>% 
  filter(year %in% seq(2021,2100,1))

#loop
p <- NULL
for (i in unique(projection_summary$primary_FAOarea)) {
  
  # i="27"
  projection_summary_loop <- projection_summary %>% 
    filter(primary_FAOarea==i) 
    # pivot_longer(-c(1,2,3), names_to = "quantile", values_to = "productivity")
  
  a <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp126")))
  b <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp245")))
  c <- summary(lm(productivity_mean~year, data = filter(projection_summary_loop, ssp == "ssp585")))
  
  p_loop <- data.frame(area = i, 
                       p_ssp1 = a$coefficients[2,4], 
                       p_ssp2 = b$coefficients[2,4],
                       p_ssp5 = c$coefficients[2,4])
  p <- bind_rows(p, p_loop)
 
  stock_number <- projection_summary_loop$stock_number[1]
  #plot
  f_summary <- ggplot(projection_summary_loop)+
    geom_vline(xintercept = seq(2020,2100,10),linetype="dotted",color="gray50",linewidth=0.25)+
    geom_ribbon(aes(x = year, ymin = productivity_25,  ymax = productivity_75, fill = ssp), alpha = 0.1, show.legend = F)+
    geom_line(aes(x = year, y = productivity_mean, color = ssp), show.legend = F, linewidth=0.25)+
    geom_smooth(aes(x = year, y = productivity_mean, color = ssp), method = "lm", show.legend = F, se = FALSE, linetype = "dashed", linewidth = 0.5)+
    # geom_line(aes(x = year, y = productivity_25, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
    # geom_line(aes(x = year, y = productivity_75, color = ssp), show.legend = F, linewidth = 0.5, linetype = "dashed", alpha = 0.5)+
    scale_color_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[2], "ssp585"=mypal[1]))+
    scale_fill_manual(values = c("ssp126"=mypal[3], "ssp245"=mypal[2], "ssp585"=mypal[1]))+
    annotate("text",x = 2022,y=Inf,label=paste0("FAO area ",i),
             vjust=1,hjust=0,color="black",family="Calibri",fontface=1)+
    annotate("text",x = 2022,y=Inf,label=paste0("Number of stocks: ", stock_number),
             vjust=2.5,hjust=0,color="black",family="Calibri",fontface=1,size = 3)+
    # facet_grid(quantile~., scales = "free_y", labeller = labeller(quantile = productivity_labs))+
    # ggtitle(label = paste0("FAO area ",i))+
    scale_x_continuous("Year", limits = c(2020,2100),expand = c(0,0))+
    scale_y_continuous("Productivity", expand = c(0,0), n.breaks = 4,
                       labels = label_number(accuracy = 0.001))+
    theme(plot.title = element_text(family = "Calibri", face = 2, size = 8, color = mypal[2]),
          plot.subtitle = element_text(family = "Calibri", size = 6),
          axis.text.y = element_text(family = "Calibri", angle = 90, hjust=0.5),
          axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.length=unit(-0.1, "cm"),
          strip.background = element_blank(),
          strip.text = element_blank())
  
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
f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.825,ymin=0.25,ymax=0.75)

ggsave("Figures/projection_summary.png",width = 9,height = 6)
ggsave("Figures/projection_summary.PDF",device = cairo_pdf,width = 9,height = 6)



# 6 Productivity projection winner loser ----------------------------------
projection_IPSL <- read_rds("GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")

projection_IPSL <- left_join(projection_IPSL,stock_success) %>% 
  select(year,ssp,ESM,stockid,primary_FAOarea,10:1009,scientificname) 

projection_MPI <- read_rds("GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")

projection_MPI <- left_join(projection_MPI,stock_success) %>% 
  select(year,ssp,ESM,stockid,primary_FAOarea,10:1009,scientificname) 

projection_Nor <- read_rds("GLMM projection results/productivity_projection_NorESM2-LM.rds")

projection_Nor <- left_join(projection_Nor,stock_success) %>% 
  select(year,ssp,ESM,stockid,primary_FAOarea,10:1009,scientificname) 

#combine all projection data
projection_all <- bind_rows(projection_IPSL, projection_MPI, projection_Nor)

projection_all <- projection_all %>% 
  drop_na()

unique(projection_all$stockid) # 678 stocks
unique(projection_all$scientificname) # 272 species

#projection 2020s
projection_all_2020s <- projection_all %>% 
  filter(year %in% seq(2020,2029,1))

#projection 2050s
projection_all_2050s <- projection_all %>% 
  filter(year %in% seq(2050,2059,1))

#projection 2090s
projection_all_2090s <- projection_all %>% 
  filter(year %in% seq(2090,2099,1))

#productivity change 2050s-2020s
projection_all_50s_20s <- projection_all_2050s[,6:1005]-projection_all_2020s[,6:1005]
projection_all_50s_20s[,1001:1005] <- projection_all_2020s[,1:5]
projection_all_50s_20s <- projection_all_50s_20s %>% 
  pivot_longer(-c(year, ssp, ESM, stockid, primary_FAOarea), names_to = "number", values_to = "productivity")

projection_all_50s_20s <- projection_all_50s_20s %>% 
  group_by(stockid, ssp) %>% 
  summarize(p_change_mean = mean(productivity, na.rm= T),
            p_change_2.5 = quantile(productivity, 0.025, na.rm= T),
            p_change_25 = quantile(productivity, 0.25, na.rm= T),
            p_change_50 = quantile(productivity, 0.50, na.rm= T),
            p_change_75 = quantile(productivity, 0.75, na.rm= T),
            p_change_97.5 = quantile(productivity, 0.975, na.rm= T))

write_rds(projection_all_50s_20s,"GLMM projection results/projection_change_allESM_50s_20s.rds")


#productivity change 2090s-2020s
projection_all_90s_20s <- projection_all_2090s[,6:1005]-projection_all_2020s[,6:1005]
projection_all_90s_20s[,1001:1005] <- projection_all_2020s[,1:5]

projection_all_90s_20s <- projection_all_90s_20s %>% 
  pivot_longer(-c(year, ssp, ESM, stockid, primary_FAOarea), names_to = "number", values_to = "productivity")

projection_all_90s_20s <- projection_all_90s_20s %>% 
  group_by(stockid, ssp) %>% 
  summarize(p_change_mean = mean(productivity, na.rm= T),
            p_change_2.5 = quantile(productivity, 0.025, na.rm= T),
            p_change_25 = quantile(productivity, 0.25, na.rm= T),
            p_change_50 = quantile(productivity, 0.50, na.rm= T),
            p_change_75 = quantile(productivity, 0.75, na.rm= T),
            p_change_97.5 = quantile(productivity, 0.975, na.rm= T))

write_rds(projection_all_90s_20s,"GLMM projection results/projection_change_allESM_90s_20s.rds")



# 7 Figure productivity winner loser --------------------------------------
#2050s - 2020s
productivity_change_50s_20s <- read_rds("GLMM projection results/projection_change_allESM_50s_20s.rds") 
productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  drop_na()
unique(productivity_change_50s_20s$stockid) #678 stocks

#order ssp585 from low to high
order <- productivity_change_50s_20s %>% 
  filter(ssp == "ssp585") %>% 
  arrange(p_change_mean)

order <- as.character(order$stockid)

#stockid order
productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  mutate(stockid = factor(stockid, levels = order))

f_50s_20s <- ggplot(productivity_change_50s_20s)+
  geom_vline(xintercept = 0)+
  geom_point(aes(x = p_change_mean, y = stockid, color = ssp), show.legend = F, size = 0.5)+
  scale_color_manual(values = c("ssp126" = mypal[3],
                                "ssp245" = mypal[2],
                                "ssp585" = mypal[1]))+
  new_scale_color() +
  geom_errorbar(aes(x = p_change_mean, y = stockid, color = ssp, xmin = p_change_25, xmax = p_change_75),
                width = 0, show.legend = F, linewidth = 0.25)+
  scale_color_manual(values = c("ssp126" = alpha(mypal[3], 0.25),
                                "ssp245" = alpha(mypal[2], 0.25),
                                "ssp585" = alpha(mypal[1], 0.25)))+
  scale_x_continuous("Productivity change", limits=c(-0.15,0.15), expand = c(0,0))+
  scale_y_discrete("Stock", expand = c(0,0))+
  theme(axis.text.x = element_text(family = "Calibri"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

#2090s - 2020s
productivity_change_90s_20s <- read_rds("GLMM projection results/projection_change_allESM_90s_20s.rds") 

productivity_change_90s_20s %>% 
  filter(stockid %in% c("BGRDRNSWWA", "HOKIENZ", "HOKIWNZ"))

numbers <- productivity_change_90s_20s %>%
  mutate(significance = ifelse(p_change_mean>0&p_change_25>0,
                               1,
                               ifelse(p_change_mean<0&p_change_75<0,
                                      -1,
                                      0)))

numbers <- numbers %>%
  ungroup() %>%
  select(-1) %>%
  count(ssp, significance)

#order ssp585 from low to high
order <- productivity_change_90s_20s %>% 
  filter(ssp == "ssp585") %>% 
  arrange(p_change_mean)

order <- as.character(order$stockid)

#stockid order
productivity_change_90s_20s <- productivity_change_90s_20s %>% 
  mutate(stockid = factor(stockid, levels = order))

f_90s_20s <- ggplot(productivity_change_90s_20s)+
  geom_vline(xintercept = 0)+
  geom_point(aes(x = p_change_mean, y = stockid, color = ssp), show.legend = F, size = 0.5)+
  scale_color_manual(values = c("ssp126" = mypal[3],
                                "ssp245" = mypal[2],
                                "ssp585" = mypal[1]))+
  new_scale_color() +
  geom_errorbar(aes(x = p_change_mean, y = stockid, color = ssp, xmin = p_change_25, xmax = p_change_75),
                width = 0, show.legend = F, linewidth = 0.25)+
  scale_color_manual(values = c("ssp126" = alpha(mypal[3], 0.25),
                                "ssp245" = alpha(mypal[2], 0.25),
                                "ssp585" = alpha(mypal[1], 0.25)))+
  scale_x_continuous("Productivity change", limits=c(-0.15,0.15), expand = c(0,0))+
  scale_y_discrete("Stock", expand = c(0,0))+
  theme(axis.text.x = element_text(family = "Calibri"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

f <- ggarrange(f_50s_20s,f_90s_20s,nrow = 1)

ggsave("Figures/projection_winner_loser.png",width = 6,height = 6)
ggsave("Figures/projection_winner_loser.PDF",device = cairo_pdf,width = 6,height = 6)

# 8 Figure winner loser distribution --------------------------------------
#stock distribution centroid
stock_distribution <- read_rds("Data/stock_distribution_area_centroid.rds")
stock_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,GRSF_uuid)
stock_distribution <- left_join(stock_distribution,stock_information)

#2050s - 2020s
productivity_change_50s_20s <- read_rds("GLMM projection results/projection_change_allESM_50s_20s.rds") 
productivity_change_50s_20s <- productivity_change_50s_20s %>% 
  drop_na()
unique(productivity_change_50s_20s$stockid) #678 stocks
productivity_change_50s_20s <- left_join(productivity_change_50s_20s, stock_distribution) %>% 
  mutate(reference = "2050s - 2020s")

#2090s - 2020s
productivity_change_90s_20s <- read_rds("GLMM projection results/projection_change_allESM_90s_20s.rds") 
productivity_change_90s_20s <- productivity_change_90s_20s %>% 
  drop_na()
unique(productivity_change_90s_20s$stockid)
productivity_change_90s_20s <- left_join(productivity_change_90s_20s, stock_distribution) %>% 
  mutate(reference = "2090s - 2020s")

productivity_change <- bind_rows(productivity_change_50s_20s, productivity_change_90s_20s)

#world map
world <- map_data("world")

#plot productivity map_a
data_a <- productivity_change %>% 
  filter(ssp == "ssp126" & reference == "2050s - 2020s")
hist(data_a$p_change_mean)
data_a <- data_a %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.02 ~ 0.02,
                                   p_change_mean < -0.02 ~ -0.02,
                                   TRUE ~ p_change_mean))

map_a <- ggplot(data_a)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

#plot productivity map_b
data_b <- productivity_change %>% 
  filter(ssp == "ssp126" & reference == "2090s - 2020s")
hist(data_b$p_change_mean)
data_b <- data_b %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.02 ~ 0.02,
                                   p_change_mean < -0.02 ~ -0.02,
                                   TRUE ~ p_change_mean))

map_b <- ggplot(data_b)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

#plot productivity map_c
data_c <- productivity_change %>% 
  filter(ssp == "ssp245" & reference == "2050s - 2020s")
hist(data_c$p_change_mean)
data_c <- data_c %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.04 ~ 0.04,
                                   p_change_mean < -0.04 ~ -0.04,
                                   TRUE ~ p_change_mean))

map_c <- ggplot(data_c)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

#plot productivity map_d
data_d <- productivity_change %>% 
  filter(ssp == "ssp245" & reference == "2090s - 2020s")
hist(data_d$p_change_mean)
data_d <- data_d %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.04 ~ 0.04,
                                   p_change_mean < -0.04 ~ -0.04,
                                   TRUE ~ p_change_mean))

map_d <- ggplot(data_d)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

#plot productivity map_e
data_e <- productivity_change %>% 
  filter(ssp == "ssp585" & reference == "2050s - 2020s")
hist(data_e$p_change_mean)
data_e <- data_e %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.06 ~ 0.06,
                                   p_change_mean < -0.06 ~ -0.06,
                                   TRUE ~ p_change_mean))

map_e <- ggplot(data_e)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

#plot productivity map_f
data_f <- productivity_change %>% 
  filter(ssp == "ssp585" & reference == "2090s - 2020s")
hist(data_f$p_change_mean)
data_f <- data_f %>%
  mutate(p_change_mean = case_when(p_change_mean > 0.06 ~ 0.06,
                                   p_change_mean < -0.06 ~ -0.06,
                                   TRUE ~ p_change_mean))

map_f <- ggplot(data_f)+
  geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
  # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
  # geom_sf(data = world,fill="black",color="black")+
  geom_point(aes(x = centroid_lon, y = centroid_lat, color = p_change_mean), size = 1)+
  # facet_grid(ssp~reference)+
  scale_color_gradient2(low=brewer_pal(type="div",palette = "PuOr")(9)[9],
                        high=brewer_pal(type="div",palette = "PuOr")(9)[1])+
  # scale_color_viridis()+
  scale_x_continuous("Longitude", limits = c(-180,180), breaks = seq(-180,180,60), expand = c(0, 0))+
  scale_y_continuous("Latitude", limits = c(-85,85), breaks = seq(-90,90,30), expand = c(0, 0))+
  # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),
  #                      color = "black", family = "Calibri", fontface = 2, size = 4)+
  # coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.25,0.6),
        legend.background = element_blank(),
        legend.key.height = unit(0.5,"cm"),
        legend.key.width = unit(0.5,"cm"))

ggarrange(map_a,map_b,
          map_c,map_d,
          map_e,map_f,
          ncol = 2, nrow = 3)

ggsave("Figures/winner_loser_distribution.png",width = 7,height = 6)
ggsave("Figures/winner_loser_distribution.PDF",device = cairo_pdf,width = 7,height = 6)
