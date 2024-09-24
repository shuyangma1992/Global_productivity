library(tidyverse)
library(ggsci)
library(scales)
library(ggsankey)
library(ggpubr)
library(scatterpie)

#Font
windowsFonts(B=windowsFont("Calibri"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_npg()(10))
# show_col(pal_npg()(10))
mypal <- pal_npg()(10)

# 0 stock group information -----------------------------------------------
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname,primary_FAOarea)
unique(stock_success$stockid) #725 stocks
unique(stock_success$scientificname) #290 species
unique(stock_success$primary_FAOarea) #16 FAO areas

#load ram legacy database and find family/order/class in taxonomy and fishery type/taxgroup in metadata
load("Data/DBdata[asmt][v4.61].RData")

a <- taxonomy %>% 
  select(scientificname,family,ordername,classname)

b <- metadata %>% 
  select(stockid,FisheryType,taxGroup)

#stock success group information data
stock_success_group <- left_join(stock_success,a) %>% 
  left_join(b) %>% 
  filter(!duplicated(stockid))

write_rds(stock_success_group,file="Data/stock_success_group_information.rds")


# Figure 1 stock productivity by FAO area -----------------------------------
#productivity_characteristics
productivity_characteristics <- read_rds("Data/productivity_characteristics.rds")

#stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

#combine data
productivity_characteristics <- productivity_characteristics %>% 
  left_join(stock_group_information)

#FAO area
productivity_characteristics <- productivity_characteristics %>% 
  group_by(primary_FAOarea) %>% 
  summarise(p_mean=mean(p_mean_mean),
            p_sd=sd(p_mean_mean))

productivity_characteristics <- productivity_characteristics %>% 
  arrange(p_mean) %>% 
  mutate(primary_FAOarea=factor(primary_FAOarea,levels=primary_FAOarea))

figure_1 <- ggplot(productivity_characteristics)+
  geom_point(aes(x=p_mean,y=primary_FAOarea),colour=mypal[4],size=0.5)+
  geom_errorbar(aes(xmin=p_mean-1.96*p_sd,xmax=p_mean+1.96*p_sd,y=primary_FAOarea),colour=alpha(mypal[4],0.25),width=0,linewidth=0.25)+
  annotate("text",label="16 FAO areas",x=Inf,y=-Inf,hjust=1,vjust=-1,family="Calibri")+
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity",breaks = c(0,0.2,0.4,0.6))+
  scale_y_discrete("FAO area")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.title.x = element_blank(),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave("Figures/stock productivity FAO area.PDF",device = cairo_pdf,width = 3,height = 2.5)

# Figure 2 stock productivity by fisherytype ---------------------------------------------------------------
#productivity_characteristics
productivity_characteristics <- read_rds("Data/productivity_characteristics.rds")

#stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

#combine data
productivity_characteristics <- productivity_characteristics %>% 
  left_join(stock_group_information)

#fishery type
productivity_characteristics <- productivity_characteristics %>% 
  group_by(FisheryType) %>% 
  summarise(p_mean=mean(p_mean_mean),
            p_sd=sd(p_mean_mean))

productivity_characteristics <- productivity_characteristics %>% 
  arrange(p_mean) %>% 
  mutate(FisheryType=factor(FisheryType,levels=FisheryType))

figure_2 <- ggplot(productivity_characteristics)+
  geom_point(aes(x=p_mean,y=FisheryType),colour=mypal[5],size=0.5)+
  geom_errorbar(aes(xmin=p_mean-1.96*p_sd,xmax=p_mean+1.96*p_sd,y=FisheryType),colour=alpha(mypal[5],0.25),width=0,linewidth=0.25)+
  annotate("text",label="8 fishery types",x=Inf,y=-Inf,hjust=1,vjust=-1,family="Calibri")+
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity")+
  scale_y_discrete("Fishery type")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.title.x = element_blank(),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave("Figures/stock productivity fishery type.PDF",device = cairo_pdf,width = 3,height = 2)


# Figure 3 stock productivity by family -----------------------------------
#productivity_characteristics
productivity_characteristics <- read_rds("Data/productivity_characteristics.rds")

#stock group information
stock_group_information <- read_rds("Data/stock_success_group_information.rds")

#combine data
productivity_characteristics <- productivity_characteristics %>% 
  left_join(stock_group_information)

#family
productivity_characteristics <- productivity_characteristics %>% 
  group_by(family) %>% 
  summarise(p_mean=mean(p_mean_mean),
            p_sd=sd(p_mean_mean))

productivity_characteristics <- productivity_characteristics %>% 
  arrange(p_mean) %>% 
  mutate(family=factor(family,levels=family))

figure_3 <- ggplot(productivity_characteristics)+
  geom_point(aes(x=p_mean,y=family),colour=mypal[3],size=0.5)+
  geom_errorbar(aes(xmin=p_mean-1.96*p_sd,xmax=p_mean+1.96*p_sd,y=family),colour=alpha(mypal[3],0.25),width=0,linewidth=0.25)+
  annotate("text",label="89 families",x=Inf,y=-Inf,hjust=1,vjust=-1,family="Calibri")+
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity")+
  scale_y_discrete("Family")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.title.x = element_blank(),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave("Figures/stock productivity family.PDF",device = cairo_pdf,width = 3,height = 3.5)

# Figure 4 stock productivity mean and sd-----------------------------------------------------------------
#productivity_characteristics
productivity_characteristics <- read_rds("Data/productivity_characteristics.rds")

#reorder by mean productivity
productivity_characteristics <- productivity_characteristics %>% 
  arrange(p_mean_mean) %>% 
  mutate(stockid=factor(stockid,levels=stockid))

figure_4 <- ggplot(productivity_characteristics)+
  # geom_point(aes(x=p_sd_mean,y=stockid),colour=mypal[2],size=0.5)+
  # geom_errorbar(aes(xmin=p_sd_lci,xmax=p_sd_uci,y=stockid),colour=alpha(mypal[2],0.25),width=0,linewidth=0.25)+
  geom_point(aes(x=p_mean_mean,y=stockid),colour=mypal[1],size=0.5)+
  geom_errorbar(aes(xmin=p_mean_lci,xmax=p_mean_uci,y=stockid),colour=alpha(mypal[1],0.25),width=0,linewidth=0.25)+
  annotate("text",label="725 stocks",x=Inf,y=-Inf,hjust=1,vjust=-1,family="Calibri")+
  # ggtitle("Stock productivity")+
  scale_x_continuous("Productivity")+
  scale_y_discrete("Stock")+
  theme(title = element_text(family="Calibri"),
        axis.title = element_text(family="Calibri"),
        axis.title.x = element_blank(),
        axis.text = element_text(family="Calibri"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# ggsave("Figures/stock productivity.PDF",device = cairo_pdf,width = 3,height = 8)
  
# Figure combination ------------------------------------------------------
figure_left <- ggarrange(figure_1,figure_2,figure_3,ncol = 1,heights = c(3.5,2.5,2))
figure_right <- figure_4

figure <- ggarrange(figure_left,figure_right,ncol = 2)

ggsave("Figures/stock productivity combination.PDF",device = cairo_pdf,width = 6,height = 6)











# Figure 5 stock productivity trend ---------------------------------------
#productivity_trend
productivity_trend <- read_rds("Data/productivity_trend_1981_2022.rds") 

#stock information
productivity_trend <- left_join(productivity_trend,
                                stock_success)

#total number
productivity_total_number <- productivity_trend %>% 
  group_by(primary_FAOarea) %>% 
  summarize(total_number=n()) %>% 
  ungroup()

#positive trend
productivity_positive_trend_number <- productivity_trend %>% 
  filter(p_slope_lci>0) %>% 
  group_by(primary_FAOarea) %>% 
  summarize(positive_trend_number=n()) %>% 
  ungroup()
  
#negative trend
productivity_negative_trend_number <- productivity_trend %>% 
  filter(p_slope_uci<0) %>% 
  group_by(primary_FAOarea) %>% 
  summarize(negative_trend_number=n()) %>% 
  ungroup()

#combine data
productivity_trend_number <- left_join(productivity_total_number,productivity_positive_trend_number) %>% 
  left_join(productivity_negative_trend_number)
  
#calculate proportion
productivity_trend_number <- productivity_trend_number %>% 
  filter(!primary_FAOarea %in% c("51","58")) %>% 
  mutate(positive_trend_proportion=positive_trend_number/total_number,
         negative_trend_proportion=negative_trend_number/total_number,
         no_trend_proportion=(total_number-positive_trend_number-negative_trend_number)/total_number)

#select
productivity_trend_number <- productivity_trend_number %>% 
  select(1,2,5,6,7)

productivity_trend_number <- productivity_trend_number %>% 
  pivot_longer(-c(1,2), names_to = "trend", values_to = "proportion")

# Compute the cumulative percentages (top of each rectangle)
productivity_trend_number <- productivity_trend_number %>% 
  group_by(primary_FAOarea) %>% 
  mutate(ymax = cumsum(proportion)) %>% 
  ungroup()

# Compute the bottom of each rectangle
productivity_trend_number <- productivity_trend_number %>% 
  group_by(primary_FAOarea) %>% 
  mutate(ymin = c(0,head(ymax, n=-1)))

# Make the plot
f <- ggplot(productivity_trend_number)+
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=trend), show.legend = F)+
  scale_fill_manual(values = c("positive_trend_proportion"=mypal[3],
                               "negative_trend_proportion"=mypal[1],
                               "no_trend_proportion"="grey"))+
  facet_wrap(.~primary_FAOarea+total_number)+
  coord_polar(theta="y")+ # Try to remove that to understand how the chart is built initially
  xlim(c(1, 4))+# Try to remove that to see how to make a pie chart
  theme(axis.text = element_blank())

ggsave("Figures/trend.PDF",device = cairo_pdf,width = 6,height = 6)











# #coordinates
# productivity_trend_number$x <- c(-50,-30,-70,-20,10,-40,0,100,160,-150,160,-145,160,-100)
# productivity_trend_number$y <- c(50,50,20,20,35,-40,-30,-35,40,50,5,10,-45,-40)
# 
# #world map
# #data information
# FAO_shp <- vect("Data/FAO_AREAS_CWP/FAO_AREAS_CWP.shp")
# 
# #attributes
# FAO_shp_attribute <- as.data.frame(FAO_shp)
# 
# #geometry
# FAO_shp_geometry <- geom(FAO_shp)
# 
# #major area
# FAO_shp_major <- FAO_shp %>% 
#   filter(F_LEVEL=="MAJOR")
# 
# #world data (ggplot2 package)
# # world <- map_data("world")
# 
# #world data (rnaturalearth package)
# world <- ne_countries(scale = "small", returnclass = "sf")
# 
# #plot world map
# map <- ggplot(data = world)+
#   # geom_map(map=world,data=world,aes(x=long,y=lat,map_id=region),fill="black")+
#   # geom_spatvector(data = FAO_shp_major, fill="white",color="black")+
#   # geom_sf(fill="black",color="black")+
#   # geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),color=mypal[2],family="Calibri",fontface=2,size=4)+
#   geom_scatterpie(data=productivity_trend_number,cols=c("positive_trend_proportion",
#                                                         "negative_trend_proportion",
#                                                         "no_trend_proportion"),
#                   aes(x=x,y=y,group=primary_FAOarea,r=log(total_number)+8),show.legend=F)+
#   scale_fill_manual(values = c("positive_trend_proportion"=mypal[3],
#                                 "negative_trend_proportion"=mypal[1],
#                                 "no_trend_proportion"="grey"))+
#   geom_scatterpie_legend(log(productivity_trend_number$total_number)+8, x=-160, y=-50)+
#   # coord_sf(crs = st_crs("ESRI:54030"))+
#   theme(axis.text = element_blank(),
#         axis.title = element_blank(),
#         axis.ticks = element_blank(),
#         axis.line = element_blank(),
#         plot.margin = margin(0.1,0.1,0.1,0.1))
# 
# # ggsave("Figures/map.png",width = 8,height = 6)
# ggsave("Figures/trend map.PDF",device = cairo_pdf,width = 8,height = 3)



