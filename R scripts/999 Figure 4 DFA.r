library(tidyverse)
library(terra)
library(rgdal)
library(tidyterra)
library(ggthemes)
require(rgdal)
library(sf)
library(rnaturalearth)
library(scales)
library(ggsci)
library(ggpubr)

#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_aaas()(10))
show_col(pal_npg()(10))
show_col(pal_lancet()(9))
# show_col(pal_npg()(10))
mypal <- pal_lancet()(9)
# mypal <- pal_npg()(10)


# 1 world map -------------------------------------------------------------------
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
  geom_spatvector_text(data = FAO_shp_major, aes(label = F_CODE),color=mypal[3],family="Calibri",fontface=2,size=4)+
  coord_sf(crs = st_crs("ESRI:54030"))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(0.1,0.1,0.1,0.1))

# ggsave("Figures/map.png",width = 8,height = 6)
# ggsave("Figures/map.PDF",device = cairo_pdf,width = 8,height = 6)



# 2 DFA length-------------------------------------------------
#load data
productivity <- read_rds("Data/productivity_summary.rds") %>% 
  select(year,stockid,p_mean) %>% 
  drop_na() %>% 
  arrange(year) %>% 
  filter(year>=1950)

#stock information
#delete FAO area 51, 58, less than 5 stocks
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  filter(!primary_FAOarea %in% c("51","58"))

stock_success <- stock_success %>% 
  filter(primary_FAOarea %in% c("58"))


productivity_stock_number <- NULL
for (i in unique(stock_success$primary_FAOarea)) {
  
  # i="21"
  stock_success_loop <- stock_success %>% 
    filter(primary_FAOarea==i)
  stock_number <- nrow(stock_success_loop)
  
  productivity_loop <- productivity %>% 
    filter(stockid %in% stock_success_loop$stockid) %>% 
    drop_na()
  
  productivity_loop <- productivity_loop %>% 
    group_by(year) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    mutate(FAOarea=i,
           stock_number=stock_number)
  
  productivity_stock_number <- bind_rows(productivity_stock_number,productivity_loop)
  
}

#stock_number_proportion
productivity_stock_number <- productivity_stock_number %>% 
  rowwise() %>% 
  mutate(stock_number_proportion=n/stock_number) 

#plot
DFA_period <- NULL
for (i in unique(productivity_stock_number$FAOarea)) {
  
  # i=21
  productivity_stock_number_loop <- productivity_stock_number %>% 
    filter(FAOarea==i)
  period <- range(productivity_stock_number_loop %>% 
                    filter(stock_number_proportion>=0.6) %>% 
                    select(year))
  DFA_period <- bind_rows(DFA_period,
                          data.frame(FAOarea=i,start_year=period[1],end_year=period[2]))
  
  stock_number <- productivity_stock_number_loop$stock_number[1]
  
  f <- ggplot(productivity_stock_number_loop)+
    annotate("rect",xmin=period[1],xmax=period[2],ymin=-Inf,ymax=Inf,fill=scales::alpha(mypal[1],0.1))+
    annotate("text",x = 1952,y=Inf,label=paste0("FAO Area ",i),vjust=1,hjust=0,color=mypal[3],family="Calibri",fontface=2)+
    annotate("text",x = 1952,y=Inf,label=paste0("Total stock: ",stock_number),vjust=2.5,hjust=0,color="black",family="Calibri",fontface=2,size=3)+
    annotate("text",x = 1952,y=-Inf,label=paste0(period[1],"-",period[2]),vjust=-0.5,hjust=0,color=mypal[1],family="Calibri",fontface=2)+
    geom_line(aes(x=year,y=stock_number_proportion))+
    geom_hline(yintercept = 0.6,linetype="dashed")+
    scale_x_continuous("Year", limits = c(1950,2020), expand = c(0,0), breaks = seq(1970,2020,20))+
    scale_y_continuous("Proportion", limits = c(0,1), expand = c(0,0), breaks = seq(0,1,0.5))+
    theme(axis.text = element_text(family="Calibri"),
          axis.title = element_blank())
  
  assign(paste0("f_",i),f)
  
}

#save DFA_period
# write_rds(DFA_period,file="DFA results/0_DFA_period.rds")

#layout
f_null <- ggplot()+
  theme(axis.line = element_blank())
f_top <- ggarrange(f_67,f_21,f_31,f_34,f_27,f_37,nrow = 1)
f_middle1 <- ggarrange(f_77,f_null,f_null,f_null,f_null,f_61,nrow = 1)
f_middle2 <- ggarrange(f_81,f_null,f_null,f_null,f_null,f_71,nrow = 1)
f_bottom <- ggarrange(f_87,f_41,f_47,f_57,f_null,f_null,nrow = 1)
f <- ggarrange(f_top,f_middle1,f_middle2,f_bottom,nrow = 4)
# 
# ggsave("Figures/stock_number_proportion.png",width = 9,height = 6)
# ggsave("Figures/stock_number_proportion.PDF",device = cairo_pdf,width = 9,height = 6)

g <- ggplotGrob(map)
f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.825,ymin=0.2,ymax=0.8)
ggsave("Figures/stock_number_proportion.png",width = 9,height = 6)

ggsave("Figures/stock_number_proportion.PDF",device = cairo_pdf,width = 9,height = 6)















# 3 DFA trend -------------------------------------------------------------
DFA_loading <- read_rds("DFA results/0_best_DFA_loading.rds")

# change sign of fao area 21 trend 1
DFA_loading <- DFA_loading %>% 
  mutate(mean = case_when(
    area == "21" & trend == "trend1" ~ -mean,
    .default = mean
  )) %>% 
  mutate(up = case_when(
    area == "21" & trend == "trend1" ~ -up,
    .default = up
  )) %>% 
  mutate(low = case_when(
    area == "21" & trend == "trend1" ~ -low,
    .default = low
  )) 

DFA_trend <- read_rds("DFA results/0_best_DFA_trend.rds")

# change sign of fao area 21 trend 1
DFA_trend <- DFA_trend %>% 
  mutate(mean = case_when(
    area == "21" & trend == "trend1" ~ -mean,
    .default = mean
  )) %>% 
  mutate(up = case_when(
    area == "21" & trend == "trend1" ~ -up,
    .default = up
  )) %>% 
  mutate(low = case_when(
    area == "21" & trend == "trend1" ~ -low,
    .default = low
  )) 


productivity_stock_number <- rename(productivity_stock_number,area=3)
for (i in unique(DFA_trend$area)) {
  
  # i="34"
  #get stock loading results
  DFA_loading_loop <- DFA_loading %>% 
    filter(area==i) 
  DFA_loading_loop <- DFA_loading_loop %>% 
    mutate(names=factor(names,levels= arrange(filter(DFA_loading_loop,trend=="trend1"),mean)$names)) %>% 
    rowwise() %>% 
    mutate(significance=ifelse((mean>0)&(quantile(range(low,up),0.05)>0),
                               "significant",
                               ifelse((mean<0)&(quantile(range(low,up),0.95)<0),
                                      "significant",
                                      "not significant")))
  #stock number information
  stock_number <- length(unique(DFA_loading_loop$names))
  stock_number_trend1_positive <- dim(filter(DFA_loading_loop,(mean>0)&significance=="significant"&trend=="trend1"))[1]
  stock_number_trend1_negative <-dim(filter(DFA_loading_loop,(mean<0)&significance=="significant"&trend=="trend1"))[1]
  stock_number_trend2_positive <- dim(filter(DFA_loading_loop,(mean>0)&significance=="significant"&trend=="trend2"))[1]
  stock_number_trend2_negative <-dim(filter(DFA_loading_loop,(mean<0)&significance=="significant"&trend=="trend2"))[1]
  
  #plot trend
  DFA_trend_loop <- DFA_trend %>% 
    filter(area==i)
  DFA_trend_loop <- left_join(DFA_trend_loop, productivity_stock_number)
  
  # f_trend <- ggplot(DFA_trend_loop)+
  #   geom_vline(xintercept = c(1950,1960,1970,1980,1990,2000,2010),linetype="dotted",color="gray50",linewidth=0.25)+
  #   geom_ribbon(aes(x=year,ymin=low,ymax=up,fill=trend),alpha=0.2,show.legend = F)+
  #   geom_line(aes(x=year,y=mean,color=trend),show.legend = F)+
  #   scale_fill_manual(values = c("trend1"=mypal[1],"trend2"=mypal[2]))+
  #   scale_color_manual(values = c("trend1"=mypal[1],"trend2"=mypal[2]))+
  #   annotate("text",x = 1952,y=Inf,label=paste0("FAO area ",i),
  #            vjust=1,hjust=0,color=mypal[3],family="Calibri",fontface=2)+
  #   annotate("text",x = 1952,y=Inf,label=paste0(min(DFA_trend_loop$year),"-",max(DFA_trend_loop$year)),
  #            vjust=3,hjust=0,color="black",family="Calibri",fontface=1,size=3)+
  #   annotate("text",x = 1952,y=-Inf,label=paste0("Total: ",stock_number),
  #            vjust=-9,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
  #   annotate("text",x = 1952,y=-Inf,label=paste0("Trend 1+: ",stock_number_trend1_positive),
  #            vjust=-7,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
  #   annotate("text",x = 1952,y=-Inf,label=paste0("Trend 1-: ",stock_number_trend1_negative),
  #            vjust=-5,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
  #   annotate("text",x = 1952,y=-Inf,label=paste0("Trend 2+: ",stock_number_trend2_positive),
  #            vjust=-3,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
  #   annotate("text",x = 1952,y=-Inf,label=paste0("Trend 2-: ",stock_number_trend2_negative),
  #            vjust=-1,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
  #   scale_x_continuous("Year", limits = c(1950,2020), expand = c(0,0), breaks = seq(1970,2020,20))+
  #   scale_y_continuous("Value",)+
  #   theme(axis.text = element_blank(),
  #         axis.title = element_blank(),
  #         axis.ticks = element_blank())
    
  
  
  f_trend <- ggplot(DFA_trend_loop)+
    geom_vline(xintercept = c("1950","1960","1970","1980","1990","2000","2010"),linetype="dotted",color="gray50",linewidth=0.25)+
    geom_boxplot(data= filter(DFA_trend_loop, trend == "trend1"),
                 aes(x = as.factor(year), ymin = low, lower = low, middle = mean, upper = up, ymax = up, 
                     alpha = stock_number_proportion, fill = "trend1"), 
                 stat = "identity", show.legend = F, color = NA)+
    geom_boxplot(data= filter(DFA_trend_loop, trend == "trend2"),
                 aes(x = as.factor(year), ymin = low, lower = low, middle = mean, upper = up, ymax = up, 
                     alpha = stock_number_proportion, fill = "trend2"), 
                 stat = "identity", show.legend = F, color = NA)+
    geom_line(data= filter(DFA_trend_loop, trend == "trend1"),
              aes(x=as.factor(year),y=mean,group = 1,color="trend1"), linewidth = 0.25, show.legend = F)+
    geom_line(data= filter(DFA_trend_loop, trend == "trend2"),
              aes(x=as.factor(year),y=mean,group = 1,color="trend2"), linewidth = 0.25, show.legend = F)+
    scale_alpha_continuous(range = c(0.1, 0.5))+
    scale_fill_manual(values = c("trend1"=mypal[1],"trend2"=mypal[2]))+
    scale_color_manual(values = c("trend1"=mypal[1],"trend2"=mypal[2]))+
    annotate("text",x = 2,y=Inf,label=paste0("FAO area ",i),
             vjust=1,hjust=0,color="black",family="Calibri",fontface=1)+
    annotate("text",x = 2,y=Inf,label=paste0(min(DFA_trend_loop$year),"-",max(DFA_trend_loop$year)),
             vjust=3,hjust=0,color="black",family="Calibri",fontface=1,size=3)+
    annotate("text",x = 2,y=-Inf,label=paste0("Total: ",stock_number),
             vjust=-9,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = 2,y=-Inf,label=paste0("Trend 1+: ",stock_number_trend1_positive),
             vjust=-7,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = 2,y=-Inf,label=paste0("Trend 1-: ",stock_number_trend1_negative),
             vjust=-5,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = 2,y=-Inf,label=paste0("Trend 2+: ",stock_number_trend2_positive),
             vjust=-3,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = 2,y=-Inf,label=paste0("Trend 2-: ",stock_number_trend2_negative),
             vjust=-1,hjust=0,color="black",family="Calibri",fontface=1,size=2)+
    scale_x_discrete("Year", limits = factor(c(1950:2020)), expand = c(0,0))+
    scale_y_continuous("Value",)+
    theme(axis.text = element_blank(),
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

# g <- ggplotGrob(map)
# f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.825,ymin=0.2,ymax=0.8)

ggsave("Figures/DFA_trend.png",width = 9,height = 6)
ggsave("Figures/DFA_trend_new.PDF",device = cairo_pdf,width = 9,height = 6)


# 4 DFA loading -----------------------------------------------------------
DFA_loading <- read_rds("DFA results/0_best_DFA_loading.rds")

# change sign of fao area 21 trend 1
DFA_loading <- DFA_loading %>% 
  mutate(mean = case_when(
    area == "21" & trend == "trend1" ~ -mean,
    .default = mean
  )) %>% 
  mutate(up = case_when(
    area == "21" & trend == "trend1" ~ -up,
    .default = up
  )) %>% 
  mutate(low = case_when(
    area == "21" & trend == "trend1" ~ -low,
    .default = low
  )) 

DFA_trend <- read_rds("DFA results/0_best_DFA_trend.rds")

# change sign of fao area 21 trend 1
DFA_trend <- DFA_trend %>% 
  mutate(mean = case_when(
    area == "21" & trend == "trend1" ~ -mean,
    .default = mean
  )) %>% 
  mutate(up = case_when(
    area == "21" & trend == "trend1" ~ -up,
    .default = up
  )) %>% 
  mutate(low = case_when(
    area == "21" & trend == "trend1" ~ -low,
    .default = low
  )) 


for (i in unique(DFA_loading$area)) {
  
  # i="47"
  DFA_loading_loop <- DFA_loading %>% 
    filter(area==i) 
  DFA_loading_loop <- DFA_loading_loop %>% 
    mutate(names=factor(names,levels= arrange(filter(DFA_loading_loop,trend=="trend1"),mean)$names)) %>% 
    rowwise() %>% 
    mutate(significance=ifelse((mean>0)&(quantile(range(low,up),0.05)>0),
                               "significant",
                               ifelse((mean<0)&(quantile(range(low,up),0.95)<0),
                               "significant",
                               "not significant")))
  
  #stock number information
  stock_number <- length(unique(DFA_loading_loop$names))
  stock_number_trend1_positive <- dim(filter(DFA_loading_loop,(mean>0)&significance=="significant"&trend=="trend1"))[1]
  stock_number_trend1_negative <-dim(filter(DFA_loading_loop,(mean<0)&significance=="significant"&trend=="trend1"))[1]
  stock_number_trend2_positive <- dim(filter(DFA_loading_loop,(mean>0)&significance=="significant"&trend=="trend2"))[1]
  stock_number_trend2_negative <-dim(filter(DFA_loading_loop,(mean<0)&significance=="significant"&trend=="trend2"))[1]
  
  f_loading <- ggplot(DFA_loading_loop)+
    geom_vline(xintercept = 0,linetype="dotted",color="gray50",linewidth=0.25)+
    geom_errorbar(aes(xmin=low,xmax=up,y=names,alpha=significance,color=trend),show.legend = F,position = position_dodge(width =1),width=0,linewidth=0.1)+
    geom_point(aes(x=mean,y=names,alpha=significance,color=trend),show.legend = F,position = position_dodge(width =1),size=0.25)+
    scale_color_manual(values = c("trend1"=mypal[1],"trend2"=mypal[2]))+
    scale_alpha_manual(values = c("significant"=1,"not significant"=0.1))+
    annotate("text",x = -Inf,y=Inf,label=paste0("FAO area ",i),
             vjust=1,hjust=-0.05,color=mypal[3],family="Calibri",fontface=2)+
    annotate("text",x = Inf,y=-Inf,label=paste0("Total: ",stock_number),
             vjust=-9,hjust=1,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = Inf,y=-Inf,label=paste0("Trend1+: ",stock_number_trend1_positive),
             vjust=-7,hjust=1,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = Inf,y=-Inf,label=paste0("Trend1-: ",stock_number_trend1_negative),
             vjust=-5,hjust=1,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = Inf,y=-Inf,label=paste0("Trend1+: ",stock_number_trend2_positive),
             vjust=-3,hjust=1,color="black",family="Calibri",fontface=1,size=2)+
    annotate("text",x = Inf,y=-Inf,label=paste0("Trend1-: ",stock_number_trend2_negative),
             vjust=-1,hjust=1,color="black",family="Calibri",fontface=1,size=2)+
    scale_x_continuous("Loading", expand = c(0,0),limits = c(quantile(DFA_loading_loop$low,0.025),quantile(DFA_loading_loop$up,0.975)),breaks = c(0))+
    scale_y_discrete("Stock", expand = c(0,0))+
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  assign(paste0("f_loading_",i),f_loading)
  
}

#layout
f_null <- ggplot()+
  theme(axis.line = element_blank())

f_top <- ggarrange(f_loading_67,f_loading_21,f_loading_31,f_loading_34,f_loading_27,f_loading_37,nrow = 1)
f_middle1 <- ggarrange(f_loading_77,f_null,f_null,f_null,f_null,f_loading_61,nrow = 1)
f_middle2 <- ggarrange(f_loading_81,f_null,f_null,f_null,f_null,f_loading_71,nrow = 1)
f_bottom <- ggarrange(f_loading_87,f_loading_41,f_loading_47,f_loading_57,f_null,f_null,nrow = 1)
f <- ggarrange(f_top,f_middle1,f_middle2,f_bottom,nrow = 4)

# g <- ggplotGrob(map)
# f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.825,ymin=0.2,ymax=0.8)

ggsave("Figures/DFA_loading.png",width = 9,height = 6)
ggsave("Figures/DFA_loading.PDF",device = cairo_pdf,width = 9,height = 6)








