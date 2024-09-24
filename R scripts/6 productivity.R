library(tidyverse)
library(JABBA)
library(scales)
library(ggsci)
library(ggpubr)
library(strucchange)
library(doParallel)
library(foreach)

#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_aaas()(10))
show_col(pal_d3()(10))
show_col(pal_lancet()(9))
# show_col(pal_npg()(10))
mypal <- pal_lancet()(9)

# 1 Productivity data -----------------------------------------------
best_model <- read_rds("JABBA results/0_best_JABBA_final.rds") %>% 
  mutate(file_name=paste0("JABBA results/",stockid,"_seed1_",Model,"_jabba.rdata")) 

#make best model file name
best_model_file_name <- best_model$file_name

#parallel run
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

productivity <- foreach(x=best_model_file_name[1:725],
                        .combine = "rbind",
                        .packages = c("tidyverse","JABBA")) %dopar% {
                          
                          # x <- best_model_file_name[1]
                          load(x) #load jabba result
                          
                          #loop get productivity candidate time sereis
                          productivity_loop <- NULL
                          for (i in 1:6000) {
                            
                            data_loop <- filter(as.data.frame(jabba$kbtrj),iter==i) %>% 
                              arrange(year) #make sure year order
                            data_loop <- data_loop %>% 
                              mutate(Bt0=B,Bt1=lead(B,1)) 
                            data_loop <- data_loop %>% 
                              mutate(sp=Bt1+Catch-Bt0)
                            data_loop <- data_loop %>% 
                              mutate(P=sp/Bt0) #calculate productivity 
                            data_loop <- data_loop %>% 
                              select(year,P)
                            data_loop <- data_loop %>% 
                              mutate(stockid=str_sub(jabba$assessment,1,-7),
                                     time_series=i) #add stockid and time series number
                            productivity_loop <- bind_rows(productivity_loop,data_loop)
                            
                            }
                          
                          productivity_loop <- productivity_loop %>% 
                            pivot_wider(values_from = P,names_from = time_series)
                          
                        }
                    
#save information
write_rds(productivity,file="Data/productivity.rds")

#stop cluster
stopCluster(cl)

#productivity summary
productivity <- read_rds("Data/productivity.rds")

productivity_summary <- productivity %>% 
  drop_na() %>% 
  rowwise(year,stockid) %>% 
  summarise(p_mean=mean(c_across(1:6000)),
            p_sd=sd(c_across(1:6000)),
            p_median=median(c_across(1:6000)),
            p_2.5=quantile(c_across(1:6000),0.025),
            p_97.5=quantile(c_across(1:6000),0.975))

write_rds(productivity_summary,file="Data/productivity_summary.rds")


# 2 Productivity characteristics ------------------------------------------
#mean, SD, CV and trend
productivity <- read_rds("Data/productivity.rds")

stock_information <- read_rds("Data/stock_success_full_information_final.rds")
unique(stock_information$scientificname)
#stocks
unique(productivity$stockid) #725 stocks
productivity_characteristics <- NULL
for (i in unique(productivity$stockid)) {
  
  # i <- unique(productivity$stockid)[1]
  productivity_loop <- filter(productivity,stockid==i) %>% 
    drop_na()
  
  # statistics
  p_mean <- apply(productivity_loop[,3:6002],MARGIN = 2,FUN = mean) #mean
  p_sd <- apply(productivity_loop[,3:6002],MARGIN = 2,FUN = sd) #sd
  p_CV <- p_sd/p_mean #CV
  
  #statistics summary
  p_mean_lci <- quantile(p_mean,0.025)
  p_mean_mean <- mean(p_mean)
  p_mean_uci <- quantile(p_mean,0.975)
  
  p_sd_lci <- quantile(p_sd,0.025)
  p_sd_mean <- mean(p_sd)
  p_sd_uci <- quantile(p_sd,0.975)
  
  p_CV_lci <- quantile(p_CV,0.025)
  p_CV_mean <- mean(p_CV)
  p_CV_uci <- quantile(p_CV,0.975)
  
  #linear regression on year (trend)
  lm_results <- summary(lm(as.matrix(productivity_loop[,3:6002])~productivity_loop$year))
  p_slope <- NULL
  p_slope_significance <- NULL
  for (j in 1:6000) {
    
    # j=1
    p_slope <- c(p_slope,lm_results[[j]]$coefficients[2,1]) #slope
    p_slope_significance <- c(p_slope_significance,lm_results[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci <- quantile(p_slope,0.025)
  p_slope_mean <- mean(p_slope)
  p_slope_uci <- quantile(p_slope,0.975)
  
  p_slope_significance_95 <- quantile(p_slope_significance,0.95)
  
  #make a data frame
  productivity_characteristics_loop <- data.frame(stockid=i,
                                                  p_mean_lci,p_mean_mean,p_mean_uci,
                                                  p_sd_lci,p_sd_mean,p_sd_uci,
                                                  p_CV_lci,p_CV_mean,p_CV_uci,
                                                  p_slope_lci,p_slope_mean,p_slope_uci,
                                                  p_slope_significance_95)
  
  #combind data
  productivity_characteristics <- bind_rows(productivity_characteristics,
                                            productivity_characteristics_loop)
  
  print(i)
  
}

#save data
write_rds(productivity_characteristics,file="Data/productivity_characteristics.rds")




# 3 Productivity trend 1981-2022 ------------------------------------------
#trend 1981-2022
productivity <- read_rds("Data/productivity.rds")

#stocks
unique(productivity$stockid) #725 stocks
productivity_trend <- NULL
for (i in unique(productivity$stockid)) {
  
  # i <- unique(productivity$stockid)[1]
  productivity_loop <- filter(productivity,stockid==i) %>% 
    filter(year %in% c(1981:2022)) %>% 
    drop_na()
  
  #linear regression on year (trend)
  lm_results <- summary(lm(as.matrix(productivity_loop[,3:6002])~productivity_loop$year))
  p_slope <- NULL
  p_slope_significance <- NULL
  for (j in 1:6000) {
    
    # j=1
    p_slope <- c(p_slope,lm_results[[j]]$coefficients[2,1]) #slope
    p_slope_significance <- c(p_slope_significance,lm_results[[j]]$coefficients[2,4]) #p value
    
  }
  
  p_slope_lci <- quantile(p_slope,0.025)
  p_slope_mean <- mean(p_slope)
  p_slope_uci <- quantile(p_slope,0.975)
  
  p_slope_significance_95 <- quantile(p_slope_significance,0.95)
  
  #make a data frame
  productivity_trend_loop <- data.frame(stockid=i,
                                                  p_slope_lci,p_slope_mean,p_slope_uci,
                                                  p_slope_significance_95)
  
  #combind data
  productivity_trend <- bind_rows(productivity_trend,
                                  productivity_trend_loop)
  
  print(i)
  
}

#save data
write_rds(productivity_trend,file="Data/productivity_trend_1981_2022.rds")



# 4 Productivity dynamics comparison --------------------------------------
#read data
productivity_characteristics <- read_rds("Data/productivity_characteristics.rds")


















# 5 Productivity time series length overview for DFA -------------------------------------------------
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
    annotate("text",x = 1952,y=Inf,label=paste0("FAO Area ",i),vjust=1,hjust=0,color=mypal[2],family="Calibri",fontface=2,size)+
    annotate("text",x = 1952,y=Inf,label=paste0("Total stock number: ",stock_number),vjust=2.5,hjust=0,color=mypal[2],family="Calibri",fontface=2)+
    annotate("text",x = 1952,y=-Inf,label=paste0(period[1],"-",period[2]),vjust=-0.5,hjust=0,color=mypal[1],family="Calibri",fontface=2)+
    geom_line(aes(x=year,y=stock_number_proportion))+
    geom_hline(yintercept = 0.6,linetype="dashed")+
    scale_x_continuous("Year", limits = c(1950,2020), expand = c(0,0), breaks = seq(1950,2020,20))+
    scale_y_continuous("Proportion", limits = c(0,1), expand = c(0,0), breaks = seq(0,1,0.5))+
    theme(axis.text = element_text(family="Calibri"),
          axis.title = element_blank())
  
  assign(paste0("f_",i),f)
  
}

#save DFA_period
write_rds(DFA_period,file="DFA results/0_DFA_period.rds")

#layout
f_null <- ggplot()+
  theme(axis.line = element_blank())
f_top <- ggarrange(f_67,f_21,f_31,f_34,f_27,f_37,nrow = 1)
f_middle1 <- ggarrange(f_77,f_null,f_null,f_null,f_null,f_61,nrow = 1)
f_middle2 <- ggarrange(f_81,f_null,f_null,f_null,f_null,f_71,nrow = 1)
f_bottom <- ggarrange(f_87,f_41,f_47,f_57,f_null,f_null,nrow = 1)
f <- ggarrange(f_top,f_middle1,f_middle2,f_bottom,nrow = 4)

ggsave("Figures/stock_number_proportion.png",width = 9,height = 6)
# ggsave("Figures/stock_number_proportion.PDF",device = cairo_pdf,width = 12,height = 8)

g <- ggplotGrob(map)
f_final <- f+annotation_custom(g,xmin=0.175,xmax=0.825,ymin=0.2,ymax=0.8)
ggsave("Figures/stock_number_proportion.png",width = 12,height = 8)

ggsave("Figures/stock_number_proportion.PDF",device = cairo_pdf,width = 9,height = 6)
























