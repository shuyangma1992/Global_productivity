library(tidyverse)
library(RColorBrewer)
library(ggdist)
library(glmmTMB)
library(performance)
library(psych)
library(DHARMa) #glmm dignose
library(broom.mixed)
library(glmtoolbox)
library(foreach)
library(doParallel)

#Font
windowsFonts(B=windowsFont("Calibri"))
#theme
theme_set(theme_classic())
#plot color
show_col(pal_npg()(10))
# show_col(pal_npg()(10))
mypal <- pal_npg()(10)


# 1 Productivity data compile ----------------------------------------------------------
#stock productivity
productivity <- read_rds("Data/productivity.rds")

#stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,GRSF_uuid,primary_FAOarea)

#combine data
productivity <- left_join(productivity,stock_success)

# 2 Physical data compile --------------------------------------------
#physical driver
physics_file_name <- list.files("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/") 

physics <- NULL
for (i in physics_file_name) {
  
  # i <- physics_file_name[1]
  
  #read data
  physics_loop <- read_rds(paste0("Data/Copernicus/Global Ocean Ensemble Physics Reanalysis/",i))
  
  #GRSF_uuid
  uuid <- unique(physics_loop$uuid)
  
  #mutate year and month (problems with the last 18 data points)
  physics_loop <- physics_loop %>% 
    mutate(year=rep(c(1993:2020),each=12),
           month=rep(c(1:12),times=28))
  
  #calculate annual averages
  physics_loop <- physics_loop %>% 
    group_by(year) %>% 
    summarise(temperature=mean(temperature),
              salinity=mean(salinity),
              seasurfaceheight=mean(seasurfaceheight),
              mixedlayerthickness=mean(mixedlayerthickness)) %>% 
    ungroup()
  
  #mutate uuid
  physics_loop <- physics_loop %>% 
    mutate(GRSF_uuid=uuid)
  
  #combine data
  physics <- bind_rows(physics,physics_loop)
  
}


# 3 Biogeochemical data compile --------------------------------------------
#biogeochemical driver
biogeochemistry_file_name <- list.files("Data/Copernicus/Global Ocean Biogeochemistry Hindcast/") 

biogeochemistry <- NULL
for (i in biogeochemistry_file_name) {
  
  # i <- biogeochemistry_file_name[1]
  
  #read data
  biogeochemistry_loop <- read_rds(paste0("Data/Copernicus/Global Ocean Biogeochemistry Hindcast/",i))
  
  #GRSF_uuid
  uuid <- unique(biogeochemistry_loop$uuid)
  
  #mutate year and month (problems with the last 18 data points)
  biogeochemistry_loop <- biogeochemistry_loop %>% 
    mutate(year=rep(c(1993:2020),each=12),
           month=rep(c(1:12),times=28))
  
  #calculate annual averages
  biogeochemistry_loop <- biogeochemistry_loop %>% 
    group_by(year) %>% 
    summarise(chlorophyll=mean(chlorophyll),
              dissolvedoxygen=mean(dissolvedoxygen),
              primaryproduction=mean(primaryproduction),
              ph=mean(ph_mean_loop)) %>% 
    ungroup()
  
  #mutate uuid
  biogeochemistry_loop <- biogeochemistry_loop %>% 
    mutate(GRSF_uuid=uuid)
  
  #combine data
  biogeochemistry <- bind_rows(biogeochemistry,biogeochemistry_loop)
  
}

# 4 Data combination and clean --------------------------------------------
#combine data, delete NAs
productivity_for_glmm <- left_join(productivity,physics) %>% 
  left_join(biogeochemistry) %>% 
  drop_na()

#save data
write_rds(productivity_for_glmm,file = "Data/producitivity_for_glmm.rds")

unique(productivity_for_glmm$stockid) #673 stocks

# 5 Biophysical driver collinearity ---------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds") %>% 
  select(c(1,2,6005:6012))

r <- NULL
p <- NULL
for (i in unique(productivity_for_glmm$stockid)) {
  
  # i <- unique(productivity_for_glmm$stockid)[1]
  data_loop <- filter(productivity_for_glmm,stockid==i)
  r_loop <- as.data.frame(corr.test(data_loop[,-c(1,2)])$r) 
  r_loop <- r_loop %>% 
    mutate(variable1=rownames(r_loop))
  p_loop <- as.data.frame(corr.test(data_loop[,-c(1,2)])$p)
  p_loop <- p_loop %>% 
    mutate(variable1=rownames(p_loop))
  r <- bind_rows(r,r_loop)
  p <- bind_rows(p,p_loop)
  
}

#data processing
r <- r %>% 
  pivot_longer(-variable1,names_to = "variable2",values_to = "r") %>% 
  mutate(variable1=factor(variable1,levels=unique(variable1)),
         variable2=factor(variable2,levels=unique(variable2)))

r_statistics <- r %>% 
  group_by(variable1,variable2) %>% 
  summarise(r_2.5=quantile(r,0.025),
            r_97.5=quantile(r,0.975),
            r_median=quantile(r,0.5))

#facet labels
driver.labs <- c("T","S","SSH","MLD","CHL","DO","TPP","PH")
names(driver.labs) <- c("temperature","salinity","seasurfaceheight","mixedlayerthickness",
                      "chlorophyll","dissolvedoxygen","primaryproduction","ph")

#violin figure
f_r <- ggplot(r)+
  stat_halfeye(aes(x="a",y=r),scale=5)+
  geom_hline(yintercept=c(-0.374,0.374),color="pink")+
  geom_hline(yintercept=c(-0.478,0.478),color="red")+
  scale_x_discrete()+
  scale_y_continuous("Correlation coefficients")+
  facet_grid(variable1~variable2,labeller = labeller(variable1=driver.labs,
                                                     variable2=driver.labs))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(family="Calibri"),
        axis.text.x = element_blank(),
        axis.text.y= element_text(family="Calibri"),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color="grey75",linetype="dotted"))

ggsave("Figures/biophysical driver collinearity.png",width = 6,height = 6)
ggsave("Figures/biophysical driver collinearity.PDF",device = cairo_pdf,width = 6,height = 6)



# 6 glmmTMB ---------------------------------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds")
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm,species_information)

a1 <- unique(filter(productivity_for_glmm,scientificname %in% a$scientificname)$stockid) #134 stocks negative effects of temperature
a2 <- unique(filter(productivity_for_glmm,scientificname %in% b$scientificname)$stockid) #133 stocks positive effects of temperature

b1 <- unique(filter(productivity_for_glmm,scientificname %in% c$scientificname)$stockid) #118 stocks negative effects of CHL
b2 <- unique(filter(productivity_for_glmm,scientificname %in% d$scientificname)$stockid) #159 stocks positive effects of CHL

c <- c(a1,a2,b1,b2)



unique(productivity_for_glmm$stockid) #673 stocks
# productivity_for_glmm_1 <- productivity_for_glmm %>% 
#   filter(stockid=="ACADREDGOMGB")
# 
# cor.test(productivity_for_glmm_1$`1`,productivity_for_glmm_1$`150`)

# hist(productivity_for_glmm$`1`,breaks = 100) #student t

#set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000,size=1000,replace = F)

#loop
information_criteria <- NULL #AIC and BIC
for (i in 1:5) {
  
  # i=1
  
  #data for loop
  data_loop <- productivity_for_glmm %>% 
    select(year,stockid,primary_FAOarea,scientificname, #basic informaiton
           temperature,chlorophyll,mixedlayerthickness, #environment drivers
           as.character(resample_number[i])) %>% 
    rename(productivity=8)
  
  # #3 year moving avearage
  # data_loop <- data_loop %>% 
  #   group_by(stockid) %>% 
  #   mutate(temperature=zoo::rollmean(temperature,3,fill=NA),
  #          chlorophyll=zoo::rollmean(chlorophyll,3,fill=NA),
  #          mixedlayerthickness=zoo::rollmean(mixedlayerthickness,3,fill=NA),
  #          productivity=zoo::rollmean(productivity,3,fill=NA)) %>% 
  #   ungroup() %>% 
  #   drop_na()
  
  #model 1, fixed slope, fixed intercept
  model1 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness,
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))

  model1_information_criteria <- glance(model1) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model1",
           number=i) 
  
  write_rds(model1, file = paste0("GLMM results/model1_", i, ".rds"))
  
  #model 2, fixed slope, random intercept
  model2 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model2_information_criteria <- glance(model2) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model2",
           number=i) 
  
  write_rds(model2, file = paste0("GLMM results/model2_", i, ".rds"))
  
  #model 3, random slope (one variable,temperature), random intercept
  model3 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model3_information_criteria <- glance(model3) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model3",
           number=i) 
  
  write_rds(model3, file = paste0("GLMM results/model3_", i, ".rds"))
  
  #model 4, random slope (one variable, chlorophyll), random intercept
  model4 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+chlorophyll|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model4_information_criteria <- glance(model4) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model4",
           number=i) 
  
  write_rds(model4, file = paste0("GLMM results/model4_", i, ".rds"))
  
  #model 5, random slope (one variable, mixedlayerthickness), random intercept
  model5 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model5_information_criteria <- glance(model5) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model5",
           number=i) 
  
  write_rds(model5, file = paste0("GLMM results/model5_", i, ".rds"))
  
  #model 6, random slope (two variables, temperature+chlorophyll), random intercept
  model6 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature+chlorophyll|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model6_information_criteria <- glance(model6) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model6",
           number=i) 
  
  write_rds(model6, file = paste0("GLMM results/model6_", i, ".rds"))
  
  #model 7, random slope (two variables, temperature+mixedlayerthickness), random intercept
  model7 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model7_information_criteria <- glance(model7) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model7",
    number=i) 
  
  write_rds(model7, file = paste0("GLMM results/model7_", i, ".rds"))
  
  #model 8, random slope (two variables, chlorophyll+mixedlayerthickness), random intercept
  model8 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+chlorophyll+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  model8_information_criteria <- glance(model8) %>% 
    select(AIC,BIC) %>% 
    mutate(model="model8",
           number=i) 
  
  write_rds(model8, file = paste0("GLMM results/model8_", i, ".rds"))
  
  #model information criteria (AIC, BIC) 
  information_criteria_loop <- bind_rows(model1_information_criteria,
                                         model2_information_criteria,
                                         model3_information_criteria,
                                         model4_information_criteria,
                                         model5_information_criteria,
                                         model6_information_criteria,
                                         model7_information_criteria,
                                         model8_information_criteria)
  
  information_criteria <- bind_rows(information_criteria,information_criteria_loop)
  
  print(i)

}




a <- simulateResiduals(model6)

plot(a)

r2_nakagawa(model6)








































# 7 glmmTMB run paralell ---------------------------------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds")
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm,species_information)

# productivity_for_glmm_1 <- productivity_for_glmm %>% 
#   filter(stockid=="ACADREDGOMGB")
# 
# cor.test(productivity_for_glmm_1$`1`,productivity_for_glmm_1$`150`)

# hist(productivity_for_glmm$`1`,breaks = 100) #student t

#set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000,size=1000,replace = F)

#function for paralell run
# i=1
glmm_compare <- function(i) {
  
  #data for loop
  data_loop <- productivity_for_glmm %>% 
    select(year,stockid,primary_FAOarea,scientificname, #basic informaiton
           temperature,chlorophyll,mixedlayerthickness, #environment drivers
           as.character(resample_number[i])) %>% 
    rename(productivity=8)
  
  # #3 year moving avearage
  # data_loop <- data_loop %>% 
  #   group_by(stockid) %>% 
  #   mutate(temperature=zoo::rollmean(temperature,3,fill=NA),
  #          chlorophyll=zoo::rollmean(chlorophyll,3,fill=NA),
  #          mixedlayerthickness=zoo::rollmean(mixedlayerthickness,3,fill=NA),
  #          productivity=zoo::rollmean(productivity,3,fill=NA)) %>% 
  #   ungroup() %>% 
  #   drop_na()
  
  #model 1, fixed slope, fixed intercept
  model1 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness,
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model1_information_criteria <- glance(model1) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model1",
  #          number=i) 
  
  # write_rds(model1, file = paste0("GLMM results/model1_", i, ".rds"))
  
  #model 2, fixed slope, random intercept
  model2 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model2_information_criteria <- glance(model2) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model2",
  #          number=i) 
  # 
  # write_rds(model2, file = paste0("GLMM results/model2_", i, ".rds"))
  
  #model 3, random slope (one variable,temperature), random intercept
  model3 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model3_information_criteria <- glance(model3) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model3",
  #          number=i) 
  
  # write_rds(model3, file = paste0("GLMM results/model3_", i, ".rds"))
  
  #model 4, random slope (one variable, chlorophyll), random intercept
  model4 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+chlorophyll|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model4_information_criteria <- glance(model4) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model4",
  #          number=i) 
  
  # write_rds(model4, file = paste0("GLMM results/model4_", i, ".rds"))
  
  #model 5, random slope (one variable, mixedlayerthickness), random intercept
  model5 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model5_information_criteria <- glance(model5) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model5",
  #          number=i) 
  
  # write_rds(model5, file = paste0("GLMM results/model5_", i, ".rds"))
  
  #model 6, random slope (two variables, temperature+chlorophyll), random intercept
  model6 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature+chlorophyll|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model6_information_criteria <- glance(model6) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model6",
  #          number=i) 
  
  # write_rds(model6, file = paste0("GLMM results/model6_", i, ".rds"))
  
  #model 7, random slope (two variables, temperature+mixedlayerthickness), random intercept
  model7 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+temperature+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  # model7_information_criteria <- glance(model7) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model7",
  #          number=i) 
  
  # write_rds(model7, file = paste0("GLMM results/model7_", i, ".rds"))
  
  #model 8, random slope (two variables, chlorophyll+mixedlayerthickness), random intercept
  model8 <- glmmTMB(productivity~temperature+chlorophyll+mixedlayerthickness+
                      (1+chlorophyll+mixedlayerthickness|scientificname),
                    data=data_loop,REML=T,
                    family=t_family(link = "identity"))
  
  #list models
  model_results <- list(model1,model2,model3,model4,model5,model6,model7,model8)
  write_rds(model_results, file = paste0("GLMM results/model_results_", i, ".rds"))
  
  # model8_information_criteria <- glance(model8) %>% 
  #   select(AIC,BIC) %>% 
  #   mutate(model="model8",
  #          number=i) 
  
  # write_rds(model8, file = paste0("GLMM results/model8_", i, ".rds"))
  
  #model information criteria (AIC, BIC) 
  # information_criteria_loop <- bind_rows(model1_information_criteria,
  #                                        model2_information_criteria,
  #                                        model3_information_criteria,
  #                                        model4_information_criteria,
  #                                        model5_information_criteria,
  #                                        model6_information_criteria,
  #                                        model7_information_criteria,
  #                                        model8_information_criteria)
  # 
  # return(information_criteria_loop)
  
}

#test
# glmm_compare(1)


#run paralell
#how many cores can be used
detectCores() 

#use 8 cores
cl <- makeCluster(getOption("cl.cores", 8));

#register cores
registerDoParallel(cl)   

#run
foreach(i = c(1:8),
        .combine = "rbind",
        .packages = c("tidyverse","glmmTMB","broom.mixed")) %dopar% {
          
          # i=1
          try(glmm_compare(i))
          
        }
                                  
#save results
# write_rds(information_criteria, "GLMM results/model comparison.rds")   

#stop cluster
stopCluster(cl)                                
                                           

a <- read_rds("GLMM results/model_results_6.rds")


glance(a[[6]])


































# 8 GLMM comparison -------------------------------------------------------
#file names
file_names <- list.files("GLMM results/")

#loop read file and glance them
glmm_information <- NULL
for (i in 1:length(file_names)) {
  
  # i=1
  #read file
  glmm_results <- read_rds(file = paste0("GLMM results/", file_names[i]))
  
  #glance models
  glmm_information_loop <- bind_rows(glance(glmm_results[[1]]),
                                     glance(glmm_results[[2]]),
                                     glance(glmm_results[[3]]),
                                     glance(glmm_results[[4]]),
                                     glance(glmm_results[[5]]),
                                     glance(glmm_results[[6]]),
                                     glance(glmm_results[[7]]),
                                     glance(glmm_results[[8]]))
  
  glmm_information_loop <- glmm_information_loop %>% 
    mutate(loop=i,model=c(1:8))
  
  glmm_information <- bind_rows(glmm_information,glmm_information_loop)
  
}

#save results
write_rds(glmm_information, file = "GLMM results/glmm_information.rds")








# 9 Best GLMM results -----------------------------------------------------
#file names
file_names <- list.files("D:/GLMM results/")

#loop read file and glance them
fixed_effects <- NULL
random_effects <- NULL
length(file_names)
for (i in 1:3) {
  
  # i=1
  #read file
  glmm_results <- read_rds(file = paste0("D:/GLMM results/", file_names[i]))
  
  #best model is model 6
  best_glmm <- glmm_results[[6]]
  
  #fixed effects
  fixed_effects_loop <- tidy(best_glmm) %>% 
    mutate(loop=i)
  fixed_intercept <- filter(fixed_effects_loop,term=="(Intercept)")$estimate
  fixed_temperature <- filter(fixed_effects_loop,term=="temperature")$estimate
  fixed_chlorophyll <- filter(fixed_effects_loop,term=="chlorophyll")$estimate
  
  #random effects
  random_effects_loop <- ranef(best_glmm)[["cond"]]$scientificname 
  random_effects_loop <- rownames_to_column(random_effects_loop, var = "scientificname")
  
  random_effects_loop <- random_effects_loop %>%  #random effects adjustment, plus fixed effects
    mutate(temperature_adjusted=temperature+fixed_temperature,
           chlorophyll_adjusted=chlorophyll+fixed_chlorophyll) %>% 
    mutate(loop=i)
  
  #combine data
  fixed_effects <- bind_rows(fixed_effects,fixed_effects_loop)
  random_effects <- bind_rows(random_effects,random_effects_loop)
 
  print(i)
  
}

#save results
write_rds(fixed_effects, file = "GLMM results/fixed_effects.rds")
write_rds(random_effects, file = "GLMM results/random_effects.rds")




# 10 Best GLMM diagnostics ------------------------------------------------
#file names
file_names <- list.files("D:/GLMM results/")

for (i in 1:3) {
  
  # i=401
  #read file
  glmm_results <- read_rds(file = paste0("D:/GLMM results/", file_names[i]))
  
  glmm_results_1 <- read_rds(file = paste0("D:/GLMM results/", file_names[1]))
  glmm_results_2 <- read_rds(file = paste0("D:/GLMM results/", file_names[2]))
  
  
  #best model is model 6
  best_glmm <- glmm_results[[6]]
  best_glmm_1 <- glmm_results_1[[6]]
  best_glmm_2 <- glmm_results_2[[6]]
  
  diagnose(best_glmm_1)
  simres <- simulateResiduals(best_glmm)
  simres_1 <- simulateResiduals(best_glmm_1,n = 10000)
  simres_2 <- simulateResiduals(best_glmm_2,n = 10000)
  
  testDispersion(simres_1)
  testUniformity(simres_1) 
  testUniformity(simres_2) 
  testOutliers(simres)
  
  plot(simres_1)  
  plot(simres_2)  
  # plotQQunif(simres) # left plot in plot.DHARMa()

  print(i)
  
}










