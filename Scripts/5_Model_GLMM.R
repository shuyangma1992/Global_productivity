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

# 1 glmmTMB run paralell ---------------------------------------------------------------
productivity_for_glmm <- read_rds("Outputs/Productivity/productivity_for_glmm.rds")
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm, species_information)

unique(productivity_for_glmm$stockid) # 652 stocks
unique(productivity_for_glmm$scientificname) # 265 species

# # distribution
# productivity_for_glmm_1 <- productivity_for_glmm %>%
#   filter(stockid=="ACADREDGOMGB")
# hist(productivity_for_glmm$`3568`,breaks = 100) #student t

# set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

# function for parallel run
# i=1
glmm_compare <- function(i) {
  # data for loop
  data_loop <- productivity_for_glmm %>%
    select(
      year, stockid, primary_FAOarea, scientificname, # basic information
      temperature, chlorophyll, mixedlayerthickness, # environment drivers
      as.character(resample_number[i])
    ) %>%
    rename(productivity = 8)

  # #3 year moving avearage
  # data_loop <- data_loop %>%
  #   group_by(stockid) %>%
  #   mutate(temperature=zoo::rollmean(temperature,3,fill=NA),
  #          chlorophyll=zoo::rollmean(chlorophyll,3,fill=NA),
  #          mixedlayerthickness=zoo::rollmean(mixedlayerthickness,3,fill=NA),
  #          productivity=zoo::rollmean(productivity,3,fill=NA)) %>%
  #   ungroup() %>%
  #   drop_na()

  # model 1, fixed slope, fixed intercept
  model1 <- glmmTMB(productivity ~ temperature + chlorophyll + mixedlayerthickness,
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model1_information_criteria <- glance(model1) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model1",
  #          number=i)

  # write_rds(model1, file = paste0("GLMM results/model1_", i, ".rds"))

  # model 2, fixed slope, random intercept
  model2 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model2_information_criteria <- glance(model2) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model2",
  #          number=i)
  #
  # write_rds(model2, file = paste0("GLMM results/model2_", i, ".rds"))

  # model 3, random slope (one variable,temperature), random intercept
  model3 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + temperature | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model3_information_criteria <- glance(model3) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model3",
  #          number=i)

  # write_rds(model3, file = paste0("GLMM results/model3_", i, ".rds"))

  # model 4, random slope (one variable, chlorophyll), random intercept
  model4 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + chlorophyll | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model4_information_criteria <- glance(model4) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model4",
  #          number=i)

  # write_rds(model4, file = paste0("GLMM results/model4_", i, ".rds"))

  # model 5, random slope (one variable, mixedlayerthickness), random intercept
  model5 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + mixedlayerthickness | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model5_information_criteria <- glance(model5) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model5",
  #          number=i)

  # write_rds(model5, file = paste0("GLMM results/model5_", i, ".rds"))

  # model 6, random slope (two variables, temperature+chlorophyll), random intercept
  model6 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + temperature + chlorophyll | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model6_information_criteria <- glance(model6) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model6",
  #          number=i)

  # write_rds(model6, file = paste0("GLMM results/model6_", i, ".rds"))

  # model 7, random slope (two variables, temperature+mixedlayerthickness), random intercept
  model7 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + temperature + mixedlayerthickness | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # model7_information_criteria <- glance(model7) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model7",
  #          number=i)

  # write_rds(model7, file = paste0("GLMM results/model7_", i, ".rds"))

  # model 8, random slope (two variables, chlorophyll+mixedlayerthickness), random intercept
  model8 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + chlorophyll + mixedlayerthickness | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # list models
  model_results <- list(model1, model2, model3, model4, model5, model6, model7, model8)
  write_rds(model_results, file = paste0("Outputs/GLMM results/model_results_", i, ".rds"))

  # model8_information_criteria <- glance(model8) %>%
  #   select(AIC,BIC) %>%
  #   mutate(model="model8",
  #          number=i)

  # write_rds(model8, file = paste0("GLMM results/model8_", i, ".rds"))

  # model information criteria (AIC, BIC)
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

# test
# glmm_compare(1)

# run parallel
# how many cores can be used
detectCores()

# use 8 cores
cl <- makeCluster(getOption("cl.cores", 20))

# register cores
registerDoParallel(cl)

# run
foreach(
  i = c(1:1000),
  .combine = "rbind",
  .packages = c("tidyverse", "glmmTMB", "broom.mixed")
) %dopar% {
  # i=1
  try(glmm_compare(i))
}

# save results
# write_rds(information_criteria, "GLMM results/model comparison.rds")

# stop cluster
stopCluster(cl)                         


# 2 GLMM comparison -------------------------------------------------------
#file names
file_names <- list.files("Outputs/GLMM results/")

#loop read file and glance them
glmm_information <- NULL
for (i in 1:length(file_names)) {
  
  # i=1
  # read file
  glmm_results <- read_rds(file = paste0("Outputs/GLMM results/", file_names[i]))
  
  # glance models
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
  
  print(i)
}

#save results
write_rds(glmm_information, file = "Outputs/GLMM results/glmm_information.rds")

# 3 Best GLMM results -----------------------------------------------------
# file names
file_names <- list.files("Outputs/GLMM results/")[-1]

# loop read file and glance them
fixed_effects <- NULL
random_effects <- NULL
length(file_names)
for (i in 1:length(file_names)) {
  
  # i=1
  # read file
  glmm_results <- read_rds(file = paste0("Outputs/GLMM results/", file_names[i]))
  
  # best model is model 6
  best_glmm <- glmm_results[[6]]
  
  # fixed effects
  fixed_effects_loop <- tidy(best_glmm) %>% 
    mutate(loop=i)
  fixed_intercept <- filter(fixed_effects_loop,term=="(Intercept)")$estimate
  fixed_temperature <- filter(fixed_effects_loop,term=="temperature")$estimate
  fixed_chlorophyll <- filter(fixed_effects_loop,term=="chlorophyll")$estimate
  
  # random effects
  random_effects_loop <- ranef(best_glmm)[["cond"]]$scientificname 
  random_effects_loop <- rownames_to_column(random_effects_loop, var = "scientificname")
  
  random_effects_loop <- random_effects_loop %>%  #random effects adjustment, plus fixed effects
    mutate(temperature_adjusted=temperature+fixed_temperature,
           chlorophyll_adjusted=chlorophyll+fixed_chlorophyll) %>% 
    mutate(loop=i)
  
  # combine data
  fixed_effects <- bind_rows(fixed_effects,fixed_effects_loop)
  random_effects <- bind_rows(random_effects,random_effects_loop)
 
  print(i)
  
}

#save results
write_rds(fixed_effects, file = "Outputs/GLMM results/fixed_effects.rds")
write_rds(random_effects, file = "Outputs/GLMM results/random_effects.rds")


# 999 Best GLMM diagnostics ------------------------------------------------
# file names
file_names <- list.files("Outputs/GLMM results/")
file_names <- file_names[!file_names %in% c(
  "glmm_information.rds",
  "fixed_effects.rds",
  "random_effects.rds"
)]

for (i in 1:1000) {
  
  # i = 1
  # read file
  glmm_results <- read_rds(file = paste0("Outputs/GLMM results/", file_names[i]))
  
  glmm_results_1 <- read_rds(file = paste0("D:/GLMM results/", file_names[1]))
  glmm_results_2 <- read_rds(file = paste0("D:/GLMM results/", file_names[2]))
  
  
  #best model is model 6
  best_glmm <- glmm_results[[6]]
  best_glmm_1 <- glmm_results_1[[6]]
  best_glmm_2 <- glmm_results_2[[6]]
  
  diagnose(best_glmm)
  simres <- simulateResiduals(best_glmm)
  testDispersion(simres)
  testUniformity(simres) 
  
  simres_1 <- simulateResiduals(best_glmm_1,n = 10000)
  simres_2 <- simulateResiduals(best_glmm_2,n = 10000)
  
  testDispersion(simres_1)
  testUniformity(simres_1) 
  testUniformity(simres_2) 
  testOutliers(simres)
  
  plot(simres)  
  plot(simres_2)  
  # plotQQunif(simres) # left plot in plot.DHARMa()

  print(i)
  
}










# 999 glmmTMB ---------------------------------------------------------------
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








































# 999 GLM by stock ----------------------------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds")


#set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000,size=1000,replace = F)

#loop
model_results <- NULL 
for (i in 1:1000) {
  
  # i=2
  
  #data for loop
  data_loop_1 <- productivity_for_glmm %>% 
    select(year,stockid,primary_FAOarea, #basic informaiton
           temperature,salinity,seasurfaceheight,mixedlayerthickness, #physical drivers
           chlorophyll,dissolvedoxygen,primaryproduction,ph, #biogeochemical drivers
           as.character(resample_number[i])) %>% 
    rename(productivity=12)
  
  model_results_loop1 <- NULL
  for (j in unique(productivity_for_glmm$stockid[153])) {
    
    # j <- unique(productivity_for_glmm$stockid)[1]
    
    data_loop_2 <- data_loop_1 %>% 
      filter(stockid==j) %>% 
      mutate(temperature=scale(zoo::rollmean(temperature,3,fill=NA)),
             salinity=scale(zoo::rollmean(salinity,3,fill=NA)),
             seasurfaceheight=scale(zoo::rollmean(seasurfaceheight,3,fill=NA)),
             mixedlayerthickness=scale(zoo::rollmean(mixedlayerthickness,3,fill=NA)),
             chlorophyll=scale(zoo::rollmean(chlorophyll,3,fill=NA)),
             dissolvedoxygen=scale(zoo::rollmean(dissolvedoxygen,3,fill=NA)),
             primaryproduction=scale(zoo::rollmean(primaryproduction,3,fill=NA)),
             ph=scale(zoo::rollmean(ph,3,fill=NA)),
             productivity=scale(zoo::rollmean(productivity,3,fill=NA))) %>% 
      drop_na()
    
    #full model
    full_mod_loop <- glm(productivity~temperature+mixedlayerthickness+chlorophyll,
                         data=data_loop_2)
    
    # plot(full_mod_loop)
    
    #selection by step
    # best_mod_loop <- step(full_mod_loop)
    # summary(best_mod_loop)
    
    #selection by stepCriterion
    best_model_formula <- stepCriterion(full_mod_loop, criterion="aic")

    best_mod_loop <- glm(paste0("productivity",best_model_formula$final),
                         data=data_loop_2)
    
    model_results_loop2 <- tidy(best_mod_loop) %>% 
      mutate(stockid=j,
             timeseries_number=i)
       
    model_results_loop1 <- bind_rows(model_results_loop1,model_results_loop2)
  }
  
  model_results <- bind_rows(model_results,model_results_loop1)
  print(i)
}  

model_results %>% 
  group_by(term) %>% 
  summarise(p90=quantile(p.value,0.90))



a <- model_results %>% 
  filter(stockid=="ACADRED2J3K") 
 












# 999 case ----------------------------------------------------------------
#fit model
fit_test <- glmmTMB(`2`~temperature+chlorophyll+mixedlayerthickness+(1+temperature|stockid),
                    dispformula=~stockid,
                    data=productivity_for_glmm,REML=F,
                    family="gaussian")

summary(fit_test)
glance(fit_test)
r2_nakagawa(fit_test,tolerance = 1e-10000)

simulateResiduals(fittedModel = fit_test, plot = T)

#fixed effects
a <- fixef(fit_test)
a <- broom.mixed::tidy(fit_test)

#random effects
b <- ranef(fit_test)



# 999 case model for each stock -------------------------------------------
productivity_for_glmm <- read_rds("Data/producitivity_for_glmm.rds")

#set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000,size=1000,replace = F)

i=1

#data for loop
data_loop <- productivity_for_glmm %>% 
  select(year,stockid,primary_FAOarea, #basic informaiton
         temperature,chlorophyll,mixedlayerthickness, #environment drivers
         as.character(resample_number[i])) %>% 
  rename(productivity=7)

#scale
data_loop <- data_loop %>% 
  group_by(stockid) %>% 
  mutate(temperature=scale(temperature),
         chlorophyll=scale(chlorophyll),
         mixedlayerthickness=scale(mixedlayerthickness),
         productivity=scale(productivity)) %>% 
  ungroup() %>% 
  drop_na()

#case data
data_case <- data_loop %>% 
  filter(stockid=="ACADRED2J3K")

a <- glm(productivity~temperature,
         data=data_case,family="gaussian")
b <- glm(productivity~temperature+chlorophyll,
         data=data_case,family="gaussian")
c <- glm(productivity~temperature+chlorophyll+mixedlayerthickness,
         data=data_case,family="gaussian")

glance(c)

tidy(c)
summary(a)
r2(c)

plot(c)

# 9999  different model structure--------------------------------------------------------------------

##fixed effects contain temperature and mixed layer depth

#model 4, fixed slope (temperature + mixedlayerthickness), random intercept on stockid
model4 <- glmmTMB(productivity_3year~temperature_3year+mixedlayerthickness_3year+(1|stockid),
                  data=data_loop,REML=T,
                  family=t_family(link = "identity"))

model4_information_criteria <- glance(model4) %>% 
  select(AIC,BIC) %>% 
  mutate(model="model4",
         number=i) 

#model 5, fixed slope (temperature + mixedlayerthickness), random intercept on FAOarea
model5 <- glmmTMB(productivity~temperature+mixedlayerthickness+(1|primary_FAOarea),
                  data=data_loop,REML=T,
                  family="gaussian")

model5_information_criteria <- glance(model5) %>% 
  select(AIC,BIC) %>% 
  mutate(model="model5",
         number=i) 

#model 6, random slope (temperature), random intercept on stockid
model6 <- glmmTMB(productivity~temperature+mixedlayerthickness+(1+temperature|stockid),
                  data=data_loop,REML=T,
                  family="gaussian")

model6_information_criteria <- glance(model6) %>% 
  select(AIC,BIC) %>% 
  mutate(model="model6",
         number=i) 

#model 7, random slope (temperature+mixedlayerdepth), random intercept on stockid
model7 <- glmmTMB(productivity~temperature+mixedlayerthickness+(1+temperature+mixedlayerthickness|stockid),
                  data=data_loop,REML=T,
                  family=t_family(link = "identity"),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method="BFGS")))

model7_scale <- glmmTMB(productivity_scale~temperature_scale+mixedlayerthickness_scale+(1+temperature_scale+mixedlayerthickness_scale|stockid),
                        data=data_loop,REML=T,
                        family=t_family(link = "identity"),
                        control = glmmTMBControl(optimizer = optim,
                                                 optArgs = list(method="BFGS")))

model7_information_criteria <- glance(model7) %>% 
  select(AIC,BIC) %>% 
  mutate(model="model7",
         number=i) 

#model 8, random slope (temperature+mixedlayerdepth), random intercept on stockid, heterogenity
model8 <- glmmTMB(productivity~temperature+mixedlayerthickness+(1+temperature+mixedlayerthickness|stockid),
                  dispformula=~stockid,
                  data=data_loop,REML=T,
                  # family="gaussian")
                  family=t_family(link = "identity"),
                  control = glmmTMBControl(optimizer = optim,
                                           optArgs = list(method="BFGS")))


model8_information_criteria <- glance(model8) %>%
  select(AIC,BIC) %>%
  mutate(model="model8",
         number=i)


# 9999999smooth -----------------------------------------------------------
model6 <- glmmTMB(productivity~s(temperature, k=4)+s(chlorophyll, k=4)+s(mixedlayerthickness, k=4)+
                    (1|scientificname),
                  data=data_loop,REML=T,
                  family=t_family(link = "identity"))

summary(model6)

