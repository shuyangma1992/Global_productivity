library(tidyverse)
library(glmmTMB)
library(doParallel)
library(foreach)

# 1 Data compile --------------------------------------------
# species name
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(GRSF_uuid, stockid, scientificname)

# CMIP6 data change chl
CMIP6_Nor <- read_rds("Data/CMIP6_NorESM2-LM_for_glmm_projection.rds") %>%
  left_join(species_information) %>%
  mutate(chl = chl * 1000 * 1000) %>% # kg to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)
CMIP6_MPI <- read_rds("Data/CMIP6_MPI-ESM1-2-LR_for_glmm_projection.rds") %>%
  left_join(species_information) %>%
  mutate(chl = chl * 1000 * 1000) %>% # kg to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)
CMIP6_IPSL <- read_rds("Data/CMIP6_IPSL-CM6A-LR_for_glmm_projection.rds") %>%
  left_join(species_information) %>%
  mutate(chl = chl * 1000) %>% # g to mg
  rename("temperature" = thetao, "chlorophyll" = chl, "mixedlayerthickness" = mlotst)

CMIP6_Nor <- CMIP6_Nor %>%
  drop_na()
unique(CMIP6_Nor$stockid) # 616 stocks
unique(CMIP6_Nor$scientificname) # 259 species

CMIP6_MPI <- CMIP6_MPI %>%
  drop_na()
unique(CMIP6_MPI$stockid) # 610 stocks
unique(CMIP6_MPI$scientificname) # 258 species

CMIP6_IPSL <- CMIP6_IPSL %>%
  drop_na()
unique(CMIP6_IPSL$stockid) # 658 stocks
unique(CMIP6_IPSL$scientificname) # 266 species, a new species

# 2 glmmTMB projection ---------------------------------------------------------------
productivity_for_glmm <- read_rds("Outputs/Productivity/productivity_for_glmm.rds")
productivity_for_glmm <- productivity_for_glmm %>% 
  drop_na()
species_information <- read_rds("Data/stock_success_full_information_final.rds") %>% 
  select(stockid,scientificname)
productivity_for_glmm <- left_join(productivity_for_glmm,species_information)
unique(productivity_for_glmm$scientificname) # 265 species

#adjust data ipsl as a new species in it
CMIP6_IPSL <- CMIP6_IPSL %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_IPSL$stockid) # 657 stocks
unique(CMIP6_IPSL$scientificname) # 265 species

CMIP6_MPI <- CMIP6_MPI %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_MPI$stockid) # 608 stocks
unique(CMIP6_MPI$scientificname) # 256 species

CMIP6_Nor <- CMIP6_Nor %>% 
  filter(scientificname %in% unique(productivity_for_glmm$scientificname))
unique(CMIP6_Nor$stockid) # 614 stocks
unique(CMIP6_Nor$scientificname) # 257 species

# set resample seed
set.seed(1026)
resample_number <- sample(x = 1:6000, size = 1000, replace = F)

# how many cores can be used
detectCores()

# use 20 cores
cl <- makeCluster(getOption("cl.cores", 20))

# register cores
registerDoParallel(cl)

# run parallel
productivity_projection <- foreach(
  i = 1:1000,
  .combine = function(x, y) {left_join(x, y)},
  .packages = c("glmmTMB", "tidyverse")
) %dopar% {
  # data for loop
  data_loop <- productivity_for_glmm %>%
    select(
      year, stockid, primary_FAOarea, scientificname, # basic information
      temperature, chlorophyll, mixedlayerthickness, # environment drivers
      as.character(resample_number[i])
    ) %>%
    rename(productivity = 8)

  # best model
  model6 <- glmmTMB(
    productivity ~ temperature + chlorophyll + mixedlayerthickness +
      (1 + temperature + chlorophyll | scientificname),
    data = data_loop, REML = T,
    family = t_family(link = "identity")
  )

  # NorESM-LM
  productivity_projection_Nor_loop <- CMIP6_Nor %>%
    mutate(predict(model6, newdata = CMIP6_Nor, allow.new.levels = FALSE))
  colnames(productivity_projection_Nor_loop)[10] <- i # rename with number

  # MPI-ESM1-2-LR
  productivity_projection_MPI_loop <- CMIP6_MPI %>%
    mutate(predict(model6, newdata = CMIP6_MPI, allow.new.levels = FALSE))
  colnames(productivity_projection_MPI_loop)[10] <- i # rename with number

  # IPSL-CM6A-LR
  productivity_projection_IPSL_loop <- CMIP6_IPSL %>%
    mutate(predict(model6, newdata = CMIP6_IPSL, allow.new.levels = FALSE))
  colnames(productivity_projection_IPSL_loop)[10] <- i # rename with number

  productivity_projection_loop <- bind_rows(
    productivity_projection_Nor_loop,
    productivity_projection_MPI_loop,
    productivity_projection_IPSL_loop
  )
}

write_rds(productivity_projection, file = "Outputs/GLMM projection results/productivity_projection.rds")

# 
# 
# # loop
# productivity_projection_Nor <- CMIP6_Nor
# productivity_projection_MPI <- CMIP6_MPI
# productivity_projection_IPSL <- CMIP6_IPSL 
# for (i in 1:1000) {
#   
#   # i=1
#   
#   #data for loop
#   data_loop <- productivity_for_glmm %>% 
#     select(year,stockid,primary_FAOarea,scientificname, #basic information
#            temperature,chlorophyll,mixedlayerthickness, #environment drivers
#            as.character(resample_number[i])) %>% 
#     rename(productivity=8)
#   
#   # #3 year moving avearage
#   # data_loop <- data_loop %>% 
#   #   group_by(stockid) %>% 
#   #   mutate(temperature=zoo::rollmean(temperature,3,fill=NA),
#   #          chlorophyll=zoo::rollmean(chlorophyll,3,fill=NA),
#   #          mixedlayerthickness=zoo::rollmean(mixedlayerthickness,3,fill=NA),
#   #          productivity=zoo::rollmean(productivity,3,fill=NA)) %>% 
#   #   ungroup() %>% 
#   #   drop_na()
#   # best model
#   # model 6, random slope (two variables, temperature+chlorophyll), random intercept
#   model6 <- glmmTMB(
#     productivity ~ temperature + chlorophyll + mixedlayerthickness +
#       (1 + temperature + chlorophyll | scientificname),
#     data = data_loop, REML = T,
#     family = t_family(link = "identity")
#   )
#   
#   #NorESM-LM
#   productivity_projection_Nor_loop <- CMIP6_Nor %>% 
#     mutate(predict(model6, newdata = CMIP6_Nor, allow.new.levels=FALSE)) 
#   colnames(productivity_projection_Nor_loop)[10] <- i #rename with number
#   productivity_projection_Nor <- left_join(productivity_projection_Nor, productivity_projection_Nor_loop)
#     
#   #MPI-ESM1-2-LR
#   productivity_projection_MPI_loop <- CMIP6_MPI %>% 
#     mutate(predict(model6, newdata = CMIP6_MPI, allow.new.levels=FALSE)) 
#   colnames(productivity_projection_MPI_loop)[10] <- i #rename with number
#   productivity_projection_MPI <- left_join(productivity_projection_MPI, productivity_projection_MPI_loop)
#   
#   #IPSL-CM6A-LR
#   productivity_projection_IPSL_loop <- CMIP6_IPSL %>% 
#     mutate(predict(model6, newdata = CMIP6_IPSL, allow.new.levels=FALSE)) 
#   colnames(productivity_projection_IPSL_loop)[10] <- i #rename with number
#   productivity_projection_IPSL <- left_join(productivity_projection_IPSL, productivity_projection_IPSL_loop)
#   print(i)
# 
# }
# 
# write_rds(productivity_projection_Nor, file = "Outputs/GLMM projection results/productivity_projection_NorESM2-LM.rds")
# write_rds(productivity_projection_MPI, file = "Outputs/GLMM projection results/productivity_projection_MPI-ESM1-2-LR.rds")
# write_rds(productivity_projection_IPSL, file = "Outputs/GLMM projection results/productivity_projection_IPSL-CM6A-LR.rds")

# 3 Results inspection ----------------------------------------------------
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds") %>%
  drop_na()

productivity_projection_Nor <- productivity_projection %>%
  filter(ESM == "NorESM2-LM")

unique(productivity_projection_Nor$stockid) # 614 stocks
unique(productivity_projection_Nor$scientificname) # 257 species

productivity_projection_MPI <- productivity_projection %>%
  filter(ESM == "MPI-ESM1-2-LR")
unique(productivity_projection_MPI$stockid) # 608 stocks
unique(productivity_projection_MPI$scientificname) # 256 species

productivity_projection_IPSL <- productivity_projection %>%
  filter(ESM == "IPSL-CM6A-LR")

unique(productivity_projection_IPSL$stockid) # 657 stocks
unique(productivity_projection_IPSL$scientificname) # 265 species

# 4 Productivity projection trend 2021-2100 ------------------------------------------
# productivity projection 2021-2100
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds") %>%
  drop_na()
productivity_projection_Nor <- productivity_projection %>%
  filter(ESM == "NorESM2-LM")
productivity_projection_MPI <- productivity_projection %>%
  filter(ESM == "MPI-ESM1-2-LR")
productivity_projection_IPSL <- productivity_projection %>%
  filter(ESM == "IPSL-CM6A-LR")

stocks <- unique(productivity_projection$stockid)

# SSP1-2.6
productivity_projection_trend_ssp126 <- NULL
for (i in stocks) {
  # i <- stocks[1]

  # SSP1-2.6
  productivity_loop_ssp126_Nor <- filter(productivity_projection_Nor, stockid == i & ssp == "ssp126") %>%
    filter(year > 2020)
  productivity_loop_ssp126_MPI <- filter(productivity_projection_MPI, stockid == i & ssp == "ssp126") %>%
    filter(year > 2020)
  productivity_loop_ssp126_IPSL <- filter(productivity_projection_IPSL, stockid == i & ssp == "ssp126") %>%
    filter(year > 2020)

  if (is.na(productivity_loop_ssp126_Nor[10, 10]) | is.na(productivity_loop_ssp126_MPI[10, 10]) | is.na(productivity_loop_ssp126_IPSL[10, 10])) {
    next
  } else {
    # linear regression on year (trend), Nor
    lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp126_Nor[, 10:1009]) ~ productivity_loop_ssp126_Nor$year))
    p_slope_Nor <- NULL
    p_slope_significance_Nor <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_Nor <- c(p_slope_Nor, lm_results_Nor[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_Nor <- c(p_slope_significance_Nor, lm_results_Nor[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_Nor <- quantile(p_slope_Nor, 0.025)
    p_slope_mean_Nor <- mean(p_slope_Nor)
    p_slope_uci_Nor <- quantile(p_slope_Nor, 0.975)

    p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor, 0.95)

    # make a data frame
    productivity_projection_trend_ssp126_loop_Nor <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_Nor,
      p_slope_mean = p_slope_mean_Nor,
      p_slope_uci = p_slope_uci_Nor,
      p_slope_significance_95 = p_slope_significance_95_Nor,
      ESM = "NorESM2-LM",
      ssp = "ssp126"
    )

    # linear regression on year (trend), MPI
    lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp126_MPI[, 10:1009]) ~ productivity_loop_ssp126_MPI$year))
    p_slope_MPI <- NULL
    p_slope_significance_MPI <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_MPI <- c(p_slope_MPI, lm_results_MPI[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_MPI <- c(p_slope_significance_MPI, lm_results_MPI[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_MPI <- quantile(p_slope_MPI, 0.025)
    p_slope_mean_MPI <- mean(p_slope_MPI)
    p_slope_uci_MPI <- quantile(p_slope_MPI, 0.975)

    p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI, 0.95)

    # make a data frame
    productivity_projection_trend_ssp126_loop_MPI <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_MPI,
      p_slope_mean = p_slope_mean_MPI,
      p_slope_uci = p_slope_uci_MPI,
      p_slope_significance_95 = p_slope_significance_95_MPI,
      ESM = "MPI-ESM1-2-LR",
      ssp = "ssp126"
    )

    # linear regression on year (trend), IPSL
    lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp126_IPSL[, 10:1009]) ~ productivity_loop_ssp126_IPSL$year))
    p_slope_IPSL <- NULL
    p_slope_significance_IPSL <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_IPSL <- c(p_slope_IPSL, lm_results_IPSL[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_IPSL <- c(p_slope_significance_IPSL, lm_results_IPSL[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_IPSL <- quantile(p_slope_IPSL, 0.025)
    p_slope_mean_IPSL <- mean(p_slope_IPSL)
    p_slope_uci_IPSL <- quantile(p_slope_IPSL, 0.975)

    p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL, 0.95)

    # make a data frame
    productivity_projection_trend_ssp126_loop_IPSL <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_IPSL,
      p_slope_mean = p_slope_mean_IPSL,
      p_slope_uci = p_slope_uci_IPSL,
      p_slope_significance_95 = p_slope_significance_95_IPSL,
      ESM = "IPSL-CM6A-LR",
      ssp = "ssp126"
    )

    # combind data
    productivity_projection_trend_ssp126 <- bind_rows(
      productivity_projection_trend_ssp126,
      productivity_projection_trend_ssp126_loop_Nor,
      productivity_projection_trend_ssp126_loop_MPI,
      productivity_projection_trend_ssp126_loop_IPSL
    )
  }

  print(i)
}

# save data
write_rds(productivity_projection_trend_ssp126,
  file = "OutputS/GLMM projection results/productivity_projection_trend_ssp126_2021_2100.rds"
)

# SSP2-4.5
productivity_projection_trend_ssp245 <- NULL
for (i in stocks) {
  # i <- unique(productivity_projection_Nor$stockid)[1]

  # SSP1-2.6
  productivity_loop_ssp245_Nor <- filter(productivity_projection_Nor, stockid == i & ssp == "ssp245") %>%
    filter(year > 2020)
  productivity_loop_ssp245_MPI <- filter(productivity_projection_MPI, stockid == i & ssp == "ssp245") %>%
    filter(year > 2020)
  productivity_loop_ssp245_IPSL <- filter(productivity_projection_IPSL, stockid == i & ssp == "ssp245") %>%
    filter(year > 2020)

  if (is.na(productivity_loop_ssp245_Nor[10, 10]) | is.na(productivity_loop_ssp245_MPI[10, 10]) | is.na(productivity_loop_ssp245_IPSL[10, 10])) {
    next
  } else {
    # linear regression on year (trend), Nor
    lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp245_Nor[, 10:1009]) ~ productivity_loop_ssp245_Nor$year))
    p_slope_Nor <- NULL
    p_slope_significance_Nor <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_Nor <- c(p_slope_Nor, lm_results_Nor[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_Nor <- c(p_slope_significance_Nor, lm_results_Nor[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_Nor <- quantile(p_slope_Nor, 0.025)
    p_slope_mean_Nor <- mean(p_slope_Nor)
    p_slope_uci_Nor <- quantile(p_slope_Nor, 0.975)

    p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor, 0.95)

    # make a data frame
    productivity_projection_trend_ssp245_loop_Nor <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_Nor,
      p_slope_mean = p_slope_mean_Nor,
      p_slope_uci = p_slope_uci_Nor,
      p_slope_significance_95 = p_slope_significance_95_Nor,
      ESM = "NorESM2-LM",
      ssp = "ssp245"
    )

    # linear regression on year (trend), MPI
    lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp245_MPI[, 10:1009]) ~ productivity_loop_ssp245_MPI$year))
    p_slope_MPI <- NULL
    p_slope_significance_MPI <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_MPI <- c(p_slope_MPI, lm_results_MPI[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_MPI <- c(p_slope_significance_MPI, lm_results_MPI[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_MPI <- quantile(p_slope_MPI, 0.025)
    p_slope_mean_MPI <- mean(p_slope_MPI)
    p_slope_uci_MPI <- quantile(p_slope_MPI, 0.975)

    p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI, 0.95)

    # make a data frame
    productivity_projection_trend_ssp245_loop_MPI <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_MPI,
      p_slope_mean = p_slope_mean_MPI,
      p_slope_uci = p_slope_uci_MPI,
      p_slope_significance_95 = p_slope_significance_95_MPI,
      ESM = "MPI-ESM1-2-LR",
      ssp = "ssp245"
    )

    # linear regression on year (trend), IPSL
    lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp245_IPSL[, 10:1009]) ~ productivity_loop_ssp245_IPSL$year))
    p_slope_IPSL <- NULL
    p_slope_significance_IPSL <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_IPSL <- c(p_slope_IPSL, lm_results_IPSL[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_IPSL <- c(p_slope_significance_IPSL, lm_results_IPSL[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_IPSL <- quantile(p_slope_IPSL, 0.025)
    p_slope_mean_IPSL <- mean(p_slope_IPSL)
    p_slope_uci_IPSL <- quantile(p_slope_IPSL, 0.975)

    p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL, 0.95)

    # make a data frame
    productivity_projection_trend_ssp245_loop_IPSL <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_IPSL,
      p_slope_mean = p_slope_mean_IPSL,
      p_slope_uci = p_slope_uci_IPSL,
      p_slope_significance_95 = p_slope_significance_95_IPSL,
      ESM = "IPSL-CM6A-LR",
      ssp = "ssp245"
    )

    # combind data
    productivity_projection_trend_ssp245 <- bind_rows(
      productivity_projection_trend_ssp245,
      productivity_projection_trend_ssp245_loop_Nor,
      productivity_projection_trend_ssp245_loop_MPI,
      productivity_projection_trend_ssp245_loop_IPSL
    )
  }

  print(i)
}

# save data
write_rds(productivity_projection_trend_ssp245,
  file = "OutputS/GLMM projection results/productivity_projection_trend_ssp245_2021_2100.rds"
)



# SSP5-8.5
productivity_projection_trend_ssp585 <- NULL
for (i in stocks) {
  # i <- unique(productivity_projection_Nor$stockid)[1]

  # SSP1-2.6
  productivity_loop_ssp585_Nor <- filter(productivity_projection_Nor, stockid == i & ssp == "ssp585") %>%
    filter(year > 2020)
  productivity_loop_ssp585_MPI <- filter(productivity_projection_MPI, stockid == i & ssp == "ssp585") %>%
    filter(year > 2020)
  productivity_loop_ssp585_IPSL <- filter(productivity_projection_IPSL, stockid == i & ssp == "ssp585") %>%
    filter(year > 2020)

  if (is.na(productivity_loop_ssp585_Nor[10, 10]) | is.na(productivity_loop_ssp585_MPI[10, 10]) | is.na(productivity_loop_ssp585_IPSL[10, 10])) {
    next
  } else {
    # linear regression on year (trend), Nor
    lm_results_Nor <- summary(lm(as.matrix(productivity_loop_ssp585_Nor[, 10:1009]) ~ productivity_loop_ssp585_Nor$year))
    p_slope_Nor <- NULL
    p_slope_significance_Nor <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_Nor <- c(p_slope_Nor, lm_results_Nor[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_Nor <- c(p_slope_significance_Nor, lm_results_Nor[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_Nor <- quantile(p_slope_Nor, 0.025)
    p_slope_mean_Nor <- mean(p_slope_Nor)
    p_slope_uci_Nor <- quantile(p_slope_Nor, 0.975)

    p_slope_significance_95_Nor <- quantile(p_slope_significance_Nor, 0.95)

    # make a data frame
    productivity_projection_trend_ssp585_loop_Nor <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_Nor,
      p_slope_mean = p_slope_mean_Nor,
      p_slope_uci = p_slope_uci_Nor,
      p_slope_significance_95 = p_slope_significance_95_Nor,
      ESM = "NorESM2-LM",
      ssp = "ssp585"
    )

    # linear regression on year (trend), MPI
    lm_results_MPI <- summary(lm(as.matrix(productivity_loop_ssp585_MPI[, 10:1009]) ~ productivity_loop_ssp585_MPI$year))
    p_slope_MPI <- NULL
    p_slope_significance_MPI <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_MPI <- c(p_slope_MPI, lm_results_MPI[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_MPI <- c(p_slope_significance_MPI, lm_results_MPI[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_MPI <- quantile(p_slope_MPI, 0.025)
    p_slope_mean_MPI <- mean(p_slope_MPI)
    p_slope_uci_MPI <- quantile(p_slope_MPI, 0.975)

    p_slope_significance_95_MPI <- quantile(p_slope_significance_MPI, 0.95)

    # make a data frame
    productivity_projection_trend_ssp585_loop_MPI <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_MPI,
      p_slope_mean = p_slope_mean_MPI,
      p_slope_uci = p_slope_uci_MPI,
      p_slope_significance_95 = p_slope_significance_95_MPI,
      ESM = "MPI-ESM1-2-LR",
      ssp = "ssp585"
    )

    # linear regression on year (trend), IPSL
    lm_results_IPSL <- summary(lm(as.matrix(productivity_loop_ssp585_IPSL[, 10:1009]) ~ productivity_loop_ssp585_IPSL$year))
    p_slope_IPSL <- NULL
    p_slope_significance_IPSL <- NULL
    for (j in 1:1000) {
      # j=1
      p_slope_IPSL <- c(p_slope_IPSL, lm_results_IPSL[[j]]$coefficients[2, 1]) # slope
      p_slope_significance_IPSL <- c(p_slope_significance_IPSL, lm_results_IPSL[[j]]$coefficients[2, 4]) # p value
    }

    p_slope_lci_IPSL <- quantile(p_slope_IPSL, 0.025)
    p_slope_mean_IPSL <- mean(p_slope_IPSL)
    p_slope_uci_IPSL <- quantile(p_slope_IPSL, 0.975)

    p_slope_significance_95_IPSL <- quantile(p_slope_significance_IPSL, 0.95)

    # make a data frame
    productivity_projection_trend_ssp585_loop_IPSL <- data.frame(
      stockid = i,
      p_slope_lci = p_slope_lci_IPSL,
      p_slope_mean = p_slope_mean_IPSL,
      p_slope_uci = p_slope_uci_IPSL,
      p_slope_significance_95 = p_slope_significance_95_IPSL,
      ESM = "IPSL-CM6A-LR",
      ssp = "ssp585"
    )

    # combind data
    productivity_projection_trend_ssp585 <- bind_rows(
      productivity_projection_trend_ssp585,
      productivity_projection_trend_ssp585_loop_Nor,
      productivity_projection_trend_ssp585_loop_MPI,
      productivity_projection_trend_ssp585_loop_IPSL
    )
  }

  print(i)
}

# save data
write_rds(productivity_projection_trend_ssp585,
  file = "OutputS/GLMM projection results/productivity_projection_trend_ssp585_2021_2100.rds"
)


# 5 Productivity projection summary global, by year, ssp, esm  --------------------------------
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds")

productivity_projection <- productivity_projection %>%
  select(year, ssp, ESM, stockid, 10:1009) %>%
  pivot_longer(-c(year, ssp, ESM, stockid), names_to = "number", values_to = "productivity")

productivity_projection <- productivity_projection %>%
  group_by(year, ssp, ESM, number) %>%
  summarise(
    productivity = mean(productivity, na.rm = T),
    total_stock = n()
  ) %>%
  ungroup()

productivity_projection <- productivity_projection %>%
  group_by(year, ssp, ESM, total_stock) %>%
  summarise(
    productivity_mean = mean(productivity, na.rm = T),
    productivity_sd = sd(productivity, na.rm = T),
    productivity_CV = sd(productivity, na.rm = T) / mean(productivity, na.rm = T),
    productivity_2.5 = quantile(productivity, 0.025, na.rm = T),
    productivity_25 = quantile(productivity, 0.25, na.rm = T),
    productivity_50 = quantile(productivity, 0.20, na.rm = T),
    productivity_75 = quantile(productivity, 0.75, na.rm = T),
    productivity_97.5 = quantile(productivity, 0.975, na.rm = T)
  )

write_rds(productivity_projection, 
          "Outputs/GLMM projection results/productivity_projection_summary_global.rds")


# 6 Productivity projection summary FAO area ESM aggregated----------------------------------
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds")

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname, primary_FAOarea)

productivity_projection <- left_join(productivity_projection, stock_success)

productivity_projection <- productivity_projection %>%
  select(year, ssp, primary_FAOarea, ESM, stockid, 10:1009) %>%
  pivot_longer(-c(year, ssp, primary_FAOarea, ESM, stockid), names_to = "number", values_to = "productivity")

productivity_projection <- productivity_projection %>%
  group_by(year, ssp, primary_FAOarea, ESM, number) %>%
  summarise(
    productivity = mean(productivity, na.rm = T),
    total_stock = n()
  )

productivity_projection <- productivity_projection %>%
  group_by(year, ssp, primary_FAOarea, ESM, total_stock) %>%
  summarise(
    productivity_mean = mean(productivity, na.rm = T),
    productivity_sd = sd(productivity, na.rm = T),
    productivity_CV = sd(productivity, na.rm = T) / mean(productivity, na.rm = T),
    productivity_2.5 = quantile(productivity, 0.025, na.rm = T),
    productivity_25 = quantile(productivity, 0.25, na.rm = T),
    productivity_50 = quantile(productivity, 0.20, na.rm = T),
    productivity_75 = quantile(productivity, 0.75, na.rm = T),
    productivity_97.5 = quantile(productivity, 0.975, na.rm = T)
  )

write_rds(
  productivity_projection,
  "Outputs/GLMM projection results/productivity_projection_summary_FAOarea.rds"
)

# 7 Productivity projection winner loser ----------------------------------
productivity_projection <- read_rds("Outputs/GLMM projection results/productivity_projection.rds")

# stock information
stock_success <- read_rds("Data/stock_success_full_information_final.rds") %>%
  select(stockid, scientificname, primary_FAOarea)

productivity_projection <- left_join(productivity_projection, stock_success) %>%
  select(year, ssp, primary_FAOarea, ESM, scientificname, stockid, 10:1009)

unique(productivity_projection$stockid) # 657 stocks
unique(productivity_projection$scientificname) # 265 species

# projection 2020s
productivity_projection_2020s <- productivity_projection %>%
  filter(year %in% seq(2020, 2029, 1))

# projection 2050s
productivity_projection_2050s <- productivity_projection %>%
  filter(year %in% seq(2050, 2059, 1))

# projection 2090s
productivity_projection_2090s <- productivity_projection %>%
  filter(year %in% seq(2090, 2099, 1))

# productivity change 2050s-2020s
productivity_projection_50s_20s <- productivity_projection_2050s[, 7:1006] - productivity_projection_2020s[, 7:1006]
productivity_projection_50s_20s[, 1001:1006] <- productivity_projection_2020s[, 1:6]
productivity_projection_50s_20s <- productivity_projection_50s_20s %>%
  pivot_longer(-c(year, ssp, primary_FAOarea, ESM, scientificname, stockid), names_to = "number", values_to = "productivity")

productivity_projection_50s_20s <- productivity_projection_50s_20s %>%
  group_by(stockid, ESM, ssp) %>%
  summarize(
    p_change_mean = mean(productivity, na.rm = T),
    p_change_2.5 = quantile(productivity, 0.025, na.rm = T),
    p_change_25 = quantile(productivity, 0.25, na.rm = T),
    p_change_50 = quantile(productivity, 0.50, na.rm = T),
    p_change_75 = quantile(productivity, 0.75, na.rm = T),
    p_change_97.5 = quantile(productivity, 0.975, na.rm = T)
  )

write_rds(
  productivity_projection_50s_20s,
  "Outputs/GLMM projection results/productivity_projection_change_50s_20s.rds"
)

# productivity change 2090s-2020s
productivity_projection_90s_20s <- productivity_projection_2090s[, 7:1006] - productivity_projection_2020s[, 7:1006]
productivity_projection_90s_20s[, 1001:1006] <- productivity_projection_2020s[, 1:6]
productivity_projection_90s_20s <- productivity_projection_90s_20s %>%
  pivot_longer(-c(year, ssp, primary_FAOarea, ESM, scientificname, stockid), names_to = "number", values_to = "productivity")

productivity_projection_90s_20s <- productivity_projection_90s_20s %>%
  group_by(stockid, ESM, ssp) %>%
  summarize(
    p_change_mean = mean(productivity, na.rm = T),
    p_change_2.5 = quantile(productivity, 0.025, na.rm = T),
    p_change_25 = quantile(productivity, 0.25, na.rm = T),
    p_change_50 = quantile(productivity, 0.50, na.rm = T),
    p_change_75 = quantile(productivity, 0.75, na.rm = T),
    p_change_97.5 = quantile(productivity, 0.975, na.rm = T)
  )

write_rds(
  productivity_projection_90s_20s,
  "Outputs/GLMM projection results/productivity_projection_change_90s_20s.rds"
)


