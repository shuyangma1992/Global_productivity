library(tidyverse)
library(JABBA)
library(rstan)
library(diptest)
# library(LaplacesDemon)

# 1 Best JABBA ------------------------------------------------------------
# all JABBA result files
JABBA_file_name <- list.files("Outputs/JABBA results/")

# information files
JABBA_information_file_name <- JABBA_file_name[str_ends(JABBA_file_name, "rds")][-1]

# cycle for data and model information
data_information <- NULL
model_information <- NULL
for (i in 1:length(JABBA_information_file_name)) {
  # i=1
  # read data
  data_loop <- read_rds(paste0("Outputs/JABBA results/", JABBA_information_file_name[i]))

  # data information
  data_information_loop <- data_loop[[1]][1, ] %>%
    mutate(filename = JABBA_information_file_name[i])
  data_information <- bind_rows(data_information, data_information_loop)

  # model information
  model_information_loop <- data_loop[[2]] %>%
    mutate(Stockname = data_information_loop$Stockname) %>%
    mutate(filename = JABBA_information_file_name[i])
  model_information <- bind_rows(model_information, model_information_loop)

  print(i)
}

# best model selection based on DIC
model_information_DIC <- model_information %>%
  filter(Stastistic %in% c("DIC")) %>%
  group_by(Stockname) %>%
  mutate(delta_value = Value - min(Value)) %>%
  ungroup() %>%
  mutate(Model = factor(Model, levels = unique(Model)))

# f_DIC <- ggplot(model_information_DIC)+
#   geom_vline(xintercept = 0,color="red")+
#   geom_point(aes(x=delta_value,y=Stockname,shape=Model),show.legend = T)+
#   labs(title = "a) DIC")+
#   scale_x_continuous("Delta-DIC",limits = c(0,5))+
#   scale_y_discrete("Stock name")+
#   theme(axis.text = element_text(family = "Calibri"),
#         axis.title = element_text(family = "Calibri"),
#         plot.title = element_text(family="Calibri"),
#         plot.subtitle = element_text(family="Calibri",face="italic"),
#         legend.title = element_blank(),
#         legend.text = element_text(family = "Calibri"),
#         legend.position = "right",
#         legend.spacing.x = unit(0,"cm"),
#         legend.spacing.y = unit(0,"cm"),
#         legend.background = element_rect(fill = NA))

# best_model <- model_information_DIC %>%
#   group_by(Stockname) %>%
#   filter(Value==min(Value))
#
# write_rds(best_model,"JABBA results/0 Best model.rds")

# #best model selection based on RMSE
# model_information_RMSE <- model_information %>%
#   filter(Stastistic %in% c("RMSE")) %>%
#   group_by(Stockname) %>%
#   mutate(delta_value=Value-min(Value)) %>%
#   ungroup() %>%
#   mutate(Model=factor(Model,levels = unique(Model)))

# f_RMSE <- ggplot(model_information_RMSE)+
#   geom_vline(xintercept = 0,color="red")+
#   geom_point(aes(x=delta_value,y=Stockname,shape=Model),show.legend = T)+
#   labs(title = "b) RMSE")+
#   scale_x_continuous("Delta-RMSE",limits = c(0,1))+
#   scale_y_discrete("Stock name")+
#   theme(axis.text = element_text(family = "Calibri"),
#         axis.title = element_text(family = "Calibri"),
#         plot.title = element_text(family="Calibri"),
#         plot.subtitle = element_text(family="Calibri",face="italic"),
#         legend.title = element_blank(),
#         legend.text = element_text(family = "Calibri"),
#         legend.position = "right",
#         legend.spacing.y = unit(-5,"points"),
#         legend.background = element_rect(fill = NA))
#
# ggarrange(f_DIC,f_RMSE,nrow = 2)
#
# ggsave("Figure/data and model information.tiff",dpi = 300,width = 6,height = 5)

# select best model based on DIC
best_JAABA <- model_information_DIC %>%
  filter(delta_value == 0)

# duplicated stock
stockname_duplicate <- best_JAABA %>%
  filter(duplicated(Stockname))

# delete duplicates
best_JAABA <- anti_join(best_JAABA, stockname_duplicate)

write_rds(best_JAABA, file = "Outputs/JABBA results/0_best_JABBA.rds")

# 2 Convergence and Unimodality indices -------------------------------------
best_model <- read_rds("Outputs/JABBA results/0_best_JABBA.rds") %>%
  mutate(file_name = paste0("Outputs/JABBA results/", Stockname, "_seed1_", Model, "_jabba.rdata"))

# make best model file name
best_model_file_name <- best_model$file_name

convergence_and_unimodality <- NULL
for (i in best_model$file_name) {
  # i=best_model$file_name[1]
  # load jabba results
  load(i)

  # parameter
  parameter_JABBA <- jabba$pars %>%
    mutate(parameter = rownames(jabba$pars))

  # parameter posterior
  parameter_posterior_JABBA <- jabba$pars_posterior

  #----------------------------------------------------------convergence test Rhat
  convergen_results <- data.frame(
    parameter = c(
      "r", "K", "psi",
      "q", "q.1", "q.2", "q.3", "q.4", "q.5",
      "sigma2",
      "tau2", "tau2.1", "tau2.2", "tau2.3", "tau2.4", "tau2.5"
    ),
    Rhat = c(
      Rhat(matrix(parameter_posterior_JABBA$r, nrow = 2000, ncol = 3)),
      Rhat(matrix(parameter_posterior_JABBA$K, nrow = 2000, ncol = 3)),
      Rhat(matrix(parameter_posterior_JABBA$psi, nrow = 2000, ncol = 3)),
      try(Rhat(matrix(parameter_posterior_JABBA$q, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$q.1, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$q.2, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$q.3, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$q.4, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$q.5, nrow = 2000, ncol = 3))),
      Rhat(matrix(parameter_posterior_JABBA$sigma2, nrow = 2000, ncol = 3)),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2.1, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2.2, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2.3, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2.4, nrow = 2000, ncol = 3))),
      try(Rhat(matrix(parameter_posterior_JABBA$tau2.5, nrow = 2000, ncol = 3)))
    )
  )

  #-------------------------------------------unimodality test Hartigans’ dip test
  # jbplot_ppdist(jabba)
  unimodality_results <- data.frame(
    parameter = c(
      "r", "K", "psi",
      "q", "q.1", "q.2", "q.3", "q.4", "q.5",
      "sigma2",
      "tau2", "tau2.1", "tau2.2", "tau2.3", "tau2.4", "tau2.5"
    ),
    Hartigans_dip = c(
      dip.test(parameter_posterior_JABBA$r)[2][[1]],
      dip.test(parameter_posterior_JABBA$K)[2][[1]],
      dip.test(parameter_posterior_JABBA$psi)[2][[1]],
      try(dip.test(parameter_posterior_JABBA$q)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$q.1)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$q.2)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$q.3)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$q.4)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$q.5)[2][[1]]),
      dip.test(parameter_posterior_JABBA$sigma2)[2][[1]],
      try(dip.test(parameter_posterior_JABBA$tau2)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$tau2.1)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$tau2.2)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$tau2.3)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$tau2.4)[2][[1]]),
      try(dip.test(parameter_posterior_JABBA$tau2.5)[2][[1]])
    )
  )

  # is.unimodal(parameter_posterior_JABBA$sigma2)

  # combine data
  parameter_JABBA <- left_join(parameter_JABBA, convergen_results) %>%
    left_join(unimodality_results) %>%
    mutate(
      stock = jabba$assessment,
      model = jabba$scenario
    )
  convergence_and_unimodality <- bind_rows(convergence_and_unimodality, parameter_JABBA)
}

# write_rds(convergence_and_unimodality,file="JABBA results/0_best_JABBA_convergence_and_unimodality.rds")

# delete NA
convergence_and_unimodality_adjusted <- convergence_and_unimodality %>%
  drop_na()

# delete character
convergence_and_unimodality_adjusted <- convergence_and_unimodality_adjusted %>%
  filter(!startsWith(parameter, "tau"))

# mutate stockid
convergence_and_unimodality_adjusted <- convergence_and_unimodality_adjusted %>%
  mutate(stockid = str_sub(stock, 1, -7))

# save data
write_rds(convergence_and_unimodality_adjusted, file = "Outputs/JABBA results/0_best_JABBA_convergence_and_unimodality.rds")


# 3 Check and delete stocks with failure--------------------------------------------
convergence_and_unimodality <- read_rds("Outputs/JABBA results/0_best_JABBA_convergence_and_unimodality.rds")

# first, check convergence, Rhat < 1.05
convergece_problem <- convergence_and_unimodality %>% 
  filter(as.numeric(Rhat)>=1.05) # no problem

# second, check unimodality, Hartigans' dip p >0.05
unimodality_problem <- convergence_and_unimodality %>% 
  filter(as.numeric(Hartigans_dip)<0.05) %>% 
  distinct(stockid) # 18 stocks

# # check several of them, bimodal pattern mostly for sigma2
# file_name <- filter(best_model, Stockname == unimodality_problem$stockid[6])$file_name
# load(file_name)
# jbplot_ppdist(jabba)

# exclude these 18 stocks
best_JAABA <- read_rds("Outputs/JABBA results/0_best_JABBA.rds") %>%
  rename(stockid = "Stockname")
best_JAABA <- anti_join(best_JAABA, unimodality_problem) # 710 stocks
write_rds(best_JAABA, file = "Outputs/JABBA results/0_best_JABBA_final.rds")

# stock with successful jabba
stock_success <- read_rds("Data/stock_success.rds") %>%
  rename(stockid = stock_id)
stock_success <- anti_join(stock_success, unimodality_problem) # 710 stocks

write_rds(stock_success, file = "Data/stock_success_final.rds")















