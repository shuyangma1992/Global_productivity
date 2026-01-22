library(tidyverse)
library(JABBA)
library(ggsci)
library(ggforce)

# Figure settings
theme_set(theme_classic()) # theme
windowsFonts(A = windowsFont("Arial")) # Font
scales::show_col(pal_npg()(9))
mypal <- pal_npg()(9) # Color
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
  axis.text = element_text(family = "Arial"),
  axis.title = element_text(family = "Arial"),
  axis.ticks = element_blank(),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)


# 1 Data compile -------------------------------------------------------
best_model <- read_rds("Outputs/JABBA results/0_best_JABBA_final.rds") %>%
  mutate(file_name = paste0("Outputs/JABBA results/", stockid, "_seed1_", Model, "_jabba.rdata"))

# make best model file name
best_model_file_name <- best_model$file_name

# calculate PPMR and PPVR
PP_data <- NULL
for (i in best_model$file_name) {
  # i = best_model$file_name[1]
  # load jabba results
  load(i)
  
  ############################################################## r, k ,psi prior
  r_prior <- rlnorm(6000, log(jabba$setting$r.pr[1]), jabba$setting$r.pr[2])
  K_prior <- rlnorm(6000, log(jabba$setting$K.pr[1]), jabba$setting$K.pr[2])
  psi_prior <- rlnorm(6000, log(jabba$setting$psi.pr[1]), jabba$setting$psi.pr[2])
  
  ########################################################## r, k ,psi posterior
  r_posterior <- jabba$pars_posterior$r
  K_posterior <- jabba$pars_posterior$K
  psi_posterior <- jabba$pars_posterior$psi
  
  ################################################################ PPMR and PPVR
  PPMR_r <- round(mean(r_posterior) / mean(r_prior), 3)
  PPVR_r <- round((sd(r_posterior) / mean(r_posterior))^2 / (sd(r_prior) / mean(r_prior))^2, 3)
  PPMR_K <- round(mean(K_posterior) / mean(K_prior), 3)
  PPVR_K <- round((sd(K_posterior) / mean(K_posterior))^2 / (sd(K_prior) / mean(K_prior))^2, 3)
  PPMR_psi <- round(mean(psi_posterior) / mean(psi_prior), 3)
  PPVR_psi <- round((sd(psi_posterior) / mean(psi_posterior))^2 / (sd(psi_prior) / mean(psi_prior))^2, 3)
  
  ################################################################# combine data
  PP_data_loop <- data.frame(
    parameter = c("r", "K", "psi"),
    PPMR = c(PPMR_r, PPMR_K, PPMR_psi),
    PPVR = c(PPVR_r, PPVR_K, PPVR_psi),
    stockid = jabba$assessment,
    mean = c(jabba$setting$r.pr[1], jabba$setting$K.pr[1], jabba$setting$psi.pr[1]),
    sd = c(jabba$setting$r.pr[2], jabba$setting$K.pr[2], jabba$setting$psi.pr[2]),
    CV = c(sqrt(exp(jabba$setting$r.pr[2]^2)-1), sqrt(exp(jabba$setting$K.pr[2]^2)-1), sqrt(exp(jabba$setting$psi.pr[2]^2)-1))
  )
  
  PP_data <- bind_rows(PP_data, PP_data_loop)
  print(i)
}




# 2 Figure ----------------------------------------------------------------
# threshold selection
# CV < 0.1, threshold = 0.03
# 0.1 < CV < 0.25, threshold = 0.04
# 0.25 < CV < 0.5, threshold = 0.05
# 0.5 < CV < 0.75, threshold = 0.075
# 0.75 < CV < 1, threshold = 0.1
# 1 < CV < 1.25, threshold = 0.15
# 1.25 < CV < 1.5, threshold = 0.15
# 1.5 < CV < 2, threshold = 0.2
# 2 < CV < 3, threshold = 0.2
# 3 < CV < 4, threshold = 0.2

# identify informative
PP_data <- PP_data %>%
  mutate(informative = case_when(
    CV <= 0.1 ~ ifelse(abs(PPMR-1) < 0.03 & abs(PPVR-1) < 0.03, "NO", "YES"),
    CV > 0.1 & CV <= 0.25 ~ ifelse(abs(PPMR-1) < 0.04 & abs(PPVR-1) < 0.04, "NO", "YES"),
    CV > 0.25 & CV <= 0.5 ~ ifelse(abs(PPMR-1) < 0.05 & abs(PPVR-1) < 0.05, "NO", "YES"),
    CV > 0.5 & CV <= 0.75 ~ ifelse(abs(PPMR-1) < 0.075 & abs(PPVR-1) < 0.075, "NO", "YES"),
    CV > 0.75 & CV <= 1 ~ ifelse(abs(PPMR-1) < 0.1 & abs(PPVR-1) < 0.1, "NO", "YES"),
    CV > 1 & CV <= 1.25 ~ ifelse(abs(PPMR-1) < 0.15 & abs(PPVR-1) < 0.15, "NO", "YES"),
    CV > 1.25 & CV <= 1.5 ~ ifelse(abs(PPMR-1) < 0.15 & abs(PPVR-1) < 0.15, "NO", "YES"),
    CV > 1.5 & CV <= 2 ~ ifelse(abs(PPMR-1) < 0.2 & abs(PPVR-1) < 0.2, "NO", "YES"),
    CV > 2 & CV <= 3 ~ ifelse(abs(PPMR-1) < 0.2 & abs(PPVR-1) < 0.2, "NO", "YES"),
    CV > 3 & CV <= 4 ~ ifelse(abs(PPMR-1) < 0.2 & abs(PPVR-1) < 0.2, "NO", "YES")
  ))


PP_data_summary <- PP_data %>% 
  group_by(parameter, informative) %>% 
  summarise(n = n()) %>% 
  ungroup()

PP_data_summary <- PP_data_summary %>% 
  mutate(percentage = n/710) %>% 
  add_row(parameter = "K", informative = "NO", n = 0, percentage = 0) %>% 
  mutate(parameter = factor(parameter, levels = c("r", "K", "psi")))

PP_data <- PP_data %>% 
  mutate(parameter = factor(parameter, levels = c("r", "K", "psi")))

f <- ggplot(PP_data) +
  geom_point(aes(x = log(PPMR), y = log(PPVR), color = informative), 
             size = 0.1, show.legend = FALSE) +
  # geom_circle(aes(x0 = 0, y0 = 0, r = 0.105), color = "red") + 
  geom_text(data = filter(PP_data_summary, informative == "NO"), 
            aes(x = 2, y = 0.5, label = paste0("Not informative: ", round(percentage, 4) * 100, "%")),
            family = "Arial", size = 3, color = "red", hjust = 1) +
  facet_wrap(.~parameter) +
  scale_color_manual(values = c("YES" = "black", "NO" = "red")) +
  scale_x_continuous("Posterior-to-Prior Mean Ratio (log)", limits = c(-2,2)) +
  scale_y_continuous("Posterior-to-Prior Variance Ratio (log)", limits = c(-2,0.5)) +
  mytheme

ggsave("Figures/PPMR_PPVR.PDF", device = cairo_pdf, width = 6, height = 3)





# 99999 Threshold for psi ---------------------------------------------------
prior_psi <- read_rds("Data/prior_psi.rds")
###################################### 1
# prior information
mean_prior <- 0.8660254
CV_prior <- 0.07348721
sd_prior <- sqrt(log(CV_prior^2 + 1))
# PPMR and PPVR
PPMR <- 0.99
PPVR <- 0.99

# posterior information
mean_post <- mean_prior * PPMR
sd_post <- sqrt(PPVR * (sd_prior / mean_prior)^2) * mean_post

# histogram
a1 <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
a2 <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)

hist(a1, col = "red", breaks = 100, xlim = c(0, 1))
hist(a2, col = "blue", add = T, breaks = 100, xlim = c(0, 1))

###################################### 2
# prior information
mean_prior <- 0.6519202
CV_prior <- 0.13598681
sd_prior <- sqrt(log(CV_prior^2 + 1))

# PPMR and PPVR
PPMR <- 0.975
PPVR <- 0.975

# posterior information
mean_post <- mean_prior * PPMR
sd_post <- sqrt(PPVR * (sd_prior / mean_prior)^2) * mean_post

# histogram
a1 <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
a2 <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)

hist(a1, col = "red", breaks = 100, xlim = c(0, 1))
hist(a2, col = "blue", add = T, breaks = 100, xlim = c(0, 1))

###################################### 3
# prior information
mean_prior <- 0.4769696
CV_prior <- 0.1589078
sd_prior <- sqrt(log(CV_prior^2 + 1))

# PPMR and PPVR
PPMR <- 0.975
PPVR <- 0.975

# posterior information
mean_post <- mean_prior * PPMR
sd_post <- sqrt(PPVR * (sd_prior / mean_prior)^2) * mean_post

# histogram
a1 <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
a2 <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)

hist(a1, col = "red", breaks = 100, xlim = c(0, 1))
hist(a2, col = "blue", add = T, breaks = 100, xlim = c(0, 1))

###################################### 4
# prior information
mean_prior <- 0.24494897
CV_prior <- 0.2541793
sd_prior <- sqrt(log(CV_prior^2 + 1))

# PPMR and PPVR
PPMR <- 0.975
PPVR <- 0.975

# posterior information
mean_post <- mean_prior * PPMR
sd_post <- sqrt(PPVR * (sd_prior / mean_prior)^2) * mean_post

# histogram
a1 <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
a2 <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)

hist(a1, col = "red", breaks = 100, xlim = c(0, 1))
hist(a2, col = "blue", add = T, breaks = 100, xlim = c(0, 1))

###################################### 5
# prior information
mean_prior <- 0.04472136
CV_prior <- 0.8906445
sd_prior <- sqrt(log(CV_prior^2 + 1))

# PPMR and PPVR
PPMR <- 0.90
PPVR <- 0.90

# posterior information
mean_post <- mean_prior * PPMR
sd_post <- sqrt(PPVR * (sd_prior / mean_prior)^2) * mean_post

# histogram
a1 <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
a2 <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)

hist(a1, col = "red", breaks = 100, xlim = c(0, 0.5))
hist(a2, col = "blue", add = T, breaks = 100, xlim = c(0, 0.5))

######################################################################## summary
# for psi mean_prior = 0.04472136 CV_prior = 0.8906445, use threshold 0.1
# for psi mean_prior = 0.24494897 CV_prior = 0.2541793, use threshold 0.025
# for psi mean_prior = 0.4769696 CV_prior = 0.1589078, use threshold 0.025
# for psi mean_prior = 0.6519202 CV_prior = 0.13598681, use threshold 0.025
# for psi mean_prior = 0.8660254 CV_prior = 0.07348721, use threshold 0.025
