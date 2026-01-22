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
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  # grid
  panel.grid.major = element_line(linetype = "dotted", linewidth = 0.25, color = "grey75"),
  # strip
  strip.background = element_blank(),
  strip.text = element_text(family = "Arial"),
  # legend
  legend.text = element_text(family = "Arial")
)

# as the difference between prior and posterior is largely depending on CV, we
# use simulation way to explore the suitable threshold for different CV.
stock_success <- read_rds("data/stock_success_final.rds")
prior_r <- read_rds("Data/prior_r.rds") %>%
  filter(stockid %in% stock_success$stockid)
prior_K <- read_rds("Data/prior_K.rds") %>%
  filter(stockid %in% stock_success$stockid)
prior_psi <- read_rds("Data/prior_psi.rds") %>%
  filter(stockid %in% stock_success$stockid)

# r CV
range(prior_r$r_CV)
hist(prior_r$r_CV)

# K CV
range(prior_K$K_CV)
hist(prior_K$K_CV)

# psi CV
range(prior_psi$psi_CV)
hist(prior_psi$psi_CV)


# simulation
CV_level <- c(0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 3, 4)

PP_change <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15, 0.2)

simulated_data <- NULL
for (i in CV_level) {
  # i = 0.1
  for (j in PP_change) {
    # j = 0.01
    mean_prior <- 100
    CV_prior <- i
    sd_prior <- sqrt(log(CV_prior^2 + 1))

    # PPMR and PPVR
    PPMR <- 1 - j
    PPVR <- 1 - j

    # posterior information
    mean_post <- mean_prior * PPMR
    sd_post <- sqrt(PPVR * (sd_prior / log(mean_prior))^2) * log(mean_post)

    # simulate data
    prior <- dlnorm(seq(0, 150, by = 1), log(mean_prior) - sd_prior^2 / 2, sd_prior)
    posterior <- dlnorm(seq(0, 150, by = 1), log(mean_post) - sd_post^2 / 2, sd_post)
    # prior <- rlnorm(10000, log(mean_prior) - sd_prior^2 / 2, sd_prior)
    # prior <- prior[prior < quantile(prior, 0.90)]
    # posterior <- rlnorm(10000, log(mean_post) - sd_post^2 / 2, sd_post)
    # posterior <- posterior[posterior < quantile(posterior, 0.90)]
    # hist(a1, col = "red", breaks = 200)
    # hist(a2, col = "blue", breaks = 200, add = T)
    simulated_data_loop <- data.frame(
      mean = 1, CV = i, PP_change = j,
      x = seq(0, 150, by = 1),
      prior = prior, posterior = posterior
    )

    simulated_data <- bind_rows(simulated_data, simulated_data_loop)
  }
}

simulated_data <- simulated_data %>%
  mutate(factor = paste0("CV = ", CV, ", PP = ", PP_change))

# figure prior and posterior distribution with different CV and PP change combination
f <- ggplot(simulated_data) +
  geom_line(aes(x = x, y = prior), color = "red", linewidth = 0.25) +
  geom_line(aes(x = x, y = posterior), color = "blue", linewidth = 0.25) +
  # geom_density(aes(x = prior), color = "red", fill = NA) +
  # geom_density(aes(x = posterior), color = "blue", fill = NA) +
  facet_wrap(. ~ factor, scales = "free", ncol = 9) +
  mytheme

ggsave("Figures/PPMR_PPVR_threshold.PDF", device = cairo_pdf, width = 12, height = 9)


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
