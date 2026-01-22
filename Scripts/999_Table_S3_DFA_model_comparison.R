library(tidyverse)

# DFA files
DFA_files <- list.files("Outputs/DFA results/")[startsWith(list.files("Outputs/DFA results/"), "kemz")]

# all DFA model information
DFA_model_information <- NULL
for (i in DFA_files) {
  # i <- DFA_files[1]
  DFA_results_cycle <- read_rds(paste0("Outputs/DFA results/", i))
  DFA_model_information_cycle <- data.frame(
    file_name = i,
    logLik = ifelse(is.null(DFA_results_cycle$logLik), NA, DFA_results_cycle$logLik),
    AICc = ifelse(is.null(DFA_results_cycle$AICc), NA, DFA_results_cycle$AICc),
    K = ifelse(is.null(DFA_results_cycle$num.params), NA, DFA_results_cycle$num.params)
  )
  
  DFA_model_information <- bind_rows(DFA_model_information, DFA_model_information_cycle)
}

best_DFA_model <- DFA_model_information %>%
  separate(file_name,
           sep = "_",
           into = c("model", "area1", "area2", "trend1", "trend2", "structure1", "structure2"),
           remove = F
  ) %>%
  mutate(
    area = paste(area1, area2),
    trend = paste(trend2, trend1),
    structure = str_sub(structure2, end = -5)
  ) %>%
  select(file_name, area, trend, structure, logLik, AICc, K)

# save results, Table S3
write.csv(best_DFA_model, "Tables/Table_S3_DFA_model_comparison.csv")


