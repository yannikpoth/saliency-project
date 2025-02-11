library(rstan)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinystan)

# set working dir to dir where R-file resides
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path)))

# import function for all csv data
load_data <- function(file_paths) {
  all_data <- lapply(file_paths, read.csv)
  return(all_data)
}

# file pathes of all csv data files
relative_file_path <- "../pilot_data/transf_data"
file_paths <- list.files(path = relative_file_path, 
                         pattern = "*.csv", full.names = TRUE)

# import all data
all_data <- load_data(file_paths)

# number of subjects and trials
nSubjects <- length(all_data)
max_nTrials <- max(sapply(all_data, function(x) nrow(x)))

# Initialize arrays
choice <- array(NA, dim = c(nSubjects, max_nTrials))
reward <- array(NA, dim = c(nSubjects, max_nTrials))

# Fill arrays with data
for (i in 1:nSubjects) {
  data <- all_data[[i]]
  nTrials <- nrow(data)
  choice[i, 1:nTrials] <- data$actions + 1
  reward[i, 1:nTrials] <- data$rewards
}


my_data <- list(nTrials=max_nTrials, nSubjects=nSubjects,
                choice=choice, reward=reward)

model_file_basic <- "ms_ql_1lr.stan"
model_file_phi <- "ms_ql_1lr_phi.stan"

basic_model <- stan_model(model_file_basic)

basic_samples <- sampling(basic_model,
                          my_data,
                          cores = getOption("mc.cores", 1L),
                          chains = 2,
                          iter = 2000)

phi_model <- stan_model(model_file_phi)

phi_samples <- sampling(phi_model,
                        my_data, 
                        cores = getOption("mc.cores", 1L),
                        chains = 2,
                        iter = 2000)

basic_samples
phi_samples

# Extrahiere die Posterior-Samples
posterior_array_basic <- as.array(my_samples)
posterior_array_basic <- as.array(my_samples)

# Konvertiere die Posterior-Samples in ein DataFrame
posterior_df <- as.data.frame(posterior_array)

# Daten in ein langes Format umwandeln
posterior_long <- posterior_df %>%
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "value")

# Parameter `lp__` für beide Chains entfernen
posterior_long <- posterior_long %>%
  filter(!parameter %in% c("chain:1.lp__", "chain:2.lp__"))

# Parametername in Chain und Parameter aufteilen
posterior_long <- posterior_long %>%
  separate(parameter, into = c("chain", "parameter"), sep = "\\.", extra = "merge")

# Chain-Spalte entfernen
posterior_combined <- posterior_long %>%
  select(-chain)

# Parameter in der gewünschten Reihenfolge sortieren
## Extrahiere die eindeutigen Indizes
indices <- unique(gsub("[^0-9]", "", posterior_combined$parameter))

## Erstelle die gewünschte Reihenfolge dynamisch
desired_order <- unlist(lapply(indices, function(i) {
  c(paste0("alpha[", i, "]"), paste0("alpha_s[", i, "]"), paste0("beta[", i, "]"))
}))

posterior_combined$parameter <- factor(posterior_combined$parameter, levels = desired_order)

# Kernel Density Plot für jeden Parameter erstellen
ggplot(posterior_combined, aes(x = value)) +
  geom_density(alpha = 0.5, fill = "blue") +
  facet_wrap(~ parameter, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Kombinierte Kernel Density Plot der Posterior-Samples",
       x = "Wert",
       y = "Dichte")

# launch_shinystan(my_samples)