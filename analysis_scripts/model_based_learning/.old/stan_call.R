library(rstan)
library(rstudioapi)

# Set working directory to the location of this R script
setwd(dirname(getSourceEditorContext()$path))

# Define the directory where the collected data files are stored
data_dir <- "../../bandit_task/collected_data"

# List only task data files (exclude questionnaire files)
files <- list.files(path = data_dir, pattern = "^[0-9]+_task_data\\.csv$", full.names = TRUE)

# Initialize lists to store choices and rewards for each participant
all_choices <- list()
all_rewards <- list()

# Loop over each task data file (each participant)
for (i in seq_along(files)) {
  d <- read.csv(files[i])
  
  # Filter rows to include only main trials
  d_main <- subset(d, mode == "main")
  
  # Ensure trials are ordered correctly (by the trial number)
  d_main <- d_main[order(d_main$trial), ]
  
  # Extract choices and rewards
  # Here we assume the choices are 0-indexed; we add 1 so that they range between 1 and 2 for Stan
  choices <- as.numeric(d_main$choice) + 1
  rewards <- as.numeric(d_main$reward)
  
  all_choices[[i]] <- choices
  all_rewards[[i]] <- rewards
}

# Check that all participants have the same number of main trials
nTrials_unique <- unique(sapply(all_choices, length))
if (length(nTrials_unique) != 1) {
  stop("Not all participants have the same number of main trials.")
} else {
  nTrials <- nTrials_unique[1]
}

nSubjects <- length(files)

# Convert the lists into matrices with dimensions [nSubjects x nTrials]
choice_matrix <- do.call(rbind, all_choices)
reward_matrix <- do.call(rbind, all_rewards)

# Prepare the data list for Stan
stan_data <- list(
  nTrials = nTrials,
  nSubjects = nSubjects,
  choice = choice_matrix,
  reward = reward_matrix
)

# Compile and run the Stan model that uses the Phi transformation
phi_model_file <- "ms_ql_1lr_phi.stan"  # adjust the path if needed
phi_model <- stan_model(phi_model_file)

phi_samples <- sampling(
  phi_model,
  data = stan_data,
  chains = 2,
  iter = 2000,
  cores = getOption("mc.cores", 1L),
  seed = 1234  # for reproducibility
)

# Print a summary of the samples
print(phi_samples)
