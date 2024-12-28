library(rstan)

# set working dir to dir where R-file resides
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path)))

data <- read.csv("/Users/yannikpoth/Documents/Studium/RWTH_Aachen/Masterthesis/Task/PsychoPy/two_armed_bandit/twoarmedbandit/analysis/transf_data.csv")



actions <- data$actions+1
rewards <- data$rewards

nTrials=length(actions)
nSubjects=1

choice <- array(actions, dim = c(nSubjects, nTrials))
reward <- array(rewards, dim = c(nSubjects, nTrials))


my_data <- list(nTrials=nTrials, nSubjects=nSubjects,
                choice=choice, reward=reward)

basic_model_file <- "ms_ql_1lr.stan"

basic_model <- stan_model(basic_model_file)

basic_samples <- sampling(basic_model, 
                       my_data, 
                       cores = getOption("mc.cores", 1L),
                       chains = 2,
                       iter = 2000)

phi_model_file <- "ms_ql_1lr_phi.stan"

phi_model <- stan_model(phi_model_file)

phi_samples <- sampling(phi_model, 
                       my_data, 
                       cores = getOption("mc.cores", 1L),
                       chains = 2,
                       iter = 2000)

basic_samples
phi_samples
