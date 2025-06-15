#      Prepare data: Merged Datasets and Stan lists
# Last edit:    2024-07-29
# Author:       Gemini
# Notes:        - Inspired by StanList.R
#               - Creates two merged datasets: one for task data and one for questionnaire data.
#               - Provides a StanCleaned function to prepare data for Stan models.

#### Imports ####

library(tidyverse)
library(janitor)
library(fs)


#### Merge Task Data ####
#########################

.MergeTaskData <- function(data_dir = "data/raw", processed_dir = "data/processed") {
  #' Merge all separate participant task data.
  #'
  #' Collect and merge the data of all the participant task files
  #' (`*_task_data.csv`) in the given directory. The output is stored as a
  #' tsv file in the processed directory.
  #'
  #' @param data_dir Path to the directory containing raw data files.
  #' @param processed_dir Path to the directory where processed data will be saved.
  #' @return A data.frame with the merged task data, including a "subject_id" column.

  task_files <- fs::dir_ls(data_dir, regexp = "_task_data[.]csv$")
  cat("Found", length(task_files), "task data files.\n")

  all_task_data <- map_dfr(task_files, function(file_path) {
    subject_id <- sub("_task_data[.]csv$", "", basename(file_path))
    read_csv(file_path, show_col_types = FALSE) %>%
      mutate(subject_id = as.character(subject_id))
  })

  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }
  
  output_file <- file.path(processed_dir, "all_task_data.tsv")
  write_tsv(all_task_data, output_file)
  cat("Task data saved to", output_file, "\n")

  return(all_task_data)
}


#### Merge Questionnaire Data ####
##################################

.MergeQuestionnaireData <- function(data_dir = "data/raw", processed_dir = "data/processed") {
  #' Merge and clean all separate participant questionnaire data.
  #'
  #' Collect, merge, and clean the data of all the participant questionnaire
  #' files (`*_questionnaire_data.csv`) in the given directory. The output is
  #' stored as a tsv file in the processed directory.
  #'
  #' @param data_dir Path to the directory containing raw data files.
  #' @param processed_dir Path to the directory where processed data will be saved.
  #' @return A data.frame with the merged and cleaned questionnaire data.
  
  questionnaire_files <- fs::dir_ls(data_dir, regexp = "_questionnaire_data[.]csv$")
  cat("Found", length(questionnaire_files), "questionnaire data files.\n")

  read_questionnaire_file <- function(file_path) {
    subject_id_from_filename <- sub("_questionnaire_data[.]csv$", "", basename(file_path))
    data <- read_csv(file_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
    data_cleaned <- data %>% janitor::clean_names()

    if ("participant_id" %in% names(data_cleaned)) {
      data_with_subject_id <- data_cleaned %>%
        mutate(subject_id = as.character(participant_id))
      
      if ("participant_id" %in% names(data_with_subject_id) && "participant_id" != "subject_id") {
        data_with_subject_id <- data_with_subject_id %>% select(-participant_id)
      }
    } else {
      data_with_subject_id <- data_cleaned %>%
        mutate(subject_id = as.character(subject_id_from_filename))
    }

    potential_ordered_factor_colnames <- grep("^bis_\\d+$", names(data_with_subject_id), value = TRUE)
    cols_to_convert_to_ordered_factor <- intersect(potential_ordered_factor_colnames, names(data_with_subject_id))

    potential_numeric_colnames <- c("ss_percent", "bis_total", "sst", "sse", "ssd", "ssb", "ss_total")
    cols_to_convert_to_numeric <- intersect(potential_numeric_colnames, names(data_with_subject_id))

    potential_sss_colnames <- grep("^sss_\\d+$", names(data_with_subject_id), value = TRUE)
    cols_to_convert_to_sss_factor <- intersect(potential_sss_colnames, names(data_with_subject_id))
    
    potential_other_factor_colnames <- grep("^q_choice_", names(data_with_subject_id), value = TRUE)
    cols_to_convert_to_other_factor <- intersect(potential_other_factor_colnames, names(data_with_subject_id))

    data_typed <- data_with_subject_id

    if (length(cols_to_convert_to_ordered_factor) > 0) {
      data_typed <- data_typed %>%
        mutate(across(all_of(cols_to_convert_to_ordered_factor), ~factor(., levels = c("1", "2", "3", "4"), ordered = TRUE)))
    }
    
    if (length(cols_to_convert_to_numeric) > 0) {
      data_typed <- data_typed %>%
        mutate(across(all_of(cols_to_convert_to_numeric), as.numeric))
    }

    if (length(cols_to_convert_to_sss_factor) > 0) {
      data_typed <- data_typed %>%
        mutate(across(all_of(cols_to_convert_to_sss_factor), ~factor(., levels = c("a", "b"))))
    }
    
    if (length(cols_to_convert_to_other_factor) > 0) {
      data_typed <- data_typed %>%
        mutate(across(all_of(cols_to_convert_to_other_factor), as.factor))
    }
    
    data_final <- data_typed %>%
      mutate(subject_id = as.character(subject_id))
      
    return(data_final)
  }

  questionnaire_data <- map_dfr(questionnaire_files, read_questionnaire_file)
  
  cat("Successfully loaded and combined questionnaire data for", length(unique(questionnaire_data$subject_id)), "subjects.\n")

  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }
  
  # Clean character fields before writing to TSV to avoid issues with newlines.
  questionnaire_data_for_tsv <- questionnaire_data %>%
    mutate(across(where(is.character), ~ str_replace_all(., "[\r\n]+", " ")))
  
  output_file <- file.path(processed_dir, "all_questionnaire_data.tsv")
  write_tsv(questionnaire_data_for_tsv, output_file)
  cat("Questionnaire data saved to", output_file, "\n")

  return(questionnaire_data)
}


#### Stan lists ####
####################

StanCleaned <- function(debugsize = 0, saveStan = FALSE) {
  #' Create the Stan list of cleaned data.
  #'
  #' Create the list for the Stan models used to fit to the cleaned data.
  #' Missing values are recoded as -9. The output is not saved by default.
  #'
  #' @param debugsize Sample size used to debug the model. When 0, all subjects are included. Default is 0.
  #' @param saveStan Save the Stan list as `data/processed/StanList.RData`. Default is FALSE.
  #' @return A list with cleaned data in the proper format for Stan models.

  # Load data
  task_data_path <- "data/processed/all_task_data.tsv"
  questionnaire_data_path <- "data/processed/all_questionnaire_data.tsv"
  if (file.exists(task_data_path)) {
    trialData <- read_tsv(task_data_path, show_col_types = FALSE)
  } else {
    trialData <- .MergeTaskData()
  }
  if (file.exists(questionnaire_data_path)) {
    questionnaireData <- read_tsv(questionnaire_data_path, show_col_types = FALSE)
  } else {
    questionnaireData <- .MergeQuestionnaireData()
  }
  mainTrialData <- trialData[trialData$mode == "main", ]

  subs <- unique(mainTrialData$subject_id)
  nSubs <- length(subs)
  if (debugsize > 0 && debugsize <= nSubs) {
    subs <- subs[1:debugsize]
    nSubs <- debugsize
  }
  maxTrials <- max(mainTrialData$trial, na.rm = TRUE)
  
  # Define values
  subTrials <- array(-9, nSubs)
  reward <- array(-9, c(nSubs, maxTrials))
  choice <- array(-9, c(nSubs, maxTrials))
  salient_feedback <- array(-9, c(nSubs, maxTrials))
  
  for (subi in 1:nSubs) {
    subid <- subs[subi]
    sub_data <- subset(mainTrialData, mainTrialData$subject_id == subid)
    
    subTrials[subi] <- nrow(sub_data)
    if (subTrials[subi] > 0) {
      for (triali in 1:subTrials[subi]) {
        current_trial_data <- sub_data[triali, ]

        if (!is.na(current_trial_data$choice)) {
          reward[subi, triali] <- current_trial_data$reward
          # Add one because R does not count from 0. Stan model expects 1 or 2.
          choice[subi, triali] <- current_trial_data$choice + 1
          
          if (!is.na(current_trial_data$condition)) {
            if (current_trial_data$condition == 1) {
              salient_feedback[subi, triali] <- 1
            } else if (current_trial_data$condition == 0) {
              salient_feedback[subi, triali] <- 0
            } # 'missed' trials (condition == 2) will not be coded, so they remain -9
          }
        }
      }
    }
  }
  
  StanList <- list(
    nSubs = nSubs, maxTrials = maxTrials, subTrials = subTrials,
    reward = reward, choice = choice, salient_feedback = salient_feedback
  )
  
  # Save list
  if (saveStan == TRUE) {
    outname <- "data/processed/StanList.RData"
    if (debugsize > 0) {
      outname <- "data/processed/StanList_debug.RData"
    } 
    save(StanList, file = outname)
  }

  return(StanList)
} 