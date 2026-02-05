preprocess <- function(data_raw) {
  #####
  # Preprocess raw task and questionnaire data
  #
  # Takes the raw data list (task + questionnaire tibbles) and performs
  # minimal cleaning: validation, filtering to main trials, type conversions,
  # and adding derived columns needed for analysis.
  #
 # Parameters
 # ----
  # data_raw : list
 #     Named list with 'task' and 'questionnaire' tibbles from io_read_raw()
  #
  # Returns
 # ----
  # list
 #     Named list with cleaned 'task' and 'questionnaire' tibbles
  #####

  task <- data_raw$task
  quest <- data_raw$questionnaire

  # Validation: check required columns exist
  required_task_cols <- c("participant_id", "mode", "trial", "choice", "reward",
                          "condition", "reward_prob_1", "reward_prob_2", "reaction_time")
  required_quest_cols <- c("participant_id", "bis_total", "ss_total")

  stopifnot("task data missing required columns" =
              all(required_task_cols %in% names(task)))
  stopifnot("questionnaire data missing required columns" =
              all(required_quest_cols %in% names(quest)))

  # Filter to main trials only (exclude practice)
  task <- task %>% dplyr::filter(mode == "main")

  # Ensure correct types
  task <- task %>%
    dplyr::mutate(
      participant_id = as.character(participant_id),
      choice = as.numeric(choice),
      reward = as.numeric(reward),
      condition = as.numeric(condition),
      trial = as.integer(trial),
      reaction_time = as.numeric(reaction_time),
      missed = is.na(choice)
    )

  # Drop redundant mode column (all rows are "main" after filtering)
  task <- task %>%
    dplyr::select(-mode)

  # Ensure questionnaire participant_id is character
  quest <- quest %>%
    dplyr::mutate(participant_id = as.character(participant_id))

  # Return cleaned data
  list(task = task, questionnaire = quest)
}


prepare_stan_data <- function(task_data) {
  #####
  # Prepare task data for Stan hierarchical RL models
  #
  # Converts long-format cleaned task data into Stan-compatible rectangular
  # arrays. Missing/excluded trials are coded as -9 (sentinel value).
  #
  # IMPORTANT CONVENTION
  # - Trials are *compacted* within each subject: only valid trials are kept,
  #   in chronological order, and written to columns 1:subTrials[subj].
  # - All remaining columns (subTrials+1:maxTrials) are *padding* and remain -9.
  # - This matches the Stan models, which iterate `triali in 1:subTrials[subi]`.
  # Matches format expected by rl_ncp_shift_persev_uniform.stan and related models.
  #
  # Parameters
  # ----
  # task_data : tibble/data.frame
  #     Cleaned task data from preprocess() with columns: participant_id,
  #     trial, choice, reward, condition, missed.
  #     NOTE: Pass data_proc$task, not the whole data_proc list!
  #
  # Returns
  # ----
  # list
  #     Stan data list with:
  #     - nSubs: number of subjects (integer)
  #     - maxTrials: maximum number of trials across subjects (integer)
  #     - subTrials: number of valid (non-missed) trials per subject (integer vector)
  #     - reward: reward matrix (nSubs × maxTrials), values 0/1, -9 for missing
  #     - choice: choice matrix (nSubs × maxTrials), values 1/2, -9 for missing
  #     - salient_feedback: condition matrix (nSubs × maxTrials), values 0/1, -9 for missing
  #####

  # Get sorted unique subjects for consistent ordering
  subj_ids <- sort(unique(task_data$participant_id))
  nSubs <- length(subj_ids)

  # maxTrials is the maximum number of recorded main-trial rows for any subject.
  # (With 200 main trials, this should typically be 200, but we keep it data-driven.)
  maxTrials <- task_data %>%
    dplyr::count(participant_id, name = "n_trials_recorded") %>%
    dplyr::pull("n_trials_recorded") %>%
    max(na.rm = TRUE)

  # Initialize rectangular arrays with -9 sentinel value
  reward <- matrix(-9L, nrow = nSubs, ncol = maxTrials)
  choice <- matrix(-9L, nrow = nSubs, ncol = maxTrials)
  salient_feedback <- matrix(-9L, nrow = nSubs, ncol = maxTrials)
  subTrials <- integer(nSubs)

  # Fill arrays subject by subject
  for (i in seq_along(subj_ids)) {
    subj_data <- task_data %>%
      dplyr::filter(participant_id == subj_ids[i]) %>%
      dplyr::arrange(trial)

    # Define validity for RL likelihood/PPC: observed choice, reward, and (if present)
    # salient feedback indicator must be in their valid ranges.
    is_valid <- (!is.na(subj_data$choice) & subj_data$choice %in% c(0, 1)) &
      (!is.na(subj_data$reward) & subj_data$reward %in% c(0, 1)) &
      (!is.na(subj_data$condition) & subj_data$condition %in% c(0, 1))

    valid_rows <- which(is_valid)
    subTrials[i] <- length(valid_rows)

    if (subTrials[i] > 0) {
      # Compact valid trials into columns 1:subTrials (padding stays -9)
      reward[i, seq_len(subTrials[i])] <- as.integer(subj_data$reward[valid_rows])
      choice[i, seq_len(subTrials[i])] <- as.integer(subj_data$choice[valid_rows]) + 1L  # 0/1 → 1/2 for Stan
      salient_feedback[i, seq_len(subTrials[i])] <- as.integer(subj_data$condition[valid_rows])
    }
  }

  if (any(subTrials < 1L)) {
    bad_ids <- subj_ids[subTrials < 1L]
    stop(
      "prepare_stan_data(): Found subjects with 0 valid trials after filtering. ",
      "These cannot be passed to Stan because subTrials has <lower=1>. ",
      "Subject IDs: ", paste(bad_ids, collapse = ", ")
    )
  }

  list(
    nSubs = nSubs,
    maxTrials = maxTrials,
    subTrials = subTrials,
    reward = reward,
    choice = choice,
    salient_feedback = salient_feedback
  )
}
