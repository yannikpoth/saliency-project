preprocess <- function(data_raw) {
  #####
  # Preprocess raw task and questionnaire data
  #
  # Takes the raw data list (task + questionnaire tibbles) and performs
  # minimal cleaning: validation, filtering to main trials, type conversions,
  # and adding derived columns needed for analysis.
  #
  # Parameters
  # ----------
  # data_raw : list
  #     Named list with 'task' and 'questionnaire' tibbles from io_read_raw()
  #
  # Returns
  # -------
  # list
  #     Named list with cleaned 'task' and 'questionnaire' tibbles
  #####

  task <- data_raw$task
  quest <- data_raw$questionnaire

  # Validation: check required columns exist
  required_task_cols <- c("participant_id", "mode", "trial", "choice", "reward",
                          "condition", "reward_prob_1", "reward_prob_2")
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
  maxTrials <- max(task_data$trial)

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

    # Count valid trials (non-missed)
    subTrials[i] <- sum(!subj_data$missed)

    # Fill trial data
    for (j in seq_len(nrow(subj_data))) {
      trial_idx <- subj_data$trial[j]

      if (!subj_data$missed[j]) {
        # Valid trial: convert and store
        reward[i, trial_idx] <- as.integer(subj_data$reward[j])
        choice[i, trial_idx] <- as.integer(subj_data$choice[j]) + 1L  # 0/1 → 1/2 for Stan
        salient_feedback[i, trial_idx] <- as.integer(subj_data$condition[j])
      }
      # Missed trials remain -9
    }
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
