compute_cleaning_stats_subject <- function(task_data, fast_rt_threshold = 0.2) {
  #####
  # Compute subject-level data cleaning statistics
  #
  # Calculates trial counts, missed trials, fast reaction times, and validity percentages
  # for each participant.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing: participant_id, trial, choice, reaction_time
  # fast_rt_threshold : numeric
  #     Threshold in seconds for defining "fast" reaction times (default: 0.2)
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary statistics
  #####

  task_data %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      total_main_trials = dplyr::n(),
      n_missed_choice = sum(is.na(choice)),
      n_fast_rt = sum(!is.na(reaction_time) & reaction_time < fast_rt_threshold),
      n_valid_trials = sum(!is.na(choice) & (is.na(reaction_time) | reaction_time >= fast_rt_threshold)),
      n_invalid_trials = n_missed_choice + n_fast_rt,
      percent_valid_trials = (n_valid_trials / total_main_trials) * 100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id)
}

compute_cleaning_stats_aggregate <- function(subject_stats) {
  #####
  # Compute aggregate data cleaning statistics across all participants
  #
  # Aggregates the subject-level statistics to provide a cohort-level overview.
  #
  # Parameters
  # ----
  # subject_stats : tibble
  #     Output from compute_cleaning_stats_subject()
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary of data quality
  #####

  tibble::tibble(
    "N Participants" = nrow(subject_stats),
    "Total Valid Trials" = sum(subject_stats$n_valid_trials),
    "Total Invalid Trials" = sum(subject_stats$n_invalid_trials),
    "N With Missed Trials" = sum(subject_stats$n_missed_choice > 0),
    "N With Fast RT" = sum(subject_stats$n_fast_rt > 0),
    "Mean Valid Trials (%)" = mean(subject_stats$percent_valid_trials),
    "SD Valid Trials (%)" = sd(subject_stats$percent_valid_trials),
    "Min Valid Trials (%)" = min(subject_stats$percent_valid_trials),
    "Max Valid Trials (%)" = max(subject_stats$percent_valid_trials)
  )
}

compute_stimulus_preference_subject <- function(task_data) {
  #####
  # Compute subject-level stimulus preference (choice proportions)
  #
  # Calculates, for each participant, the number and proportion of choices
  # for stimulus 0 vs stimulus 1. Missed trials (choice == NA) are excluded.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing at least:
  #     participant_id, choice, missed (or choice NA indicates missed).
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns:
  #     participant_id, n_valid_trials, n_choice_0, n_choice_1,
  #     prop_choice_0, prop_choice_1
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))

  task_data %>%
    dplyr::filter(!is.na(choice)) %>%
    dplyr::mutate(choice = as.integer(choice)) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      n_valid_trials = dplyr::n(),
      n_choice_0 = sum(choice == 0L, na.rm = TRUE),
      n_choice_1 = sum(choice == 1L, na.rm = TRUE),
      prop_choice_0 = n_choice_0 / n_valid_trials,
      prop_choice_1 = n_choice_1 / n_valid_trials,
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id)
}

compute_stimulus_preference_aggregate <- function(task_data) {
  #####
  # Compute aggregate stimulus preference (overall choice proportions)
  #
  # Aggregates all valid (non-missed) choices across participants.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing at least:
  #     participant_id, choice.
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary with counts and proportions for stimulus 0 vs 1.
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))

  valid <- task_data %>%
    dplyr::filter(!is.na(choice)) %>%
    dplyr::mutate(choice = as.integer(choice))

  n_participants <- dplyr::n_distinct(valid$participant_id)
  n_valid_trials <- nrow(valid)
  n_choice_0 <- sum(valid$choice == 0L, na.rm = TRUE)
  n_choice_1 <- sum(valid$choice == 1L, na.rm = TRUE)

  tibble::tibble(
    "N Participants" = n_participants,
    "Total Valid Trials" = n_valid_trials,
    "N Choice Stimulus 0" = n_choice_0,
    "N Choice Stimulus 1" = n_choice_1,
    "Prop Choice Stimulus 0" = n_choice_0 / n_valid_trials,
    "Prop Choice Stimulus 1" = n_choice_1 / n_valid_trials
  )
}

test_stimulus_preference <- function(stimulus_pref_subject, mu = 0.5) {
  #####
  # Test for overall stimulus preference (t-test against chance)
  #
  # Performs a one-sample t-test of each participant's proportion of choosing
  # stimulus 1 against mu (default: 0.5 = chance for two options).
  #
  # Parameters
  # ----
  # stimulus_pref_subject : tibble
  #     Output from compute_stimulus_preference_subject()
  # mu : numeric
  #     Null hypothesis mean proportion (default: 0.5)
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary of the t-test (t, df, p, CI, mean, SD).
  #####
  stopifnot("stimulus_pref_subject must have prop_choice_1" = "prop_choice_1" %in% names(stimulus_pref_subject))

  x <- stimulus_pref_subject$prop_choice_1
  x <- x[is.finite(x)]

  if (length(x) < 2) {
    return(tibble::tibble(
      n = length(x),
      mu = mu,
      mean_prop_choice_1 = ifelse(length(x) == 1, x[1], NA_real_),
      sd_prop_choice_1 = NA_real_,
      mean_diff_from_mu = ifelse(length(x) == 1, x[1] - mu, NA_real_),
      t = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  tt <- stats::t.test(x, mu = mu)

  tibble::tibble(
    n = length(x),
    mu = mu,
    mean_prop_choice_1 = mean(x),
    sd_prop_choice_1 = stats::sd(x),
    mean_diff_from_mu = mean(x) - mu,
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2])
  )
}

compute_rt_subject_summary <- function(task_data, fast_rt_threshold = 0.2) {
  #####
  # Compute subject-level reaction time (RT) summary statistics
  #
  # Calculates per-participant RT distribution summaries on trials with a valid
  # choice (choice != NA) and non-missing RT. Also reports the fraction of
  # "fast RT" trials below the specified threshold.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing: participant_id, choice, reaction_time
  # fast_rt_threshold : numeric
  #     Threshold in seconds for defining "fast" reaction times (default: 0.2)
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns:
  #     participant_id, n_trials_rt, mean_rt, median_rt, sd_rt, min_rt, max_rt,
  #     n_fast_rt, percent_fast_rt
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))

  task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reaction_time)) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      n_trials_rt = dplyr::n(),
      mean_rt = mean(reaction_time, na.rm = TRUE),
      median_rt = stats::median(reaction_time, na.rm = TRUE),
      sd_rt = stats::sd(reaction_time, na.rm = TRUE),
      min_rt = min(reaction_time, na.rm = TRUE),
      max_rt = max(reaction_time, na.rm = TRUE),
      n_fast_rt = sum(reaction_time < fast_rt_threshold, na.rm = TRUE),
      percent_fast_rt = (n_fast_rt / n_trials_rt) * 100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(percent_fast_rt), participant_id)
}

compute_rt_aggregate_summary <- function(task_data, fast_rt_threshold = 0.2) {
  #####
  # Compute aggregate reaction time (RT) summary statistics
  #
  # Aggregates RTs across all participants on trials with valid choice and
  # non-missing RT, providing descriptive statistics and fast-RT prevalence.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing: participant_id, choice, reaction_time
  # fast_rt_threshold : numeric
  #     Threshold in seconds for defining "fast" reaction times (default: 0.2)
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary table.
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))

  rt <- task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reaction_time))

  n_trials <- nrow(rt)
  n_participants <- dplyr::n_distinct(rt$participant_id)
  n_fast <- sum(rt$reaction_time < fast_rt_threshold, na.rm = TRUE)

  tibble::tibble(
    "N Participants" = n_participants,
    "N Trials (RT available)" = n_trials,
    "Mean RT (s)" = mean(rt$reaction_time, na.rm = TRUE),
    "Median RT (s)" = stats::median(rt$reaction_time, na.rm = TRUE),
    "SD RT (s)" = stats::sd(rt$reaction_time, na.rm = TRUE),
    "Min RT (s)" = min(rt$reaction_time, na.rm = TRUE),
    "Max RT (s)" = max(rt$reaction_time, na.rm = TRUE),
    "N Fast RT" = n_fast,
    "Fast RT (%)" = (n_fast / n_trials) * 100
  )
}

compute_rt_over_trial_bins <- function(task_data, n_bins = 10, fast_rt_threshold = 0.2) {
  #####
  # Summarize reaction times over trial bins (aggregate across subjects)
  #
  # Bins trials (by trial number) into n_bins equally-sized bins and computes
  # the median RT and SEM per bin on valid-choice trials with non-missing RT.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing: trial, choice, reaction_time
  # n_bins : integer
  #     Number of bins over trials (default: 10)
  # fast_rt_threshold : numeric
  #     Threshold in seconds for defining "fast" reaction times (used only for consistency;
  #     RTs are not filtered by default here; default: 0.2)
  #
  # Returns
  # ----
  # tibble
  #     Data frame with one row per bin: trial_bin, n, mean_rt, median_rt, sem_rt.
  #####
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))
  stopifnot("n_bins must be >= 2" = is.numeric(n_bins) && n_bins >= 2)

  rt <- task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reaction_time)) %>%
    dplyr::mutate(trial_bin = dplyr::ntile(trial, n_bins))

  rt %>%
    dplyr::group_by(trial_bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_rt = mean(reaction_time, na.rm = TRUE),
      median_rt = stats::median(reaction_time, na.rm = TRUE),
      sem_rt = stats::sd(reaction_time, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    ) %>%
    dplyr::arrange(trial_bin)
}

compute_rt_by_condition <- function(task_data) {
  #####
  # Prepare RT data and summaries by feedback condition
  #
  # Filters to trials with non-missing choice and RT, then keeps only feedback
  # conditions 0 (Non-Salient) and 1 (Salient). Returns both a trial-level
  # data frame (with condition labels) and an aggregate summary table.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data (from preprocess) containing: choice, reaction_time, condition
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - data: tibble with condition_label and reaction_time
  #     - summary: tibble summarizing RT by condition
  #####
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))

  df <- task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reaction_time)) %>%
    dplyr::filter(condition %in% c(0, 1)) %>%
    dplyr::mutate(
      condition_label = dplyr::case_when(
        condition == 0 ~ "Non-Salient",
        condition == 1 ~ "Salient",
        TRUE ~ NA_character_
      )
    )

  summary <- df %>%
    dplyr::group_by(condition_label) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_rt = mean(reaction_time, na.rm = TRUE),
      median_rt = stats::median(reaction_time, na.rm = TRUE),
      sd_rt = stats::sd(reaction_time, na.rm = TRUE),
      min_rt = min(reaction_time, na.rm = TRUE),
      max_rt = max(reaction_time, na.rm = TRUE),
      .groups = "drop"
    )

  list(data = df, summary = summary)
}

compute_accuracy_trial_level <- function(task_data) {
  #####
  # Compute trial-level accuracy (chose higher reward-probability option)
  #
  # Defines accuracy as choosing the option with the higher reward probability:
  # - If reward_prob_1 > reward_prob_2, choice == 0 is "correct"
  # - If reward_prob_2 > reward_prob_1, choice == 1 is "correct"
  # - If reward_prob_1 == reward_prob_2, assigns 0.5 (neutral; legacy behavior)
  #
  # Missed trials (choice == NA) and trials with missing reward probabilities are excluded.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, trial, choice, reward_prob_1, reward_prob_2
  #
  # Returns
  # ----
  # tibble
  #     Trial-level data with column chose_better_option in {0, 0.5, 1}.
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward_prob_1" = "reward_prob_1" %in% names(task_data))
  stopifnot("task_data must have reward_prob_2" = "reward_prob_2" %in% names(task_data))

  task_data %>%
    dplyr::filter(!is.na(choice)) %>%
    dplyr::filter(!is.na(reward_prob_1) & !is.na(reward_prob_2)) %>%
    dplyr::mutate(
      choice = as.integer(choice),
      chose_better_option = dplyr::case_when(
        reward_prob_1 > reward_prob_2 ~ ifelse(choice == 0L, 1, 0),
        reward_prob_2 > reward_prob_1 ~ ifelse(choice == 1L, 1, 0),
        reward_prob_1 == reward_prob_2 ~ 0.5,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(chose_better_option))
}

compute_accuracy_subject <- function(task_data) {
  #####
  # Compute subject-level accuracy summary
  #
  # Computes per-participant mean accuracy using chose_better_option from
  # compute_accuracy_trial_level().
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing reward probabilities and choices
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns: participant_id, mean_accuracy, n_valid_trials
  #####
  acc <- compute_accuracy_trial_level(task_data)

  acc %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      mean_accuracy = mean(chose_better_option, na.rm = TRUE),
      n_valid_trials = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mean_accuracy, participant_id)
}

compute_accuracy_aggregate <- function(accuracy_subject) {
  #####
  # Compute aggregate accuracy summary across participants
  #
  # Parameters
  # ----
  # accuracy_subject : tibble
  #     Output from compute_accuracy_subject()
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary table.
  #####
  stopifnot("accuracy_subject must have mean_accuracy" = "mean_accuracy" %in% names(accuracy_subject))

  x <- accuracy_subject$mean_accuracy
  x <- x[is.finite(x)]

  tibble::tibble(
    "N Participants" = length(x),
    "Mean Accuracy" = mean(x, na.rm = TRUE),
    "Median Accuracy" = stats::median(x, na.rm = TRUE),
    "SD Accuracy" = stats::sd(x, na.rm = TRUE),
    "Min Accuracy" = min(x, na.rm = TRUE),
    "Max Accuracy" = max(x, na.rm = TRUE)
  )
}

test_accuracy_vs_chance <- function(accuracy_subject, mu = 0.5) {
  #####
  # One-sample t-test of accuracy vs chance
  #
  # Performs a one-sample t-test on subject-level mean accuracy against mu.
  #
  # Parameters
  # ----
  # accuracy_subject : tibble
  #     Output from compute_accuracy_subject() with mean_accuracy
  # mu : numeric
  #     Chance level (default: 0.5)
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary of the t-test (t, df, p, CI, mean, SD).
  #####
  stopifnot("accuracy_subject must have mean_accuracy" = "mean_accuracy" %in% names(accuracy_subject))

  x <- accuracy_subject$mean_accuracy
  x <- x[is.finite(x)]

  if (length(x) < 2) {
    return(tibble::tibble(
      n = length(x),
      mu = mu,
      mean_accuracy = ifelse(length(x) == 1, x[1], NA_real_),
      sd_accuracy = NA_real_,
      mean_diff_from_mu = ifelse(length(x) == 1, x[1] - mu, NA_real_),
      t = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  tt <- stats::t.test(x, mu = mu)

  tibble::tibble(
    n = length(x),
    mu = mu,
    mean_accuracy = mean(x),
    sd_accuracy = stats::sd(x),
    mean_diff_from_mu = mean(x) - mu,
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2])
  )
}

compute_accuracy_over_trial_bins <- function(task_data, n_bins = 20) {
  #####
  # Compute accuracy over trial bins (learning curve)
  #
  # Bins trials by trial number into n_bins bins and computes mean accuracy
  # across all trials, with SEM across trials (legacy behavior).
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing trial, choice, reward_prob_1, reward_prob_2
  # n_bins : integer
  #     Number of trial bins (default: 20)
  #
  # Returns
  # ----
  # tibble
  #     Data frame with: trial_bin, mean_accuracy_across_subjects, sem_accuracy, n
  #####
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("n_bins must be >= 2" = is.numeric(n_bins) && n_bins >= 2)

  acc <- compute_accuracy_trial_level(task_data) %>%
    dplyr::mutate(trial_bin = dplyr::ntile(trial, n_bins))

  acc %>%
    dplyr::group_by(trial_bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_accuracy_across_subjects = mean(chose_better_option, na.rm = TRUE),
      sem_accuracy = stats::sd(chose_better_option, na.rm = TRUE) / sqrt(n),
      .groups = "drop"
    ) %>%
    dplyr::arrange(trial_bin)
}

compute_reward_rate_by_choice <- function(task_data) {
  #####
  # Compute reward rate by chosen stimulus/arm
  #
  # Aggregates over valid-choice trials and computes the proportion of rewarded
  # outcomes for each choice (0 vs 1).
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: choice, reward
  #
  # Returns
  # ----
  # tibble
  #     Summary table with counts and reward_rate by choice_label.
  #####
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))

  df <- task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reward)) %>%
    dplyr::mutate(choice = as.integer(choice))

  df %>%
    dplyr::group_by(choice) %>%
    dplyr::summarise(
      n_rewards = sum(reward == 1, na.rm = TRUE),
      n_trials_for_choice = dplyr::n(),
      reward_rate = n_rewards / n_trials_for_choice,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      choice_label = dplyr::case_when(
        choice == 0L ~ "Stimulus 0",
        choice == 1L ~ "Stimulus 1",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::select(choice, choice_label, dplyr::everything()) %>%
    dplyr::arrange(choice)
}

compute_reward_rate_over_trial_bins <- function(task_data, n_bins = 10) {
  #####
  # Compute reward rate over trial bins (aggregate across subjects)
  #
  # Bins trials (by trial number) into n_bins and computes mean reward rate and
  # SEM across trials within each bin on valid-choice trials.
  #
  # If the task data includes reward probability columns (reward_prob_1,
  # reward_prob_2), also computes a lightweight proxy for the underlying
  # environment drift per bin: mean(max(reward_prob_1, reward_prob_2)).
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: trial, choice, reward
  # n_bins : integer
  #     Number of trial bins (default: 10)
  #
  # Returns
  # ----
  # tibble
  #     Data frame with: trial_bin, n, mean_reward_rate_bin, sem_reward_rate_bin
  #####
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))
  stopifnot("n_bins must be >= 2" = is.numeric(n_bins) && n_bins >= 2)

  df <- task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reward)) %>%
    dplyr::mutate(trial_bin = dplyr::ntile(trial, n_bins))

  has_prob_cols <- all(c("reward_prob_1", "reward_prob_2") %in% names(df))
  if (isTRUE(has_prob_cols)) {
    df <- df %>% dplyr::mutate(optimal_reward_prob = pmax(reward_prob_1, reward_prob_2))
  } else {
    df <- df %>% dplyr::mutate(optimal_reward_prob = NA_real_)
  }

  df %>%
    dplyr::group_by(trial_bin) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_reward_rate_bin = mean(reward == 1, na.rm = TRUE),
      sem_reward_rate_bin = stats::sd(reward == 1, na.rm = TRUE) / sqrt(n),
      mean_optimal_reward_prob_bin = mean(optimal_reward_prob, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(trial_bin)
}

compute_feedback_condition_proportions <- function(task_data, valid_only = TRUE) {
  #####
  # Compute proportions of feedback conditions (overall)
  #
  # Computes the distribution of feedback conditions across trials. By default
  # uses valid-choice trials only.
  #
  # NOTE: For condition == 0 (non-salient feedback), this function splits trials
  # into "Non-Salient Win" vs "Non-Salient Loss" based on reward.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: choice, condition, reward
  # valid_only : logical
  #     If TRUE, exclude missed choices (choice == NA) (default: TRUE)
  #
  # Returns
  # ----
  # tibble
  #     Table with condition_label, n, prop.
  #####
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))

  df <- task_data
  if (isTRUE(valid_only)) df <- df %>% dplyr::filter(!is.na(choice))

  df %>%
    dplyr::mutate(
      condition_label = dplyr::case_when(
        condition == 0 & reward == 1 ~ "Non-Salient Win",
        condition == 0 & reward == 0 ~ "Loss",
        condition == 1 ~ "Salient",
        condition == 2 ~ "Missed",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::count(condition_label, name = "n") %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::arrange(dplyr::desc(n))
}

compute_feedback_condition_proportions_by_subject <- function(task_data, valid_only = TRUE) {
  #####
  # Compute feedback condition proportions by subject
  #
  # Computes per-participant proportions of condition labels. By default uses
  # valid-choice trials only.
  #
  # NOTE: For condition == 0 (non-salient feedback), this function splits trials
  # into "Non-Salient Win" vs "Non-Salient Loss" based on reward.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, choice, condition, reward
  # valid_only : logical
  #     If TRUE, exclude missed choices (choice == NA) (default: TRUE)
  #
  # Returns
  # ----
  # tibble
  #     Table with participant_id, condition_label, n, prop.
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))

  df <- task_data
  if (isTRUE(valid_only)) df <- df %>% dplyr::filter(!is.na(choice))

  df %>%
    dplyr::mutate(
      condition_label = dplyr::case_when(
        condition == 0 & reward == 1 ~ "Non-Salient Win",
        condition == 0 & reward == 0 ~ "Loss",
        condition == 1 ~ "Salient",
        condition == 2 ~ "Missed",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::count(participant_id, condition_label, name = "n") %>%
    dplyr::group_by(participant_id) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(participant_id, condition_label)
}

compute_wsls_by_outcome_subject <- function(task_data,
                                            enforce_consecutive_trials = TRUE,
                                            exclude_fast_rt_wsls = TRUE,
                                            fast_rt_threshold = 0.15) {
  #####
  # Compute win/lose-stay probabilities by previous-trial outcome type (subject-level)
  #
  # For each participant, we compute P(stay | outcome on t-1), where "stay" means
  # repeating the previous choice. Outcome types are defined from previous trial:
  # - Salient Win: reward[t-1] == 1 & condition[t-1] == 1
  # - Non-Salient Win: reward[t-1] == 1 & condition[t-1] == 0
  # - Loss: reward[t-1] == 0 (condition ignored)
  #
  # Best-practice inclusion rules
  # ----
  # We only count opportunities where the behavioral sequence is unambiguous:
  # - choice[t] is non-missing
  # - choice[t-1] is non-missing
  # - reward[t-1] is non-missing
  # - for win trials: condition[t-1] must be 0/1 (non-salient/salient)
  # - if enforce_consecutive_trials: require trial[t] == trial[t-1] + 1
  # - if exclude_fast_rt_wsls: require reaction_time[t] and reaction_time[t-1]
  #   to be non-missing and >= fast_rt_threshold
  #
  # This means missed trials (choice == NA) break sequences: if trial t is missed,
  # trial t+1 is *not* treated as "post-(t outcome)" and will only contribute again
  # once a valid consecutive (t-1, t) pair exists.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, trial, choice, reward, condition, reaction_time
  # enforce_consecutive_trials : logical
  #     If TRUE (default), only count opportunities where trial[t] == trial[t-1] + 1
  # exclude_fast_rt_wsls : logical
  #     If TRUE (default), exclude WSLS opportunities where either trial t or trial t-1
  #     has a "fast" reaction time (< fast_rt_threshold) or missing reaction_time.
  # fast_rt_threshold : numeric
  #     Threshold (seconds) for defining "fast" RTs (default: 0.15)
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns:
  #     participant_id, outcome_type, n_opportunities, n_stay, prob_stay
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("enforce_consecutive_trials must be logical" = is.logical(enforce_consecutive_trials))
  stopifnot("exclude_fast_rt_wsls must be logical" = is.logical(exclude_fast_rt_wsls))
  stopifnot("fast_rt_threshold must be numeric scalar" =
              is.numeric(fast_rt_threshold) && length(fast_rt_threshold) == 1 && is.finite(fast_rt_threshold))

  df <- task_data %>%
    dplyr::arrange(participant_id, trial) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::mutate(
      prev_trial = dplyr::lag(trial, 1),
      prev_choice = dplyr::lag(choice, 1),
      prev_rt = dplyr::lag(reaction_time, 1),
      prev_reward = dplyr::lag(reward, 1),
      prev_condition = dplyr::lag(condition, 1)
    ) %>%
    dplyr::ungroup() %>%
    # Current trial must be observed (not missed)
    dplyr::filter(!is.na(choice)) %>%
    # Conditioning trial (t-1) must be observed and have an outcome
    dplyr::filter(!is.na(prev_choice) & !is.na(prev_reward)) %>%
    dplyr::mutate(
      stayed = (choice == prev_choice),
      outcome_type = dplyr::case_when(
        prev_reward == 1 & prev_condition == 1 ~ "Salient Win",
        prev_reward == 1 & prev_condition == 0 ~ "Non-Salient Win",
        prev_reward == 0 ~ "Loss",
        TRUE ~ NA_character_
      )
    )

  if (isTRUE(exclude_fast_rt_wsls)) {
    df <- df %>%
      dplyr::filter(
        !is.na(reaction_time) & reaction_time >= fast_rt_threshold,
        !is.na(prev_rt) & prev_rt >= fast_rt_threshold
      )
  }

  if (isTRUE(enforce_consecutive_trials)) {
    df <- df %>%
      dplyr::filter(!is.na(prev_trial) & trial == prev_trial + 1L)
  }

  df <- df %>%
    dplyr::filter(!is.na(outcome_type))

  df %>%
    dplyr::group_by(participant_id, outcome_type) %>%
    dplyr::summarise(
      n_opportunities = dplyr::n(),
      n_stay = sum(stayed, na.rm = TRUE),
      prob_stay = ifelse(n_opportunities > 0, n_stay / n_opportunities, NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id, outcome_type)
}

compute_prp_median_by_outcome_subject <- function(task_data,
                                                  enforce_consecutive_trials = TRUE,
                                                  require_valid_choice_conditioning = TRUE,
                                                  exclude_fast_rt_prp = TRUE,
                                                  fast_rt_threshold = 0.15) {
  #####
  # Compute post-outcome pauses (PRP) as median RT on the next trial (subject-level)
  #
  # For each participant, compute the median reaction time on trial t+1, grouped by
  # the outcome type on the conditioning trial t:
  # - Post-Salient Win: reward[t] == 1 & condition[t] == 1
  # - Post-Non-Salient Win: reward[t] == 1 & condition[t] == 0
  # - Post-Loss: reward[t] == 0
  #
  # This implementation uses an explicit next-trial mapping (via lead()),
  # rather than filtering rows and using lag(). This avoids unintentionally
  # pairing PRPs with the previous *retained* row when there are missed trials
  # or missing RT values.
  #
  # Inclusion rules
  # ----
  # Conditioning trial (t) inclusion:
  # - reward[t] must be non-missing
  # - if reward[t] == 1 (win): condition[t] must be 0/1 (non-salient/salient)
  # - if require_valid_choice_conditioning: choice[t] must be non-missing
  #
  # PRP trial (t+1) inclusion:
  # - choice[t+1] must be non-missing (i.e., not a missed choice)
  # - reaction_time[t+1] must be non-missing
  # - if exclude_fast_rt_prp: reaction_time[t+1] must be >= fast_rt_threshold
  #
  # Edge handling
  # ----
  # - Last trial per participant is automatically excluded (no t+1 to compute PRP)
  # - If enforce_consecutive_trials: only pairs where trial[t+1] == trial[t] + 1
  #   are included (guards against gaps/missing rows in the dataset).
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, trial, choice, reaction_time, reward, condition
  # enforce_consecutive_trials : logical
  #     If TRUE (default), require trial[t+1] == trial[t] + 1 for PRP pairing
  # require_valid_choice_conditioning : logical
  #     If TRUE (default), require a non-missing choice on the conditioning trial t
  # exclude_fast_rt_prp : logical
  #     If TRUE, exclude PRP trials with reaction_time[t+1] < fast_rt_threshold
  # fast_rt_threshold : numeric
  #     Threshold (seconds) for defining "fast" RTs (default: 0.15)
  #
  # Returns
  # ----
  # tibble
  #     Subject-level table with columns:
  #     participant_id, outcome_type_prp, median_prp, n_trials
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("enforce_consecutive_trials must be logical" = is.logical(enforce_consecutive_trials))
  stopifnot("require_valid_choice_conditioning must be logical" = is.logical(require_valid_choice_conditioning))
  stopifnot("exclude_fast_rt_prp must be logical" = is.logical(exclude_fast_rt_prp))
  stopifnot("fast_rt_threshold must be numeric scalar" =
              is.numeric(fast_rt_threshold) && length(fast_rt_threshold) == 1 && is.finite(fast_rt_threshold))

  df <- task_data %>%
    dplyr::arrange(participant_id, trial) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::mutate(
      next_trial = dplyr::lead(trial, 1),
      prp_choice = dplyr::lead(choice, 1),
      prp_rt = dplyr::lead(reaction_time, 1),
      outcome_type_prp = dplyr::case_when(
        reward == 1 & condition == 1 ~ "Post-Salient Win",
        reward == 1 & condition == 0 ~ "Post-Non-Salient Win",
        reward == 0 ~ "Post-Loss",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup()

  # Conditioning-trial validity (t)
  df <- df %>%
    dplyr::filter(!is.na(outcome_type_prp)) %>%
    dplyr::filter(!is.na(reward)) # explicit, even though outcome_type_prp already implies this

  if (isTRUE(require_valid_choice_conditioning)) {
    df <- df %>% dplyr::filter(!is.na(choice))
  }

  # PRP-trial validity (t+1)
  df <- df %>%
    dplyr::filter(!is.na(prp_choice) & !is.na(prp_rt))

  if (isTRUE(exclude_fast_rt_prp)) {
    df <- df %>% dplyr::filter(prp_rt >= fast_rt_threshold)
  }

  if (isTRUE(enforce_consecutive_trials)) {
    df <- df %>% dplyr::filter(!is.na(next_trial) & next_trial == trial + 1L)
  }

  df %>%
    dplyr::group_by(participant_id, outcome_type_prp) %>%
    dplyr::summarise(
      median_prp = stats::median(prp_rt, na.rm = TRUE),
      n_trials = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id, outcome_type_prp)
}

test_paired_outcome_difference <- function(df, subject_col, outcome_col, value_col, a, b) {
  #####
  # Paired t-test between two outcome types (within-subject)
  #
  # Takes a long-format subject-level data frame and performs a paired t-test
  # comparing values under outcome a vs outcome b for the same subjects.
  #
  # Parameters
  # ----
  # df : tibble
  #     Long-format subject-level data
  # subject_col : character
  #     Column name identifying subjects (e.g., "participant_id")
  # outcome_col : character
  #     Column name containing outcome labels (e.g., "outcome_type")
  # value_col : character
  #     Column name containing the numeric value to compare
  # a : character
  #     Outcome label for condition A
  # b : character
  #     Outcome label for condition B
  #
  # Returns
  # ----
  # tibble
  #     Single-row summary of paired comparison (means, mean diff, t, df, p, CI).
  #####
  stopifnot("df must be a data.frame" = is.data.frame(df))
  stopifnot("subject_col exists" = subject_col %in% names(df))
  stopifnot("outcome_col exists" = outcome_col %in% names(df))
  stopifnot("value_col exists" = value_col %in% names(df))

  wide <- df %>%
    dplyr::select(
      subject = dplyr::all_of(subject_col),
      outcome = dplyr::all_of(outcome_col),
      value = dplyr::all_of(value_col)
    ) %>%
    dplyr::filter(outcome %in% c(a, b)) %>%
    tidyr::pivot_wider(names_from = outcome, values_from = value)

  if (!(a %in% names(wide)) || !(b %in% names(wide))) {
    return(tibble::tibble(
      n = 0L,
      a = a,
      b = b,
      mean_a = NA_real_,
      mean_b = NA_real_,
      mean_diff = NA_real_,
      sd_diff = NA_real_,
      t = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  x <- wide[[a]]
  y <- wide[[b]]

  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]

  if (length(x) < 2) {
    return(tibble::tibble(
      n = length(x),
      a = a,
      b = b,
      mean_a = ifelse(length(x) == 0, NA_real_, mean(x)),
      mean_b = ifelse(length(y) == 0, NA_real_, mean(y)),
      mean_diff = ifelse(length(x) == 0, NA_real_, mean(x - y)),
      sd_diff = ifelse(length(x) < 2, NA_real_, stats::sd(x - y)),
      t = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  tt <- stats::t.test(x, y, paired = TRUE)

  tibble::tibble(
    n = length(x),
    a = a,
    b = b,
    mean_a = mean(x),
    mean_b = mean(y),
    mean_diff = mean(x - y),
    sd_diff = stats::sd(x - y),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2])
  )
}

test_wsls_salient_vs_nonsalient <- function(wsls_by_outcome_subj) {
  #####
  # Paired test: WSLS salient win vs non-salient win
  #
  # Parameters
  # ----
  # wsls_by_outcome_subj : tibble
  #     Output from compute_wsls_by_outcome_subject()
  #
  # Returns
  # ----
  # tibble
  #     Paired t-test summary (Salient Win vs Non-Salient Win).
  #####
  stopifnot("wsls must have prob_stay" = "prob_stay" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have outcome_type" = "outcome_type" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have participant_id" = "participant_id" %in% names(wsls_by_outcome_subj))

  test_paired_outcome_difference(
    df = wsls_by_outcome_subj,
    subject_col = "participant_id",
    outcome_col = "outcome_type",
    value_col = "prob_stay",
    a = "Salient Win",
    b = "Non-Salient Win"
  )
}

test_wsls_loss_vs_wins <- function(wsls_by_outcome_subj) {
  #####
  # Paired test: WSLS loss vs all wins (pooled across win types)
  #
  # Constructs per-subject P(stay|Win) by pooling Salient Win and Non-Salient Win
  # opportunities (opportunity-weighted within subject), then compares
  # P(stay|Loss) vs P(stay|Win) in a paired t-test.
  #
  # Parameters
  # ----
  # wsls_by_outcome_subj : tibble
  #     Output from compute_wsls_by_outcome_subject()
  #
  # Returns
  # ----
  # tibble
  #     Single-row paired t-test summary (Loss vs All Wins).
  #####
  stopifnot("wsls must have participant_id" = "participant_id" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have outcome_type" = "outcome_type" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have n_opportunities" = "n_opportunities" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have n_stay" = "n_stay" %in% names(wsls_by_outcome_subj))

  df <- wsls_by_outcome_subj %>%
    dplyr::filter(outcome_type %in% c("Loss", "Non-Salient Win", "Salient Win")) %>%
    dplyr::mutate(participant_id = as.character(participant_id))

  loss <- df %>%
    dplyr::filter(outcome_type == "Loss") %>%
    dplyr::select(participant_id, n_opportunities_loss = n_opportunities, n_stay_loss = n_stay) %>%
    dplyr::mutate(p_loss = ifelse(n_opportunities_loss > 0, n_stay_loss / n_opportunities_loss, NA_real_))

  win <- df %>%
    dplyr::filter(outcome_type %in% c("Non-Salient Win", "Salient Win")) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      n_opportunities_win = sum(n_opportunities, na.rm = TRUE),
      n_stay_win = sum(n_stay, na.rm = TRUE),
      p_win = ifelse(n_opportunities_win > 0, n_stay_win / n_opportunities_win, NA_real_),
      .groups = "drop"
    )

  wide <- loss %>%
    dplyr::inner_join(win, by = "participant_id") %>%
    dplyr::filter(is.finite(p_loss) & is.finite(p_win))

  x <- wide$p_loss
  y <- wide$p_win

  if (length(x) < 2) {
    return(tibble::tibble(
      n = length(x),
      mean_a = ifelse(length(x) == 0, NA_real_, mean(x)),
      mean_b = ifelse(length(y) == 0, NA_real_, mean(y)),
      mean_diff = ifelse(length(x) == 0, NA_real_, mean(x - y)),
      sd_diff = ifelse(length(x) < 2, NA_real_, stats::sd(x - y)),
      t = NA_real_,
      df = NA_real_,
      p_value = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_
    ))
  }

  tt <- stats::t.test(x, y, paired = TRUE)
  tibble::tibble(
    n = length(x),
    mean_a = mean(x),
    mean_b = mean(y),
    mean_diff = mean(x - y),
    sd_diff = stats::sd(x - y),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2])
  )
}

test_prp_salient_vs_nonsalient <- function(prp_by_outcome_subj) {
  #####
  # Paired test: PRP after salient win vs after non-salient win
  #
  # Parameters
  # ----
  # prp_by_outcome_subj : tibble
  #     Output from compute_prp_median_by_outcome_subject()
  #
  # Returns
  # ----
  # tibble
  #     Paired t-test summary (Post-Salient Win vs Post-Non-Salient Win).
  #####
  stopifnot("prp must have median_prp" = "median_prp" %in% names(prp_by_outcome_subj))
  stopifnot("prp must have outcome_type_prp" = "outcome_type_prp" %in% names(prp_by_outcome_subj))
  stopifnot("prp must have participant_id" = "participant_id" %in% names(prp_by_outcome_subj))

  test_paired_outcome_difference(
    df = prp_by_outcome_subj,
    subject_col = "participant_id",
    outcome_col = "outcome_type_prp",
    value_col = "median_prp",
    a = "Post-Salient Win",
    b = "Post-Non-Salient Win"
  )
}

compute_reward_rate_subject <- function(task_data) {
  #####
  # Compute subject-level reward rate (mean reward on valid trials)
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, choice, reward
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns: participant_id, n_valid_trials, reward_rate
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))

  task_data %>%
    dplyr::filter(!is.na(choice) & !is.na(reward)) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      n_valid_trials = dplyr::n(),
      reward_rate = mean(reward == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id)
}

compute_win_stay_overall_subject <- function(wsls_by_outcome_subj) {
  #####
  # Compute overall win-stay probability (collapsed across win types)
  #
  # Computes P(stay | rewarded on t-1) per subject by pooling Salient Win and
  # Non-Salient Win opportunities from compute_wsls_by_outcome_subject().
  #
  # Parameters
  # ----
  # wsls_by_outcome_subj : tibble
  #     Output from compute_wsls_by_outcome_subject()
  #
  # Returns
  # ----
  # tibble
  #     Subject-level summary with columns: participant_id, n_opportunities_win, n_stay_win, win_stay_prob
  #####
  stopifnot("wsls must have participant_id" = "participant_id" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have outcome_type" = "outcome_type" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have n_opportunities" = "n_opportunities" %in% names(wsls_by_outcome_subj))
  stopifnot("wsls must have n_stay" = "n_stay" %in% names(wsls_by_outcome_subj))

  wsls_by_outcome_subj %>%
    dplyr::filter(outcome_type %in% c("Salient Win", "Non-Salient Win")) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      n_opportunities_win = sum(n_opportunities, na.rm = TRUE),
      n_stay_win = sum(n_stay, na.rm = TRUE),
      win_stay_prob = ifelse(n_opportunities_win > 0, n_stay_win / n_opportunities_win, NA_real_),
      .groups = "drop"
    ) %>%
    dplyr::arrange(participant_id)
}

compute_questionnaire_subject_scores <- function(questionnaire_data, score_cols = c("bis_total", "ss_total")) {
  #####
  # Extract a clean subject-level questionnaire score table (EDA)
  #
  # Keeps participant_id and a configurable set of score columns, dropping any
  # score columns that are not present. This is used for EDA tables/plots in the
  # HTML data browser report.
  #
  # Parameters
  # ----
  # questionnaire_data : tibble
  #     Processed questionnaire data containing participant_id and score columns
  # score_cols : character
  #     Vector of score column names to extract (default: c("bis_total", "ss_total"))
  #
  # Returns
  # ----
  # tibble
  #     Subject-level questionnaire table with participant_id and present score columns
  #####
  stopifnot("questionnaire_data must have participant_id" = "participant_id" %in% names(questionnaire_data))
  stopifnot("score_cols must be character" = is.character(score_cols))

  present <- score_cols[score_cols %in% names(questionnaire_data)]
  questionnaire_data %>%
    dplyr::select(participant_id, dplyr::all_of(present)) %>%
    dplyr::mutate(participant_id = as.character(participant_id)) %>%
    dplyr::arrange(participant_id)
}

compute_questionnaire_scores_long <- function(questionnaire_data, score_cols = c("bis_total", "ss_total")) {
  #####
  # Convert questionnaire scores to long format (EDA)
  #
  # Parameters
  # ----
  # questionnaire_data : tibble
  #     Processed questionnaire data containing participant_id and score columns
  # score_cols : character
  #     Vector of score column names to pivot (default: c("bis_total", "ss_total"))
  #
  # Returns
  # ----
  # tibble
  #     Long-format table with columns: participant_id, scale_name, score
  #####
  stopifnot("questionnaire_data must have participant_id" = "participant_id" %in% names(questionnaire_data))
  stopifnot("score_cols must be character" = is.character(score_cols))

  present <- score_cols[score_cols %in% names(questionnaire_data)]
  if (length(present) == 0) {
    return(tibble::tibble(participant_id = character(), scale_name = character(), score = numeric()))
  }

  questionnaire_data %>%
    dplyr::select(participant_id, dplyr::all_of(present)) %>%
    dplyr::mutate(participant_id = as.character(participant_id)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(present),
      names_to = "scale_name",
      values_to = "score"
    ) %>%
    dplyr::filter(!is.na(score))
}

compute_questionnaire_score_summaries <- function(questionnaire_scores_long) {
  #####
  # Compute descriptive summaries for questionnaire scores (EDA)
  #
  # Parameters
  # ----
  # questionnaire_scores_long : tibble
  #     Long-format questionnaire score table with columns: scale_name, score
  #
  # Returns
  # ----
  # tibble
  #     Summary table with columns: scale_name, n, mean, median, sd, iqr, min, max
  #####
  stopifnot("questionnaire_scores_long must have scale_name" = "scale_name" %in% names(questionnaire_scores_long))
  stopifnot("questionnaire_scores_long must have score" = "score" %in% names(questionnaire_scores_long))

  questionnaire_scores_long %>%
    dplyr::group_by(scale_name) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(score, na.rm = TRUE),
      median = stats::median(score, na.rm = TRUE),
      sd = stats::sd(score, na.rm = TRUE),
      iqr = stats::IQR(score, na.rm = TRUE),
      min = min(score, na.rm = TRUE),
      max = max(score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(scale_name)
}

compute_bis_ss_relationship_stats <- function(questionnaire_data) {
  #####
  # Compute BIS-total vs SSS-total relationship summaries (EDA)
  #
  # Provides a Pearson correlation test summary and a simple linear model
  # summary for ss_total ~ bis_total.
  #
  # Parameters
  # ----
  # questionnaire_data : tibble
  #     Processed questionnaire data containing bis_total and ss_total
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - corr: tibble with correlation test summary
  #     - lm: tibble with linear model summary
  #####
  stopifnot("questionnaire_data must have bis_total" = "bis_total" %in% names(questionnaire_data))
  stopifnot("questionnaire_data must have ss_total" = "ss_total" %in% names(questionnaire_data))

  df <- questionnaire_data %>%
    dplyr::select(participant_id, bis_total, ss_total) %>%
    dplyr::filter(is.finite(bis_total) & is.finite(ss_total))

  if (nrow(df) < 3) {
    return(list(
      corr = tibble::tibble(
        n = nrow(df),
        r = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p_value = NA_real_
      ),
      lm = tibble::tibble(
        n = nrow(df),
        intercept = NA_real_,
        slope = NA_real_,
        r_squared = NA_real_,
        p_slope = NA_real_
      )
    ))
  }

  ct <- stats::cor.test(df$bis_total, df$ss_total, method = "pearson")
  lm_fit <- stats::lm(ss_total ~ bis_total, data = df)
  lm_sum <- summary(lm_fit)

  list(
    corr = tibble::tibble(
      n = nrow(df),
      r = unname(ct$estimate),
      conf_low = unname(ct$conf.int[1]),
      conf_high = unname(ct$conf.int[2]),
      t = unname(ct$statistic),
      df = unname(ct$parameter),
      p_value = ct$p.value
    ),
    lm = tibble::tibble(
      n = nrow(df),
      intercept = unname(stats::coef(lm_fit)[1]),
      slope = unname(stats::coef(lm_fit)[2]),
      r_squared = unname(lm_sum$r.squared),
      p_slope = unname(lm_sum$coefficients["bis_total", "Pr(>|t|)"])
    )
  )
}
