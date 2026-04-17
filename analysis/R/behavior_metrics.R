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

glmm_prepare_stay_trial_level <- function(task_data,
                                          enforce_consecutive_trials = TRUE,
                                          exclude_fast_rt = TRUE,
                                          fast_rt_threshold = 0.15) {
  #####
  # Prepare trial-level data for GLMM of stay behavior by previous outcome
  #
  # Constructs a trial-level dataset where each row represents a valid (t-1, t)
  # decision pair for modeling P(stay_t | outcome_{t-1}).
  #
  # This is aligned with the WSLS inclusion logic used in compute_wsls_by_outcome_subject().
  # Missed trials break sequences and are not bridged.
  #
  # Inclusion rules
  # ----
  # - Current trial t must have non-missing choice.
  # - Previous trial t-1 must have non-missing choice and reward.
  # - Outcome type is derived from trial t-1:
  #   - Salient Win: prev_reward == 1 & prev_condition == 1
  #   - Non-Salient Win: prev_reward == 1 & prev_condition == 0
  #   - Loss: prev_reward == 0
  # - If enforce_consecutive_trials: require trial[t] == trial[t-1] + 1.
  # - If exclude_fast_rt: require reaction_time[t] and reaction_time[t-1] to be
  #   non-missing and >= fast_rt_threshold.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, trial, choice, reward, condition, reaction_time
  # enforce_consecutive_trials : logical
  #     If TRUE (default), require consecutive trial numbering for (t-1, t) pairs
  # exclude_fast_rt : logical
  #     If TRUE (default), exclude pairs with fast/missing RT on either t-1 or t
  # fast_rt_threshold : numeric
  #     Threshold (seconds) for defining fast RTs (default: 0.15)
  #
  # Returns
  # ----
  # tibble
  #     Trial-level GLMM data with columns:
  #     participant_id, trial, outcome_type, stayed, trial_scaled
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reaction_time" = "reaction_time" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("enforce_consecutive_trials must be logical" = is.logical(enforce_consecutive_trials))
  stopifnot("exclude_fast_rt must be logical" = is.logical(exclude_fast_rt))
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
    dplyr::filter(!is.na(choice)) %>%
    dplyr::filter(!is.na(prev_choice) & !is.na(prev_reward)) %>%
    dplyr::mutate(
      stayed = (choice == prev_choice),
      outcome_type = dplyr::case_when(
        prev_reward == 1 & prev_condition == 1 ~ "Salient Win",
        prev_reward == 1 & prev_condition == 0 ~ "Non-Salient Win",
        prev_reward == 0 ~ "Loss",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(outcome_type))

  if (isTRUE(exclude_fast_rt)) {
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

  df %>%
    dplyr::mutate(
      participant_id = as.character(participant_id),
      outcome_type = factor(outcome_type, levels = c("Loss", "Non-Salient Win", "Salient Win")),
      stayed = as.integer(stayed),
      trial_scaled = as.numeric(scale(trial))
    ) %>%
    dplyr::select(participant_id, trial, outcome_type, stayed, trial_scaled)
}


glmm_fit_stay_by_prev_outcome <- function(task_data,
                                          enforce_consecutive_trials = TRUE,
                                          exclude_fast_rt = TRUE,
                                          fast_rt_threshold = 0.15,
                                          optimizer = "bobyqa") {
  #####
  # Fit GLMM predicting stay behavior from previous outcome type
  #
  # Fits a logistic mixed-effects model:
  # stayed ~ outcome_type + trial_scaled + (1 | participant_id)
  #
  # This is a model-agnostic complement to the preregistered WSLS t-tests.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data
  # enforce_consecutive_trials : logical
  #     Passed to glmm_prepare_stay_trial_level() (default: TRUE)
  # exclude_fast_rt : logical
  #     Passed to glmm_prepare_stay_trial_level() (default: TRUE)
  # fast_rt_threshold : numeric
  #     Passed to glmm_prepare_stay_trial_level() (default: 0.15)
  # optimizer : character
  #     Optimizer name for lme4::glmerControl() (default: "bobyqa")
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - data: trial-level data used for fitting
  #     - fit: fitted glmer model
  #     - tidy_or: broom.mixed::tidy table (odds ratios, 95% CI)
  #####
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package `broom.mixed` is required for tidying GLMMs. Please install it (e.g., via renv).")
  }

  df <- glmm_prepare_stay_trial_level(
    task_data = task_data,
    enforce_consecutive_trials = enforce_consecutive_trials,
    exclude_fast_rt = exclude_fast_rt,
    fast_rt_threshold = fast_rt_threshold
  )

  fit <- lme4::glmer(
    stayed ~ outcome_type + trial_scaled + (1 | participant_id),
    data = df,
    family = stats::binomial(),
    control = lme4::glmerControl(optimizer = optimizer)
  )

  tidy_or <- broom.mixed::tidy(fit, conf.int = TRUE, exponentiate = TRUE)

  list(data = df, fit = fit, tidy_or = tidy_or)
}


glmm_report_stay_by_prev_outcome <- function(task_data,
                                             enforce_consecutive_trials = TRUE,
                                             exclude_fast_rt = TRUE,
                                             fast_rt_threshold = 0.15,
                                             optimizer = "bobyqa") {
  #####
  # Create a structured, machine-readable report for the "stay" GLMM
  #
  # Fits the stay GLMM and returns a named list containing:
  # - model specification/metadata
  # - fixed effects (log-odds + OR)
  # - random effects summary (SDs)
  # - planned contrasts via emmeans (including Salient vs Non-Salient)
  # - estimated marginal means (predicted probabilities)
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data
  # enforce_consecutive_trials : logical
  #     Passed to glmm_fit_stay_by_prev_outcome() (default: TRUE)
  # exclude_fast_rt : logical
  #     Passed to glmm_fit_stay_by_prev_outcome() (default: TRUE)
  # fast_rt_threshold : numeric
  #     Passed to glmm_fit_stay_by_prev_outcome() (default: 0.15)
  # optimizer : character
  #     Optimizer name for lme4::glmerControl() (default: "bobyqa")
  #
  # Returns
  # ----
  # list
  #     Named list with elements:
  #     specification, fixed_logit, fixed_or, random_effects, contrasts, emmeans_prob, notes
  #####
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package `broom.mixed` is required for tidying GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package `emmeans` is required for planned contrasts. Please install it (e.g., via renv).")
  }

  fitted <- glmm_fit_stay_by_prev_outcome(
    task_data = task_data,
    enforce_consecutive_trials = enforce_consecutive_trials,
    exclude_fast_rt = exclude_fast_rt,
    fast_rt_threshold = fast_rt_threshold,
    optimizer = optimizer
  )

  df <- fitted$data
  fit <- fitted$fit

  conv_msgs <- NULL
  conv_warnings <- NULL
  opt <- tryCatch(fit@optinfo, error = function(e) NULL)
  if (!is.null(opt)) {
    conv_msgs <- opt$conv$lme4$messages
    conv_warnings <- opt$conv$lme4$warnings
  }
  conv_all <- c(conv_msgs, conv_warnings)
  conv_all <- conv_all[!is.na(conv_all)]
  conv_all <- unique(as.character(conv_all))
  conv_str <- if (length(conv_all) == 0) "" else paste(conv_all, collapse = " | ")

  specification <- list(
    formula = "stay ~ outcome_type + trial_scaled + (1 | participant_id)",
    family = "binomial(link = \"logit\")",
    reference_level_outcome_type = "Loss",
    n_observations = nrow(df),
    n_participants = dplyr::n_distinct(df$participant_id),
    optimizer = optimizer,
    convergence_warnings = conv_str,
    singular_fit = lme4::isSingular(fit, tol = 1e-4)
  )

  fixed_logit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE, exponentiate = FALSE) %>%
    dplyr::transmute(
      term = .data$term,
      estimate_logit = .data$estimate,
      std.error = .data$std.error,
      statistic = .data$statistic,
      p.value = .data$p.value,
      conf.low_logit = .data$conf.low,
      conf.high_logit = .data$conf.high
    )

  fixed_or <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::transmute(
      term = .data$term,
      odds_ratio = .data$estimate,
      conf.low_or = .data$conf.low,
      conf.high_or = .data$conf.high
    )

  vc <- lme4::VarCorr(fit)
  re_sd <- attr(vc[["participant_id"]], "stddev")
  random_effects <- tibble::tibble(
    group = "participant_id",
    term = "sd__(Intercept)",
    estimate = unname(re_sd[[1]])
  )

  emm_link <- emmeans::emmeans(fit, ~ outcome_type, type = "link")
  emm_prob <- emmeans::emmeans(fit, ~ outcome_type, type = "response")

  # Planned contrasts (no multiplicity correction; planned)
  contr <- emmeans::contrast(
    emm_link,
    method = list(
      "Non-Salient Win vs Loss" = c(-1, 1, 0),
      "Salient Win vs Loss" = c(-1, 0, 1),
      "Salient Win vs Non-Salient Win" = c(0, -1, 1)
    ),
    adjust = "none"
  )

  contr_sum <- summary(contr, infer = c(TRUE, TRUE))
  contr_df <- as.data.frame(contr_sum)
  lcl_col <- if ("lower.CL" %in% names(contr_df)) "lower.CL" else if ("asymp.LCL" %in% names(contr_df)) "asymp.LCL" else NA_character_
  ucl_col <- if ("upper.CL" %in% names(contr_df)) "upper.CL" else if ("asymp.UCL" %in% names(contr_df)) "asymp.UCL" else NA_character_

  contrasts <- contr_df %>%
    dplyr::transmute(
      contrast = .data$contrast,
      estimate_logit = .data$estimate,
      std.error = .data$SE,
      z.ratio = .data$z.ratio,
      p.value = .data$p.value,
      conf.low_logit = if (!is.na(lcl_col)) .data[[lcl_col]] else NA_real_,
      conf.high_logit = if (!is.na(ucl_col)) .data[[ucl_col]] else NA_real_,
      odds_ratio = exp(.data$estimate),
      conf.low_or = if (!is.na(lcl_col)) exp(.data[[lcl_col]]) else NA_real_,
      conf.high_or = if (!is.na(ucl_col)) exp(.data[[ucl_col]]) else NA_real_
    )

  emmprob_df <- as.data.frame(summary(emm_prob, infer = c(TRUE, TRUE)))
  lcl_col <- if ("lower.CL" %in% names(emmprob_df)) "lower.CL" else if ("asymp.LCL" %in% names(emmprob_df)) "asymp.LCL" else NA_character_
  ucl_col <- if ("upper.CL" %in% names(emmprob_df)) "upper.CL" else if ("asymp.UCL" %in% names(emmprob_df)) "asymp.UCL" else NA_character_

  emmeans_prob_tbl <- emmprob_df %>%
    dplyr::transmute(
      outcome_type = as.character(.data$outcome_type),
      prob = .data$prob,
      conf.low = if (!is.na(lcl_col)) .data[[lcl_col]] else NA_real_,
      conf.high = if (!is.na(ucl_col)) .data[[ucl_col]] else NA_real_
    )

  notes <- c(
    "outcome_type reference category was `Loss`",
    "direct salience test is `Salient Win vs Non-Salient Win`",
    "contrast estimates are computed with `emmeans`",
    "logit-scale contrasts are exponentiated for odds-ratio interpretation"
  )

  list(
    specification = specification,
    fixed_logit = fixed_logit,
    fixed_or = fixed_or,
    random_effects = random_effects,
    contrasts = contrasts,
    emmeans_prob = emmeans_prob_tbl,
    notes = notes
  )
}


glmm_prepare_optimality_trial_level <- function(task_data,
                                                enforce_consecutive_trials = TRUE,
                                                exclude_ties = TRUE) {
  #####
  # Prepare trial-level data for GLMM of choice optimality by previous outcome
  #
  # Constructs a trial-level dataset for modeling whether the current choice is
  # optimal (i.e., choosing the higher-probability arm), conditional on the
  # previous trial's outcome type.
  #
  # Inclusion rules
  # ----
  # - Current trial t must have non-missing choice and reward probabilities.
  # - Previous trial t-1 must be observed (choice, reward, condition) to define prev_outcome.
  # - If exclude_ties: exclude trials where reward_prob_1 == reward_prob_2.
  # - If enforce_consecutive_trials: require trial[t] == trial[t-1] + 1.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing: participant_id, trial, choice, reward, condition, reward_prob_1, reward_prob_2
  # enforce_consecutive_trials : logical
  #     If TRUE (default), require consecutive trial numbering for (t-1, t) pairs
  # exclude_ties : logical
  #     If TRUE (default), exclude trials with equal reward probabilities
  #
  # Returns
  # ----
  # tibble
  #     Trial-level GLMM data with columns:
  #     participant_id, trial, prev_outcome, optimal_choice, trial_scaled
  #####
  stopifnot("task_data must have participant_id" = "participant_id" %in% names(task_data))
  stopifnot("task_data must have trial" = "trial" %in% names(task_data))
  stopifnot("task_data must have choice" = "choice" %in% names(task_data))
  stopifnot("task_data must have reward" = "reward" %in% names(task_data))
  stopifnot("task_data must have condition" = "condition" %in% names(task_data))
  stopifnot("task_data must have reward_prob_1" = "reward_prob_1" %in% names(task_data))
  stopifnot("task_data must have reward_prob_2" = "reward_prob_2" %in% names(task_data))
  stopifnot("enforce_consecutive_trials must be logical" = is.logical(enforce_consecutive_trials))
  stopifnot("exclude_ties must be logical" = is.logical(exclude_ties))

  df <- task_data %>%
    dplyr::arrange(participant_id, trial) %>%
    dplyr::group_by(participant_id) %>%
    dplyr::mutate(
      prev_trial = dplyr::lag(trial, 1),
      prev_reward = dplyr::lag(reward, 1),
      prev_condition = dplyr::lag(condition, 1),
      prev_choice = dplyr::lag(choice, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(choice)) %>%
    dplyr::filter(!is.na(reward_prob_1) & !is.na(reward_prob_2)) %>%
    dplyr::filter(!is.na(prev_choice) & !is.na(prev_reward) & !is.na(prev_condition)) %>%
    dplyr::mutate(
      prev_outcome = dplyr::case_when(
        prev_reward == 0 ~ "Loss",
        prev_reward == 1 & prev_condition == 0 ~ "Non-Salient Win",
        prev_reward == 1 & prev_condition == 1 ~ "Salient Win",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(prev_outcome))

  if (isTRUE(enforce_consecutive_trials)) {
    df <- df %>%
      dplyr::filter(!is.na(prev_trial) & trial == prev_trial + 1L)
  }

  if (isTRUE(exclude_ties)) {
    df <- df %>% dplyr::filter(reward_prob_1 != reward_prob_2)
  }

  df %>%
    dplyr::mutate(
      participant_id = as.character(participant_id),
      prev_outcome = factor(prev_outcome, levels = c("Loss", "Non-Salient Win", "Salient Win")),
      choice = as.integer(choice),
      optimal_choice = as.integer(ifelse(reward_prob_1 > reward_prob_2, 0L, 1L) == choice),
      trial_scaled = as.numeric(scale(trial))
    ) %>%
    dplyr::select(participant_id, trial, prev_outcome, optimal_choice, trial_scaled)
}


glmm_fit_optimal_choice_by_prev_outcome <- function(task_data,
                                                    enforce_consecutive_trials = TRUE,
                                                    exclude_ties = TRUE,
                                                    optimizer = "bobyqa") {
  #####
  # Fit GLMM predicting choice optimality from previous outcome type
  #
  # Fits a logistic mixed-effects model:
  # optimal_choice ~ prev_outcome + trial_scaled + (1 | participant_id)
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data
  # enforce_consecutive_trials : logical
  #     Passed to glmm_prepare_optimality_trial_level() (default: TRUE)
  # exclude_ties : logical
  #     Passed to glmm_prepare_optimality_trial_level() (default: TRUE)
  # optimizer : character
  #     Optimizer name for lme4::glmerControl() (default: "bobyqa")
  #
  # Returns
  # ----
  # list
  #     Named list with:
  #     - data: trial-level data used for fitting
  #     - fit: fitted glmer model
  #     - tidy_or: broom.mixed::tidy table (odds ratios, 95% CI)
  #####
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package `broom.mixed` is required for tidying GLMMs. Please install it (e.g., via renv).")
  }

  df <- glmm_prepare_optimality_trial_level(
    task_data = task_data,
    enforce_consecutive_trials = enforce_consecutive_trials,
    exclude_ties = exclude_ties
  )

  fit <- lme4::glmer(
    optimal_choice ~ prev_outcome + trial_scaled + (1 | participant_id),
    data = df,
    family = stats::binomial(),
    control = lme4::glmerControl(optimizer = optimizer)
  )

  tidy_or <- broom.mixed::tidy(fit, conf.int = TRUE, exponentiate = TRUE)

  list(data = df, fit = fit, tidy_or = tidy_or)
}


glmm_report_optimal_choice_by_prev_outcome <- function(task_data,
                                                       enforce_consecutive_trials = TRUE,
                                                       exclude_ties = TRUE,
                                                       optimizer = "bobyqa") {
  #####
  # Create a structured, machine-readable report for the "optimal choice" GLMM
  #
  # Fits the optimality GLMM and returns a named list containing:
  # - model specification/metadata
  # - fixed effects (log-odds + OR)
  # - random effects summary (SDs)
  # - planned contrasts via emmeans (including Salient vs Non-Salient)
  # - estimated marginal means (predicted probabilities)
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data
  # enforce_consecutive_trials : logical
  #     Passed to glmm_fit_optimal_choice_by_prev_outcome() (default: TRUE)
  # exclude_ties : logical
  #     Passed to glmm_fit_optimal_choice_by_prev_outcome() (default: TRUE)
  # optimizer : character
  #     Optimizer name for lme4::glmerControl() (default: "bobyqa")
  #
  # Returns
  # ----
  # list
  #     Named list with elements:
  #     specification, fixed_logit, fixed_or, random_effects, contrasts, emmeans_prob, notes
  #####
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` is required for GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("broom.mixed", quietly = TRUE)) {
    stop("Package `broom.mixed` is required for tidying GLMMs. Please install it (e.g., via renv).")
  }
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("Package `emmeans` is required for planned contrasts. Please install it (e.g., via renv).")
  }

  fitted <- glmm_fit_optimal_choice_by_prev_outcome(
    task_data = task_data,
    enforce_consecutive_trials = enforce_consecutive_trials,
    exclude_ties = exclude_ties,
    optimizer = optimizer
  )

  df <- fitted$data
  fit <- fitted$fit

  conv_msgs <- NULL
  conv_warnings <- NULL
  opt <- tryCatch(fit@optinfo, error = function(e) NULL)
  if (!is.null(opt)) {
    conv_msgs <- opt$conv$lme4$messages
    conv_warnings <- opt$conv$lme4$warnings
  }
  conv_all <- c(conv_msgs, conv_warnings)
  conv_all <- conv_all[!is.na(conv_all)]
  conv_all <- unique(as.character(conv_all))
  conv_str <- if (length(conv_all) == 0) "" else paste(conv_all, collapse = " | ")

  specification <- list(
    formula = "optimal_choice ~ prev_outcome + trial_scaled + (1 | participant_id)",
    family = "binomial(link = \"logit\")",
    reference_level_prev_outcome = "Loss",
    n_observations = nrow(df),
    n_participants = dplyr::n_distinct(df$participant_id),
    optimizer = optimizer,
    convergence_warnings = conv_str,
    singular_fit = lme4::isSingular(fit, tol = 1e-4)
  )

  fixed_logit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE, exponentiate = FALSE) %>%
    dplyr::transmute(
      term = .data$term,
      estimate_logit = .data$estimate,
      std.error = .data$std.error,
      statistic = .data$statistic,
      p.value = .data$p.value,
      conf.low_logit = .data$conf.low,
      conf.high_logit = .data$conf.high
    )

  fixed_or <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::transmute(
      term = .data$term,
      odds_ratio = .data$estimate,
      conf.low_or = .data$conf.low,
      conf.high_or = .data$conf.high
    )

  vc <- lme4::VarCorr(fit)
  re_sd <- attr(vc[["participant_id"]], "stddev")
  random_effects <- tibble::tibble(
    group = "participant_id",
    term = "sd__(Intercept)",
    estimate = unname(re_sd[[1]])
  )

  emm_link <- emmeans::emmeans(fit, ~ prev_outcome, type = "link")
  emm_prob <- emmeans::emmeans(fit, ~ prev_outcome, type = "response")

  contr <- emmeans::contrast(
    emm_link,
    method = list(
      "Non-Salient Win vs Loss" = c(-1, 1, 0),
      "Salient Win vs Loss" = c(-1, 0, 1),
      "Salient Win vs Non-Salient Win" = c(0, -1, 1)
    ),
    adjust = "none"
  )

  contr_sum <- summary(contr, infer = c(TRUE, TRUE))
  contr_df <- as.data.frame(contr_sum)
  lcl_col <- if ("lower.CL" %in% names(contr_df)) "lower.CL" else if ("asymp.LCL" %in% names(contr_df)) "asymp.LCL" else NA_character_
  ucl_col <- if ("upper.CL" %in% names(contr_df)) "upper.CL" else if ("asymp.UCL" %in% names(contr_df)) "asymp.UCL" else NA_character_

  contrasts <- contr_df %>%
    dplyr::transmute(
      contrast = .data$contrast,
      estimate_logit = .data$estimate,
      std.error = .data$SE,
      z.ratio = .data$z.ratio,
      p.value = .data$p.value,
      conf.low_logit = if (!is.na(lcl_col)) .data[[lcl_col]] else NA_real_,
      conf.high_logit = if (!is.na(ucl_col)) .data[[ucl_col]] else NA_real_,
      odds_ratio = exp(.data$estimate),
      conf.low_or = if (!is.na(lcl_col)) exp(.data[[lcl_col]]) else NA_real_,
      conf.high_or = if (!is.na(ucl_col)) exp(.data[[ucl_col]]) else NA_real_
    )

  emmprob_df <- as.data.frame(summary(emm_prob, infer = c(TRUE, TRUE)))
  lcl_col <- if ("lower.CL" %in% names(emmprob_df)) "lower.CL" else if ("asymp.LCL" %in% names(emmprob_df)) "asymp.LCL" else NA_character_
  ucl_col <- if ("upper.CL" %in% names(emmprob_df)) "upper.CL" else if ("asymp.UCL" %in% names(emmprob_df)) "asymp.UCL" else NA_character_

  emmeans_prob_tbl <- emmprob_df %>%
    dplyr::transmute(
      outcome_type = as.character(.data$prev_outcome),
      prob = .data$prob,
      conf.low = if (!is.na(lcl_col)) .data[[lcl_col]] else NA_real_,
      conf.high = if (!is.na(ucl_col)) .data[[ucl_col]] else NA_real_
    )

  notes <- c(
    "prev_outcome reference category was `Loss`",
    "direct salience test is `Salient Win vs Non-Salient Win`",
    "contrast estimates are computed with `emmeans`",
    "logit-scale contrasts are exponentiated for odds-ratio interpretation"
  )

  list(
    specification = specification,
    fixed_logit = fixed_logit,
    fixed_or = fixed_or,
    random_effects = random_effects,
    contrasts = contrasts,
    emmeans_prob = emmeans_prob_tbl,
    notes = notes
  )
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
  # Paired comparison between two outcome types with Shapiro traceability
  #
  # Takes a long-format subject-level data frame and performs a paired t-test
  # comparing values under outcome a vs outcome b for the same subjects.
  # Additionally stores a Shapiro-Wilk normality check on paired difference
  # scores so the reporting layer can document preregistered assumptions.
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
  #     Single-row summary of paired comparison including Shapiro-Wilk
  #     diagnostics for the paired difference scores.
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
      conf_high = NA_real_,
      shapiro_n = NA_integer_,
      shapiro_w = NA_real_,
      shapiro_p_value = NA_real_,
      normality_assumption_met = NA,
      recommended_test = "paired t-test (comparison unavailable)",
      assumption_note = "Outcome columns were not both available for the requested paired comparison."
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
      conf_high = NA_real_,
      shapiro_n = length(x),
      shapiro_w = NA_real_,
      shapiro_p_value = NA_real_,
      normality_assumption_met = NA,
      recommended_test = "paired t-test (Shapiro-Wilk unavailable)",
      assumption_note = "Fewer than 3 paired observations were available, so Shapiro-Wilk could not be computed."
    ))
  }

  diff_scores <- x - y
  shapiro_res <- NULL
  shapiro_note <- NA_character_

  if (length(diff_scores) >= 3 && length(diff_scores) <= 5000) {
    shapiro_res <- stats::shapiro.test(diff_scores)
  } else if (length(diff_scores) > 5000) {
    shapiro_note <- "More than 5000 paired observations were available, so Shapiro-Wilk was not computed."
  } else {
    shapiro_note <- "Fewer than 3 paired observations were available, so Shapiro-Wilk was not computed."
  }

  shapiro_p <- if (is.null(shapiro_res)) NA_real_ else shapiro_res$p.value
  normality_ok <- if (is.na(shapiro_p)) NA else shapiro_p >= 0.05
  recommended_test <- if (isTRUE(normality_ok)) {
    "paired t-test"
  } else if (identical(normality_ok, FALSE)) {
    "Wilcoxon signed-rank test"
  } else {
    "paired t-test (Shapiro-Wilk unavailable)"
  }

  tt <- stats::t.test(x, y, paired = TRUE)

  tibble::tibble(
    n = length(x),
    a = a,
    b = b,
    mean_a = mean(x),
    mean_b = mean(y),
    mean_diff = mean(diff_scores),
    sd_diff = stats::sd(diff_scores),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2]),
    shapiro_n = length(diff_scores),
    shapiro_w = if (is.null(shapiro_res)) NA_real_ else unname(shapiro_res$statistic),
    shapiro_p_value = shapiro_p,
    normality_assumption_met = normality_ok,
    recommended_test = recommended_test,
    assumption_note = shapiro_note
  )
}

test_within_subject_outcome_effect <- function(df,
                                               subject_col,
                                               outcome_col,
                                               value_col,
                                               outcome_levels,
                                               normality_alpha = 0.05) {
 #####
 # Evaluate a within-subject outcome effect with RM-ANOVA diagnostics
 #
 # Builds complete-case subject-by-condition data, checks Shapiro-Wilk
 # normality on all pairwise difference scores, computes the preregistered
 # repeated-measures ANOVA, evaluates sphericity with Mauchly's test, and
 # determines the primary omnibus test. If normality is supported for all
 # pairwise difference scores, the primary test is the repeated-measures
 # ANOVA with the appropriate sphericity correction. Otherwise, the primary
 # test is the Friedman test as a justified within-subject nonparametric
 # alternative.
 #
 # Parameters
 # ----
 # df : tibble
 #     Long-format subject-level data
 # subject_col : character
 #     Column name identifying subjects
 # outcome_col : character
 #     Column name containing outcome labels
 # value_col : character
 #     Column name containing the numeric value to compare
 # outcome_levels : character
 #     Ordered vector of outcome labels to include in the within-subject test
 # normality_alpha : numeric
 #     Alpha threshold for Shapiro-Wilk normality checks (default: 0.05)
 #
 # Returns
 # ----
 # list
 # Named list with:
 # - wide_complete: complete-case subject-by-condition tibble
 # - long_complete: complete-case long-format tibble
 # - assumptions: tibble with Shapiro-Wilk diagnostics for all pairwise
 #   difference scores
 # - rm_anova: single-row tibble with repeated-measures ANOVA results,
 #   including Greenhouse-Geisser and Huynh-Feldt corrected p-values
 # - sphericity: single-row tibble with Mauchly's test and epsilon estimates
 # - omnibus: single-row tibble summarizing the selected primary omnibus test
 #####
  stopifnot("df must be a data.frame" = is.data.frame(df))
  stopifnot("subject_col exists" = subject_col %in% names(df))
  stopifnot("outcome_col exists" = outcome_col %in% names(df))
  stopifnot("value_col exists" = value_col %in% names(df))
  stopifnot("outcome_levels must have at least 3 entries" =
              is.character(outcome_levels) && length(outcome_levels) >= 3)
  stopifnot("normality_alpha must be numeric scalar in (0, 1)" =
              is.numeric(normality_alpha) &&
              length(normality_alpha) == 1 &&
              is.finite(normality_alpha) &&
              normality_alpha > 0 &&
              normality_alpha < 1)

  wide <- df %>%
    dplyr::select(
      subject = dplyr::all_of(subject_col),
      outcome = dplyr::all_of(outcome_col),
      value = dplyr::all_of(value_col)
    ) %>%
    dplyr::filter(outcome %in% outcome_levels) %>%
    dplyr::mutate(subject = as.character(subject)) %>%
    tidyr::pivot_wider(names_from = outcome, values_from = value)

  missing_outcomes <- setdiff(outcome_levels, names(wide))
  if (length(missing_outcomes) > 0) {
    return(list(
      wide_complete = tibble::tibble(),
      long_complete = tibble::tibble(),
      rm_anova = tibble::tibble(
        n = 0L,
        k = length(outcome_levels),
        test = "repeated-measures ANOVA",
        statistic_name = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p_value_uncorrected = NA_real_,
        p_value_gg = NA_real_,
        p_value_hf = NA_real_,
        correction_used = NA_character_,
        corrected_df1 = NA_real_,
        corrected_df2 = NA_real_,
        p_value_selected = NA_real_,
        significant = NA,
        normality_assumption_met = NA,
        sphericity_assumption_met = NA,
        assumption_note = paste0(
          "The following outcome levels were unavailable: ",
          paste(missing_outcomes, collapse = ", "),
          "."
        )
      ),
      sphericity = tibble::tibble(
        n = 0L,
        mauchly_w = NA_real_,
        mauchly_p_value = NA_real_,
        sphericity_assumption_met = NA,
        gg_epsilon = NA_real_,
        hf_epsilon = NA_real_,
        correction_recommended = NA_character_,
        assumption_note = paste0(
          "The following outcome levels were unavailable: ",
          paste(missing_outcomes, collapse = ", "),
          "."
        )
      ),
      omnibus = tibble::tibble(
        n = 0L,
        k = length(outcome_levels),
        test = "comparison unavailable",
        statistic_name = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p_value = NA_real_,
        significant = NA,
        normality_assumption_met = NA,
        sphericity_assumption_met = NA,
        correction_used = NA_character_,
        deviation_from_preregistration = NA,
        recommended_test = "comparison unavailable",
        assumption_note = paste0(
          "The following outcome levels were unavailable: ",
          paste(missing_outcomes, collapse = ", "),
          "."
        ),
        deviation_note = paste0(
          "The preregistered repeated-measures ANOVA could not be evaluated because the following outcome levels were unavailable: ",
          paste(missing_outcomes, collapse = ", "),
          "."
        )
      ),
      assumptions = tibble::tibble(
        comparison = character(),
        a = character(),
        b = character(),
        shapiro_n = integer(),
        shapiro_w = numeric(),
        shapiro_p_value = numeric(),
        normality_assumption_met = logical(),
        assumption_note = character()
      )
    ))
  }

  wide_complete <- wide %>%
    dplyr::select(subject, dplyr::all_of(outcome_levels)) %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(outcome_levels), ~ is.finite(.x)))

  long_complete <- wide_complete %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(outcome_levels),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      subject = factor(subject),
      outcome = factor(outcome, levels = outcome_levels)
    )

  n_complete <- nrow(wide_complete)
  outcome_pairs <- utils::combn(outcome_levels, 2, simplify = FALSE)

  assumptions <- lapply(outcome_pairs, function(pair) {
    diff_scores <- wide_complete[[pair[1]]] - wide_complete[[pair[2]]]
    shapiro_res <- NULL
    shapiro_note <- NA_character_

    if (length(diff_scores) >= 3 && length(diff_scores) <= 5000) {
      shapiro_res <- stats::shapiro.test(diff_scores)
    } else if (length(diff_scores) > 5000) {
      shapiro_note <- "More than 5000 paired observations were available, so Shapiro-Wilk was not computed."
    } else {
      shapiro_note <- "Fewer than 3 paired observations were available, so Shapiro-Wilk was not computed."
    }

    shapiro_p <- if (is.null(shapiro_res)) NA_real_ else shapiro_res$p.value

    tibble::tibble(
      comparison = paste0(pair[1], " - ", pair[2]),
      a = pair[1],
      b = pair[2],
      shapiro_n = length(diff_scores),
      shapiro_w = if (is.null(shapiro_res)) NA_real_ else unname(shapiro_res$statistic),
      shapiro_p_value = shapiro_p,
      normality_assumption_met = if (is.na(shapiro_p)) NA else shapiro_p >= normality_alpha,
      assumption_note = shapiro_note
    )
  }) %>%
    dplyr::bind_rows()

  if (n_complete < 2) {
    return(list(
      wide_complete = wide_complete,
      long_complete = long_complete,
      rm_anova = tibble::tibble(
        n = n_complete,
        k = length(outcome_levels),
        test = "repeated-measures ANOVA",
        statistic_name = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p_value_uncorrected = NA_real_,
        p_value_gg = NA_real_,
        p_value_hf = NA_real_,
        correction_used = NA_character_,
        corrected_df1 = NA_real_,
        corrected_df2 = NA_real_,
        p_value_selected = NA_real_,
        significant = NA,
        normality_assumption_met = NA,
        sphericity_assumption_met = NA,
        assumption_note = "Fewer than 2 complete-case subjects were available for the repeated-measures ANOVA."
      ),
      sphericity = tibble::tibble(
        n = n_complete,
        mauchly_w = NA_real_,
        mauchly_p_value = NA_real_,
        sphericity_assumption_met = NA,
        gg_epsilon = NA_real_,
        hf_epsilon = NA_real_,
        correction_recommended = NA_character_,
        assumption_note = "Fewer than 2 complete-case subjects were available for sphericity diagnostics."
      ),
      omnibus = tibble::tibble(
        n = n_complete,
        k = length(outcome_levels),
        test = "comparison unavailable",
        statistic_name = NA_character_,
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p_value = NA_real_,
        significant = NA,
        normality_assumption_met = NA,
        sphericity_assumption_met = NA,
        correction_used = NA_character_,
        deviation_from_preregistration = NA,
        recommended_test = "comparison unavailable",
        assumption_note = "Fewer than 2 complete-case subjects were available for the omnibus PRP analysis.",
        deviation_note = "The preregistered repeated-measures ANOVA could not be evaluated because fewer than 2 complete-case subjects were available."
      ),
      assumptions = assumptions
    ))
  }

  all_normality_known <- nrow(assumptions) > 0 && all(is.finite(assumptions$shapiro_p_value))
  all_normality_met <- all_normality_known && all(assumptions$shapiro_p_value >= normality_alpha)

  mlm_fit <- stats::lm(as.matrix(wide_complete[outcome_levels]) ~ 1)
  mlm_null <- stats::update(mlm_fit, ~0)
  spherical_tab <- stats::anova(mlm_fit, mlm_null, X = ~1, test = "Spherical")
  spherical_row <- spherical_tab[nrow(spherical_tab), , drop = FALSE]
  heading <- attr(spherical_tab, "heading")
  gg_line <- heading[grepl("Greenhouse-Geisser epsilon:", heading, fixed = TRUE)]
  hf_line <- heading[grepl("Huynh-Feldt epsilon:", heading, fixed = TRUE)]
  gg_epsilon <- if (length(gg_line) == 0) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(sub(".*Greenhouse-Geisser epsilon:\\s*", "", gg_line[1])))
  }
  hf_epsilon <- if (length(hf_line) == 0) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(sub(".*Huynh-Feldt epsilon:\\s*", "", hf_line[1])))
  }

  mauchly_res <- tryCatch(
    stats::mauchly.test(mlm_fit, X = ~1),
    error = function(e) NULL
  )
  mauchly_p <- if (is.null(mauchly_res)) NA_real_ else mauchly_res$p.value
  mauchly_w <- if (is.null(mauchly_res)) NA_real_ else unname(mauchly_res$statistic)
  sphericity_met <- if (is.na(mauchly_p)) NA else mauchly_p >= 0.05
  correction_used <- if (isTRUE(sphericity_met)) {
    "none"
  } else if (!is.na(gg_epsilon) && gg_epsilon < 0.75) {
    "Greenhouse-Geisser"
  } else if (!is.na(hf_epsilon)) {
    "Huynh-Feldt"
  } else {
    "none"
  }

  rm_p_uncorrected <- unname(spherical_row[["Pr(>F)"]][1])
  rm_p_gg <- unname(spherical_row[["G-G Pr"]][1])
  rm_p_hf <- unname(spherical_row[["H-F Pr"]][1])
  rm_df1 <- unname(spherical_row[["num Df"]][1])
  rm_df2 <- unname(spherical_row[["den Df"]][1])
  corrected_df1 <- if (identical(correction_used, "Greenhouse-Geisser") && is.finite(gg_epsilon)) {
    rm_df1 * gg_epsilon
  } else if (identical(correction_used, "Huynh-Feldt") && is.finite(hf_epsilon)) {
    rm_df1 * hf_epsilon
  } else {
    rm_df1
  }
  corrected_df2 <- if (identical(correction_used, "Greenhouse-Geisser") && is.finite(gg_epsilon)) {
    rm_df2 * gg_epsilon
  } else if (identical(correction_used, "Huynh-Feldt") && is.finite(hf_epsilon)) {
    rm_df2 * hf_epsilon
  } else {
    rm_df2
  }
  rm_p_selected <- if (identical(correction_used, "Greenhouse-Geisser")) {
    rm_p_gg
  } else if (identical(correction_used, "Huynh-Feldt")) {
    rm_p_hf
  } else {
    rm_p_uncorrected
  }

  rm_anova <- tibble::tibble(
    n = n_complete,
    k = length(outcome_levels),
    test = "repeated-measures ANOVA",
    statistic_name = "F",
    statistic = unname(spherical_row[["F"]][1]),
    df1 = rm_df1,
    df2 = rm_df2,
    p_value_uncorrected = rm_p_uncorrected,
    p_value_gg = rm_p_gg,
    p_value_hf = rm_p_hf,
    correction_used = correction_used,
    corrected_df1 = corrected_df1,
    corrected_df2 = corrected_df2,
    p_value_selected = rm_p_selected,
    significant = ifelse(is.na(rm_p_selected), NA, rm_p_selected < 0.05),
    normality_assumption_met = if (all_normality_known) all_normality_met else NA,
    sphericity_assumption_met = sphericity_met,
    assumption_note = if (isTRUE(sphericity_met)) {
      "Sphericity was not rejected, so the uncorrected repeated-measures ANOVA p-value is retained."
    } else if (identical(correction_used, "Greenhouse-Geisser")) {
      "Sphericity was rejected, so the Greenhouse-Geisser corrected repeated-measures ANOVA p-value is retained."
    } else if (identical(correction_used, "Huynh-Feldt")) {
      "Sphericity was rejected, so the Huynh-Feldt corrected repeated-measures ANOVA p-value is retained."
    } else {
      "Sphericity diagnostics were unavailable, so the uncorrected repeated-measures ANOVA p-value is retained."
    }
  )

  sphericity <- tibble::tibble(
    n = n_complete,
    mauchly_w = mauchly_w,
    mauchly_p_value = mauchly_p,
    sphericity_assumption_met = sphericity_met,
    gg_epsilon = gg_epsilon,
    hf_epsilon = hf_epsilon,
    correction_recommended = correction_used,
    assumption_note = if (isTRUE(sphericity_met)) {
      "Mauchly's test did not reject sphericity."
    } else if (identical(sphericity_met, FALSE)) {
      "Mauchly's test rejected sphericity."
    } else {
      "Mauchly's test could not be computed."
    }
  )

  if (isTRUE(all_normality_met)) {
    omnibus <- tibble::tibble(
      n = n_complete,
      k = length(outcome_levels),
      test = "repeated-measures ANOVA",
      statistic_name = "F",
      statistic = rm_anova$statistic,
      df1 = rm_anova$corrected_df1,
      df2 = rm_anova$corrected_df2,
      p_value = rm_anova$p_value_selected,
      significant = rm_anova$significant,
      normality_assumption_met = TRUE,
      sphericity_assumption_met = sphericity_met,
      correction_used = correction_used,
      deviation_from_preregistration = FALSE,
      recommended_test = "repeated-measures ANOVA",
      assumption_note = paste0(
        "All pairwise PRP difference scores satisfied Shapiro-Wilk normality at alpha = ",
        normality_alpha,
        "."
      ),
      deviation_note = NA_character_
    )
  } else {
    friedman_res <- stats::friedman.test(as.matrix(wide_complete[outcome_levels]))

    omnibus <- tibble::tibble(
      n = n_complete,
      k = length(outcome_levels),
      test = "Friedman test",
      statistic_name = "chi-squared",
      statistic = unname(friedman_res$statistic),
      df1 = unname(friedman_res$parameter),
      df2 = NA_real_,
      p_value = friedman_res$p.value,
      significant = ifelse(is.na(friedman_res$p.value), NA, friedman_res$p.value < 0.05),
      normality_assumption_met = if (all_normality_known) FALSE else NA,
      sphericity_assumption_met = NA,
      correction_used = NA_character_,
      deviation_from_preregistration = TRUE,
      recommended_test = "Friedman test",
      assumption_note = if (all_normality_known) {
        paste0(
          "At least one pairwise PRP difference score violated Shapiro-Wilk normality at alpha = ",
          normality_alpha,
          ", so the Friedman test was used."
        )
      } else {
        "Normality could not be established for all pairwise PRP difference scores, so the Friedman test was used."
      },
      deviation_note = "The preregistration specified a repeated-measures ANOVA framework, but the primary omnibus inference was switched to the Friedman test because the within-subject normality assumption was not supported."
    )
  }

  list(
    wide_complete = wide_complete,
    long_complete = long_complete,
    rm_anova = rm_anova,
    sphericity = sphericity,
    omnibus = omnibus,
    assumptions = assumptions
  )
}

test_paired_outcome_difference_wilcoxon <- function(df,
                                                    subject_col,
                                                    outcome_col,
                                                    value_col,
                                                    a,
                                                    b) {
 #####
 # Paired Wilcoxon signed-rank comparison between two outcome conditions
 #
 # Computes a paired Wilcoxon signed-rank test between two within-subject
 # outcome conditions. This helper is intended for post-hoc follow-up
 # comparisons after a significant Friedman test.
 #
 # Parameters
 # ----
 # df : tibble
 #     Long-format subject-level data
 # subject_col : character
 #     Column name identifying subjects
 # outcome_col : character
 #     Column name containing outcome labels
 # value_col : character
 #     Column name containing the numeric value to compare
 # a : character
 #     Outcome label for condition A
 # b : character
 #     Outcome label for condition B
 # Returns
 # ----
 # tibble
 # Single-row summary containing descriptive means and the Wilcoxon test
 # result for the paired comparison.
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
      test_used = "comparison unavailable",
      statistic_name = NA_character_,
      statistic = NA_real_,
      df = NA_real_,
      p_value_raw = NA_real_,
      p_value_adjusted = NA_real_,
      adjustment_method = NA_character_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      assumption_note = "Outcome columns were not both available for the requested paired comparison."
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
      test_used = "comparison unavailable",
      statistic_name = NA_character_,
      statistic = NA_real_,
      df = NA_real_,
      p_value_raw = NA_real_,
      p_value_adjusted = NA_real_,
      adjustment_method = NA_character_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      assumption_note = "Fewer than 2 paired observations were available for inference."
    ))
  }

  diff_scores <- x - y
  test_res <- suppressWarnings(stats::wilcox.test(
    x,
    y,
    paired = TRUE,
    exact = FALSE,
    conf.int = TRUE
  ))

  tibble::tibble(
    n = length(x),
    a = a,
    b = b,
    mean_a = mean(x),
    mean_b = mean(y),
    mean_diff = mean(diff_scores),
    sd_diff = stats::sd(diff_scores),
    test_used = "Wilcoxon signed-rank test",
    statistic_name = "V",
    statistic = unname(test_res$statistic),
    df = NA_real_,
    p_value_raw = test_res$p.value,
    p_value_adjusted = NA_real_,
    adjustment_method = "holm",
    conf_low = if (!is.null(test_res$conf.int)) unname(test_res$conf.int[1]) else NA_real_,
    conf_high = if (!is.null(test_res$conf.int)) unname(test_res$conf.int[2]) else NA_real_,
    assumption_note = NA_character_
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
  # P(stay|Loss) vs P(stay|Win) in a paired t-test. Also stores a
  # Shapiro-Wilk check on paired difference scores for reporting traceability.
  #
  # Parameters
  # ----
  # wsls_by_outcome_subj : tibble
  #     Output from compute_wsls_by_outcome_subject()
  #
  # Returns
  # ----
  # tibble
  #     Single-row paired comparison summary (Loss vs All Wins) including
  #     Shapiro-Wilk diagnostics for the paired difference scores.
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
      conf_high = NA_real_,
      shapiro_n = length(x),
      shapiro_w = NA_real_,
      shapiro_p_value = NA_real_,
      normality_assumption_met = NA,
      recommended_test = "paired t-test (Shapiro-Wilk unavailable)",
      assumption_note = "Fewer than 3 paired observations were available, so Shapiro-Wilk could not be computed."
    ))
  }

  diff_scores <- x - y
  shapiro_res <- NULL
  shapiro_note <- NA_character_

  if (length(diff_scores) >= 3 && length(diff_scores) <= 5000) {
    shapiro_res <- stats::shapiro.test(diff_scores)
  } else if (length(diff_scores) > 5000) {
    shapiro_note <- "More than 5000 paired observations were available, so Shapiro-Wilk was not computed."
  } else {
    shapiro_note <- "Fewer than 3 paired observations were available, so Shapiro-Wilk was not computed."
  }

  shapiro_p <- if (is.null(shapiro_res)) NA_real_ else shapiro_res$p.value
  normality_ok <- if (is.na(shapiro_p)) NA else shapiro_p >= 0.05
  recommended_test <- if (isTRUE(normality_ok)) {
    "paired t-test"
  } else if (identical(normality_ok, FALSE)) {
    "Wilcoxon signed-rank test"
  } else {
    "paired t-test (Shapiro-Wilk unavailable)"
  }

  tt <- stats::t.test(x, y, paired = TRUE)
  tibble::tibble(
    n = length(x),
    mean_a = mean(x),
    mean_b = mean(y),
    mean_diff = mean(diff_scores),
    sd_diff = stats::sd(diff_scores),
    t = unname(tt$statistic),
    df = unname(tt$parameter),
    p_value = tt$p.value,
    conf_low = unname(tt$conf.int[1]),
    conf_high = unname(tt$conf.int[2]),
    shapiro_n = length(diff_scores),
    shapiro_w = if (is.null(shapiro_res)) NA_real_ else unname(shapiro_res$statistic),
    shapiro_p_value = shapiro_p,
    normality_assumption_met = normality_ok,
    recommended_test = recommended_test,
    assumption_note = shapiro_note
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

test_prp_by_outcome <- function(prp_by_outcome_subj) {
 #####
 # Test PRP differences across loss, non-salient win, and salient win
 #
 # Runs a complete-case within-subject PRP analysis across the three
 # preregistered outcome categories. The preregistered repeated-measures
 # ANOVA is always computed and accompanied by Mauchly's test plus
 # Greenhouse-Geisser / Huynh-Feldt corrections. If within-subject
 # normality is not supported, the primary omnibus inference is switched to a
 # Friedman test as a justified deviation from the preregistration. Pairwise
 # follow-up tests are only conducted when the selected omnibus test is
 # significant.
 #
 # Parameters
 # ----
 # prp_by_outcome_subj : tibble
 #     Output from compute_prp_median_by_outcome_subject()
 #
 # Returns
 # ----
 # list
 # Named list with:
 # - omnibus: single-row tibble for the selected primary PRP omnibus test
 # - omnibus_assumptions: tibble with Shapiro-Wilk diagnostics for all pairwise
 #   difference scores used to choose the primary omnibus test
 # - rm_anova: single-row tibble with preregistered repeated-measures ANOVA
 #   results, including sphericity corrections
 # - sphericity: single-row tibble with Mauchly's test and epsilon estimates
 # - pairwise: tibble with post-hoc comparisons from the selected omnibus
 #   framework
 # - pairwise_note: character string describing whether pairwise tests were run
 #####
  stopifnot("prp must have median_prp" = "median_prp" %in% names(prp_by_outcome_subj))
  stopifnot("prp must have outcome_type_prp" = "outcome_type_prp" %in% names(prp_by_outcome_subj))
  stopifnot("prp must have participant_id" = "participant_id" %in% names(prp_by_outcome_subj))

  outcome_levels <- c("Post-Loss", "Post-Non-Salient Win", "Post-Salient Win")
  omnibus_res <- test_within_subject_outcome_effect(
    df = prp_by_outcome_subj,
    subject_col = "participant_id",
    outcome_col = "outcome_type_prp",
    value_col = "median_prp",
    outcome_levels = outcome_levels
  )

  pairwise <- tibble::tibble()
  pairwise_note <- "Pairwise follow-up tests were not conducted."

  if (nrow(omnibus_res$omnibus) > 0 &&
      isTRUE(omnibus_res$omnibus$significant[1]) &&
      identical(omnibus_res$omnibus$test[1], "repeated-measures ANOVA")) {
    fit_blocked <- stats::lm(value ~ subject + outcome, data = omnibus_res$long_complete)
    emm <- emmeans::emmeans(fit_blocked, ~ outcome)
    emm_means <- as.data.frame(summary(emm)) %>%
      dplyr::select(outcome, emmean)
    pairwise <- as.data.frame(emmeans::pairs(emm, adjust = "tukey", infer = c(TRUE, TRUE))) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        a = stringr::str_split_fixed(contrast, " - ", 2)[, 1],
        b = stringr::str_split_fixed(contrast, " - ", 2)[, 2]
      ) %>%
      dplyr::left_join(
        emm_means %>% dplyr::rename(a = outcome, mean_a = emmean),
        by = "a"
      ) %>%
      dplyr::left_join(
        emm_means %>% dplyr::rename(b = outcome, mean_b = emmean),
        by = "b"
      ) %>%
      dplyr::transmute(
        n = dplyr::n_distinct(omnibus_res$long_complete$subject),
        a = a,
        b = b,
        mean_a = mean_a,
        mean_b = mean_b,
        mean_diff = estimate,
        sd_diff = NA_real_,
        test_used = "Tukey-adjusted emmeans contrasts",
        statistic_name = "t",
        statistic = t.ratio,
        df = df,
        p_value_raw = NA_real_,
        p_value_adjusted = p.value,
        adjustment_method = "tukey",
        conf_low = lower.CL,
        conf_high = upper.CL,
        assumption_note = "Parametric follow-up contrasts were conducted because the repeated-measures ANOVA was the selected primary omnibus test."
      )
    pairwise_note <- "Pairwise follow-up tests were conducted as Tukey-adjusted estimated marginal mean contrasts."
  } else if (nrow(omnibus_res$omnibus) > 0 &&
             isTRUE(omnibus_res$omnibus$significant[1]) &&
             identical(omnibus_res$omnibus$test[1], "Friedman test")) {
    pairwise_pairs <- utils::combn(outcome_levels, 2, simplify = FALSE)
    pairwise <- lapply(pairwise_pairs, function(pair) {
      test_paired_outcome_difference_wilcoxon(
        df = prp_by_outcome_subj,
        subject_col = "participant_id",
        outcome_col = "outcome_type_prp",
        value_col = "median_prp",
        a = pair[1],
        b = pair[2]
      )
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        p_value_adjusted = stats::p.adjust(p_value_raw, method = "holm"),
        significant_adjusted = dplyr::if_else(is.na(p_value_adjusted), NA, p_value_adjusted < 0.05)
      )
    pairwise_note <- "Pairwise follow-up tests were conducted as Wilcoxon signed-rank tests with Holm correction because the Friedman test was the selected primary omnibus test."
  } else if (nrow(omnibus_res$omnibus) > 0 &&
             !isTRUE(omnibus_res$omnibus$significant[1])) {
    pairwise_note <- "Pairwise follow-up tests were not conducted because the selected omnibus PRP test was not significant."
  }

  list(
    omnibus = omnibus_res$omnibus,
    omnibus_assumptions = omnibus_res$assumptions,
    rm_anova = omnibus_res$rm_anova,
    sphericity = omnibus_res$sphericity,
    pairwise = pairwise,
    pairwise_note = pairwise_note
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
