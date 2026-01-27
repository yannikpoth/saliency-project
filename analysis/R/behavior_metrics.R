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
