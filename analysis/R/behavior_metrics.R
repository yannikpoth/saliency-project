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
