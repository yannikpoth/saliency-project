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
