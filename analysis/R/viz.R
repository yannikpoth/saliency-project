# ============================================
# Visualization Functions
# ============================================
# This script handles all plots and visualization logic for the analysis.
# It is divided into three main sections:
# 1. Data Inspection (Exploratory plots, participant-level checks)
# 2. Main Analysis (Behavioral metrics, model results)
# 3. Posterior Predictive Checks (Model validation)
# ============================================


# ============================================
# 1. Data Inspection
# ============================================

viz_inspection_participant_trials <- function(task_data, output_dir) {
  #####
  # Generate participant-level trial inspection plots
  #
  # Creates a plot for each participant showing reward probabilities for both arms
  # across trials. Trials with salient feedback (condition == 1) are marked with
  # vertical dashed lines. Actual choices that resulted in salient feedback are
  # highlighted with points.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing columns: participant_id, trial,
  #     reward_prob_1, reward_prob_2, condition, choice.
  # output_dir : character
  #     Base directory for saving figures. Plots will be saved in
  #     [output_dir]/inspection/participant_wise/combined_reward_saliency
  #
  # Returns
  # ----
  # NULL (invisible)
  #     Saves plots to disk as side effect.
  #####

  requireNamespace("ggplot2")
  requireNamespace("dplyr")

  # Define and create output directory
  target_dir <- file.path(output_dir, "inspection", "participant_wise", "combined_reward_saliency")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  message("Generating participant inspection plots (Combined Reward) in: ", target_dir)

  # Get unique participants
  participants <- unique(task_data$participant_id)

  for (pid in participants) {

    # Filter data for current participant
    subj_data <- dplyr::filter(task_data, participant_id == pid)

    # Identify salient feedback trials (condition == 1)
    salient_subset <- dplyr::filter(subj_data, condition == 1)
    salient_trials <- unique(salient_subset$trial)

    vline_data <- data.frame(trial = salient_trials)

    # Identify specific choices with salient feedback for points
    # Choice 0 -> Arm 1 (Reward Prob 1)
    # Choice 1 -> Arm 2 (Reward Prob 2)
    salient_choice_0 <- dplyr::filter(subj_data, condition == 1, choice == 0, !is.na(choice))
    salient_choice_1 <- dplyr::filter(subj_data, condition == 1, choice == 1, !is.na(choice))

    # Create plot
    p <- ggplot2::ggplot(subj_data, ggplot2::aes(x = trial)) +
      # Line for Option A (Reward Prob 1)
      ggplot2::geom_line(ggplot2::aes(y = reward_prob_1, color = "A"), linewidth = 0.8) +
      # Line for Option B (Reward Prob 2)
      ggplot2::geom_line(ggplot2::aes(y = reward_prob_2, color = "B"), linewidth = 0.8) +

      # Vertical dashed lines for salient feedback trials
      ggplot2::geom_vline(data = vline_data, ggplot2::aes(xintercept = trial),
                          color = "grey40", linetype = "dashed", linewidth = 0.5, alpha = 0.6) +

      # Points for Salient Feedback on Choice 0 (Option A)
      ggplot2::geom_point(data = salient_choice_0, ggplot2::aes(y = reward_prob_1),
                          shape = 21, color = "black", fill = "#ffc400", size = 3, stroke = 0.5) +

      # Points for Salient Feedback on Choice 1 (Option B)
      ggplot2::geom_point(data = salient_choice_1, ggplot2::aes(y = reward_prob_2),
                          shape = 21, color = "black", fill = "#ffc400", size = 3, stroke = 0.5) +

      # Color scale and labels
      ggplot2::scale_color_manual(
        values = c("A" = "dodgerblue", "B" = "firebrick"),
        labels = c("A" = "p(Reward | A)", "B" = "p(Reward | B)")
      ) +

      # Axes
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      ggplot2::labs(
        title = paste("Participant", pid, ": Combined Reward Probabilities & Salient Feedback"),
        y = "p(Reward)",
        x = "Trial"
      ) +

      # Theme
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        legend.position = c(0.98, 0.98),
        legend.justification = c("right", "top"),
        legend.title = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(
          color = "black",
          fill = ggplot2::alpha("white", 0.8),
          linetype = "solid",
          linewidth = 0.5
        )
      )

    # Save plot
    outfile <- file.path(target_dir, paste0("combined_reward_probs_subject_", pid, ".png"))
    ggplot2::ggsave(outfile, plot = p, width = 8, height = 4, dpi = 300)
  }
}

viz_inspection_participant_choice_reward <- function(task_data, output_dir) {
  #####
  # Generate participant-level choice and reward inspection plots
  #
  # Creates a plot for each participant showing their choices (Stimulus 0 vs 1)
  # and received rewards across valid trials.
  #
  # Parameters
  # ----
  # task_data : tibble
  #     Cleaned task data containing columns: participant_id, trial,
  #     choice, reward.
  # output_dir : character
  #     Base directory for saving figures. Plots will be saved in
  #     [output_dir]/inspection/participant_wise/choice_reward
  #
  # Returns
  # ----
  # NULL (invisible)
  #     Saves plots to disk as side effect.
  #####

  requireNamespace("ggplot2")
  requireNamespace("dplyr")

  # Define and create output directory
  target_dir <- file.path(output_dir, "inspection", "participant_wise", "choice_reward")
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  message("Generating participant inspection plots (Choice & Reward) in: ", target_dir)

  # Get unique, sorted participant IDs
  participants <- sort(unique(task_data$participant_id))

  for (pid in participants) {

    # Filter and arrange data for current participant
    p_data <- task_data %>%
      dplyr::filter(participant_id == pid) %>%
      dplyr::arrange(trial)

    # Filter valid trials (non-missing choice and reward)
    p_data_valid <- p_data %>%
      dplyr::filter(!is.na(choice) & !is.na(reward))

    if (nrow(p_data_valid) > 0) {

      # Prepare data for plotting
      p_plot_df <- p_data_valid %>%
        dplyr::mutate(
          # Ensure choice is treated as factor/discrete for y-axis
          plot_choice = factor(choice, levels = c(0, 1), labels = c("Stimulus 0", "Stimulus 1")),
          # Label rewards for shape mapping
          plot_reward = factor(reward, levels = c(0, 1), labels = c("No Reward", "Reward"))
        )

      # Create the plot
      p_behavior_plot <- ggplot2::ggplot(p_plot_df, ggplot2::aes(x = trial)) +
        # Points for Choice (Color mapped, though effectively redundant with y-axis, matches legacy style)
        ggplot2::geom_point(ggplot2::aes(y = choice, color = "Choice"), size = 2) +

        # Points for Outcome (Shifted slightly for visibility, shape mapped to Reward)
        # Shift: +0.2 for Reward, -0.2 for No Reward
        ggplot2::geom_point(ggplot2::aes(y = choice + ifelse(reward == 1, 0.2, -0.2),
                                         shape = plot_reward), size = 3) +

        # Manual shape scale
        ggplot2::scale_shape_manual(values = c("No Reward" = 4, "Reward" = 16), name = "Reward") +

        # Manual color scale (dummy to match legacy legend entry if needed, or just for consistency)
        ggplot2::scale_color_manual(values = c("Choice" = "black"), name = NULL) +

        # Y-axis settings
        ggplot2::scale_y_continuous(breaks = c(0, 1),
                                    labels = c("Stimulus 0", "Stimulus 1"),
                                    limits = c(-0.5, 1.5)) +

        # Labels and Title
        ggplot2::labs(
          title = paste("Participant", pid, "- Choices and Rewards Over Valid Trials"),
          x = "Trial Number (Valid)",
          y = "Choice"
        ) +

        # Theme
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.9)))

      # Save the plot
      outfile <- file.path(target_dir, paste0("participant_", pid, "_choice_reward_plot.png"))
      ggplot2::ggsave(filename = outfile, plot = p_behavior_plot, width = 10, height = 4, bg = "white", dpi = 150)
    }
  }
}


# ============================================
# 2. Main Analysis
# ============================================

# (Placeholders for future behavioral plots)


# ============================================
# 3. Posterior Predictive Checks
# ============================================

# (Placeholders for PPC plots)
