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
  # vertical dashed lines. Additionally, trial-by-trial outcomes on the chosen arm
  # are marked:
  # - Loss: grey
  # - Non-salient win: white
  # - Salient win: yellow
  # Choice 0 events use circles; choice 1 events use squares.
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

    # Anchor y-positions for event lanes
    # Top lane: Stimulus 0, Bottom lane: Stimulus 1
    y_anchor_stim0 <- 1.06
    y_anchor_stim1 <- -0.06

    # Trial-by-trial event markers (chosen option + outcome)
    event_df <- subj_data %>%
      dplyr::filter(!is.na(choice) & !is.na(reward)) %>%
      dplyr::mutate(
        choice = as.integer(choice),
        event_type = dplyr::case_when(
          reward == 1 & condition == 1 ~ "Salient Win",
          reward == 1 & condition == 0 ~ "Non-Salient Win",
          reward == 0 ~ "Loss",
          TRUE ~ NA_character_
        ),
        anchor_y = dplyr::case_when(
          choice == 0L ~ y_anchor_stim0,
          choice == 1L ~ y_anchor_stim1,
          TRUE ~ NA_real_
        ),
        chosen_stimulus = dplyr::case_when(
          choice == 0L ~ "Stimulus 0",
          choice == 1L ~ "Stimulus 1",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(event_type) & is.finite(anchor_y))

    win_df <- dplyr::filter(event_df, event_type %in% c("Non-Salient Win", "Salient Win"))
    loss_df <- dplyr::filter(event_df, event_type == "Loss")

    # Create plot
    p <- ggplot2::ggplot(subj_data, ggplot2::aes(x = trial)) +
      # Line for Option A (Reward Prob 1)
      ggplot2::geom_line(ggplot2::aes(y = reward_prob_1, color = "A"), linewidth = 0.8) +
      # Line for Option B (Reward Prob 2)
      ggplot2::geom_line(ggplot2::aes(y = reward_prob_2, color = "B"), linewidth = 0.8) +

      # Mark trial-by-trial outcomes on event lanes (top=Stimulus 0, bottom=Stimulus 1)
      ggplot2::geom_point(
        data = win_df,
        ggplot2::aes(
          x = trial,
          y = anchor_y,
          fill = event_type
        ),
        shape = 21,
        color = "black",
        size = 2.0,
        stroke = 0.40,
        alpha = 0.95
      ) +
      ggplot2::geom_point(
        data = loss_df,
        ggplot2::aes(
          x = trial,
          y = anchor_y,
          fill = "Loss"
        ),
        shape = 4,
        color = "grey40",
        size = 1.8,
        stroke = 0.55,
        alpha = 0.95
      ) +
      ggplot2::scale_fill_manual(
        name = "Event type",
        values = c(
          "Loss" = "grey70",
          "Non-Salient Win" = "#a6dba6",
          "Salient Win" = "#ffc400"
        )
      ) +
      ggplot2::scale_color_manual(
        name = NULL,
        values = c("A" = "dodgerblue", "B" = "firebrick"),
        labels = c("A" = "p(Reward | A)", "B" = "p(Reward | B)")
      ) +
      ggplot2::guides(
        # Keep only a single compact legend for event type (with correct shapes)
        fill = ggplot2::guide_legend(
          override.aes = list(
            shape = c(4, 21, 21),
            colour = c("grey40", "black", "black"),
            fill = c("grey70", "#a6dba6", "#ffc400"),
            size = 3
          )
        ),
        color = ggplot2::guide_legend(order = 2)
      ) +

      # Axes
      ggplot2::scale_y_continuous(
        limits = c(-0.12, 1.12),
        breaks = seq(0, 1, 0.25),
        labels = scales::number_format(accuracy = 0.01)
      ) +
      ggplot2::labs(
        title = paste("Participant", pid, ": Reward Probabilities & Trial-wise Outcomes"),
        subtitle = "Top lane = Stimulus 0, bottom lane = Stimulus 1. Markers show outcome on the chosen stimulus.",
        y = "p(Reward)",
        x = "Trial"
      ) +

      # Theme
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 9, color = "grey25", hjust = 0.5),
        legend.position = c(0.86, 0.50),
        legend.justification = c(0, 0.5),
        legend.title = ggplot2::element_text(size = 8),
        legend.text = ggplot2::element_text(size = 7),
        legend.key.height = grid::unit(0.45, "cm"),
        legend.key.width = grid::unit(0.45, "cm"),
        legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.85), color = "grey70", linewidth = 0.3),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    # Save plot
    outfile <- file.path(target_dir, paste0("combined_reward_probs_subject_", pid, ".png"))
    ggplot2::ggsave(outfile, plot = p, width = 10, height = 4, dpi = 300)
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

viz_extract_posterior_data <- function(fit, model_name) {
  #####
  # Extract posterior samples for specific parameters for visualization
  #
  # Helper function to extract lightweight data for viz_posterior_densities_grid
  # without keeping the full model in memory.
  #
  # Parameters
  # ----
  # fit : stanfit
  #     Fitted Stan model object
  # model_name : character
  #     Name of the model
  #
  # Returns
  # ----
  # data.frame
  #     Long-format data frame with columns: Model, Parameter, Value
  #     Returns empty data frame if no target parameters found.
  #####

  param_order <- c("alpha_mu", "beta_mu", "alpha_shift_mu", "kappa_mu", "kappa_shift_mu")
  avail_pars <- names(fit)
  pars_to_get <- intersect(param_order, avail_pars)

  plot_data <- data.frame()

  if (length(pars_to_get) > 0) {
    samples <- rstan::extract(fit, pars = pars_to_get)

    for (p in pars_to_get) {
      vals <- as.vector(samples[[p]])
      # Downsample if too huge
      if (length(vals) > 4000) vals <- sample(vals, 4000)

      df_chunk <- data.frame(
        Model = model_name,
        Parameter = p,
        Value = vals
      )
      plot_data <- rbind(plot_data, df_chunk)
    }
  }

  return(plot_data)
}

viz_posterior_densities_grid <- function(data_list, output_dir) {
  #####
  # Generate a multiplot grid of posterior densities for all models and parameters
  #
  # Creates a faceted density plot where rows are models (ordered from simple to complex)
  # and columns are key parameters. Includes median lines.
  #
  # Parameters
  # ----
  # data_list : list or data.frame
  #     Either a list of data frames from viz_extract_posterior_data(),
  #     or a single combined data frame.
  #     (Legacy support: If a list of stanfits is passed, it attempts to extract data,
  #      but this is deprecated for memory reasons).
  # output_dir : character
  #     Directory to save the plot
  #
  # Returns
  # ----
  # NULL (invisible)
  #     Saves plot to disk
  #####

  requireNamespace("ggplot2")
  requireNamespace("dplyr")
  requireNamespace("tidyr")

  message("Generating posterior density grid...")

  # 1. Define configuration
  # -----------------------

  # Model order and labels (Plotmath expressions for label_parsed)
  model_order <- c(
    "model01_baseline", "model07_baseline_ncp",
    "model02_alpha_shift", "model08_alpha_shift_ncp",
    "model03_kappa", "model09_kappa_ncp",
    "model04_alpha_shift_kappa", "model10_alpha_shift_kappa_ncp",
    "model05_kappa_kappa_shift", "model11_kappa_kappa_shift_ncp",
    "model06_full", "model12_full_ncp"
  )

  model_labels <- c(
    "model01_baseline" = "Baseline",
    "model07_baseline_ncp" = "Baseline~(NCP)",
    "model02_alpha_shift" = "BL + alpha[shift]",
    "model08_alpha_shift_ncp" = "BL + alpha[shift]~(NCP)",
    "model03_kappa" = "BL + kappa",
    "model09_kappa_ncp" = "BL + kappa~(NCP)",
    "model04_alpha_shift_kappa" = "BL + alpha[shift] + kappa",
    "model10_alpha_shift_kappa_ncp" = "BL + alpha[shift] + kappa~(NCP)",
    "model05_kappa_kappa_shift" = "BL + kappa + kappa[shift]",
    "model11_kappa_kappa_shift_ncp" = "BL + kappa + kappa[shift]~(NCP)",
    "model06_full" = "BL + alpha[shift] + kappa + kappa[shift]",
    "model12_full_ncp" = "BL + alpha[shift] + kappa + kappa[shift]~(NCP)"
  )

  # Parameter order and labels
  param_order <- c("alpha_mu", "beta_mu", "alpha_shift_mu", "kappa_mu", "kappa_shift_mu")

  param_labels <- c(
    "alpha_mu" = "alpha[mu]",
    "beta_mu" = "beta[mu]",
    "alpha_shift_mu" = "alpha[shift]~(mu)",
    "kappa_mu" = "kappa[mu]",
    "kappa_shift_mu" = "kappa[shift]~(mu)"
  )

  # 2. Extract Data
  # ---------------
  plot_data <- data.frame()

  # Handle input types
  if (is.data.frame(data_list)) {
    plot_data <- data_list
  } else if (is.list(data_list)) {
    # Check if elements are stanfits or dataframes
    first_elem <- data_list[[1]]
    if (inherits(first_elem, "stanfit")) {
      # Legacy mode: extract from fits
      message("Note: Extracting data from stanfits (Legacy Mode). For better memory usage, pass extracted data frames.")
      for (m_name in names(data_list)) {
        if (!m_name %in% model_order) next
        chunk <- viz_extract_posterior_data(data_list[[m_name]], m_name)
        plot_data <- rbind(plot_data, chunk)
      }
    } else {
       # List of (possibly empty) data frames
       # Be defensive: parallel workers may have failed and returned NULL.
       ok_elems <- Filter(function(x) is.data.frame(x) && ncol(x) > 0, data_list)
       plot_data <- if (length(ok_elems) > 0) do.call(rbind, ok_elems) else data.frame()
    }
  }

  if (is.null(plot_data) || !is.data.frame(plot_data) || nrow(plot_data) == 0) {
    warning("No matching parameters found for density grid plot.")
    return(NULL)
  }

  # 3. Process Factors for Plotting

  # -------------------------------

  # Convert Model to factor with specific order and labels
  # We subset model_order to only those present in the data to avoid empty levels if not all fitted
  present_models <- intersect(model_order, unique(plot_data$Model))

  # Create a named vector for labels that matches the factor levels
  lab_map_model <- model_labels[present_models]

  plot_data$Model_F <- factor(plot_data$Model,
                              levels = present_models,
                              labels = lab_map_model)

  # Convert Parameter to factor with specific order and labels
  plot_data$Parameter_F <- factor(plot_data$Parameter,
                                  levels = param_order,
                                  labels = param_labels)

  # Compute Medians for vertical lines
  medians <- plot_data %>%
    dplyr::group_by(Model_F, Parameter_F) %>%
    dplyr::summarise(median_val = median(Value), .groups = "drop")

  # 4. Create Plot
  # --------------

  # Split-fill density for alpha_shift_mu: area <= 0 (white) vs > 0 (lightblue),
  # plus a boundary line at x = 0 up to the density curve and an annotation of P(Value > 0).
  plot_data_other <- plot_data %>% dplyr::filter(.data$Parameter != "alpha_shift_mu")
  plot_data_alpha_shift <- plot_data %>% dplyr::filter(.data$Parameter == "alpha_shift_mu")

  if (nrow(plot_data_alpha_shift) == 0) {
    plot_data_alpha_shift <- data.frame(
      Model_F = factor(character()),
      Parameter_F = factor(character()),
      Value = numeric()
    )
  }

  alpha_shift_pct <- plot_data_alpha_shift %>%
    dplyr::group_by(.data$Model_F, .data$Parameter_F) %>%
    dplyr::summarise(
      p_gt0 = mean(.data$Value > 0, na.rm = TRUE) * 100,
      label = sprintf("%.1f%%", .data$p_gt0),
      .groups = "drop"
    )

  alpha_shift_density <- dplyr::bind_rows(lapply(
    split(plot_data_alpha_shift, list(plot_data_alpha_shift$Model_F, plot_data_alpha_shift$Parameter_F), drop = TRUE),
    function(df_grp) {
      if (!is.data.frame(df_grp) || nrow(df_grp) < 2) return(NULL)
      d <- stats::density(df_grp$Value, n = 512, na.rm = TRUE)
      data.frame(
        Model_F = df_grp$Model_F[[1]],
        Parameter_F = df_grp$Parameter_F[[1]],
        x = d$x,
        y = d$y,
        Region = ifelse(d$x > 0, "pos", "neg"),
        stringsAsFactors = FALSE
      )
    }
  ))

  alpha_shift_boundary <- dplyr::bind_rows(lapply(
    split(plot_data_alpha_shift, list(plot_data_alpha_shift$Model_F, plot_data_alpha_shift$Parameter_F), drop = TRUE),
    function(df_grp) {
      if (!is.data.frame(df_grp) || nrow(df_grp) < 2) return(NULL)
      d <- stats::density(df_grp$Value, n = 512, na.rm = TRUE)
      y0 <- stats::approx(d$x, d$y, xout = 0, rule = 2)$y
      data.frame(
        Model_F = df_grp$Model_F[[1]],
        Parameter_F = df_grp$Parameter_F[[1]],
        x = 0,
        xend = 0,
        y = 0,
        yend = y0,
        stringsAsFactors = FALSE
      )
    }
  ))

  if (is.null(alpha_shift_density) || !is.data.frame(alpha_shift_density) || nrow(alpha_shift_density) == 0) {
    alpha_shift_density <- data.frame(
      Model_F = factor(character()),
      Parameter_F = factor(character()),
      x = numeric(),
      y = numeric(),
      Region = character(),
      stringsAsFactors = FALSE
    )
  }

  if (is.null(alpha_shift_boundary) || !is.data.frame(alpha_shift_boundary) || nrow(alpha_shift_boundary) == 0) {
    alpha_shift_boundary <- data.frame(
      Model_F = factor(character()),
      Parameter_F = factor(character()),
      x = numeric(),
      xend = numeric(),
      y = numeric(),
      yend = numeric(),
      stringsAsFactors = FALSE
    )
  }

  # Label position: anchor at x = 0 and y = 0.05 * max(density)
  alpha_shift_label_pos <- alpha_shift_density %>%
    dplyr::group_by(.data$Model_F, .data$Parameter_F) %>%
    dplyr::summarise(y_pos = 0.05 * max(.data$y, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(x_pos = 0)

  alpha_shift_pct <- alpha_shift_pct %>%
    dplyr::left_join(alpha_shift_label_pos, by = c("Model_F", "Parameter_F"))

  p <- ggplot2::ggplot() +
    ggplot2::geom_density(
      data = plot_data_other,
      ggplot2::aes(x = .data$Value),
      fill = "lightblue",
      alpha = 0.7,
      color = "navy"
    ) +
    ggplot2::geom_area(
      data = alpha_shift_density %>% dplyr::filter(.data$Region == "neg"),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = "white",
      alpha = 1,
      color = NA
    ) +
    ggplot2::geom_area(
      data = alpha_shift_density %>% dplyr::filter(.data$Region == "pos"),
      ggplot2::aes(x = .data$x, y = .data$y),
      fill = "lightblue",
      alpha = 0.7,
      color = NA
    ) +
    ggplot2::geom_line(
      data = alpha_shift_density,
      ggplot2::aes(x = .data$x, y = .data$y),
      color = "navy",
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      data = alpha_shift_boundary,
      ggplot2::aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
      color = "navy",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = alpha_shift_pct,
      ggplot2::aes(x = .data$x_pos, y = .data$y_pos, label = .data$label),
      color = "black",
      size = 3.2,
      hjust = 0,
      vjust = 0,
      fontface = "bold"
    ) +
    ggplot2::geom_vline(data = medians, ggplot2::aes(xintercept = median_val),
                        color = "red", linetype = "dashed", linewidth = 0.5) +

    # Faceting
    ggplot2::facet_grid(Model_F ~ Parameter_F,
                        scales = "free",     # Allow x/y axes to vary
                        switch = "y",        # Move model labels to left
                        labeller = ggplot2::label_parsed) + # Parse plotmath labels

    # Aesthetics
    ggplot2::labs(
      title = "Posterior Densities of Key Parameters Across Models",
      x = "Parameter Value",
      y = "Density"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1), # Horizontal row labels
      strip.background = ggplot2::element_rect(fill = "grey95"),
      axis.text.y = ggplot2::element_blank(), # Hide y-axis ticks/numbers for density
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  # 5. Save
  # -------
  out_path <- file.path(output_dir, "all_models_posterior_density_grid.png")

  # Calculate height based on number of models (approx 1.5 inch per row)
  h_val <- max(6, length(present_models) * 1.5)

  ggplot2::ggsave(out_path, plot = p, width = 12, height = h_val, dpi = 300)
  message("Saved posterior density grid to: ", out_path)
}


# ============================================
# 3. Posterior Predictive Checks
# ============================================

# (Placeholders for PPC plots)
