###'DATENAUFBEREITUNG


# Funktion zur Verarbeitung einer einzelnen Datei
process_file <- function(file_path) {
  df <- read.csv(file_path)
  
  # Extrahieren der Versuchspersonen-ID aus den ersten beiden Zeichen des Dateinamens
  subject_id <- substr(basename(file_path), 1, 2)
  
  # Entfernen von Datensätzen mit mehr als 20% Misses (Condition = 2)
  if (mean(df$condition == 2, na.rm = TRUE) > 0.2) {
    return(NULL)
  }
  
  df <- df %>% 
    filter(mode != "practice") %>% 
    arrange(trial)
  
  # Datensatz nach Trial sortieren, damit korrekter Bezug auf vorheriges Trial hergestellt werden kann
  df <- df %>% arrange(trial)
  
  # Misses markieren, aber noch nicht entfernen; Erstellung von previous_choice und previous_reward
  df <- df %>%
    mutate(
      missed_previous = lag(condition) == 2,  # Markieren von Trials, die direkt nach einem Miss kommen
      previous_choice = ifelse(missed_previous, NA, lag(choice)),  # Falls vorher ein Miss war → NA
      previous_reward = case_when(
        missed_previous ~ NA_real_,  # Falls vorher ein Miss war → NA
        lag(reward) == 0 ~ 0,  # loss
        lag(reward) == 1 & lag(condition) == 0 ~ 1,  # non-salient win
        lag(reward) == 1 & lag(condition) == 1 ~ 2   # salient win
      )
    ) %>%
    filter(!is.na(previous_choice), !is.na(previous_reward))  # Trials entfernen, die direkt nach einem Miss kommen
  
  # *Jetzt Misses selbst entfernen*
  df <- df %>% 
    filter(condition != 2) %>% 
    filter(reaction_time >= 0.15)  # Trials mit RT < 150ms entfernen (in Sekunden)
  
   
  #Variablen für die WSLS Plots und Accuracy

  accuracy <- sum(df$reward == 1, na.rm = TRUE) / sum(!is.na(df$reward))
  
  # Neue Variable: für Accuracy anhand von reward_prob
  df <- df %>%
    mutate(
      higher_prob = case_when(
        reward_prob_1 > reward_prob_2 & choice == 0 ~ 1,  # Stimulus 1 hat höhere Wahrscheinlichkeit und wurde gewählt
        reward_prob_2 > reward_prob_1 & choice == 1 ~ 1,  # Stimulus 2 hat höhere Wahrscheinlichkeit und wurde gewählt
        TRUE ~ 0  # in allen anderen Fällen nicht die bessere Option gewählt
      )
    )
  
  # Anteil der Trials mit Wahl der Option mit höherer Reward Probability
  accuracy_prob <- sum(df$higher_prob == 1, na.rm = TRUE) / sum(!is.na(df$higher_prob))
  
  # Rückgabe als Dataframe mit einer Zeile für die Person
  return(data.frame(
    subject_id, PRP_loss, PRP_nonsalient_win, PRP_salient_win, 
    nonsalient_ws, salient_ws,
    win_stay_combined_prob = win_stay_combined_prob,
    win_nostay = win_nostay_prob,
    lose_shift = lose_shift_prob,
    lose_noshift = lose_noshift_prob,
    accuracy = accuracy,
    n_salient_win = n_salient_win,
    n_nonsalient_win = n_nonsalient_win,
    n_loss = n_loss,
    log_PRP_loss = log_PRP_loss,
    log_PRP_nonsalient_win = log_PRP_nonsalient_win,
    log_PRP_salient_win = log_PRP_salient_win,
    accuracy_prob = accuracy_prob
  ))
}

# Zusammenführen aller Dateien
process_all_files <- function(directory) {
  file_paths <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  results <- bind_rows(lapply(file_paths, process_file))
  return(results)
}

# Abruf des Datensatzes 
test_datensatz <- process_all_files("/Users/starlight/Desktop/Bachelorarbeit/collected_data_backup")

# Accuracy Plot anhand von Reward-Variable
ggplot(test_datensatz, aes(x = "", y = accuracy)) +
  geom_boxplot(
    fill = "#f4a6b8",  
    color = "#b03a48", 
    outlier.shape = NA,
    width = 0.3
  ) +
  geom_jitter(
    width = 0.1,
    size = 2.5,
    alpha = 0.7,
    color = "#b03a48"  # gleiche Farbe wie Boxrand
  ) +
  scale_y_continuous(limits = c(0.4, 0.7), breaks = seq(0.4, 0.7, 0.05)) +
  labs(
    title = "Distribution of Accuracy Across Participants",
    x = "",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.8),  
    axis.ticks = element_line(color = "black", size = 0.7)  # optional: Achsenticks klarer
  )

# Accuracy Plot anhand von Probability-Variable
ggplot(test_datensatz, aes(x = "", y = accuracy_prob)) +
  geom_boxplot(
    fill = "#a1dab4",
    color = "black",
    outlier.shape = NA,
    width = 0.3
  ) +
  geom_jitter(
    width = 0.1,
    size = 2.5,
    alpha = 0.7,
    color = "#238b45"
  ) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    color = "darkred",
    linewidth = 0.8
  ) +
  scale_y_continuous(limits = c(0.3, 0.7), breaks = seq(0.3, 0.7, 0.05)) +
  labs(
    title = "Distribution of Accuracy (Choice of Higher Reward Probability)",
    x = "",
    y = "Accuracy (higher reward choice)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 0.8),
    axis.ticks = element_line(color = "black", size = 0.7)
  )