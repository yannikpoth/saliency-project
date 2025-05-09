#      Prepare data: Stan lists -- Version 1
# Last edit:    2025/04/24
# Author:       Geysen, Steven (SG)
# Notes:        - Prepare data for Stan models
#               - Release notes:
#                   * Initial commit
# To do:        - Adjust to RL data
# Comments:     
# Sources:      https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/assign
#               Slicing in R ( https://stackoverflow.com/a/15127458 )
# --------------



#### Imports ####


##SG: Nothing needed for now.



#### Merge data ####
####################


.MergeSubData <- function() {
  #####
  # Merge all separate participant data
  # Collect and merge the data of all the participant in the raw directory
  # (./data/raw/) into one big data frame. The output is stored as a tsv file
  # in the processed directory (.data/processed/all_sub_data.tsv).
  
  # Parameters
  # ----------
  # None
  
  # Return
  # ------
  # all_data : data.frame
  #     Merged data frame containing all the data found in the raw directory.
  #     Includes a column with the participant number ("subID").
  #####


  datalist <- list.files(path = "data/raw/", full.names = FALSE)
  fullList <- list()
  for (filei in datalist) {
    sub_data <- read.csv(paste("data/raw/", filei, sep = ""))
    subID <- strsplit(filei, split = "_")[[1]][1]
    fullList[[subID]] <- sub_data
  }
  all_data <- bind_rows(fullList, .id = "subID")

  out_dir = paste(getwd(), "/data/processed", sep = "")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }
  write.table(
    all_data, file = "data/processed/all_sub_data.tsv", sep = "\t",
    row.names = FALSE
  )

  return(all_data)
}



#### Stan lists ####
####################


StanCleaned <- function(debugsize = 0, saveStan = FALSE) {
  #####
  # Create the Stan list of cleaned data
  # Create the list for the Stan models used to fit to the cleaned data.
  # Missing values are recoded as -9. The output is not saved by default.
  
  # Parameters
  # ----------
  # debugsize : int, optional
  #     Sample size used to debug the model. When 0, all subjects are included.
  #     The default is 0.
  # saveStan : bool, optional
  #     Save the Stan list as `data/processed/StanList.RData`.
  #     The default is FALSE.
  
  # Return
  # ------
  # StanList : list
  #     Cleaned data in the proper format for Stan models.
  #         * nSubs: Number of subjects
  #         * maxTrials: Maximum number of trials
  #         * subTrials: Number of trials per subject (nSubs)
  #         * reward: Reward (nSubs * maxTrials)
  #         * choice: The left (0) or right (1) choice (nSubs * maxTrials)
  #         * salient_feedback: Indicator for salient feedback (1=salient, 0=non-salient, -9=missing) (nSubs * maxTrials)
  #####


  # Load data
  if (file.exists("data/processed/all_sub_data.tsv")) {
    trialData <- read.csv("data/processed/all_sub_data.tsv", sep = "\t")
  } else {
    trialData <- .MergeSubData()
  }
  mainTrialData <- trialData[trialData["mode"] == "main", ]

  subs <- unique(mainTrialData$subID)
  nSubs <- length(subs)
  if (debugsize > 0) {
    nSubs <- length(subs[1:debugsize])
  }
  maxTrials <- max(mainTrialData$trial)
  
  # Define values
  subTrials <- array(-9, nSubs)
  reward <- array(-9, c(nSubs, maxTrials))
  choice <- array(-9, c(nSubs, maxTrials))
  salient_feedback <- array(-9, c(nSubs, maxTrials))
  
  for (subi in 1:nSubs) {
    subid <- subs[subi]
    sub_data <- subset(mainTrialData, mainTrialData$subID == subid)
    
    subTrials[subi] <- length(unique(sub_data$trial))
    for (triali in 1:subTrials[subi]) {
      if (triali <= nrow(sub_data)) {
        current_trial_data <- sub_data[triali, ]

        if (!is.na(current_trial_data$choice)) {
          reward[subi, triali] <- current_trial_data$reward
          ##SG: Add one because R does not count from 0. Stan model expects 1 or 2.
          choice[subi, triali] <- current_trial_data$choice + 1
          
          if (!is.na(current_trial_data$condition)) {
            if (current_trial_data$condition == 1) {
              salient_feedback[subi, triali] <- 1
            } else if (current_trial_data$condition == 0) {
              salient_feedback[subi, triali] <- 0
            } else {
              salient_feedback[subi, triali] <- -9 
            }
          } else {
            salient_feedback[subi, triali] <- -9
          }
        }
      }
    }
  }
  StanList <- list(
    nSubs = nSubs, maxTrials = maxTrials, subTrials = subTrials,
    reward = reward, choice = choice, salient_feedback = salient_feedback
  )
  
  # Save list
  if (saveStan == TRUE) {
    outname <- "data/processed/StanList.RData"
    if (debugsize > 0) {
      outname <- "data/processed/StanList_debug.RData"
    } 
    save(StanList, file = outname)
  }

  return (StanList)
}



# ------------------------------------------------------------------------- End