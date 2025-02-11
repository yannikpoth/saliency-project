# Saliency Project

A reinforcement learning study investigating how **salient audiovisual cues** (inspired by electronic gambling machines) influence human learning and decision-making in a restless two-armed bandit task. This repository contains:

- **Experiment code** for running the bandit task (written in Python, using PsychoPy-like workflows).  
- **Data collection** outputs (sample data).  
- **Analysis scripts** for both model-free (behavioral metrics) and model-based (Stan-based) reinforcement learning approaches.  

The work serves as the foundation for a Master’s thesis in Psychology, focusing on how reward-paired salient stimuli can alter learning rates, win-stay behavior, and post-reinforcement pauses (PRPs).

---

## Table of Contents

1. [Project Overview](#project-overview)  
2. [Repository Structure](#repository-structure)  
3. [Installation](#installation)  
4. [Running the Experiment](#running-the-experiment)  
5. [Analysis Workflows](#analysis-workflows)  
6. [Data](#data)  
7. [License](#license)  
8. [Contact](#contact)

---

## Project Overview

**Background**  
- Modern electronic gambling machines often use bright lights, celebratory sounds, and vivid animations to mark wins—a phenomenon referred to as *salient audiovisual feedback*.  
- This study explores whether such cues, when paired with monetary rewards in a bandit task, bias the learning process, measured by reinforcement learning models.  
- Factors such as **learning rate shifts**, **win-stay tendencies**, and **post-reinforcement pauses** are examined, and how they differ for high-sensation seekers or impulsive individuals.

**Core Questions**  
- Does pairing rewards with salient audiovisual cues change participants’ learning rates (e.g., alpha-shift in a Q-learning model)?  
- Do participants show different patterns of staying with a winning option when salient cues are present vs. absent?  
- Are these effects moderated by personality traits like sensation seeking or impulsivity?

---

## Repository Structure

```bash
.
├── README.md                # This file
├── analysis_scripts         # Scripts & notebooks for data analysis
│   ├── model_based_learning
│   │   ├── ms_ql_1lr.stan
│   │   ├── ms_ql_1lr_shift.stan
│   │   ├── ms_ql_1lr_shift_phi.stan
│   │   ├── stan_call.R
│   │   ├── stan_call.Rmd
│   │   ├── stan_call_basic.R
│   │   ├── stan_call_new.R
│   │   └── transf_data.csv
│   └── model_free_learning
│       └── model-free_analysis.ipynb
├── bandit_task              # Main experiment folder
│   ├── collected_data       # Example CSVs (sample data)
│   ├── media                # Audio & visual stimuli
│   │   ├── sounds
│   │   └── stimuli
│   ├── questionnaire_survey.py   # Questionnaire logic
│   ├── random_walk_data     # Random walk parameters & images
│   ├── random_walk_generation.ipynb
│   ├── requirements.txt     # Python dependencies
│   ├── run_experiment.py    # Script to run the entire experiment
│   ├── task_logic.py        # Core logic for the bandit task
│   └── variable_ratio_schedule.py
```

- **analysis_scripts**  
  - *model_based_learning/*: Stan models and R scripts for Q-learning variants (e.g., alpha shift, phi parameter, etc.).  
  - *model_free_learning/*: Jupyter notebooks for descriptive/model-free analyses.  

- **bandit_task**  
  - *collected_data/*: Place where participant data is stored (currently sample files).  
  - *media/*: Contains **stimuli** (images, videos) and **sounds** (e.g., background, salient cues).  
  - *random_walk_data/*: Pre-generated random walk CSVs and images for practice/main tasks.  
  - *run_experiment.py*: Entry point for launching the bandit experiment.  
  - *task_logic.py*: PsychoPy-style script controlling the flow of trials, stimuli presentation, data collection.  
  - *questionnaire_survey.py*: Script handling questionnaires for personality/impulsivity traits.  
  - *requirements.txt*: Python dependencies needed for the experiment.

---

## Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/saliency-project.git
   cd saliency-project
   ```

2. **Create and activate a virtual environment** (recommended):
   ```bash
   python3 -m venv venv
   source venv/bin/activate  # macOS/Linux
   # or venv\Scripts\activate on Windows
   ```

3. **Install required Python packages**:
   ```bash
   pip install -r bandit_task/requirements.txt
   ```
   *Note:* If you plan to run the Stan/R analysis, make sure you have R and the necessary R packages as well (see `analysis_scripts/model_based_learning/stan_call.Rmd` for details).

---

## Running the Experiment

1. **Navigate to the repo root** (if not already there):
   ```bash
   cd path/to/saliency-project
   ```
2. **Run the main experiment script**:
   ```bash
   python3 bandit_task/run_experiment.py
   ```
   You’ll be prompted for a `participant_id` or other parameters (depending on how you coded it).  

- The script will call `task_logic.py` (controlling the bandit trials), then proceed to `questionnaire_survey.py` for personality measures.

> **Tip:** If you have issues with file paths, ensure you updated all references to use the `Path(__file__).parent` approach described in the code so it doesn’t depend on the current working directory.

---

## Analysis Workflows

### Model-Free Analysis
- The notebook `analysis_scripts/model_free_learning/model-free_analysis.ipynb` explores descriptive statistics and basic RL metrics (e.g., win-stay ratio, choice probabilities).  
- To run it, simply open it in Jupyter or any compatible environment:
  ```bash
  cd analysis_scripts/model_free_learning
  jupyter notebook
  ```
  Then open `model-free_analysis.ipynb`.

### Model-Based Analysis
- The folder `analysis_scripts/model_based_learning` contains Stan models (`.stan` files) and R scripts (`.R`, `.Rmd`) that fit advanced reinforcement learning models (e.g., with alpha shift parameters).  
- Before running, ensure you have R and CmdStan or RStan installed.  
- Example:
  ```bash
  cd analysis_scripts/model_based_learning
  Rscript stan_call_basic.R
  ```
  or open `stan_call.Rmd` in RStudio.

---

## Data

- **Sample Data**: Located in `bandit_task/collected_data/`.  
  - For large or full datasets, consider checking the [GitHub Releases](#) (if you decide to host full datasets there).  
- **Random Walk Parameters**: Found in `bandit_task/random_walk_data/`. These define the reward probabilities for each trial across practice/main tasks.  

> **Important**: The real participant data may be excluded or anonymized to protect participant privacy. If you need the full dataset for reproducibility, please check the project’s GitHub Releases or contact the maintainer.

---

## License

*(Choose an appropriate license for your work, e.g., MIT, Apache 2.0, or GPL. If you prefer not to specify one, you can omit this section.)*

```
MIT License

Copyright (c) 2023 ...

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files...
```

---

## Contact

For questions regarding the experiment, analysis, or to request data access, please contact:
- **Your Name**: [your.email@example.com](mailto:your.email@example.com)
- Or create an [issue](https://github.com/YourUser/saliency-project/issues) in this repository.

---

*If you need more details—for example, participant recruitment methods, ethical review board info, or references to supporting scientific literature—let me know!*
