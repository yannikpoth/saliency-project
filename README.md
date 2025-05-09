# Saliency Project

A reinforcement learning study investigating how **salient audiovisual cues** (inspired by electronic gambling machines) influence human learning and decision-making in a restless two-armed bandit task. This repository contains:

- **Experiment code** for running the bandit task (written in Python, using PsychoPy).
- **Data collection** outputs (sample data).
- **Analysis scripts** encompassing both model-free (behavioral metrics) and model-based (hierarchical Bayesian RL using Stan) approaches within a unified R workflow.

The work serves as the foundation for a Master's thesis in Psychology, focusing on how reward-paired salient stimuli can alter learning rates, win-stay behavior, and post-reinforcement pauses (PRPs).

---

## Table of Contents

1.  [Project Overview](#project-overview)
2.  [Repository Structure](#repository-structure)
3.  [Installation](#installation)
4.  [Running the Experiment](#running-the-experiment)
5.  [Analysis Workflow](#analysis-workflow)
6.  [Data](#data)
7.  [License](#license)
8.  [Contact](#contact)

---

## Project Overview

**Background**
- Modern electronic gambling machines often use bright lights, celebratory sounds, and vivid animations to mark wins—a phenomenon referred to as *salient audiovisual feedback*.
- This study explores whether such cues, when paired with monetary rewards in a bandit task, bias the learning process, measured by reinforcement learning models and behavioral metrics.
- Factors such as **learning rate shifts**, **win-stay tendencies**, and **post-reinforcement pauses** are examined.
- The study also investigates potential moderation by personality traits like sensation seeking or impulsivity using questionnaire data.

**Core Questions**
- Does pairing rewards with salient audiovisual cues change participants' learning rates (specifically, does a salient *win* increase the learning rate compared to non-salient outcomes or losses)?
- Do participants show different patterns of staying with a winning option (win-stay) when the win was associated with salient vs. non-salient feedback?
- Do post-reinforcement pauses (reaction times after receiving a reward) differ following losses, non-salient wins, and salient wins?
- Are these effects moderated by individual differences in sensation seeking or impulsivity?

---

## Repository Structure

```bash
.
├── README.md                     # This file
├── analysis_scripts              # Scripts for data analysis
│   ├── model_based_learning      # R script and Stan model for RL analysis
│   │   ├── analyze_saliency_data.R # Main R script for all analyses
│   │   └── saliency_rl_model.stan  # Hierarchical Bayesian RL Stan model
│   └── model_free_learning       # (Potentially outdated - analysis consolidated in R script)
│       └── model-free_analysis.ipynb
├── bandit_task                   # Main experiment folder
│   ├── collected_data            # Example CSVs (task & questionnaire data)
│   ├── media                     # Audio & visual stimuli
│   │   ├── sounds
│   │   └── stimuli
│   ├── questionnaire_survey.py   # Questionnaire logic (Python/PsychoPy)
│   ├── random_walk_data          # Pre-generated random walk parameters & images
│   ├── random_walk_generation.ipynb
│   ├── requirements.txt          # Python dependencies (for experiment)
│   ├── run_experiment.py         # Script to run the entire experiment
│   ├── task_logic.py             # Core logic for the bandit task (Python/PsychoPy)
│   └── variable_ratio_schedule.py
└── saliency-rules.mdc            # Cursor Rules context file (metadata for AI)
```

-   **analysis_scripts/model_based_learning/**
    -   `analyze_saliency_data.R`: The primary R script for loading data, preparing it for Stan, fitting the Bayesian RL model, performing model diagnostics (Rhat, n_eff, PPC, LOO-CV), analyzing model parameters (Hypothesis 1.1), conducting model-free analyses (win-stay probabilities - Hypothesis 1.2, post-reinforcement pauses - Hypotheses 2.1 & 2.2), and correlating results with questionnaire data.
    -   `saliency_rl_model.stan`: The Stan code defining the hierarchical Q-learning model. It estimates subject-level parameters for base learning rate (`alpha`), learning rate shift due to salient wins (`alpha_shift`), and decision consistency (`beta`), using a non-centered parameterization and incorporating correlations between parameters.
-   **bandit_task/**
    -   Contains the Python code (using PsychoPy libraries) to run the experiment (`run_experiment.py`, `task_logic.py`, `questionnaire_survey.py`), stimulus media, pre-generated random walks, and sample data output (`collected_data/`.
    -   `requirements.txt`: Lists Python packages needed to run the experiment.

---

## Installation

1.  **Clone the repository**:
    ```bash
    git clone https://github.com/yannikpoth/saliency-project.git
    cd saliency-project
    ```

2.  **Python Environment (for running the experiment)**:
    - Create and activate a virtual environment (recommended):
      ```bash
      python3 -m venv venv
      source venv/bin/activate  # macOS/Linux
      # or venv\Scripts\activate on Windows
      ```
    - Install required Python packages:
      ```bash
      pip install -r bandit_task/requirements.txt
      ```

3.  **R Environment (for running the analysis)**:
    - Ensure you have a recent version of R installed.
    - Install required R packages. Open R or RStudio and run:
      ```R
      install.packages(c("tidyverse", "rstan", "bayesplot", "effsize", "rstatix", "car", "lme4", "here", "fs", "loo"))
      ```
    - Ensure you have a C++ toolchain configured for RStan (e.g., CmdStanR backend or Rtools on Windows, Xcode Command Line Tools on macOS). Follow instructions on the [Stan website](https://mc-stan.org/users/interfaces/rstan.html).

---

## Running the Experiment

1.  **Navigate to the repo root** (if not already there):
    ```bash
    cd path/to/saliency-project
    ```
2.  **Activate the Python virtual environment**:
    ```bash
    source venv/bin/activate # or equivalent
    ```
3.  **Run the main experiment script**:
    ```bash
    python bandit_task/run_experiment.py
    ```
    - The script will first run the bandit task (`task_logic.py`) and save task data, then run the questionnaires (`questionnaire_survey.py`) and save questionnaire data to `bandit_task/collected_data/`. Data files are named using the participant ID entered at the start.

---

## Analysis Workflow

The primary analysis workflow is implemented in the R script `analysis_scripts/model_based_learning/analyze_saliency_data.R`. It performs the following steps:

1.  **Load Data**: Reads and combines all `_task_data.csv` and `_questionnaire_data.csv` files from `bandit_task/collected_data/`.
2.  **Data Preprocessing**: Filters for main trials, handles missing values, creates a subject index, and generates the `salient_feedback` indicator (1 if `condition == 1` and `reward == 1`, 0 otherwise).
3.  **Stan Model Fitting**:
    - Prepares the data structure required by the Stan model (`saliency_rl_model.stan`).
    - Compiles and fits the hierarchical Bayesian RL model using `rstan`. The model estimates group-level and subject-level parameters:
        - `alpha`: Base learning rate.
        - `alpha_shift`: Additive shift to the learning rate specifically following salient wins.
        - `beta`: Inverse temperature (decision consistency).
    - The model uses a non-centered parameterization for robustness and estimates correlations between the subject-level parameters. Learning rates are estimated on a latent scale and transformed using `Phi_approx` (probit approximation) to the 0-1 range.
4.  **Model Diagnostics**:
    - Checks convergence statistics (`Rhat`, effective sample size `n_eff`).
    - Assesses sampling quality (divergences).
    - Visualizes parameter traces and posterior distributions (`bayesplot`).
    - Performs posterior predictive checks (PPCs) to evaluate model fit.
    - Calculates Leave-One-Out Cross-Validation (LOO-CV) for model assessment using the `loo` package.
5.  **Hypothesis Testing & Results Extraction**:
    - **H1.1 (RL Model - Alpha Shift)**: Examines the posterior distribution of the group-level `latent_alpha_shift` parameter (`mu_pr[2]`). Evidence for the hypothesis is assessed by checking if the 95% Credible Interval excludes zero. Also reports interpretable group-level learning rates (`mu_alpha_base`, `mu_alpha_salient_win`).
    - **H1.2 (Win-Stay Probability)**: Calculates win-stay probability for each participant under salient win vs. non-salient win conditions. Performs normality checks (Shapiro-Wilk) and uses a paired t-test or Wilcoxon signed-rank test accordingly. Reports p-value, effect size (Cohen's d), and CIs.
    - **H2.1 & H2.2 (Post-Reinforcement Pauses)**: Calculates PRPs following losses, non-salient wins, and salient wins. Performs assumption checks (normality via Shapiro-Wilk, homogeneity of variances via Levene's test). Uses repeated measures ANOVA (via `rstatix::anova_test`) or Friedman test. Conducts appropriate post-hoc tests (e.g., paired t-tests or Wilcoxon with Bonferroni correction). Reports p-values and effect sizes (e.g., partial eta squared).
    - **Questionnaire Correlations**: Merges estimated subject-level RL parameters (e.g., `alpha_shift`, `beta`) with questionnaire scores (BIS, SSS totals). Calculates and visualizes correlations.
6.  **Visualization**: Generates plots for key results (e.g., posterior distributions, win-stay probabilities, PRPs, correlations).

**To run the analysis**:

1.  Ensure the R environment is set up (see [Installation](#installation)).
2.  Open R or RStudio.
3.  Set the working directory to the repository root (`saliency-project/`).
4.  Execute the R script:
    ```R
    source("analysis_scripts/model_based_learning/analyze_saliency_data.R")
    ```
    Alternatively, open the script in RStudio and run it line-by-line or source it. Results (summaries, plots) will be printed to the console or saved if specified within the script.

---

## Data

-   **Sample Data**: Located in `bandit_task/collected_data/`. Includes example `_task_data.csv` and `_questionnaire_data.csv` files.
-   **Random Walk Parameters**: Found in `bandit_task/random_walk_data/`. These define the reward probabilities for each trial across practice/main tasks.

> **Important**: Real participant data may be excluded or anonymized to protect participant privacy. If you need the full dataset for reproducibility, please check the project's GitHub Releases or contact the maintainer.

---

## License

MIT License

Copyright (c) 2023 Yannik Poth

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

---

## Contact

For questions regarding the experiment, analysis, or to request data access, please contact:
-   **Yannik Poth**: [yannikpoth@me.com](mailto:yannikpoth@me.com)
-   Or create an [issue](https://github.com/yannikpoth/saliency-project/issues) in this repository.
