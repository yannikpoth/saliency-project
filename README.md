# Illuminating Decisions: How Salient Reward-Paired Cues Modulate Learning and Choice Behavior

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R 4.5.2](https://img.shields.io/badge/R-4.5.2-blue)](https://www.r-project.org/)
[![Stan](https://img.shields.io/badge/Stan-2.37.0-red)](https://mc-stan.org/)

A reinforcement learning study investigating how **salient audiovisual feedback** (inspired by electronic gambling machines) influences human learning and decision-making in a restless two-armed bandit task.

This repository provides a complete, reproducible scientific workflow including:
- **Experiment code** (Python/PsychoPy) for data collection
- **Analysis pipeline** (R + Stan) with *hierarchical Bayesian RL models* and *Behavioral metrics* (win-stay/lose-shift, post-reinforcement pauses)
- **Docker containerization** for full reproducibility

**Study Focus**: How do flashy lights and sounds paired with monetary rewards bias learning rates, choice behavior, and reaction times? Do individual differences in impulsivity and sensation seeking moderate these effects?

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Repository Structure](#repository-structure)
3. [Installation & Setup](#installation--setup)
4. [Running the Analysis](#running-the-analysis)
5. [Running the Experiment](#running-the-experiment)
6. [Data](#data)
7. [Key Features](#key-features)
8. [Citation](#citation)
9. [License](#license)
10. [Contact](#contact)

---

## Project Overview

### Background

Modern electronic gambling machines use **salient audiovisual cues** (bright lights, celebratory sounds, vivid animations) to mark wins. This study explores whether such cues, when paired with monetary rewards in a bandit task, systematically bias the learning process.

### Research Questions

1. **Learning Rate Modulation**: Does pairing rewards with salient cues increase learning rates (α) compared to non-salient wins or losses?
2. **Win-Stay Behavior**: Do participants show stronger win-stay tendencies following salient wins vs. non-salient wins?
3. **Post-Reinforcement Pauses (PRPs)**: Do reaction times differ following losses, non-salient wins, and salient wins?
4. **Individual Differences**: Are these effects moderated by sensation seeking or impulsivity?

### Task Design

- **Restless two-armed bandit**: Participants repeatedly choose between two options with drifting reward probabilities
- **Variable reinforcement schedule**: Rewards appear probabilistically (not every choice is rewarded)
- **Salient feedback condition**: Some rewards accompanied by flashy audiovisual stimuli, others by minimal feedback
- **Questionnaires**: Barratt Impulsiveness Scale (BIS) and Sensation Seeking Scale (SSS)

---

## Repository Structure

```
.
├── analysis/                       # Analysis pipeline (R + Stan)
│   ├── R/                          # Modular R functions
│   │   ├── io.R                    # Data loading and output
│   │   ├── preprocess.R            # Data cleaning and feature engineering
│   │   ├── behavior_metrics.R      # Win-stay, PRPs, model-free metrics
│   │   ├── rl_models.R             # RL model fitting wrappers
│   │   └── viz.R                   # Plotting and visualization
│   ├── models/                     # Stan model definitions
│   │   ├── rl_hierarchical.stan    # Baseline RL model
│   │   └── rl_hierarchical_shift.stan  # Model with α_shift (salient cue effect)
│   ├── outputs/                    # Generated figures and tables
│   │   ├── figs/
│   │   └── tables/
│   ├── run_analysis.R              # Main analysis entry point
│   └── setup.R                     # Environment setup (non-Docker users)
│
├── data/
│   ├── raw/                        # Original CSV files (read-only)
│   │   ├── 01_task_data.csv
│   │   ├── 01_questionnaire_data.csv
│   │   └── ...                     # 44 participants
│   └── processed/                  # Cleaned and combined data (generated)
│       ├── all_task_data.tsv
│       └── all_questionnaire_data.tsv
│
├── experiment/                     # PsychoPy experiment code
│   ├── bandit_task/                # Task implementation
│   └── requirements.txt            # Python dependencies
│
├── docker/
│   └── Dockerfile                  # Reproducible R + Stan environment
│
├── .devcontainer/
│   └── devcontainer.json           # VS Code Dev Container config
│
├── Makefile                        # Convenient pipeline commands
├── renv.lock                       # R package versions (renv)
├── LICENSE                         # MIT License
└── README.md                       # This file
```

---

## Installation & Setup

### Option 1: Using Docker (Recommended) 🐳

**Recommended for reproducible reruns, collaboration, and review.**

#### Prerequisites
- [Docker](https://docs.docker.com/get-docker/) installed
- [VS Code](https://code.visualstudio.com/) with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) (optional, for interactive development)

#### Steps

**A. With VS Code (Interactive Development)**
1. Clone the repository:
   ```bash
   git clone https://github.com/yannikpoth/saliency-project.git
   cd saliency-project
   ```
2. Open in VS Code: `code .`
3. When prompted, click **"Reopen in Container"** (or use Command Palette → "Dev Containers: Reopen in Container")
4. Wait for the image build to finish. The container restores the pinned R package environment from `renv.lock` during the build.
5. Run the analysis from the integrated terminal:
   ```bash
   make analysis
   ```

**B. With Docker CLI (No VS Code)**
1. Build the container:
   ```bash
   docker build -t saliency-project -f docker/Dockerfile .
   ```
2. Run the analysis:
   ```bash
   docker run --rm -v $(pwd):/workspace saliency-project Rscript analysis/run_analysis.R
   ```
   The Docker image stores the restored R library outside `/workspace`, so bind-mounting the repository does not hide installed packages.

---

### Option 2: Local R Installation

**Fallback for users who prefer or require a native R installation.**

#### Prerequisites
- **R** ≥ 4.5.2 ([download](https://cloud.r-project.org/))
- **Build tools** for compiled R and Stan packages:
  - **macOS**: Xcode Command Line Tools (`xcode-select --install`)
  - **Windows**: [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
  - **Linux**: `sudo apt install build-essential gfortran cmake` (or equivalent)

#### Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/yannikpoth/saliency-project.git
   cd saliency-project
   ```

2. Run the setup script (installs packages via renv):
   ```bash
   Rscript analysis/setup.R
   ```
   This restores the project library from `renv.lock`. After setup, interactive R sessions opened in the project root auto-activate the same environment.

3. You're ready! See [Running the Analysis](#running-the-analysis) below.

---

## Running the Analysis

Once your environment is set up (Docker or local R), run the full analysis pipeline:

### Using Make (Recommended)
```bash
make analysis
```

### Or directly with R
```bash
Rscript analysis/run_analysis.R
```

### What This Does

The pipeline will:
1. **Load and preprocess data** from `data/raw/`
2. **Compute behavioral metrics** (win-stay, PRPs)
3. **Fit hierarchical Bayesian RL models** using Stan (multiple models for comparison)
4. **Run diagnostics** (convergence checks, posterior predictive checks)
5. **Generate figures and tables** in `analysis/outputs/`

**Expected runtime**: ~10-30 minutes depending on hardware and number of models fitted.

---

## Running the Experiment

To collect **new data** using the PsychoPy task:

### Prerequisites
- Python ≥ 3.10
- PsychoPy dependencies (see `experiment/requirements.txt`)

### Steps

1. Navigate to experiment directory:
   ```bash
   cd experiment
   ```

2. Create Python virtual environment:
   ```bash
   python3 -m venv .venv
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   ```

3. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

4. Run the task:
   ```bash
   python bandit_task/main.py
   ```

5. Output files will be saved to `data/raw/` as:
   - `{participant_id}_task_data.csv` (trial-by-trial choices and rewards)
   - `{participant_id}_questionnaire_data.csv` (BIS and SSS responses)

---

## Data

### Task Data (`*_task_data.csv`)

Trial-level data from the bandit task. Key columns:

| Column | Description |
|--------|-------------|
| `trial` | Trial number (1-215: 15 practice + 200 main) |
| `mode` | `practice` or `main` |
| `choice` | Chosen arm (0 or 1; empty if missed) |
| `reward` | Reward received (0 or 1) |
| `condition` | Feedback type (0=non-salient, 1=salient, 2=missed) |
| `reaction_time` | Response time in seconds |
| `reward_prob_1`, `reward_prob_2` | True reward probabilities for each arm |

### Questionnaire Data (`*_questionnaire_data.csv`)

Participant-level data. Key columns:

| Column | Description |
|--------|-------------|
| `participant_id` | Unique identifier |
| `bis_total` | Total Barratt Impulsiveness Scale score |
| `ss_total` | Total Sensation Seeking Scale score |
| `bis_1`–`bis_14` | Individual BIS items |
| `sss_1`–`sss_8` | Individual SSS items |

### Privacy Note

The included data (`data/raw/`) contains **sample data** from the actual study. Full datasets may be available upon reasonable request (see [Contact](#contact)).

---

## Key Features

### Hierarchical Bayesian RL Models

Implemented in Stan (`analysis/models/`):

- **Baseline Model** (`rl_hierarchical.stan`):
  - Parameters: Base learning rate (α), inverse temperature (β)
  - Standard Q-learning with hierarchical structure

- **Salient Cue Model** (`rl_hierarchical_shift.stan`):
  - Adds: Learning rate shift (α_shift) for salient wins
  - Tests primary hypothesis: Do salient cues increase learning rates?

- **Future Extensions**:
  - Perseveration parameter (κ) for choice stickiness
  - Forgetting/decay parameters

### Behavioral Metrics (Model-Free)

- **Win-Stay / Lose-Shift Probabilities**: Computed separately for salient vs. non-salient feedback
- **Post-Reinforcement Pauses (PRPs)**: Median reaction times following different outcomes
- **Statistical Tests**: Paired t-tests, ANOVA, Wilcoxon, with assumption checks

### Robust Workflow

- ✅ **Reproducible**: Docker + renv + version-pinned dependencies
- ✅ **Modular**: Separate R functions for each analysis step
- ✅ **Open Science Ready**: Clear data/code separation, documented pipeline
- ✅ **Diagnostic-Rich**: Convergence checks, PPCs, model comparison (LOO/WAIC)

---

## Citation

If you use this code or data, please cite:

```bibtex
@mastersthesis{poth2025saliency,
  author  = {Poth, Yannik},
  title   = {Illuminating Decisions: How Salient Reward-Paired Cues
             Modulate Learning and Choice Behavior},
  school  = {RWTH Aachen},
  year    = {2025},
  type    = {Master's thesis},
  url     = {https://github.com/yannikpoth/saliency-project}
}
```

See also: [`CITATION.cff`](CITATION.cff)

---

## License

This project is licensed under the **MIT License** - see the [LICENSE](LICENSE) file for details.

**Summary**: You are free to use, modify, and distribute this software for any purpose, including commercial use, provided you include the original copyright notice and disclaimer.

---

## Contact

**Yannik Poth**
📧 [yannikpoth@me.com](mailto:yannikpoth@me.com)
🐙 [GitHub Issues](https://github.com/yannikpoth/saliency-project/issues)

For questions about:
- **Data access**: Please reach out via email
- **Technical issues**: Open an issue on GitHub
- **Collaboration**: Email preferred

---

## Acknowledgments

This work was conducted as part of a Master's thesis in Psychology. Special thanks to:
- Supervisors and advisors (names TBD)
- Open source communities: R, Stan, PsychoPy
- Study participants

---

## Additional Resources

- **Stan User's Guide**: https://mc-stan.org/docs/
- **Reinforcement Learning Textbook**: Sutton & Barto (2018)
- **renv Documentation**: https://rstudio.github.io/renv/
- **Docker for R Users**: https://www.rocker-project.org/

---

**Status**: 🚧 Active development | 📊 Data collection complete | 📝 Analysis in progress

*Last updated: November 2025*
