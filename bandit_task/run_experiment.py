"""
run_experiment.py

A simple GUI interface for capturing a participant's ID and running the bandit task 
(`task_logic.py`) followed by the questionnaire script (`questionnaire_survey.py`). 
"""

import tkinter as tk
from tkinter import ttk, messagebox
from pathlib import Path
import subprocess
import sys


def run_experiment(participant_id):
    """
    Run the main bandit task and questionnaire scripts for a given participant.

    This function builds absolute paths to the `task_logic.py` and 
    `questionnaire_survey.py` scripts—both located in the same directory 
    as `run_experiment.py`—then executes them sequentially, passing the 
    participant ID as a command-line argument.

    Args:
        participant_id (str): Unique identifier for the participant.
    """
    current_script_dir = Path(__file__).parent

    # Build absolute paths to the other scripts
    task_script = current_script_dir / "task_logic.py"
    questionnaire_script = current_script_dir / "questionnaire_survey.py"

    # Run the bandit task
    subprocess.run([sys.executable, str(task_script), participant_id])

    # Run the questionnaire
    subprocess.run([sys.executable, str(questionnaire_script), participant_id])


def submit_id():
    """
    Handle the event when the user submits the participant ID via the GUI.

    Retrieves the participant ID from the input field, validates it, 
    and calls `run_experiment(pid)`. If the ID is empty, shows an 
    error message dialog.
    """
    pid = id_entry.get().strip()
    if not pid:
        messagebox.showerror("Error", "Please enter a valid Participant ID.")
        return
    # Close the GUI window before running the experiment
    root.destroy()
    run_experiment(pid)


# ---------------------------
# Main GUI Setup
# ---------------------------
root = tk.Tk()
root.title("Participant ID")
root.geometry("300x150")

label = ttk.Label(root, text="Enter Participant ID:")
label.pack(pady=10)

id_entry = ttk.Entry(root, width=30)
id_entry.pack(pady=10)

submit_button = ttk.Button(root, text="Submit", command=submit_id)
submit_button.pack(pady=10)

root.mainloop()
