#!/usr/bin/env python3
"""
run_experiment.py

A simple GUI interface for capturing a participant's ID and running 
two scripts in sequence: 'task_logic.py' then 'questionnaire_survey.py'.
"""

import tkinter as tk
from tkinter import ttk, messagebox
from pathlib import Path
import subprocess
import sys

def run_experiment(participant_id):
    """
    Run the main bandit task and then the questionnaire script for a given participant.
    Capture the 'bonus' from the task script's output and pass it along to the questionnaire.
    """
    current_script_dir = Path(__file__).parent

    task_script = current_script_dir / "task_logic.py"
    questionnaire_script = current_script_dir / "questionnaire_survey.py"

    # 1) Run the bandit task, capturing its output
    task_proc = subprocess.run(
        [sys.executable, str(task_script), participant_id],
        capture_output=True,
        text=True
    )

    # Check for errors
    if task_proc.returncode != 0:
        print("Error running task_logic.py:", task_proc.stderr)
        return

    # 2) Parse the bonus from stdout (assuming it's last line)
    lines = task_proc.stdout.strip().splitlines()
    bonus_str = lines[-1]
    # Optionally handle an empty or invalid bonus
    if not bonus_str:
        bonus_str = "0"

    # 3) Pass the participant ID and the bonus to the questionnaire script
    questionnaire_proc = subprocess.run(
        [sys.executable, str(questionnaire_script), participant_id, bonus_str]
    )
    print(questionnaire_proc)
    if questionnaire_proc.returncode != 0:
        print("Error running questionnaire_survey.py")


def submit_id():
    """
    Handle the event when the user submits the participant ID via the GUI.
    """
    pid = id_entry.get().strip()
    if not pid:
        messagebox.showerror("Error", "Please enter a valid Participant ID.")
        return
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
