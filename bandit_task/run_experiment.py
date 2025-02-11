import tkinter as tk
from tkinter import ttk, messagebox
from pathlib import Path
import subprocess
import sys
import os

def run_experiment(participant_id):
    # Get the directory of this script.
    current_script_dir = Path(__file__).parent

    # Build absolute paths to the other scripts:
    task_script = current_script_dir / "task_logic.py"
    questionnaire_script = current_script_dir / "questionnaire_survey.py"

    # Run the task script
    subprocess.run([sys.executable, str(task_script), participant_id])

    # Run the questionnaire script
    subprocess.run([sys.executable, str(questionnaire_script), participant_id])

def submit_id():
    pid = id_entry.get().strip()
    if not pid:
        messagebox.showerror("Error", "Please enter a valid Participant ID.")
        return
    root.destroy()  # Close the ID input window
    run_experiment(pid)

# Create a simple Tkinter window to get the participant ID.
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
