import tkinter as tk
from tkinter import ttk, messagebox
import subprocess
import sys

def run_experiment(participant_id):
    # Run task.py and wait for it to finish.
    subprocess.run([sys.executable, "task.py", participant_id])
    # After task.py is done, run questionnaires.py.
    subprocess.run([sys.executable, "questionnaires.py", participant_id])

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
