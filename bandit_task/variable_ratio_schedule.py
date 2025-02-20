import numpy as np
import pandas as pd

def create_vr_schedule():
    # Define the base VR sequence (length = 4)
    vr_base_sequence = [0, 0, 0, 1]
    
    # Repeat and randomize the base sequence 50 times
    vr_schedule = []
    for _ in range(50):
        random_sequence = np.random.permutation(vr_base_sequence)  # Shuffle the base sequence
        vr_schedule.extend(random_sequence)  # Add the shuffled sequence to the schedule
    
    return vr_schedule

# Save the schedule to a CSV file (each row contains one value of the schedule)
df = pd.DataFrame({'schedule': create_vr_schedule()})
df.to_csv("task_data/variable_ratio_schedule/vr_schedule.csv", index=False)