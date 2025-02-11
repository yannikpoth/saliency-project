import numpy as np

def create_vr_schedule():
    # Define the base VR sequence (length = 4)
    vr_base_sequence = [0, 0, 0, 1]
    
    # Repeat and randomize the base sequence 50 times
    vr_schedule = []
    for _ in range(50):
        random_sequence = np.random.permutation(vr_base_sequence)  # Shuffle the base sequence
        vr_schedule.extend(random_sequence)  # Add the shuffled sequence to the schedule
    
    return vr_schedule