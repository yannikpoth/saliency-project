from psychopy import visual, core, event, gui, data, sound
import numpy as np
import random

# Experiment Information
exp_info = {'participant': ''}
dlg = gui.DlgFromDict(dictionary=exp_info, title='Two-Armed Bandit Task')
if dlg.OK == False:
    core.quit()

# Window Setup
win = visual.Window(size=(800, 600), color='grey', units='pix')

# Instructions
instructions = [
    "Welcome to the experiment. Press SPACE to continue.",
    "You will be presented with two options. Choose one by pressing the LEFT or RIGHT arrow key.",
    "After making a choice, you will receive feedback. Sometimes, this feedback will be more salient with a special animation and sound."
]

for instruction in instructions:
    instr_text = visual.TextStim(win, text=instruction, color='white')
    instr_text.draw()
    win.flip()
    event.waitKeys(keyList=['space'])

# Stimuli Setup
left_stim = visual.ImageStim(win, image='media/stimuli/stim1.png', pos=(-200, 0))
right_stim = visual.ImageStim(win, image='media/stimuli/stim2.png', pos=(200, 0))

non_salient_left = visual.ImageStim(win, image='media/stimuli/stim1_feedback_non_salient.png', pos=(-200, 0))
non_salient_right = visual.ImageStim(win, image='media/stimuli/stim2_feedback_non_salient.png', pos=(200, 0))

salient_animation_left = visual.MovieStim3(win, filename='media/stimuli/stim1_feedback_salient.mov', pos=(-200, 0))
salient_animation_right = visual.MovieStim3(win, filename='media/stimuli/stim2_feedback_salient.mov', pos=(200, 0))

salient_sound = sound.Sound('media/sounds/salient_feedback.wav')

fixation = visual.TextStim(win, text='+', color='white')

# Logfile Setup
filename = f'data/{exp_info["participant"]}_data.csv'
logfile = open(filename, 'w')
logfile.write('trial,choice,win,feedback_type\n')

# Task Parameters
n_practice_trials = 15
n_main_trials = 200
total_trials = n_practice_trials + n_main_trials

win_probs_left = np.random.rand(total_trials)  # Gewinnwahrscheinlichkeiten für links
win_probs_right = np.random.rand(total_trials)  # Gewinnwahrscheinlichkeiten für rechts

vr_schedule = np.random.randint(8, 13)  # Variable Ratio zwischen 8 und 12 Erfolgen
success_counter = 0  # Zähler für erfolgreiche Trials
trial_counter = 0  # Zähler für die Anzahl der Trials seit dem letzten „salient“ Feedback

def run_trial(trial_num):
    global success_counter, vr_schedule, trial_counter

    # Fixation Interval
    iti_duration = np.random.normal(loc=2.0, scale=0.5)
    fixation.draw()
    win.flip()
    core.wait(max(iti_duration, 0.5))  # Sicherstellen, dass das ITI nicht negativ wird

    # Randomize Position
    if random.choice([True, False]):
        left_stim.image, right_stim.image = right_stim.image, left_stim.image

    # Draw stimuli
    left_stim.draw()
    right_stim.draw()
    win.flip()

    keys = event.waitKeys(keyList=['left', 'right', 'escape'])

    if 'escape' in keys:
        core.quit()

    choice = 'left' if 'left' in keys else 'right'
    win_this_trial = (win_probs_left[trial_num] > 0.5) if choice == 'left' else (win_probs_right[trial_num] > 0.5)

    trial_counter += 1

    if win_this_trial:
        success_counter += 1

    # Salient Feedback Condition
    if success_counter >= vr_schedule or trial_counter >= 30:
        feedback_type = 'salient'
        success_counter = 0
        trial_counter = 0
        vr_schedule = np.random.randint(8, 13)

        if choice == 'left':
            salient_animation_left.draw()
        else:
            salient_animation_right.draw()

        salient_sound.play()
        win.flip()
        core.wait(3)  # Saliente Bedingung läuft für 3 Sekunden
        salient_sound.stop()

    else:
        feedback_type = 'non-salient'
        if choice == 'left':
            non_salient_left.draw()
        else:
            non_salient_right.draw()

        win.flip()
        core.wait(3)

    logfile.write(f'{trial_num},{choice},{int(win_this_trial)},{feedback_type}\n')

# Practice Trials
for trial in range(n_practice_trials):
    run_trial(trial)

# Main Trials
for trial in range(n_practice_trials, total_trials):
    run_trial(trial)

logfile.close()
win.close()
core.quit()