from psychopy import visual, core, event, gui, data, sound
import pyglet
import numpy as np
import os
import pandas as pd
from vr_schedule import create_vr_schedule

####################################### SET TASK PARAMETERS #######################################

n_practice_trials = 15                  # Anzahl Practice-Trials
n_main_trials = 200                     # Anzahl Haupt-Trials
vr_schedule = create_vr_schedule()   # Variable Ratio Reinforcement Plan für salientes Feedback
print(vr_schedule)

ITI_mean, ITI_sd = 2.0, 0.5             # Inter Trial Intervall Dauer und Dauer-Varianz

###################################################################################################

# Versuchspersonennummer abfragen
exp_info = {'Participant': ''}
dlg = gui.DlgFromDict(dictionary=exp_info, title="Two-Armed Bandit Task")
if dlg.OK == False:
    core.quit()  # Experiment abbrechen, wenn der Dialog abgebrochen wird
participant_id = exp_info['Participant']

# Speicherort für Log-Dateien
data_dir = os.path.join(os.getcwd(), 'data')
if not os.path.exists(data_dir):
    os.makedirs(data_dir)
log_file = os.path.join(data_dir, f"{participant_id}_data.csv")

# Bildschirmauflösung abfragen
display = pyglet.canvas.Display()
screen = display.get_default_screen()
screen_width = screen.width
screen_height = screen.height

# Window erstellen
win = visual.Window(size=(screen_width, screen_height), fullscr=False, screen=1,
                winType='pyglet', allowStencil=False,
                monitor='testMonitor', color='black', colorSpace='rgb',
                backgroundImage='', backgroundFit='none',
                blendMode='avg', useFBO=True,
                units='height',
                checkTiming=False)

# Erklärungsslides
instructions = [
    "Welcome to the experiment.\n\nPress SPACE to continue.",
    "In this task, you will choose between two options.\n\nPress SPACE to continue.",
    "Your goal is to maximize your rewards.\n\nPress SPACE to continue.",
    "You first will go through some practice rounds.\n\nPress SPACE to start the task."
]
for instruction in instructions:
    text = visual.TextStim(win, text=instruction, height=0.05, wrapWidth=1.5, color="white")
    text.draw()
    win.flip()
    event.waitKeys(keyList=['space'])

# Fixation Cross
fixation = visual.TextStim(win, text='+', height=0.1,color='white')

# Stimuli laden
stim1 = visual.ImageStim(win, image='media/stimuli/stim1.png', size=(0.4, 0.4))
stim2 = visual.ImageStim(win, image='media/stimuli/stim2.png', size=(0.4, 0.4))

# Feedback Text
feedback_text = visual.TextStim(win, text='', pos=(0, -0.3), color='white', height=0.1)

# Feedback-Images Non-salient
feedback_non_salient_1 = visual.ImageStim(win, image='media/stimuli/stim1_feedback_non_salient.png', size=(0.4, 0.4))
feedback_non_salient_2 = visual.ImageStim(win, image='media/stimuli/stim2_feedback_non_salient.png', size=(0.4, 0.4))

# Feedback-Videos salient
feedback_salient_1 = visual.MovieStim(
    win, filename='media/stimuli/stim1_feedback_salient.mov', size=(0.4, 0.4), 
    movieLib='ffpyplayer', loop=False, noAudio=True, units=win.units, ori=0.0, 
    anchor='center', opacity=None, contrast=1.0, depth=-2)
feedback_salient_2 = visual.MovieStim(
    win, filename='media/stimuli/stim2_feedback_salient.mov', size=(0.4, 0.4), 
    movieLib='ffpyplayer', loop=False, noAudio=True, units=win.units, ori=0.0, 
    anchor='center', opacity=None, contrast=1.0, depth=-2)

# Load Background Music
background_music = sound.Sound('media/sounds/ambience.mp3')
background_music.setVolume(0.8)  # Set initial volume level
background_music.play(loops=-1)  # Play in continuous loop

# Salient Feedback Sound
salient_sound = sound.Sound('media/sounds/salient_feedback.wav')
salient_sound.setVolume(0.8)


# Load the random walk data from CSV
random_walk_data = pd.read_csv('random_walk_data.csv')

# Task Parameter
total_trials = n_practice_trials + n_main_trials
schedule_index = -1  # Zähler für erfolgreiche Trials
trial_counter = 0  # Zähler für die Anzahl der Trials seit dem letzten „salient“ Feedback

'''
# Funktion zur Berechnung von Gewinnen basierend auf Wahrscheinlichkeiten
def calc_reward(prob):
    return np.random.rand() < prob
'''
# Adjust backround sound volume while salient feedback
def adjust_background_volume(during_salient=True):
    if during_salient:
        background_music.setVolume(0.2)  # Reduce volume during salient feedback
    else:
        steps = 20  # Number of steps for the fade-in
        initial_volume = 0.2  # Start volume during salient feedback
        volume_step = (0.8 - 0.2) / steps
        interval = 0.5 / steps
        
        # Gradually increase the volume
        for i in range(steps):
            current_volume = initial_volume + i * volume_step
            background_music.setVolume(current_volume)
            core.wait(interval)  # Wait between each step

# Inter Trial Interval (ITI)
def get_iti():
    return max(np.random.normal(loc=ITI_mean, scale=ITI_sd), 0.5) # Sicherstellen, dass das ITI nicht negativ wird

# Protokoll initialisieren
with open(log_file, 'w') as f:
    f.write('Mode,Trial,Arm_Chosen,Win,Condition,Reward_Prob_1,Reward_Prob_2\n')

# Funktion zur Durchführung eines Trials
def run_trial(trial_num, mode):
    global reward_probs, schedule_index, vr_schedule, trial_counter
    
    reward_probs = [random_walk_data.loc[trial_num, 'mu_1'], random_walk_data.loc[trial_num, 'mu_2']]
    payoffs = [random_walk_data.loc[trial_num, 'payoff_1'], random_walk_data.loc[trial_num, 'payoff_2']]

    print(schedule_index)

    # Randomisiere die Position der Stimuli (links/rechts)
    if np.random.rand() > 0.5:
        stim1.pos, stim2.pos = (-0.4, 0), (0.4, 0)
        stim_choice = [stim1, stim2]
    else:
        stim1.pos, stim2.pos = (0.4, 0), (-0.4, 0)
        stim_choice = [stim2, stim1]

    # Zeige die Stimuli
    stim1.draw()
    stim2.draw()
    win.flip()

    # Warte auf Antwort (links/rechts)
    keys = event.waitKeys(keyList=['left', 'right'])
    
    # Finde heraus, welcher Stimulus gewählt wurde
    if keys[0] == 'left':
        chosen_stim = stim_choice[0]  # Der linke Stimulus wurde gewählt
        other_stim = stim_choice[1]  # Der rechte Stimulus bleibt unverändert
        chosen_arm = 0 if stim_choice[0] == stim1 else 1  # Index des gewählten Arms
    else:
        chosen_stim = stim_choice[1]  # Der rechte Stimulus wurde gewählt
        other_stim = stim_choice[0]  # Der linke Stimulus bleibt unverändert
        chosen_arm = 0 if stim_choice[1] == stim1 else 1  # Index des gewählten Arms


    # Berechne, ob ein Gewinn erzielt wird
    win_this_trial = payoffs[chosen_arm]==1
    
    trial_counter += 1

    if win_this_trial:
        feedback_text.text = '+100 Punkte'
        schedule_index += 1
    else:
        feedback_text.text = '+0 Punkte'

    # Bestimme die Bedingung (salient vs. non-salient)
    if (vr_schedule[schedule_index] == 1 or trial_counter >= 10) and win_this_trial:
        feedback_cond = 'salient'
        trial_counter = 0
        
        adjust_background_volume(during_salient=True)  # Lower background music
        salient_sound.play()

        # Ersetze den gewählten Stimulus durch den entsprechenden salient Feedback Stimulus
        feedback_vid = feedback_salient_1 if chosen_arm == 0 else feedback_salient_2
        feedback_vid.size = chosen_stim.size  # Größe des Feedback-Stimulus anpassen
        feedback_vid.pos = chosen_stim.pos  # Position des Feedback-Stimulus anpassen
        feedback_vid.seek(0)  # Zeige die Animation
        feedback_vid.play()
        
        feedback_text.draw()
        feedback_text.setAutoDraw(True)
        
        other_stim.setAutoDraw(True)
        
        while not feedback_vid.isFinished:
            feedback_vid.draw()
            win.flip()
        
        while salient_sound.status == "PLAYING":
            core.wait(0.01)  # Check every 10 ms to see if sound is still playing

        adjust_background_volume(during_salient=False)
        
        other_stim.setAutoDraw(False)
        feedback_text.setAutoDraw(False)
        

    else:
        feedback_cond = 'non-salient'

        # Ersetze den gewählten Stimulus durch den entsprechenden non-salient Feedback Stimulus
        feedback_img = feedback_non_salient_1 if chosen_arm == 0 else feedback_non_salient_2
        feedback_img.size = chosen_stim.size  # Größe des Feedback-Stimulus anpassen
        feedback_img.pos = chosen_stim.pos  # Position des Feedback-Stimulus anpassen
        feedback_img.draw()  # Zeige das Bild
        other_stim.draw()
        feedback_text.draw()
        win.flip()
        core.wait(3)  # 3 Sekunden Feedback-Anzeige
    
    

    # Logge den Trial
    with open(log_file, 'a') as f:
        f.write(f'{mode},{trial_num+1},{chosen_arm},{win_this_trial},{feedback_cond},{reward_probs[0]},{reward_probs[1]},{payoffs[0]},{payoffs[1]}\n')


    # Inter Trial Interval (ITI)
    fixation.draw()
    win.flip()
    core.wait(get_iti())

# Practice Trials
for trial in range(n_practice_trials):
    run_trial(trial, "practice")

# Main Trials
for trial in range(n_main_trials):
    run_trial(trial, "main")

# Ende des Experiments
thanks = visual.TextStim(win, text="Thank you for participating!", color="white")
thanks.draw()
win.flip()
core.wait(3)
win.close()
core.quit()
