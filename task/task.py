from psychopy import visual, core, event, gui, data, sound
import pyglet
import numpy as np
import os
import pandas as pd

####################################### SET TASK PARAMETERS #######################################

n_practice_trials = 15                  # Anzahl Practice-Trials
n_main_trials = 200                     # Anzahl Haupt-Trials
vr_schedule = np.random.randint(4, 8)   # Variable Ratio Reinforcement Plan für salientes Feedback

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
win = visual.Window(size=(screen_width, screen_height), fullscr=False, screen=0,
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

# Salient Feedback Sound
salient_sound = sound.Sound('media/sounds/salient_feedback.wav')

# Load the random walk data from CSV
random_walk_data = pd.read_csv('random_walk_data.csv')

# Task Parameter
total_trials = n_practice_trials + n_main_trials
success_counter = 0  # Zähler für erfolgreiche Trials
trial_counter = 0  # Zähler für die Anzahl der Trials seit dem letzten „salient“ Feedback

'''
# Funktion zur Berechnung von Gewinnen basierend auf Wahrscheinlichkeiten
def calc_reward(prob):
    return np.random.rand() < prob
'''

# Inter Trial Interval (ITI)
def get_iti():
    return max(np.random.normal(loc=ITI_mean, scale=ITI_sd), 0.5) # Sicherstellen, dass das ITI nicht negativ wird

# Protokoll initialisieren
with open(log_file, 'w') as f:
    f.write('Mode,Trial,Arm_Chosen,Win,Condition,Reward_Prob_1,Reward_Prob_2\n')

# Funktion zur Durchführung eines Trials
def run_trial(trial_num, mode):
    global reward_probs, success_counter, vr_schedule, trial_counter
    
    reward_probs = [random_walk_data.loc[trial_num, 'mu_1'], random_walk_data.loc[trial_num, 'mu_2']]
    payoffs = [random_walk_data.loc[trial_num, 'payoff_1'], random_walk_data.loc[trial_num, 'payoff_2']]

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
        success_counter += 1
        feedback_text.text = '+100 Punkte'
    else:
        feedback_text.text = '+0 Punkte'

    # Bestimme die Bedingung (salient vs. non-salient)
    if (success_counter >= vr_schedule or trial_counter >= 10) and win_this_trial:
        feedback_cond = 'salient'
        success_counter = 0
        trial_counter = 0
        vr_schedule = np.random.randint(4, 8)
        
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
