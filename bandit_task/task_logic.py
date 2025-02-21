"""
task_logic.py

This module implements the main experimental logic for the Saliency Project,
which investigates how salient audiovisual cues impact learning and decision-making
in a two-armed bandit task. The experiment includes instructions, practice and main trials,
feedback (salient and non-salient), and bonus calculation. Stimulus presentation and event handling
are managed by PsychoPy, while audio management uses pygame.

Usage (standalone):
    Run this script with a participant ID as a command-line argument, e.g.:
        python task_logic.py PARTICIPANT_ID
    If no argument is provided, the script will prompt for input in the terminal.

For import, simply call:
    bonus = task_logic.run_task(participant_id)
"""

import csv
import os
import sys
from pathlib import Path
from typing import List, Tuple
import numpy as np
import pandas as pd
import pygame
import pyglet
from psychopy import core, event, visual

# ================================= CONFIGURATION =================================
BASE_DIR = Path(__file__).parent
MEDIA_DIR = BASE_DIR / "media"
STIMULI_DIR = MEDIA_DIR / "stimuli"
SOUNDS_DIR = MEDIA_DIR / "sounds"

CONFIG = {
    'TASK_PARAMS': {
        'N_PRACTICE_TRIALS': 1,
        'N_MAIN_TRIALS': 5,
        'ITI_MEAN': 1.0,
        'ITI_SD': 0.5,
        'MAX_RESPONSE_TIME': 5,
        'FEEDBACK_DURATION': 3,
        'BONUS_MAX_EUR': 3.0,
        'FORCED_SALIENCY': False,
        'STIM_SIZE': (0.4, 0.4),
        'STIM_POSITIONS': [(-0.4, 0), (0.4, 0)],
        'TEXT_PARAMS': {
            'height': 0.04,
            'wrapWidth': 1.5,
            'color': 'white',
            'pos': (0, 0.2)
        },
        'VOLUME': {
            'BACKGROUND': 0.8,
            'SALIENT': 0.2,
            'FADE_STEPS': 20,
            'FADE_DURATION': 0.5
        }
    },
    'PATHS': {
        'STIMULI': {
            'STIM1': str(STIMULI_DIR / "stim1.png"),
            'STIM2': str(STIMULI_DIR / "stim2.png"),
            'PRAC_STIM1': str(STIMULI_DIR / "prac_stim1.png"),
            'PRAC_STIM2': str(STIMULI_DIR / "prac_stim2.png"),
            'FEEDBACK': {
                'NON_SALIENT_1': str(STIMULI_DIR / "stim1_feedback_non_salient.png"),
                'NON_SALIENT_2': str(STIMULI_DIR / "stim2_feedback_non_salient.png"),
                'SALIENT_1': str(STIMULI_DIR / "stim1_feedback_salient.mov"),
                'SALIENT_2': str(STIMULI_DIR / "stim2_feedback_salient.mov"),
                'PRAC_NON_SALIENT_1': str(STIMULI_DIR / "prac_stim1_feedback_non_salient.png"),
                'PRAC_NON_SALIENT_2': str(STIMULI_DIR / "prac_stim2_feedback_non_salient.png"),
                'PRAC_SALIENT_1': str(STIMULI_DIR / "prac_stim1_feedback_salient.mov"),
                'PRAC_SALIENT_2': str(STIMULI_DIR / "prac_stim2_feedback_salient.mov")
            }
        },
        'SOUNDS': {
            'BACKGROUND': str(SOUNDS_DIR / "ambience.mp3"),
            'SALIENT_FEEDBACK': str(SOUNDS_DIR / "salient_feedback.wav")
        },
        'DATA_DIR': str(BASE_DIR / "collected_data"),
        'RANDOM_WALK_DATA_MAIN': str(BASE_DIR / "task_data/random_walk/csv/main_random_walk.csv"),
        'RANDOM_WALK_DATA_PRACTICE': str(BASE_DIR / "task_data/random_walk/csv/prac_random_walk.csv"),
        'VARIABLE_RATIO_SCHEDULE': str(BASE_DIR / "task_data/variable_ratio_schedule/vr_schedule.csv")
    },
    'INSTRUCTIONS': {
        'PRE_PRACTICE': [
            (
                "Willkommen!\n\nDu bist jetzt Teil eines spannenden Experiments. Deine Aufgabe ist es, Entscheidungen zu treffen, um möglichst viele Punkte zu sammeln.\n\nIn jeder Runde hast du zwei Optionen zur Auswahl. Du wählst eine davon aus und siehst sofort, ob du Punkte gewonnen hast oder nicht.\n\nBenutze die Pfeiltasten auf der Tastatur, um eine Option auszuwählen.\nMit der '←'-Taste wählst du die linke Option, mit der '→'-Taste wählst du die rechte Option",
                "Drücke LEERTASTE, um fortzufahren."
            ),
            (
                "Die Wahrscheinlichkeit, mit einer Option Punkte zu gewinnen, verändert sich im Laufe des Spiels. Deine Aufgabe ist es herauszufinden, welche Option momentan die beste ist.\n\nAchte darauf: Die Symbole erscheinen manchmal links, manchmal rechts – das ist zufällig und spielt keine Rolle. Entscheidend ist nur, welches Symbol du auswählst und NICHT auf welcher Seite sich das Symbol gerade befindet.",
                "Drücke LEERTASTE, um fortzufahren."
            ),
            (
                "Manchmal wirst du zusätzlich zum Gewinn Rückmeldung mit besonderen Effekten erhalten.\nDenke immer daran: Dein Ziel ist es, möglichst viele Punkte zu sammeln.",
                "Drücke LEERTASTE, um fortzufahren."
            ),
            (
                "Am Ende des Spiels erhältst du eine Bonuszahlung basierend auf deinen gesammelten Punkten. Du kannst dabei bis zu 3€ zusätzlich verdienen.",
                "Drücke LEERTASTE, um fortzufahren."
            ),
            (
                "Bevor das eigentliche Spiel beginnt, absolvierst du ein paar Übungsrunden, um dich mit den Regeln und Abläufen vertraut zu machen.\n\nDie Übungsrunden fließen NICHT in deine Bonuszahlung mit ein.\n\nDENKE DARAN: Du wählst die Optionen mit den Pfeiltasten (←/→) aus",
                "Drücke LEERTASTE, um die Übungsrunden zu starten."
            )
        ],
        'POST_PRACTICE': (
            "Gut gemacht, du hast die Übungsrunden erfolgreich abgeschlossen!\n\nDenke immer daran: Dein Ziel ist es, möglichst viele Punkte zu sammeln.\n\nBeobachte genau, wie sich deine Entscheidungen auszahlen, und passe deine Strategie entsprechend an.\n\nDas eigentliche Spiel beginnt jetzt.\nViel Erfolg!",
            "Drücke LEERTASTE, um das Spiel zu starten."
        )
    }
}

# ================================= HELPER CLASSES & FUNCTIONS =================================

class AudioManager:
    def __init__(self):
        pygame.mixer.init()
        pygame.mixer.set_num_channels(2)
        self.background_channel = pygame.mixer.Channel(0)
        self.background_music = pygame.mixer.Sound(CONFIG['PATHS']['SOUNDS']['BACKGROUND'])
        self.salient_channel = pygame.mixer.Channel(1)
        self.salient_sound = pygame.mixer.Sound(CONFIG['PATHS']['SOUNDS']['SALIENT_FEEDBACK'])

    def play_background(self):
        self.background_channel.play(self.background_music, loops=-1)
        self.set_background_volume(CONFIG['TASK_PARAMS']['VOLUME']['BACKGROUND'])

    def play_salient_sound(self):
        self.salient_channel.play(self.salient_sound)

    def set_background_volume(self, volume: float):
        self.background_music.set_volume(volume)

    def fade_background_volume(self, target_volume: float):
        current_volume = self.background_music.get_volume()
        steps = CONFIG['TASK_PARAMS']['VOLUME']['FADE_STEPS']
        duration = CONFIG['TASK_PARAMS']['VOLUME']['FADE_DURATION']
        delta = (target_volume - current_volume) / steps

        for _ in range(steps):
            current_volume += delta
            self.set_background_volume(max(0, min(current_volume, 1.0)))
            core.wait(duration / steps)

class TrialData:
    def __init__(self, participant_id: str):
        self.participant_id = participant_id
        self.data = []
        data_dir = CONFIG['PATHS']['DATA_DIR']
        os.makedirs(data_dir, exist_ok=True)
        self.file_path = os.path.join(data_dir, f"{self.participant_id}_data.csv")
        with open(self.file_path, 'w', newline='', encoding='utf-8') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=[
                'mode', 'trial', 'choice', 'reaction_time', 'reward',
                'condition', 'reward_prob_1', 'reward_prob_2', 'payoff_1', 'payoff_2'
            ])
            writer.writeheader()

    def add_trial(self, mode: str, trial_num: int, choice: int,
                  reaction_time: float, reward: int, condition: int,
                  reward_probs: Tuple, payoffs: Tuple):
        trial_entry = {
            'mode': mode,
            'trial': trial_num + 1,
            'choice': choice,
            'reaction_time': reaction_time,
            'reward': reward,
            'condition': condition,
            'reward_prob_1': reward_probs[0],
            'reward_prob_2': reward_probs[1],
            'payoff_1': payoffs[0],
            'payoff_2': payoffs[1]
        }
        self.data.append(trial_entry)
        with open(self.file_path, 'a', newline='', encoding='utf-8') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=[
                'mode', 'trial', 'choice', 'reaction_time', 'reward',
                'condition', 'reward_prob_1', 'reward_prob_2', 'payoff_1', 'payoff_2'
            ])
            writer.writerow(trial_entry)

def get_iti() -> float:
    iti = np.random.normal(loc=CONFIG['TASK_PARAMS']['ITI_MEAN'],
                           scale=CONFIG['TASK_PARAMS']['ITI_SD'])
    return max(iti, 0.5)

def calculate_bonus(total_wins: int, max_wins: int) -> float:
    if max_wins == 0:
        return 0.0
    raw_bonus = (total_wins / max_wins) * CONFIG['TASK_PARAMS']['BONUS_MAX_EUR']
    return round(raw_bonus / 0.2) * 0.2

def initialize_window() -> visual.Window:
    display = pyglet.canvas.Display()
    screen = display.get_default_screen()
    return visual.Window(
        size=(screen.width, screen.height),
        fullscr=False,
        screen=0,
        winType='pyglet',
        monitor='testMonitor',
        color='black',
        units='height',
        allowStencil=False,
        colorSpace='rgb',
        backgroundImage='',
        backgroundFit='none',
        blendMode='avg',
        useFBO=True,
        checkTiming=False
    )

def show_instructions(win: visual.Window, instructions: list):
    for main_text, space_text in instructions:
        text_stim = visual.TextStim(win, text=main_text, **CONFIG['TASK_PARAMS']['TEXT_PARAMS'])
        space_prompt = visual.TextStim(win, text=space_text,
                                       pos=(0, -0.3), color='yellow', height=0.05)
        text_stim.draw()
        space_prompt.draw()
        win.flip()
        event.waitKeys(keyList=['space'])

def initialize_stimuli(win: visual.Window, practice: bool = False) -> dict:
    if practice:
        n = ['PRAC_STIM1', 'PRAC_STIM2', 'PRAC_NON_SALIENT_1', 'PRAC_NON_SALIENT_2', 'PRAC_SALIENT_1', 'PRAC_SALIENT_2']
    else:
        n = ['STIM1', 'STIM2', 'NON_SALIENT_1', 'NON_SALIENT_2', 'SALIENT_1', 'SALIENT_2']
    return {
        'stim1': visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI'][n[0]], size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
        'stim2': visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI'][n[1]], size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
        'fixation': visual.TextStim(win, text='+', height=0.1, color='white'),
        'feedback_text': visual.TextStim(win, text='', pos=(0, -0.3), color='white', height=0.1),
        'feedback_non_salient': [
            visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['FEEDBACK'][n[2]], size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
            visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['FEEDBACK'][n[3]], size=CONFIG['TASK_PARAMS']['STIM_SIZE'])
        ],
        'feedback_salient': [
            visual.MovieStim(win, CONFIG['PATHS']['STIMULI']['FEEDBACK'][n[4]],
                             size=CONFIG['TASK_PARAMS']['STIM_SIZE'], movieLib='ffpyplayer',
                             loop=False, noAudio=True, units=win.units, ori=0.0,
                             anchor='center', opacity=None, contrast=1.0, depth=-2),
            visual.MovieStim(win, CONFIG['PATHS']['STIMULI']['FEEDBACK'][n[5]],
                             size=CONFIG['TASK_PARAMS']['STIM_SIZE'], movieLib='ffpyplayer',
                             loop=False, noAudio=True, units=win.units, ori=0.0,
                             anchor='center', opacity=None, contrast=1.0, depth=-2)
        ]
    }

# ================================= MAIN EXPERIMENT CLASS =================================

class BanditExperiment:
    def __init__(self, participant_id: str):
        self.win = initialize_window()
        self.audio = AudioManager()
        self.main_stimuli = initialize_stimuli(self.win, practice=False)
        self.practice_stimuli = initialize_stimuli(self.win, practice=True)
        self.stimuli = self.main_stimuli
        self.data = TrialData(participant_id)
        self.vr_schedule = pd.read_csv(CONFIG['PATHS']['VARIABLE_RATIO_SCHEDULE'])['schedule'].tolist()
        self.random_walk_data_main = pd.read_csv(CONFIG['PATHS']['RANDOM_WALK_DATA_MAIN'])
        self.random_walk_data_practice = pd.read_csv(CONFIG['PATHS']['RANDOM_WALK_DATA_PRACTICE'])
        self.schedule_index = -1
        self.trial_counter = 0
        self.total_wins = 0

    def run_instruction_phase(self):
        show_instructions(self.win, CONFIG['INSTRUCTIONS']['PRE_PRACTICE'])

    def run_practice_trials(self):
        self.stimuli = self.practice_stimuli
        for trial_num in range(CONFIG['TASK_PARAMS']['N_PRACTICE_TRIALS']):
            self.trial_counter += 1
            self._run_single_trial(trial_num, "practice")
        self.total_wins = 0

    def run_main_trials(self):
        self.stimuli = self.main_stimuli
        main_text, space_text = CONFIG['INSTRUCTIONS']['POST_PRACTICE']
        show_instructions(self.win, [(main_text, space_text)])
        for trial_num in range(CONFIG['TASK_PARAMS']['N_MAIN_TRIALS']):
            self.trial_counter += 1
            self._run_single_trial(trial_num, "main")

    def _run_single_trial(self, trial_num: int, mode: str):
        current_rw = self.random_walk_data_practice if mode == "practice" else self.random_walk_data_main
        reward_probs = (
            current_rw.loc[trial_num, 'mu_1'],
            current_rw.loc[trial_num, 'mu_2']
        )
        payoffs = (
            current_rw.loc[trial_num, 'payoff_1'],
            current_rw.loc[trial_num, 'payoff_2']
        )
        stim_positions = CONFIG['TASK_PARAMS']['STIM_POSITIONS']
        if np.random.rand() <= 0.5:
            stim_positions = list(reversed(stim_positions))
        self.stimuli['stim1'].pos = stim_positions[0]
        self.stimuli['stim2'].pos = stim_positions[1]
        self.stimuli['feedback_non_salient'][0].pos = stim_positions[0]
        self.stimuli['feedback_non_salient'][1].pos = stim_positions[1]
        self.stimuli['feedback_salient'][0].pos = stim_positions[0]
        self.stimuli['feedback_salient'][1].pos = stim_positions[1]
        stim_mapping = [self.stimuli['stim1'], self.stimuli['stim2']]
        choice, reaction_time, missed = self._get_response(stim_mapping)
        if missed:
            self._handle_missed_trial(mode, trial_num, reward_probs, payoffs)
            return
        win_this_trial, feedback_cond = self._process_outcome(choice, payoffs)
        self._show_feedback(choice, stim_mapping, feedback_cond, win_this_trial)
        self._log_trial(mode, trial_num, choice, reaction_time, win_this_trial, feedback_cond, reward_probs, payoffs)
        self._show_iti()

    def _get_response(self, stim_mapping: List) -> Tuple[int, float, bool]:
        for stim in stim_mapping:
            stim.draw()
        self.win.flip()
        clock = core.Clock()
        keys = event.waitKeys(maxWait=CONFIG['TASK_PARAMS']['MAX_RESPONSE_TIME'],
                              keyList=['left', 'right'], timeStamped=clock)
        if not keys:
            return None, None, True
        key, rt = keys[0]
        choice = 0 if ((key == 'left' and stim_mapping[0].pos[0] < 0) or
                       (key == 'right' and stim_mapping[0].pos[0] > 0)) else 1
        return choice, rt, False

    def _process_outcome(self, choice: int, payoffs: Tuple[int, int]) -> Tuple[bool, str]:
        win_this_trial = payoffs[choice] == 1
        feedback_cond = "non-salient"
        if choice is None:
            feedback_cond = "non-salient"
        elif win_this_trial:
            self.total_wins += 1
            self.schedule_index += 1
            feedback_cond = self._determine_feedback_condition()
        return win_this_trial, feedback_cond

    def _determine_feedback_condition(self) -> str:
        if CONFIG['TASK_PARAMS']['FORCED_SALIENCY']:
            if self.trial_counter >= 10 or self.vr_schedule[self.schedule_index] == 1:
                self.trial_counter = 0
                return "salient"
        else:
            if self.vr_schedule[self.schedule_index] == 1:
                return "salient"
        return "non-salient"

    def _show_feedback(self, choice: int, stim_mapping: List, feedback_cond: str, win: bool):
        feedback_text = '+100 Punkte' if win else '+0 Punkte'
        self.stimuli['feedback_text'].text = feedback_text
        if feedback_cond == "salient":
            self._show_salient_feedback(choice, stim_mapping)
        else:
            self._show_non_salient_feedback(choice, stim_mapping)

    def _show_salient_feedback(self, choice: int, stim_mapping: List):
        self.audio.set_background_volume(CONFIG['TASK_PARAMS']['VOLUME']['SALIENT'])
        self.audio.play_salient_sound()
        feedback_vid = self.stimuli['feedback_salient'][choice]
        feedback_vid.seek(0)
        feedback_vid.play()
        while not feedback_vid.isFinished:
            feedback_vid.draw()
            stim_mapping[1 - choice].draw()
            self.stimuli['feedback_text'].draw()
            self.win.flip()
        while self.audio.salient_channel.get_busy():
            core.wait(0.01)
        self.audio.fade_background_volume(CONFIG['TASK_PARAMS']['VOLUME']['BACKGROUND'])

    def _show_non_salient_feedback(self, choice: int, stim_mapping: List):
        feedback_img = self.stimuli['feedback_non_salient'][choice]
        feedback_img.draw()
        stim_mapping[1 - choice].draw()
        self.stimuli['feedback_text'].draw()
        self.win.flip()
        core.wait(CONFIG['TASK_PARAMS']['FEEDBACK_DURATION'])

    def _log_trial(self, mode: str, trial_num: int, choice: int, rt: float,
                   reward: bool, condition: str, reward_probs: Tuple, payoffs: Tuple):
        condition_code = {
            "non-salient": 0,
            "salient": 1,
            "missed": 2
        }.get(condition, 2)
        self.data.add_trial(mode=mode, trial_num=trial_num, choice=choice,
                            reaction_time=rt, reward=int(reward), condition=condition_code,
                            reward_probs=reward_probs, payoffs=payoffs)

    def _show_iti(self):
        self.stimuli['fixation'].draw()
        self.win.flip()
        core.wait(get_iti())

    def _handle_missed_trial(self, mode: str, trial_num: int, reward_probs: Tuple, payoffs: Tuple):
        self.stimuli['feedback_text'].text = "Zu spät"
        self.stimuli['feedback_text'].draw()
        self.win.flip()
        core.wait(2)
        self._log_trial(mode, trial_num, None, None, False, "missed", reward_probs, payoffs)
        self._show_iti()

    def show_final_screen(self):
        max_wins = np.sum(np.max(self.random_walk_data_main[['payoff_1', 'payoff_2']], axis=1))
        bonus = calculate_bonus(self.total_wins, max_wins)
        message = (
            f"Vielen Dank für Ihre Teilnahme!\n\n"
            f"Sie haben insgesamt {self.total_wins} Gewinne erzielt.\n"
            f"Ihr Bonus beträgt: {bonus:.2f} Euro."
        )
        text = visual.TextStim(self.win, text=message, color="white", height=0.05, wrapWidth=1.5)
        text.draw()
        self.win.flip()
        core.wait(10)
        self.win.close()
        return bonus

# ================================= EXPERIMENT EXECUTION =================================

def run_task(participant_id: str) -> float:
    """
    Run the bandit task for the specified participant.
    
    This function initializes the BanditExperiment, starts background audio,
    executes the instruction phase, practice trials, main trials, and displays the final screen.
    
    Args:
        participant_id (str): Unique identifier for the participant.
    
    Returns:
        float: The bonus amount earned by the participant.
    """
    experiment = BanditExperiment(participant_id)
    experiment.audio.play_background()
    try:
        experiment.run_instruction_phase()
        experiment.run_practice_trials()
        experiment.run_main_trials()
        bonus = experiment.show_final_screen()
    except Exception as e:
        print(f"Experiment failed: {str(e)}")
        core.quit()
    finally:
        experiment.win.close()
        pygame.mixer.quit()
    return bonus

# If run as a standalone script, use command-line arguments or prompt for input.
def main():
    """Handle command-line execution."""
    if len(sys.argv) > 1:
        participant_id = sys.argv[1]
    else:
        participant_id = input("Enter participant ID: ").strip()
        if not participant_id:
            print("No participant ID provided. Exiting.")
            core.quit()
    bonus = run_task(participant_id)
    print(bonus)

if __name__ == "__main__":
    main()
