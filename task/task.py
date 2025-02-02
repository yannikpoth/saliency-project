from psychopy import visual, core, event, gui, data
import numpy as np
import os
import pandas as pd
from vr_schedule import create_vr_schedule
import pyglet
import pygame
from typing import Tuple, Dict, List


# ================================= CONFIGURATION =================================
CONFIG = {
    'TASK_PARAMS': {
        'N_PRACTICE_TRIALS': 15,
        'N_MAIN_TRIALS': 200,
        'ITI_MEAN': 1.0,
        'ITI_SD': 0.5,
        'MAX_RESPONSE_TIME': 5,
        'FEEDBACK_DURATION': 3,
        'BONUS_MAX_EUR': 3.0,
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
            'STIM1': 'media/stimuli/stim1.png',
            'STIM2': 'media/stimuli/stim2.png',
            'FEEDBACK': {
                'NON_SALIENT_1': 'media/stimuli/stim1_feedback_non_salient.png',
                'NON_SALIENT_2': 'media/stimuli/stim2_feedback_non_salient.png',
                'SALIENT_1': 'media/stimuli/stim1_feedback_salient.mov',
                'SALIENT_2': 'media/stimuli/stim2_feedback_salient.mov'
            }
        },
        'SOUNDS': {
            'BACKGROUND': 'media/sounds/ambience.mp3',
            'SALIENT_FEEDBACK': 'media/sounds/salient_feedback.wav'
        },
        'DATA_DIR': 'data',
        'RANDOM_WALK_DATA': 'random_walk_data.csv'
    },
    'INSTRUCTIONS': {
        'PRE_PRACTICE': [
            ("Willkommen!\n\nDu bist jetzt Teil eines spannenden Experiments. Deine Aufgabe ist es, Entscheidungen zu treffen, um möglichst viele Punkte zu sammeln.\n\nIn jeder Runde hast du zwei Optionen zur Auswahl. Du wählst eine davon aus und siehst sofort, ob du Punkte gewonnen hast oder nicht.",
            "Drücke LEERTASTE, um fortzufahren."),
            
            ("Die Wahrscheinlichkeit, mit einer Option Punkte zu gewinnen, verändert sich im Laufe des Spiels. Deine Aufgabe ist es herauszufinden, welche Option momentan die beste ist.\n\nAchte darauf: Die Symbole erscheinen manchmal links, manchmal rechts – das ist zufällig und spielt keine Rolle. Entscheidend ist nur, welches Symbol du auswählst.",
            "Drücke LEERTASTE, um fortzufahren."),
            
            ("Manchmal wirst du zusätzlich zum Gewinn Rückmeldung mit besonderen Effekten erhalten.\nDenke immer daran: Dein Ziel ist es, möglichst viele Punkte zu sammeln.",
            "Drücke LEERTASTE, um fortzufahren."),
            
            ("Am Ende des Spiels erhältst du eine Bonuszahlung basierend auf deinen gesammelten Punkten. Du kannst dabei bis zu 3€ zusätzlich verdienen.",
            "Drücke LEERTASTE, um fortzufahren."),
            
            ("Bevor das eigentliche Spiel beginnt, absolvierst du ein paar Übungsrunden, um dich mit den Regeln und Abläufen vertraut zu machen.\n\n Die Übungsrunden fließen NICHT in deine Bonuszahlung mit ein.",
            "Drücke LEERTASTE, um die Übungsrunden zu starten.")
        ],
        'POST_PRACTICE': (
            "Gut gemacht, du hast die Übungsrunden erfolgreich abgeschlossen!\n\nDenke immer daran: Dein Ziel ist es, möglichst viele Punkte zu sammeln.\n\nBeobachte genau, wie sich deine Entscheidungen auszahlen, und passe deine Strategie entsprechend an.\n\nDas eigentliche Spiel beginnt jetzt.\nViel Erfolg!",
            "Drücke LEERTASTE, um das Spiel zu starten."
        )
    }
}

# ================================= HELPER CLASSES & FUNCTIONS =================================
class AudioManager:
    """Manage audio playback and volume adjustments"""
    def __init__(self):
        pygame.mixer.init()
        pygame.mixer.set_num_channels(2)  # Create at least 2 channels
        
        # Channel 0: Background music
        self.background_channel = pygame.mixer.Channel(0)
        self.background_music = pygame.mixer.Sound(CONFIG['PATHS']['SOUNDS']['BACKGROUND'])
        
        # Channel 1: Salient sounds
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
        """Gradually adjust background music volume"""
        current_volume = self.background_music.get_volume()
        steps = CONFIG['TASK_PARAMS']['VOLUME']['FADE_STEPS']
        duration = CONFIG['TASK_PARAMS']['VOLUME']['FADE_DURATION']
        delta = (target_volume - current_volume) / steps
        
        for _ in range(steps):
            current_volume += delta
            self.set_background_volume(max(0, min(current_volume, 1.0)))
            core.wait(duration / steps)

class TrialData:
    """Handle data logging for trials"""
    def __init__(self, participant_id: str):
        self.participant_id = participant_id
        self.data = []
        
    def add_trial(
        self,
        mode: str,
        trial_num: int,
        choice: int,
        reaction_time: float,
        reward: int,
        condition: int,
        reward_probs: Tuple[float, float],
        payoffs: Tuple[int, int]
    ):
        self.data.append({
            'mode': mode,
            'trial': trial_num + 1,  # Convert to 1-based indexing
            'choice': choice,
            'reaction_time': reaction_time,
            'reward': reward,
            'condition': condition,
            'reward_prob_1': reward_probs[0],
            'reward_prob_2': reward_probs[1],
            'payoff_1': payoffs[0],
            'payoff_2': payoffs[1]
        })
        
    def save(self):
        """Save collected data to CSV file"""
        data_dir = CONFIG['PATHS']['DATA_DIR']
        os.makedirs(data_dir, exist_ok=True)
        path = os.path.join(data_dir, f"{self.participant_id}_data.csv")
        pd.DataFrame(self.data).to_csv(path, index=False)

def get_iti() -> float:
    """Calculate inter-trial interval with safety bounds"""
    iti = np.random.normal(
        loc=CONFIG['TASK_PARAMS']['ITI_MEAN'],
        scale=CONFIG['TASK_PARAMS']['ITI_SD']
    )
    return max(iti, 0.5)

def calculate_bonus(total_wins: int, max_wins: int) -> float:
    """Calculate participant bonus based on performance"""
    if max_wins == 0:
        return 0.0
        
    raw_bonus = (total_wins / max_wins) * CONFIG['TASK_PARAMS']['BONUS_MAX_EUR']
    return round(raw_bonus / 0.2) * 0.2

def initialize_window() -> visual.Window:
    """Initialize and return the Psychopy window"""
    display = pyglet.canvas.Display()
    screen = display.get_default_screen()
    return visual.Window(
        size=(screen.width, screen.height),
        fullscr=False,
        screen=1,
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
    """Display instructions with spacebar continuation"""
    for main_text, space_text in instructions:
        text_stim = visual.TextStim(win, text=main_text, **CONFIG['TASK_PARAMS']['TEXT_PARAMS'])
        space_prompt = visual.TextStim(
            win, text=space_text, 
            pos=(0, -0.3), color='yellow', height=0.05
        )
        text_stim.draw()
        space_prompt.draw()
        win.flip()
        event.waitKeys(keyList=['space'])

def initialize_stimuli(win: visual.Window) -> dict:
    """Initialize and return all experiment stimuli"""
    return {
        'stim1': visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['STIM1'], 
                                size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
        'stim2': visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['STIM2'], 
                                size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
        'fixation': visual.TextStim(win, text='+', height=0.1, color='white'),
        'feedback_text': visual.TextStim(win, text='', pos=(0, -0.3), color='white', height=0.1),
        'feedback_non_salient': [
            visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['FEEDBACK']['NON_SALIENT_1'],
                           size=CONFIG['TASK_PARAMS']['STIM_SIZE']),
            visual.ImageStim(win, image=CONFIG['PATHS']['STIMULI']['FEEDBACK']['NON_SALIENT_2'],
                           size=CONFIG['TASK_PARAMS']['STIM_SIZE'])
        ],
        'feedback_salient': [
            visual.MovieStim(
                win, CONFIG['PATHS']['STIMULI']['FEEDBACK']['SALIENT_1'],
                size=CONFIG['TASK_PARAMS']['STIM_SIZE'], movieLib='ffpyplayer',
                loop=False, noAudio=True, units=win.units, ori=0.0, 
                anchor='center', opacity=None, contrast=1.0, depth=-2
            ),
            visual.MovieStim(
                win, CONFIG['PATHS']['STIMULI']['FEEDBACK']['SALIENT_2'],
                size=CONFIG['TASK_PARAMS']['STIM_SIZE'], movieLib='ffpyplayer',
                loop=False, noAudio=True, units=win.units, ori=0.0, 
                anchor='center', opacity=None, contrast=1.0, depth=-2
            )
        ]
    }

# ================================= MAIN EXPERIMENT CLASS =================================
class BanditExperiment:
    def __init__(self, participant_id: str):
        self.win = initialize_window()
        self.audio = AudioManager()
        self.stimuli = initialize_stimuli(self.win)
        self.data = TrialData(participant_id)
        self.vr_schedule = create_vr_schedule()
        self.random_walk_data = pd.read_csv(CONFIG['PATHS']['RANDOM_WALK_DATA'])
        
        # State variables
        self.schedule_index = -1
        self.trial_counter = 0
        self.total_wins = 0
        
    def run_instruction_phase(self):
        """Run all instruction phases"""
        show_instructions(self.win, CONFIG['INSTRUCTIONS']['PRE_PRACTICE'])
        
    def run_practice_trials(self):
        """Run practice trials if configured"""
        for trial_num in range(CONFIG['TASK_PARAMS']['N_PRACTICE_TRIALS']):
            self._run_single_trial(trial_num, "practice")
            self.total_wins = 0  # Reset wins after practice
            
    def run_main_trials(self):
        """Run main experimental trials"""
        # Show post-practice instructions
        main_text, space_text = CONFIG['INSTRUCTIONS']['POST_PRACTICE']
        show_instructions(self.win, [(main_text, space_text)])
        
        # Run main trials
        for trial_num in range(CONFIG['TASK_PARAMS']['N_MAIN_TRIALS']):
            self._run_single_trial(trial_num, "main")
            
    def _run_single_trial(self, trial_num: int, mode: str):
        """Run a single trial with specified mode"""
        # Get current trial parameters
        reward_probs = (
            self.random_walk_data.loc[trial_num, 'mu_1'],
            self.random_walk_data.loc[trial_num, 'mu_2']
        )
        payoffs = (
            self.random_walk_data.loc[trial_num, 'payoff_1'],
            self.random_walk_data.loc[trial_num, 'payoff_2']
        )
        
        # Randomize stimulus positions
        if np.random.rand() > 0.5:
            stim_positions = CONFIG['TASK_PARAMS']['STIM_POSITIONS']
        else:
            stim_positions = list(reversed(CONFIG['TASK_PARAMS']['STIM_POSITIONS']))
            
        # Set positions and determine mapping
        self.stimuli['stim1'].pos = stim_positions[0]
        self.stimuli['stim2'].pos = stim_positions[1]
        self.stimuli['feedback_non_salient'][0].pos = stim_positions[0]
        self.stimuli['feedback_non_salient'][1].pos = stim_positions[1]
        self.stimuli['feedback_salient'][0].pos = stim_positions[0]
        self.stimuli['feedback_salient'][1].pos = stim_positions[1]
        stim_mapping = [self.stimuli['stim1'], self.stimuli['stim2']]
        
        # Show stimuli and get response
        choice, reaction_time, missed = self._get_response(stim_mapping)
        
        if missed:
            self._handle_missed_trial(mode, trial_num, reward_probs, payoffs)
            return
            
        # Process trial outcome
        win_this_trial, feedback_cond = self._process_outcome(
            choice, stim_mapping, payoffs
        )
        
        # Show feedback
        self._show_feedback(choice, stim_mapping, feedback_cond, win_this_trial)
        
        # Log trial data
        self._log_trial(
            mode, trial_num, choice, reaction_time,
            win_this_trial, feedback_cond, reward_probs, payoffs
        )
        
        # Show ITI
        self._show_iti()
        
    def _get_response(self, stim_mapping: List) -> Tuple[int, float, bool]:
        """Present stimuli and collect response"""
        for stim in stim_mapping:
            stim.draw()
        self.win.flip()
        
        clock = core.Clock()
        keys = event.waitKeys(
            maxWait=CONFIG['TASK_PARAMS']['MAX_RESPONSE_TIME'],
            keyList=['left', 'right'],
            timeStamped=clock
        )
        
        if not keys:
            return None, None, True
            
        key, rt = keys[0]
        choice = 0 if ((key == 'left' and stim_mapping[0].pos[0] < 0) or (key == 'right' and stim_mapping[0].pos[0] > 0)) else 1
        return choice, rt, False
        
    def _process_outcome(self, choice: int, stim_mapping: List, payoffs: Tuple[int, int]) -> Tuple[bool, str]:
        """Determine trial outcome and feedback condition"""
        win_this_trial = payoffs[choice] == 1
        feedback_cond = "non-salient"

        if choice == None:
            feedback_cond = "non-salient"
        elif win_this_trial:
            self.total_wins += 1
            self.schedule_index += 1
            feedback_cond = self._determine_feedback_condition()
            
        return win_this_trial, feedback_cond
        
    def _determine_feedback_condition(self) -> str:
        """Determine if feedback should be salient based on VR schedule"""
        if self.trial_counter >= 10 or self.vr_schedule[self.schedule_index] == 1:
            self.trial_counter = 0
            return "salient"
        self.trial_counter += 1
        return "non-salient"
        
    def _show_feedback(self, choice: int, stim_mapping: List, feedback_cond: str, win: bool):
        """Show appropriate feedback based on condition"""
        feedback_text = '+100 Punkte' if win else '+0 Punkte'
        self.stimuli['feedback_text'].text = feedback_text
        
        if feedback_cond == "salient":
            self._show_salient_feedback(choice, stim_mapping)
        else:
            self._show_non_salient_feedback(choice, stim_mapping)
            
    def _show_salient_feedback(self, choice: int, stim_mapping: List):
        """Handle salient feedback presentation"""
        self.audio.fade_background_volume(CONFIG['TASK_PARAMS']['VOLUME']['SALIENT'])
        self.audio.play_salient_sound()
        
        feedback_vid = self.stimuli['feedback_salient'][choice]
        feedback_vid.seek(0)
        feedback_vid.play()
        
        # Draw while video is playing
        while not feedback_vid.isFinished:
            feedback_vid.draw()
            stim_mapping[1 - choice].draw()
            self.stimuli['feedback_text'].draw()
            self.win.flip()
            
        # Wait for sound to finish
        while self.audio.salient_channel.get_busy():
            core.wait(0.01)
            
        self.audio.fade_background_volume(CONFIG['TASK_PARAMS']['VOLUME']['BACKGROUND'])
        
    def _show_non_salient_feedback(self, choice: int, stim_mapping: List):
        """Handle non-salient feedback presentation"""
        feedback_img = self.stimuli['feedback_non_salient'][choice]
        feedback_img.draw()
        stim_mapping[1 - choice].draw()  # Show other stimulus
        self.stimuli['feedback_text'].draw()
        self.win.flip()
        core.wait(CONFIG['TASK_PARAMS']['FEEDBACK_DURATION'])
        
    def _log_trial(self, mode: str, trial_num: int, choice: int, rt: float,
                 reward: bool, condition: str, reward_probs: Tuple, payoffs: Tuple):
        """Log trial data"""
        condition_code = {
            "non-salient": 0,
            "salient": 1,
            "missed": 2
        }.get(condition, 2)
        
        self.data.add_trial(
            mode=mode,
            trial_num=trial_num,
            choice=choice,
            reaction_time=rt,
            reward=int(reward),
            condition=condition_code,
            reward_probs=reward_probs,
            payoffs=payoffs
        )
        
    def _show_iti(self):
        """Show inter-trial interval fixation"""
        self.stimuli['fixation'].draw()
        self.win.flip()
        core.wait(get_iti())
        
    def _handle_missed_trial(self, mode: str, trial_num: int, reward_probs: Tuple, payoffs: Tuple):
        """Handle missed trial response"""
        self.stimuli['feedback_text'].text = "Zu spät"
        self.stimuli['feedback_text'].draw()
        self.win.flip()
        core.wait(2)
        self._log_trial(
            mode, trial_num, None, None,
            False, "missed", reward_probs, payoffs
        )
        self._show_iti()
        
    def show_final_screen(self):
        """Display final screen with bonus information"""
        max_wins = np.sum(np.max(self.random_walk_data[['payoff_1', 'payoff_2']], axis=1))
        bonus = calculate_bonus(self.total_wins, max_wins)
        
        message = (
            f"Vielen Dank für Ihre Teilnahme!\n\n"
            f"Sie haben insgesamt {self.total_wins} Gewinne erzielt.\n"
            f"Ihr Bonus beträgt: {bonus} Euro."
        )
        
        text = visual.TextStim(
            self.win, text=message,
            color="white", height=0.05,
            wrapWidth=1.5
        )
        text.draw()
        self.win.flip()
        core.wait(10)
        self.win.close()

# ================================= EXPERIMENT EXECUTION =================================
def main():
    import sys

    if len(sys.argv) > 1:
        participant_id = sys.argv[1]
    else:
        # Fallback to terminal input
        participant_id = input("Enter participant ID: ").strip()
        if not participant_id:
            print("No participant ID provided. Exiting.")
            core.quit()
        
    # Initialize experiment
    experiment = BanditExperiment(participant_id)
    experiment.audio.play_background()
    
    try:
        # Run experiment phases
        experiment.run_instruction_phase()
        experiment.run_practice_trials()
        experiment.run_main_trials()
        
        # Save data and show final screen
        experiment.data.save()
        experiment.show_final_screen()
        
    except Exception as e:
        print(f"Experiment failed: {str(e)}")
        core.quit()
        
    finally:
        experiment.win.close()
        pygame.mixer.quit()

if __name__ == "__main__":
    main()