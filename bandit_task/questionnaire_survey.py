"""
questionnaire_survey.py

This module implements a Tkinter-based GUI application for administering 
two questionnaires (BIS-15 and SSS) plus an open-ended questionnaire as part 
of the Saliency Project study. It collects participant responses, validates them, 
and saves the results to a CSV file in the collected_data folder.

Usage:
    As an importable module:
        from questionnaire_survey import run_questionnaire
        run_questionnaire("PARTICIPANT_ID", bonus_value)
    
    Standalone:
        python questionnaire_survey.py <participant_id> <bonus>
    If no participant_id is provided, the GUI will prompt for one.
"""

import tkinter as tk
from tkinter import ttk, messagebox, font
import csv
import sys
import os
from pathlib import Path

class QuestionnaireApp:
    """
    A class representing the questionnaire application.
    
    This class creates a full-screen Tkinter window, displays instructions,
    and presents the BIS-15, SSS, and an open-ended questionnaire. Participant 
    responses are collected, validated, and saved to disk in a single CSV file.
    """
    def __init__(self, participant_id=None, bonus=None):
        # Initialize main Tkinter window.
        self.root = tk.Tk()
        self.root.title("Questionnaires")
        self.root.state('zoomed')
        
        # Set reference dimensions and font size for scaling.
        self.base_width = 1920
        self.base_height = 1080
        self.base_font_size = 18
        
        # Create dynamic and bold fonts.
        self.dynamic_font = font.Font(family="Arial", size=self.base_font_size)
        self.bold_font = font.Font(family="Arial", size=self.base_font_size, weight="bold")
        
        # Set up custom widget styles.
        self.style = ttk.Style()
        self.style.configure("Custom.TLabel", font=self.dynamic_font)
        self.style.configure("CustomBold.TLabel", font=self.bold_font)
        self.style.configure("Custom.TRadiobutton", font=self.dynamic_font)
        self.style.configure("TButton", font=self.dynamic_font)
        
        # Bind the window resize event to adjust fonts dynamically.
        self.root.bind("<Configure>", self.on_resize)

        # Initialize variables for GUI frames and response variables.
        self.current_frame = None
        self.bis_vars = []  # List of IntVar for BIS responses.
        self.sss_vars = []  # List of StringVar for SSS responses.
        self.last_question_data = {}  # Dictionary for last questionnaire responses.
        
        # Accept the participant ID as an argument.
        self.participant_id = participant_id
        self.bonus = bonus

        # Define the BIS-15 items with reverse-scoring information.
        self.bis_items = [
            {"text": "1. Ich plane meine Vorhaben gründlich.", "reverse": True},
            {"text": "2. Ich mache häufig Dinge ohne vorher darüber nachzudenken.", "reverse": False},
            {"text": "3. Ich bin unaufmerksam.", "reverse": False},
            {"text": "4. Ich kann mich gut konzentrieren.", "reverse": True},
            {"text": "5. Ich sichere mich im Leben in allen Dingen ab.", "reverse": True},
            {"text": "6. Ich rutsche bei Spielen oder Vorträgen oft hin und her.", "reverse": False},
            {"text": "7. Ich denke gründlich nach.", "reverse": True},
            {"text": "8. Ich plane für meine berufliche Sicherheit.", "reverse": True},
            {"text": "9. Ich sage Dinge ohne darüber nachzudenken.", "reverse": False},
            {"text": "10. Ich handele spontan.", "reverse": False},
            {"text": "11. Mir wird beim Lösen von Denkaufgaben schnell langweilig.", "reverse": False},
            {"text": "12. Ich handele gerne aus dem Moment heraus.", "reverse": False},
            {"text": "13. Ich kaufe Sachen ganz spontan.", "reverse": False},
            {"text": "14. Ich werde bei Vorlesungen oder Vorträgen schnell unruhig.", "reverse": False},
            {"text": "15. Ich plane für die Zukunft.", "reverse": True},
        ]

        # Define the SSS items with answer options and subscale information.
        self.sss_items = [
            {"question": "Frage", "a": "Ich liebe ausgelassene, „wilde“ Partys.", "b": "Ich bevorzuge ruhige Partys mit guten Gesprächen.", "subscale": "SSD", "correct": "a"},
            {"question": "Frage", "a": "Mir macht es nichts aus, wenn ich bei Filmen oder Schauspielen weiß, was als nächstes passiert.", "b": "Ich kann mich normalerweise nicht an Filmen oder Schauspielen erfreuen, bei denen ich genau weiß, was als nächstes passiert.", "subscale": "SSB", "correct": "b"},
            {"question": "Frage", "a": "Manchmal liebe ich es, Dinge zu tun, die einem ein wenig Angst einflößen.", "b": "Eine vernünftige Person vermeidet Aktivitäten, die gefährlich sind.", "subscale": "SST", "correct": "a"},
            {"question": "Frage", "a": "Ich liebe es, mich häufig durch Alkohol oder Rauchen in eine gute Stimmung zu versetzen.", "b": "Ich finde, dass mir künstliche Anregungsmittel wie Alkohol oder Rauchen nicht bekommen.", "subscale": "SSD", "correct": "a"},
            {"question": "Frage", "a": "Wenn ich eine Reise unternehme, dann lege ich vorher meine Reiseroute und Zeitplanung sorgfältig fest.", "b": "Ich würde gerne eine Reise machen, ohne vorher die Route oder den zeitlichen Ablauf zu planen.", "subscale": "SSE", "correct": "b"},
            {"question": "Frage", "a": "Ich bevorzuge „normale“ Personen aus meinem Umfeld als Freunde.", "b": "Ich würde gerne Freunde in Außenseitergruppen wie „Punks“ oder „Spirituelle“ kennen lernen.", "subscale": "SSE", "correct": "b"},
            {"question": "Frage", "a": "Ich würde gerne einmal einen Fallschirmabsprung versuchen.", "b": "Ich würde niemals einen Fallschirmabsprung aus einem Flugzeug wagen.", "subscale": "SST", "correct": "a"},
            {"question": "Frage", "a": "Ich finde etwas Interessantes an fast jeder Person, mit der ich rede.", "b": "Ich habe keine Geduld mit trägen oder langweiligen Personen.", "subscale": "SSB", "correct": "b"},
        ]
        
        # Define the last (open-ended) questionnaire.
        # The questions are later saved horizontally with the following column names:
        # 1: q-open_goal_of_study
        # 2: q-open_noticable_aspects
        # 3: q-choice_noticed_saliency
        # 4: q-choice_saliency_strength
        # 5: q-choice_saliency_impact
        # 6: q-open_saliency_impact
        # 7: q-choice_saliency_value
        # 8: q-choice_win_motivation
        # 9: q-open_comments
        self.last_questions = [
            {"number": 1, "text": "Was glaubst du, war das Ziel dieser Studie?", "type": "open"},
            {"number": 2, "text": "Welche Aspekte der Aufgabe fandest du besonders auffällig?", "type": "open"},
            {"number": 3, "text": "Hast du während der Aufgabe bemerkt, dass einige Gewinnmeldungen mit speziellen audiovisuellen Feedbackreizen verbunden waren?", "type": "radio", "options": ["Ja", "Nein"]},
            {"number": 4, "text": "Falls ja, wie hast du die speziellen audiovisuellen Feedbackreize wahrgenommen?", "type": "radio", "options": ["Sehr auffällig", "Moderat auffällig", "Kaum auffällig", "Gar nicht auffällig"]},
            {"number": 5, "text": "Glaubst du, dass die speziellen audiovisuellen Feedbackreize dein Entscheidungsverhalten beeinflusst haben?", "type": "radio", "options": ["Ja", "Nein", "Unsicher"]},
            {"number": 6, "text": "Falls ja, in welcher Weise haben die speziellen audiovisuellen Feedbackreize dein Entscheidungsverhalten beeinflusst?", "type": "open"},
            {"number": 7, "text": "Hast du das Gefühl, dass du aufgrund der speziellen Feedbackreize anders eingeschätzt hast, welche Auswahloption besser ist?", "type": "radio", "options": ["Ja", "Nein", "Unsicher"]},
            {"number": 8, "text": "Wie motiviert warst du, während der Aufgabe den höchstmöglichen Gesamtgewinn zu erzielen?", "type": "radio", "options": ["Sehr motiviert", "Moderat motiviert", "Wenig motiviert", "Gar nicht motiviert"]},
            {"number": 9, "text": "Hast du noch weitere Anmerkungen oder Feedback zur Studie?", "type": "open"}
        ]
        
        # If a participant ID was provided, skip the prompt.
        if self.participant_id:
            self.show_instructions_general()
        else:
            self.show_participant_id()

    def on_resize(self, event):
        """
        Adjust font sizes dynamically when the window is resized.
        """
        current_width = self.root.winfo_width()
        current_height = self.root.winfo_height()
        scale_w = current_width / self.base_width
        scale_h = current_height / self.base_height
        scale = min(scale_w, scale_h)
        new_font_size = max(8, int(self.base_font_size * scale))
        self.dynamic_font.configure(size=new_font_size)
        self.bold_font.configure(size=new_font_size)
        self.style.configure("Custom.TLabel", font=self.dynamic_font)
        self.style.configure("CustomBold.TLabel", font=self.bold_font)
        self.style.configure("Custom.TRadiobutton", font=self.dynamic_font)
        self.style.configure("TButton", font=self.dynamic_font)

    def show_participant_id(self):
        """
        Show a frame prompting for the participant's ID.
        """
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(pady=50, expand=True)
        ttk.Label(self.current_frame, text="Bitte gib deine Teilnehmer-ID ein:", style="Custom.TLabel").pack()
        self.id_entry = ttk.Entry(self.current_frame, font=self.dynamic_font)
        self.id_entry.pack(pady=10)
        ttk.Button(self.current_frame, text="Start", command=self.start_questionnaires, style="TButton").pack()

    def start_questionnaires(self):
        """
        Retrieve the participant ID and proceed to the general instructions.
        """
        self.participant_id = self.id_entry.get().strip()
        if not self.participant_id:
            messagebox.showerror("Fehler", "Bitte gib eine gültige Teilnehmer-ID ein.")
            return
        self.current_frame.destroy()
        self.show_instructions_general()

    def show_instructions_general(self):
        """
        Display the general instructions for the experiment.
        """
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        text = (
            "Sehr gut, du hast die Aufgabe erfolgreich abgeschlossen.\n\n"
            "Nun bitten wir dich, noch drei kurze Fragebögen auszufüllen. Das Ausfüllen dauert ungefähr 5 bis 10 Minuten. "
            "Es gibt keine ‚richtigen‘ oder ‚falschen‘ Antworten, wie es in anderen Tests der Fall ist.\n\n"
            "Bitte beantworte die Fragen ehrlich und nach bestem Wissen. Deine Antworten werden anonym und vertraulich behandelt."
        )
        ttk.Label(self.current_frame, text=text, style="Custom.TLabel", wraplength=800, justify="center").pack(pady=20)
        ttk.Button(self.current_frame, text="Weiter", command=self.show_instructions_bis, style="TButton").pack(pady=10)

    def show_instructions_bis(self):
        """
        Display instructions for the BIS-15 questionnaire.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        text = (
            "Anleitung zum BIS-15 Fragebogen\n\n"
            "Bitte lies jede Aussage aufmerksam durch und wähle die Zahl, die am besten beschreibt, wie häufig diese Aussage auf dich zutrifft.\n"
            "Versuche, jede Frage so ehrlich und spontan wie möglich zu beantworten.\n\n"
            "Bewertungsskala:\n"
            "1 = Selten / Nie\n"
            "2 = Gelegentlich\n"
            "3 = Oft\n"
            "4 = Fast immer / Immer\n\n"
            "Es gibt keine „richtigen“ oder „falschen“ Antworten. Wähle einfach die Antwort, die am besten zu dir passt."
        )
        ttk.Label(self.current_frame, text=text, style="Custom.TLabel", wraplength=800, justify="center").pack(pady=20)
        ttk.Button(self.current_frame, text="Weiter", command=self.show_bis, style="TButton").pack(pady=10)

    def show_bis(self):
        """
        Display the BIS questionnaire.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        container = ttk.Frame(self.current_frame)
        container.pack(fill="both", expand=True)
        headers = ["Frage", "1 - selten/nie", "2 - gelegentlich", "3 - oft", "4 - fast immer/immer"]
        for col, header in enumerate(headers):
            ttk.Label(container, text=header, style="CustomBold.TLabel", anchor="center").grid(row=0, column=col, padx=10, pady=5, sticky="nsew")
            container.columnconfigure(col, weight=1)
        self.bis_vars = [tk.IntVar() for _ in self.bis_items]
        for row_idx, item in enumerate(self.bis_items, start=1):
            ttk.Label(container, text=item["text"], wraplength=400, style="Custom.TLabel", anchor="w").grid(row=row_idx, column=0, sticky="w", padx=10, pady=5)
            for col_idx in range(1, 5):
                rb_frame = ttk.Frame(container)
                rb_frame.grid(row=row_idx, column=col_idx, padx=10, pady=5, sticky="nsew")
                ttk.Radiobutton(rb_frame, variable=self.bis_vars[row_idx-1], value=col_idx, style="Custom.TRadiobutton").place(relx=0.5, rely=0.5, anchor="center")
        btn_frame = ttk.Frame(self.current_frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, text="Weiter zum nächsten Fragebogen", command=self.validate_bis, style="TButton").pack()

    def validate_bis(self):
        """
        Validate that all BIS questions have been answered.
        """
        if any(var.get() == 0 for var in self.bis_vars):
            messagebox.showerror("Fehler", "Bitte beantworte alle Fragen im BIS-Fragebogen.")
            return
        self.current_frame.destroy()
        self.show_instructions_sss()

    def show_instructions_sss(self):
        """
        Display instructions for the SSS questionnaire.
        """
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        text = (
            "Sehr gut!\nNun folgt der zweite Fragebogen\n\n"
            "Anleitung zum Fragebogen\n\n"
            "Jede der folgenden Aussagen enthält zwei Antwortmöglichkeiten, A und B.\n"
            "Bitte wähle die Option, die am besten beschreibt, was du bevorzugst oder wie du dich fühlst.\n\n"
            "In manchen Fällen können beide Optionen teilweise auf dich zutreffen. Wähle dann bitte die Antwort, die deine Präferenz besser widerspiegelt.\n"
            "Falls du mit keiner der beiden Antworten übereinstimmst, entscheide dich für die, die dir am ehesten zusagt.\n\n"
            "Wichtige Hinweise:\n"
            "- Lass keine Frage unbeantwortet.\n"
            "- Wähle immer nur eine der beiden Antworten (A oder B).\n"
            "- Es geht um deine persönlichen Vorlieben und Gefühle – nicht darum, wie andere darüber denken oder was allgemein als „richtig“ gilt.\n\n"
            "Es gibt keine „richtigen“ oder „falschen“ Antworten. Bitte sei ehrlich und beantworte die Fragen möglichst spontan."
        )
        ttk.Label(self.current_frame, text=text, style="Custom.TLabel", wraplength=800, justify="center").pack(pady=20)
        ttk.Button(self.current_frame, text="Weiter", command=self.show_sss, style="TButton").pack(pady=10)

    def show_sss(self):
        """
        Display the SSS questionnaire.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        canvas_container = ttk.Frame(self.current_frame)
        canvas_container.pack(side="top", fill="both", expand=True)
        canvas = tk.Canvas(canvas_container)
        v_scrollbar = ttk.Scrollbar(canvas_container, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=v_scrollbar.set)
        canvas.pack(side="left", fill="both", expand=True)
        v_scrollbar.pack(side="right", fill="y")
        scrollable_frame = ttk.Frame(canvas)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        def on_frame_configure(event):
            canvas.configure(scrollregion=canvas.bbox("all"))
        scrollable_frame.bind("<Configure>", on_frame_configure)
        def _on_mousewheel(event):
            canvas.yview_scroll(-1 * int(event.delta/120), "units")
        canvas.bind("<Enter>", lambda event: canvas.bind_all("<MouseWheel>", _on_mousewheel))
        canvas.bind("<Leave>", lambda event: canvas.unbind_all("<MouseWheel>"))
        self.sss_vars = [tk.StringVar() for _ in self.sss_items]
        for i, item in enumerate(self.sss_items):
            question_frame = ttk.Frame(scrollable_frame)
            question_frame.pack(fill="x", pady=10)
            ttk.Label(question_frame, text=f"{i+1}. {item['question']}", style="CustomBold.TLabel").pack(anchor="w")
            ttk.Radiobutton(question_frame, text=item["a"], variable=self.sss_vars[i], value="a", style="Custom.TRadiobutton").pack(anchor="w", padx=20, pady=5)
            ttk.Radiobutton(question_frame, text=item["b"], variable=self.sss_vars[i], value="b", style="Custom.TRadiobutton").pack(anchor="w", padx=20, pady=5)
        ttk.Button(self.current_frame, text="Weiter", command=self.validate_sss, style="TButton").pack(side="bottom", pady=10)

    def validate_sss(self):
        """
        Validate that all SSS questions have been answered and then move to the last questionnaire.
        """
        if any(var.get() == "" for var in self.sss_vars):
            messagebox.showerror("Fehler", "Bitte beantworte alle Fragen im SSS-Fragebogen.")
            return
        self.current_frame.destroy()
        self.show_last_questionnaire()

    def show_last_questionnaire(self):
        """
        Display the last (open-ended) questionnaire.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        canvas_container = ttk.Frame(self.current_frame)
        canvas_container.pack(side="top", fill="both", expand=True)
        canvas = tk.Canvas(canvas_container)
        v_scrollbar = ttk.Scrollbar(canvas_container, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=v_scrollbar.set)
        canvas.pack(side="left", fill="both", expand=True)
        v_scrollbar.pack(side="right", fill="y")
        scrollable_frame = ttk.Frame(canvas)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        def on_frame_configure(event):
            canvas.configure(scrollregion=canvas.bbox("all"))
        scrollable_frame.bind("<Configure>", on_frame_configure)
        def _on_mousewheel(event):
            canvas.yview_scroll(-1 * int(event.delta/120), "units")
        canvas.bind("<Enter>", lambda event: canvas.bind_all("<MouseWheel>", _on_mousewheel))
        canvas.bind("<Leave>", lambda event: canvas.unbind_all("<MouseWheel>"))
        self.last_question_vars = {}
        for q in self.last_questions:
            q_frame = ttk.Frame(scrollable_frame)
            q_frame.pack(fill="x", pady=10)
            ttk.Label(q_frame, text=f"{q['number']}. {q['text']}", style="CustomBold.TLabel", wraplength=800, justify="left").pack(anchor="w", pady=2)
            if q["type"] == "open":
                text_widget = tk.Text(q_frame, height=4, width=100, font=self.dynamic_font, wrap="word")
                text_widget.pack(pady=5)
                self.last_question_vars[q["number"]] = text_widget
            elif q["type"] == "radio":
                var = tk.StringVar()
                self.last_question_vars[q["number"]] = var
                options_frame = ttk.Frame(q_frame)
                options_frame.pack(anchor="w", pady=2)
                for option in q["options"]:
                    ttk.Radiobutton(options_frame, text=option, variable=var, value=option, style="Custom.TRadiobutton").pack(side="left", padx=10)
        ttk.Button(self.current_frame, text="Fragebogen abschließen", command=self.validate_last_questionnaire, style="TButton").pack(side="bottom", pady=10)

    def validate_last_questionnaire(self):
        """
        Validate and collect responses from the last questionnaire, then save all data.
        """
        last_responses = {}
        for q in self.last_questions:
            if q["type"] == "open":
                widget = self.last_question_vars[q["number"]]
                response = widget.get("1.0", "end-1c").strip()
                if not response:
                    messagebox.showerror("Fehler", f"Bitte beantworte Frage {q['number']}.")
                    return
                last_responses[q["number"]] = response
            elif q["type"] == "radio":
                response = self.last_question_vars[q["number"]].get().strip()
                if not response:
                    messagebox.showerror("Fehler", f"Bitte wähle eine Antwort für Frage {q['number']}.")
                    return
                last_responses[q["number"]] = response
        self.last_question_data = last_responses
        self.save_all_data()
        self.show_thank_you()

    def save_all_data(self):
        """
        Save BIS, SSS, and last questionnaire responses into one CSV file.
        The last questionnaire responses are saved horizontally with the desired column names.
        """
        base_dir = Path(__file__).parent
        data_dir = base_dir / "collected_data"
        data_dir.mkdir(exist_ok=True)
        filename = data_dir / f"{self.participant_id}_questionnaire_data.csv"

        bis_responses = [var.get() for var in self.bis_vars]
        sss_responses = [var.get() for var in self.sss_vars]
        bis_total = 0
        for i, value in enumerate(bis_responses):
            if self.bis_items[i]["reverse"]:
                bis_total += (5 - value)
            else:
                bis_total += value
        ss_scores = {"SST": 0, "SSE": 0, "SSD": 0, "SSB": 0}
        for i, response in enumerate(sss_responses):
            item = self.sss_items[i]
            if response == item["correct"]:
                ss_scores[item["subscale"]] += 1
        ss_total = (ss_scores["SST"] + ss_scores["SSE"] + ss_scores["SSD"] + ss_scores["SSB"]) / 4
        ss_percent = ss_total * 25

        # Define new headers for the last questionnaire.
        last_headers = [
            "q-open_goal_of_study",
            "q-open_noticable_aspects",
            "q-choice_noticed_saliency",
            "q-choice_saliency_strength",
            "q-choice_saliency_impact",
            "q-open_saliency_impact",
            "q-choice_saliency_value",
            "q-choice_win_motivation",
            "q-open_comments"
        ]

        headers = (["participant_id"] +
                   [f"bis_{i+1}" for i in range(len(self.bis_items))] +
                   [f"sss_{i+1}" for i in range(len(self.sss_items))] +
                   ["bis_total", "SST", "SSE", "SSD", "SSB", "ss_total", "ss_percent"] +
                   last_headers)

        # Order last questionnaire responses by question number 1 to 9.
        last_data_ordered = [self.last_question_data[q_num] for q_num in range(1, 10)]
        data_row = ([self.participant_id] +
                    bis_responses +
                    sss_responses +
                    [bis_total,
                     ss_scores["SST"], ss_scores["SSE"], ss_scores["SSD"], ss_scores["SSB"],
                     ss_total, ss_percent] +
                    last_data_ordered)

        with open(filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            writer.writerow(headers)
            writer.writerow(data_row)

    def show_thank_you(self):
        """
        Display a final thank-you screen.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        thank_font = font.Font(family="Arial", size=self.base_font_size + 6, weight="bold")
        thank_text = (
            "Vielen Dank!\n\n"
            "Du hast das Experiment erfolgreich abgeschlossen. "
            f"Die Versuchsleiterin bzw. der Versuchsleiter wird sich nun an dich wenden und dir deine Vergütung sowie deinen erspielten Gewinn in Höhe von {self.bonus:.2f}€ auszahlen."
        )
        ttk.Label(self.current_frame, text=thank_text, font=thank_font, wraplength=800, justify="center").pack(pady=20)

def run_questionnaire(participant_id=None, bonus=None):
    """
    Create and run the questionnaire GUI.

    Args:
        participant_id (str, optional): The participant's ID. If not provided,
                                        the GUI will prompt for it.
        bonus (float, optional): The bonus/earnings to be displayed on the thank-you page.
    """
    app = QuestionnaireApp(participant_id, bonus)
    app.root.mainloop()

if __name__ == "__main__":
    # Expect 2 arguments: participant_id, bonus
    if len(sys.argv) < 3:
        print("Usage: questionnaire_survey.py <participant_id> <bonus>")
        sys.exit(1)
    participant_id = sys.argv[1]
    bonus_str = sys.argv[2]
    try:
        bonus = float(bonus_str.strip())
    except ValueError:
        bonus = 0.0
    run_questionnaire(participant_id, bonus)
