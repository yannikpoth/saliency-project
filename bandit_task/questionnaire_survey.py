"""
questionnaire_survey.py

This script implements a Tkinter-based GUI application for administering 
two questionnaires (BIS-15 and SSS) as part of the Saliency Project study.
It collects participant responses, validates them, and saves the results 
to a CSV file in the collected_data folder.

Usage:
    python questionnaire_survey.py [participant_id]

If no participant_id is provided as a command-line argument, the user is prompted
to enter one in the GUI.
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
    and presents the BIS-15 and SSS questionnaires. Participant responses are
    collected, validated, and saved to disk.
    """
    def __init__(self):
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

        # Retrieve participant ID from command-line arguments if provided.
        self.participant_id = sys.argv[1] if len(sys.argv) > 1 else None

        # Define the BIS-15 items with a flag indicating if an item is reverse-scored.
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
            {"question": "Frage", "a": "Ich bevorzuge „normale“ Personen aus meinem Umfeld als Freunde.", "b": "Ich würde gerne Freunde in Außenseitergruppen wie „Skinheads“ oder „Zigeuner“ kennen lernen.", "subscale": "SSE", "correct": "b"},
            {"question": "Frage", "a": "Ich würde gerne einmal einen Fallschirmabsprung versuchen.", "b": "Ich würde niemals einen Fallschirmabsprung aus einem Flugzeug wagen.", "subscale": "SST", "correct": "a"},
            {"question": "Frage", "a": "Ich finde etwas Interessantes an fast jeder Person, mit der ich rede.", "b": "Ich habe keine Geduld mit trägen oder langweiligen Personen.", "subscale": "SSB", "correct": "b"},
        ]
        
        # If no participant ID is provided, ask for it; otherwise, start instructions.
        if self.participant_id is None:
            self.show_participant_id()
        else:
            self.show_instructions_general()

    def on_resize(self, event):
        """
        Adjust font sizes dynamically when the window is resized.
        
        The scaling is based on reference dimensions (base_width and base_height).
        """
        current_width = self.root.winfo_width()
        current_height = self.root.winfo_height()
        
        scale_w = current_width / self.base_width
        scale_h = current_height / self.base_height
        scale = min(scale_w, scale_h)
        
        new_font_size = max(8, int(self.base_font_size * scale))
        
        # Update font sizes.
        self.dynamic_font.configure(size=new_font_size)
        self.bold_font.configure(size=new_font_size)
        
        # Update widget styles with the new fonts.
        self.style.configure("Custom.TLabel", font=self.dynamic_font)
        self.style.configure("CustomBold.TLabel", font=self.bold_font)
        self.style.configure("Custom.TRadiobutton", font=self.dynamic_font)
        self.style.configure("TButton", font=self.dynamic_font)

    def show_participant_id(self):
        """
        Display a frame prompting the participant to enter their ID.
        """
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(pady=50, expand=True)
        
        ttk.Label(self.current_frame, text="Bitte geben Sie deine Teilnehmer-ID ein:", style="Custom.TLabel").pack()
        self.id_entry = ttk.Entry(self.current_frame, font=self.dynamic_font)
        self.id_entry.pack(pady=10)
        ttk.Button(self.current_frame, text="Start", command=self.start_questionnaires, style="TButton").pack()

    def start_questionnaires(self):
        """
        Retrieve the participant ID from the entry field and proceed to instructions.
        If no ID is provided, show an error message.
        """
        self.participant_id = self.id_entry.get()
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
            "Willkommen zu unserem Experiment\n\n"
            "Bevor die eigentliche Aufgabe beginnt, wirst du gebeten, zwei Fragebögen auszufüllen. "
            "Das Ausfüllen dauert ungefähr 5 bis 10 Minuten. Es gibt keine „richtigen“ oder „falschen“ Antworten, "
            "wie es in anderen Tests der Fall ist.\n\n"
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
        Display the BIS questionnaire with response options.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        
        # Create a container frame for the questionnaire.
        container = ttk.Frame(self.current_frame)
        container.pack(fill="both", expand=True)

        headers = ["Frage", "1 - selten/nie", "2 - gelegentlich", "3 - oft", "4 - fast immer/immer"]
        # Create header labels for the response scale.
        for col, header in enumerate(headers):
            ttk.Label(container, text=header, style="CustomBold.TLabel",
                      anchor="center").grid(row=0, column=col, padx=10, pady=5, sticky="nsew")
            container.columnconfigure(col, weight=1)

        # Create an IntVar for each BIS item.
        self.bis_vars = [tk.IntVar() for _ in self.bis_items]
        # Populate the BIS questionnaire.
        for row_idx, item in enumerate(self.bis_items, start=1):
            ttk.Label(container, text=item["text"], wraplength=400,
                      style="Custom.TLabel", anchor="w").grid(row=row_idx, column=0, sticky="w", padx=10, pady=5)
            for col_idx in range(1, 5):
                rb_frame = ttk.Frame(container)
                rb_frame.grid(row=row_idx, column=col_idx, padx=10, pady=5, sticky="nsew")
                ttk.Radiobutton(rb_frame, variable=self.bis_vars[row_idx-1],
                                value=col_idx, style="Custom.TRadiobutton")\
                                .place(relx=0.5, rely=0.5, anchor="center")
                
        btn_frame = ttk.Frame(self.current_frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, text="Weiter zum nächsten Fragebogen",
                   command=self.validate_bis, style="TButton").pack()

    def validate_bis(self):
        """
        Validate that all BIS questions have been answered.
        Proceed to the SSS questionnaire if validation passes.
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
        Display the SSS questionnaire inside a scrollable frame.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)

        # Create a container for a canvas and vertical scrollbar.
        canvas_container = ttk.Frame(self.current_frame)
        canvas_container.pack(side="top", fill="both", expand=True)

        canvas = tk.Canvas(canvas_container)
        v_scrollbar = ttk.Scrollbar(canvas_container, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=v_scrollbar.set)

        canvas.pack(side="left", fill="both", expand=True)
        v_scrollbar.pack(side="right", fill="y")

        # Create an internal frame within the canvas.
        scrollable_frame = ttk.Frame(canvas)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")

        # Update the scroll region when the internal frame's size changes.
        def on_frame_configure(event):
            canvas.configure(scrollregion=canvas.bbox("all"))
        scrollable_frame.bind("<Configure>", on_frame_configure)

        # Bind the mouse wheel for scrolling.
        def _on_mousewheel(event):
            canvas.yview_scroll(-1 * int(event.delta/120), "units")
        canvas.bind("<Enter>", lambda event: canvas.bind_all("<MouseWheel>", _on_mousewheel))
        canvas.bind("<Leave>", lambda event: canvas.unbind_all("<MouseWheel>"))

        # Populate the SSS questionnaire.
        self.sss_vars = [tk.StringVar() for _ in self.sss_items]
        for i, item in enumerate(self.sss_items):
            question_frame = ttk.Frame(scrollable_frame)
            question_frame.pack(fill="x", pady=10)
            
            ttk.Label(question_frame, text=f"{i+1}. {item['question']}",
                      style="CustomBold.TLabel").pack(anchor="w")
            ttk.Radiobutton(question_frame, text=item["a"],
                            variable=self.sss_vars[i],
                            value="a", style="Custom.TRadiobutton")\
                            .pack(anchor="w", padx=20, pady=5)
            ttk.Radiobutton(question_frame, text=item["b"],
                            variable=self.sss_vars[i],
                            value="b", style="Custom.TRadiobutton")\
                            .pack(anchor="w", padx=20, pady=5)

        # Place the finish button below the scrollable content.
        ttk.Button(self.current_frame, text="Fragebogen abschließen",
                   command=self.validate_sss, style="TButton")\
                   .pack(side="bottom", pady=10)

    def validate_sss(self):
        """
        Validate that all SSS questions have been answered.
        If validation passes, save the responses and display a thank-you message.
        """
        if any(var.get() == "" for var in self.sss_vars):
            messagebox.showerror("Fehler", "Bitte beantworte alle Fragen im SSS-Fragebogen.")
            return
        
        self.save_data()
        self.show_thank_you()

    def show_thank_you(self):
        """
        Display a final thank-you screen indicating successful completion.
        """
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        
        # Increase font size for the thank-you message.
        thank_font = font.Font(family="Arial", size=self.base_font_size + 6, weight="bold")
        
        thank_text = (
            "Vielen Dank!\n\n"
            "Du hast das Experiment erfolgreich abgeschlossen. "
            "Die Versuchsleiterin bzw. der Versuchsleiter wird sich nun an dich wenden."
        )
        
        ttk.Label(self.current_frame, text=thank_text, font=thank_font,
                  wraplength=800, justify="center").pack(pady=20)

    def save_data(self):
        """
        Save responses from both questionnaires (BIS and SSS) to a CSV file.
        The file is created in the collected_data folder and is named using the participant ID.
        """
        base_dir = Path(__file__).parent
        data_dir = base_dir / "collected_data"
        data_dir.mkdir(exist_ok=True)
        filename = data_dir / f"{self.participant_id}_questions.csv"
        
        # Retrieve responses.
        bis_responses = [var.get() for var in self.bis_vars]
        sss_responses = [var.get() for var in self.sss_vars]

        # Calculate the total BIS score with reverse scoring where applicable.
        bis_total = 0
        for i, value in enumerate(bis_responses):
            if self.bis_items[i]["reverse"]:
                bis_total += (5 - value)
            else:
                bis_total += value

        # Calculate SSS scores per subscale.
        ss_scores = {"SST": 0, "SSE": 0, "SSD": 0, "SSB": 0}
        for i, response in enumerate(sss_responses):
            item = self.sss_items[i]
            if response == item["correct"]:
                ss_scores[item["subscale"]] += 1

        ss_total = (ss_scores["SST"] + ss_scores["SSE"] + ss_scores["SSD"] + ss_scores["SSB"]) / 4
        ss_percent = ss_total * 25

        # Prepare header and data row.
        headers = ["participant_id"] + \
                  [f"bis_{i+1}" for i in range(len(self.bis_items))] + \
                  [f"sss_{i+1}" for i in range(len(self.sss_items))] + \
                  ["bis_total", "SST", "SSE", "SSD", "SSB", "ss_total", "ss_percent"]

        data_row = [self.participant_id] + bis_responses + sss_responses + [bis_total,
                    ss_scores["SST"], ss_scores["SSE"], ss_scores["SSD"], ss_scores["SSB"],
                    ss_total, ss_percent]

        # Write data to CSV.
        with open(filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            writer.writerow(headers)
            writer.writerow(data_row)


if __name__ == "__main__":
    app = QuestionnaireApp()
    app.root.mainloop()
