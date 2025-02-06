import tkinter as tk
from tkinter import ttk, messagebox, font
import csv
import subprocess
import sys
import os

class QuestionnaireApp:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Questionnaires")
        self.root.state('zoomed')
        
        # Reference dimensions
        self.base_width = 1920
        self.base_height = 1080
        self.base_font_size = 18
        
        # Create dynamic fonts
        self.dynamic_font = font.Font(family="Arial", size=self.base_font_size)
        self.bold_font = font.Font(family="Arial", size=self.base_font_size, weight="bold")
        
        # Create a style and configure custom styles for labels and radiobuttons.
        self.style = ttk.Style()
        self.style.configure("Custom.TLabel", font=self.dynamic_font)
        self.style.configure("CustomBold.TLabel", font=self.bold_font)
        self.style.configure("Custom.TRadiobutton", font=self.dynamic_font)
        self.style.configure("TButton", font=self.dynamic_font)
        
        # Bind resize event
        self.root.bind("<Configure>", self.on_resize)

        self.participant_id = None
        self.current_frame = None
        self.bis_vars = []
        self.sss_vars = []

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

        # SSS Items (8 questions)
        self.sss_items = [
            {"question": "Frage 1", "a": "Ich liebe ausgelassene, „wilde“ Partys.", "b": "Ich bevorzuge ruhige Partys mit guten Gesprächen.", "subscale": "SSD", "correct": "a"},
            {"question": "Frage 2", "a": "Mir macht es nichts aus, wenn ich bei Filmen oder Schauspielen weiß, was als nächstes passiert.", "b": "Ich kann mich normalerweise nicht an Filmen oder Schauspielen erfreuen, bei denen ich genau weiß, was als nächstes passiert.", "subscale": "SSB", "correct": "b"},
            {"question": "Frage 3", "a": "Manchmal liebe ich es, Dinge zu tun, die einem ein wenig Angst einflößen.", "b": "Eine vernünftige Person vermeidet Aktivitäten, die gefährlich sind.", "subscale": "SST", "correct": "a"},
            {"question": "Frage 4", "a": "Ich liebe es, mich häufig durch Alkohol oder Rauchen in eine gute Stimmung zu versetzen.", "b": "Ich finde, dass mir künstliche Anregungsmittel wie Alkohol oder Rauchen nicht bekommen.", "subscale": "SSD", "correct": "a"},
            {"question": "Frage 5", "a": "Wenn ich eine Reise unternehme, dann lege ich vorher meine Reiseroute und Zeitplanung sorgfältig fest.", "b": "Ich würde gerne eine Reise machen, ohne vorher die Route oder den zeitlichen Ablauf zu planen.", "subscale": "SSE", "correct": "b"},
            {"question": "Frage 6", "a": "Ich bevorzuge „normale“ Personen aus meinem Umfeld als Freunde.", "b": "Ich würde gerne Freunde in Außenseitergruppen wie „Skinheads“ oder „Zigeuner“ kennen lernen.", "subscale": "SSE", "correct": "b"},
            {"question": "Frage 7", "a": "Ich würde gerne einmal einen Fallschirmabsprung versuchen.", "b": "Ich würde niemals einen Fallschirmabsprung aus einem Flugzeug wagen.", "subscale": "SST", "correct": "a"},
            {"question": "Frage 8", "a": "Ich finde etwas Interessantes an fast jeder Person, mit der ich rede.", "b": "Ich habe keine Geduld mit trägen oder langweiligen Personen.", "subscale": "SSB", "correct": "b"},
        ]
        self.show_participant_id()

    def on_resize(self, event):
        # Get the current full window dimensions.
        current_width = self.root.winfo_width()
        current_height = self.root.winfo_height()
        
        # Calculate scaling factors based on your reference dimensions.
        scale_w = current_width / self.base_width
        scale_h = current_height / self.base_height
        scale = min(scale_w, scale_h)
        
        new_font_size = max(8, int(self.base_font_size * scale))
        
        # Update font objects
        self.dynamic_font.configure(size=new_font_size)
        self.bold_font.configure(size=new_font_size)
        
        # Update styles so widgets refresh their font
        self.style.configure("Custom.TLabel", font=self.dynamic_font)
        self.style.configure("CustomBold.TLabel", font=self.bold_font)
        self.style.configure("Custom.TRadiobutton", font=self.dynamic_font)
        self.style.configure("TButton", font=self.dynamic_font)

    def show_participant_id(self):
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(pady=50, expand=True)
        
        ttk.Label(self.current_frame, text="Bitte geben Sie deine Teilnehmer-ID ein:", style="Custom.TLabel").pack()
        self.id_entry = ttk.Entry(self.current_frame, font=self.dynamic_font)
        self.id_entry.pack(pady=10)
        ttk.Button(self.current_frame, text="Start", command=self.start_questionnaires, style="TButton").pack()

    def start_questionnaires(self):
        self.participant_id = self.id_entry.get()
        if not self.participant_id:
            messagebox.showerror("Fehler", "Bitte gib eine gültige Teilnehmer-ID ein.")
            return
        self.current_frame.destroy()
        self.show_instructions_general()

    def show_instructions_general(self):
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
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        
        container = ttk.Frame(self.current_frame)
        container.pack(fill="both", expand=True)

        headers = ["Frage", "1 - selten/nie", "2 - gelegentlich", "3 - oft", "4 - fast immer/immer"]
        for col, header in enumerate(headers):
            ttk.Label(container, 
                      text=header, 
                      style="CustomBold.TLabel",
                      anchor="center").grid(row=0, column=col, padx=10, pady=5, sticky="nsew")
            container.columnconfigure(col, weight=1)

        self.bis_vars = [tk.IntVar() for _ in self.bis_items]
        for row_idx, item in enumerate(self.bis_items, start=1):
            ttk.Label(container, 
                      text=item["text"], 
                      wraplength=400,
                      style="Custom.TLabel",
                      anchor="w").grid(row=row_idx, column=0, sticky="w", padx=10, pady=5)
            for col_idx in range(1, 5):
                rb_frame = ttk.Frame(container)
                rb_frame.grid(row=row_idx, column=col_idx, padx=10, pady=5, sticky="nsew")
                ttk.Radiobutton(rb_frame,
                                variable=self.bis_vars[row_idx-1],
                                value=col_idx,
                                style="Custom.TRadiobutton").place(relx=0.5, rely=0.5, anchor="center")
                
        btn_frame = ttk.Frame(self.current_frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, 
                   text="Weiter zum nächsten Fragebogen",
                   command=self.validate_bis,
                   style="TButton").pack()

    def validate_bis(self):
        if any(var.get() == 0 for var in self.bis_vars):
            messagebox.showerror("Fehler", "Bitte beantworte alle Fragen im BIS-Fragebogen.")
            return
        self.current_frame.destroy()
        self.show_instructions_sss()

    def show_instructions_sss(self):
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        
        text = (
            "Sehr gut!\nNun folgt der zweite Frageogen\n\n"
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
        self.current_frame.destroy()
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)

        # Create a container frame for the canvas and scrollbar.
        canvas_container = ttk.Frame(self.current_frame)
        canvas_container.pack(side="top", fill="both", expand=True)

        # Create a canvas and a vertical scrollbar within the container.
        canvas = tk.Canvas(canvas_container)
        v_scrollbar = ttk.Scrollbar(canvas_container, orient="vertical", command=canvas.yview)
        canvas.configure(yscrollcommand=v_scrollbar.set)

        # Pack the canvas and scrollbar inside the container.
        canvas.pack(side="left", fill="both", expand=True)
        v_scrollbar.pack(side="right", fill="y")

        # Create a frame inside the canvas to contain the SSS widgets.
        scrollable_frame = ttk.Frame(canvas)
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")

        # Update the scroll region when the frame changes.
        def on_frame_configure(event):
            canvas.configure(scrollregion=canvas.bbox("all"))
        scrollable_frame.bind("<Configure>", on_frame_configure)

        def _on_mousewheel(event):
            # For Windows, event.delta is usually in multiples of 120.
            canvas.yview_scroll(-1 * int(event.delta/120), "units")

        # Bind the mouse wheel when the cursor is over the canvas.
        canvas.bind("<Enter>", lambda event: canvas.bind_all("<MouseWheel>", _on_mousewheel))
        canvas.bind("<Leave>", lambda event: canvas.unbind_all("<MouseWheel>"))

        # Populate the scrollable frame with SSS questionnaire items.
        self.sss_vars = [tk.StringVar() for _ in self.sss_items]
        for i, item in enumerate(self.sss_items):
            question_frame = ttk.Frame(scrollable_frame)
            question_frame.pack(fill="x", pady=10)
            
            ttk.Label(question_frame,
                      text=f"{i+1}. {item['question']}",
                      style="CustomBold.TLabel").pack(anchor="w")
            ttk.Radiobutton(
                question_frame,
                text=item["a"],
                variable=self.sss_vars[i],
                value="a",
                style="Custom.TRadiobutton"
            ).pack(anchor="w", padx=20, pady=5)
            ttk.Radiobutton(
                question_frame,
                text=item["b"],
                variable=self.sss_vars[i],
                value="b",
                style="Custom.TRadiobutton"
            ).pack(anchor="w", padx=20, pady=5)

        # Now, place the finish button outside of the scrollable canvas container.
        ttk.Button(
            self.current_frame,
            text="Fragebogen abschließen",
            command=self.validate_sss,
            style="TButton"
        ).pack(side="bottom", pady=10)

    def validate_sss(self):
        if any(var.get() == "" for var in self.sss_vars):
            messagebox.showerror("Fehler", "Bitte beantworte alle Fragen im SSS-Fragebogen.")
            return
        
        self.save_data()
        self.launch_experiment()

    def save_data(self):
        os.makedirs('data', exist_ok=True)
        filename = os.path.join('data', f'{self.participant_id}_questions.csv')
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

        headers = ["participant_id"]
        headers += [f"bis_{i+1}" for i in range(len(self.bis_items))]
        headers += [f"sss_{i+1}" for i in range(len(self.sss_items))]
        headers += ["bis_total", "SST", "SSE", "SSD", "SSB", "ss_total", "ss_percent"]

        data_row = [self.participant_id] + bis_responses + sss_responses + [bis_total,
                    ss_scores["SST"], ss_scores["SSE"], ss_scores["SSD"], ss_scores["SSB"],
                    ss_total, ss_percent]

        with open(filename, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)
            writer.writerow(headers)
            writer.writerow(data_row)

    def launch_experiment(self):
        self.root.destroy()
        subprocess.Popen([sys.executable, "task.py", self.participant_id])

if __name__ == "__main__":
    app = QuestionnaireApp()
    app.root.mainloop()
