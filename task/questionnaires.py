import tkinter as tk
from tkinter import ttk, messagebox
from tkinter import font
import csv
import subprocess
import sys
import os

class QuestionnaireApp:
    def __init__(self):
        self.root = tk.Tk()
        self.root.title("Questionnaires")
        self.root.state('zoomed')
        
        self.participant_id = None
        self.current_frame = None
        self.bis_vars = []
        self.sss_vars = []


        
        # BIS Items (15 questions)
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

    def show_participant_id(self):
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(pady=50)
        
        ttk.Label(self.current_frame, text="Bitte geben Sie Ihre Teilnehmer-ID ein:").pack()
        self.id_entry = ttk.Entry(self.current_frame)
        self.id_entry.pack(pady=10)
        ttk.Button(self.current_frame, text="Start", command=self.start_questionnaires).pack()

    def start_questionnaires(self):
        self.participant_id = self.id_entry.get()
        if not self.participant_id:
            messagebox.showerror("Fehler", "Bitte geben Sie eine gültige Teilnehmer-ID ein.")
            return
        self.current_frame.destroy()
        self.show_bis()

    def show_bis(self):
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)

        # Main container
        container = ttk.Frame(self.current_frame)
        container.pack(fill="both", expand=True)

        # Configure grid columns
        headers = ["Frage", "1 - selten/nie", "2 - gelegentlich", "3 - oft", "4 - fast immer/immer"]
        
        # Calculate column widths
        header_widths = []
        for header in headers:
            temp_label = ttk.Label(self.root, text=header, font=('Arial', 14, 'bold'))
            header_widths.append(temp_label.winfo_reqwidth() + 20)
        max_width = max(header_widths)

        # Create headers
        for col, header in enumerate(headers):
            ttk.Label(container, 
                    text=header, 
                    font=('Arial', 14, 'bold'),
                    anchor="center").grid(row=0, column=col, padx=10, pady=5, sticky="nsew")
            container.columnconfigure(col, minsize=max_width, weight=1)

        # Create variables and questions
        self.bis_vars = [tk.IntVar() for _ in self.bis_items]

        # Add questions and radio buttons
        for row_idx, item in enumerate(self.bis_items, start=1):
            # Question text
            ttk.Label(container, 
                    text=item["text"], 
                    wraplength=400,
                    font=('Arial', 14),
                    anchor="w").grid(row=row_idx, column=0, sticky="w", padx=10, pady=5)

            # Radio buttons
            for col_idx in range(1, 5):
                rb_frame = ttk.Frame(container, width=max_width)
                rb_frame.grid(row=row_idx, column=col_idx, padx=10, pady=5, sticky="nsew")
                
                ttk.Radiobutton(rb_frame,
                              variable=self.bis_vars[row_idx-1],
                              value=col_idx).place(relx=0.5, rely=0.5, anchor="center")

        # Navigation button
        btn_frame = ttk.Frame(self.current_frame)
        btn_frame.pack(pady=10)
        ttk.Button(btn_frame, 
                 text="Weiter zum nächsten Fragebogen",
                 command=self.validate_bis).pack()

    def validate_bis(self):
        if any(var.get() == 0 for var in self.bis_vars):
            messagebox.showerror("Fehler", "Bitte beantworten Sie alle Fragen im BIS-Fragebogen.")
            return
        self.current_frame.destroy()
        self.show_sss()

    def show_sss(self):
        self.current_frame = ttk.Frame(self.root)
        self.current_frame.pack(fill="both", expand=True, padx=150, pady=50)
        
        container = ttk.Frame(self.current_frame)
        container.pack(fill="both", expand=True)

        self.sss_vars = [tk.StringVar() for _ in self.sss_items]
        
        for i, item in enumerate(self.sss_items):
            question_frame = ttk.Frame(container)
            question_frame.pack(fill="x", pady=10)
            
            ttk.Label(question_frame, text=f"{i+1}. {item['question']}", font=('Arial', 14, 'bold')).pack(anchor="w")
            
            ttk.Radiobutton(
                question_frame,
                text=item["a"],
                variable=self.sss_vars[i],
                value="a"
            ).pack(anchor="w", padx=20, pady=5)
            
            ttk.Radiobutton(
                question_frame,
                text=item["b"],
                variable=self.sss_vars[i],
                value="b"
            ).pack(anchor="w", padx=20, pady=5)

        ttk.Button(
            self.current_frame,
            text="Fragebogen abschließen",
            command=self.validate_sss
        ).pack(pady=10)

    def validate_sss(self):
        if any(var.get() == "" for var in self.sss_vars):
            messagebox.showerror("Fehler", "Bitte beantworten Sie alle Fragen im SSS-Fragebogen.")
            return
        
        self.save_data()
        self.launch_experiment()

    def save_data(self):
        # Create data directory if it doesn't exist
        os.makedirs('data', exist_ok=True)
        filename = os.path.join('data', f'{self.participant_id}_questions.csv')

        # Collect BIS responses (raw scores)
        bis_responses = [var.get() for var in self.bis_vars]

        # Collect SSS responses ('a'/'b')
        sss_responses = [var.get() for var in self.sss_vars]

        # Compute BIS total (with reverse scoring)
        bis_total = 0
        for i, value in enumerate(bis_responses):
            if self.bis_items[i]["reverse"]:
                bis_total += (5 - value)
            else:
                bis_total += value

        # Compute SSS subscales
        ss_scores = {"SST": 0, "SSE": 0, "SSD": 0, "SSB": 0}
        for i, response in enumerate(sss_responses):
            item = self.sss_items[i]
            if response == item["correct"]:
                ss_scores[item["subscale"]] += 1

        # Calculate SSS metrics
        ss_total = (ss_scores["SST"] + ss_scores["SSE"] + ss_scores["SSD"] + ss_scores["SSB"]) / 4
        ss_percent = ss_total * 25

        # Prepare headers and data row
        headers = ["participant_id"]
        headers += [f"bis_{i+1}" for i in range(15)]  # BIS items 1-15
        headers += [f"sss_{i+1}" for i in range(8)]   # SSS items 1-8
        headers += ["bis_total", "SST", "SSE", "SSD", "SSB", "ss_total", "ss_percent"]

        data_row = [self.participant_id]
        data_row += bis_responses  # Add raw BIS responses
        data_row += sss_responses  # Add raw SSS responses
        data_row += [bis_total]
        data_row += [ss_scores["SST"], ss_scores["SSE"], ss_scores["SSD"], ss_scores["SSB"]]
        data_row += [ss_total, ss_percent]

        # Write to CSV
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