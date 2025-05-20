A typical Method section in psychological research covers *who* participated, *what* materials were used, *what* the participants did (Procedure & Task Design), and *how* the data will be analyzed.

**Method Section Outline**

1.  **Participants**
    *   **Recruitment:** Participants were primarily recruited through the University of Cologne subject pool, WhatsApp groups, and word-of-mouth. Additional recruitment methods included University of Cologne email lists, flyers posted in university buildings and canteens, and advertisements in social network groups (WhatsApp).
    *   **Sample Size:** 
        *   A final sample of 44 participants was included in the analysis.
        *   The initial target was 42 participants, aiming for approximate gender balance.
        *   An a priori power analysis was conducted using G*Power to estimate the sample size needed for Hypothesis 1.2 (win-stay probability comparison).
        *   Parameters for the power analysis (paired t-test, one-tailed): effect size dz = 0.4, α = 0.05, power = 0.8.
        *   The target effect size (dz = 0.4) was informed by prior research indicating moderate effects of salient cues on decision-making and learning, chosen somewhat conservatively to account for potential individual variability (e.g., Cherkasova et al., 2018; Liu & Yu, 2018).
        *   The analysis indicated a minimum required sample size of 41 participants.
        *   Although the primary analysis employs a Bayesian framework, this power analysis served as a conventional check to ensure adequate sensitivity for detecting medium-sized effects.
    *   **Demographics:** Report key demographics (e.g., age range, mean age, gender distribution). *You'll need to gather this information if you don't have it readily available.*
    *   **Inclusion/Exclusion Criteria:** Any specific criteria used (e.g., normal or corrected-to-normal vision/hearing, age limits)? Mention if any participants were excluded and why (e.g., technical issues, failure to follow instructions).
    *   **Compensation:** How were participants compensated (e.g., course credit, monetary payment)?
    *   **Ethical Approval:** State that the study received approval from the relevant Institutional Review Board (IRB) or Ethics Committee and that informed consent was obtained.

2.  **Materials**
    *   **Task Apparatus:** Specify the software used for task presentation and data collection (`PsychoPy`). Mention relevant hardware if controlled (e.g., screen resolution, type of headphones if audio was critical).
        * Use dummy values for screen size and headphones as i'll have to look up the exact information later
        * The experiment was developed and run using **Python 3.10.0 (64-bit)**.
        * **PsychoPy (v2024.2.4)** was used for precise visual stimulus presentation, timing, and response collection.
        * **Pygame (v2.6.1)** was utilized for auditory feedback management, including background music and salient feedback sounds.
        * Core Python libraries involved in data handling and experimental logic included:
            * **Numpy (v1.25.1)** for numerical operations.
            * **Pandas (v2.0.3)** for data structure management.
        * PsychoPy internally utilized libraries such as **pyglet** for windowing and **ffpyplayer (v4.5.1)** for video stimulus playback.
        * The overall experiment flow was managed by a custom script (`run_experiment.py`) that sequentially launched the main task script (`task_logic.py`) and the questionnaire script (`questionnaire_survey.py`).
    *   **Task Stimuli:**
        *   Describe the choice stimuli (e.g., visual shapes, images, their location on screen).
            *   **Main Trials:** Participants chose between two distinct fractal images generated using DALL-E 3 (April 17, 2024).
                *   One stimulus was a red, symmetric, radial fractal with layered, petal-like structures, featuring high-contrast red, black, and white hues, framed in a rounded square with a metallic gradient border.
                *   The other stimulus was a blue, symmetric, geometric fractal with nested, grid-based square/rectangular elements in concentric diamond formations, creating a 3D architectural look with varied blue shades and circuit-like details, similarly framed.
            *   **Practice Trials:** Two different fractal images (one green, one purple) were used, created in a similar manner to the main trial stimuli.
        *   Describe the feedback stimuli in detail:
            *   **Non-Salient Feedback (Visual):** Upon choosing an option, a simple border appeared around the selected fractal image. The outcome (win/loss) was indicated by text displayed at the bottom of the screen (details in Procedure section).
            *   **Salient Feedback (Visual):** For salient feedback, a dynamic, attention-grabbing animation of the chosen fractal image was displayed. The animation begins with a brief glow, accompanied by a pulsating, blooming effect that accentuates the layered, petal-like structure. Dynamic lighting and contrast shifts create the impression of depth and movement, enhancing the visual salience of the feedback. The entire animation lasts 3 seconds and is designed to serve as a high-salience reward cue intended to amplify the impact of feedback on decision-making behavior. These animations were inspired by visual effects in online slot machines (e.g., Starburst) and created by the author using Apple Motion (v5.7).
            *   **Auditory Stimuli:**
                *   **Background Music:** A continuous, spacey ambient music track played throughout the task. This track, created by the author using Apple Logic Pro (v10.7.7), had a duration of approximately 105 seconds and looped infinitely. It featured a sustained and modulated dynamic profile to provide a consistent, non-intrusive auditory environment.
                *   **Salient Feedback Sound:** Paired with salient visual feedback, an explosive and highly noticeable sound effect was played. This 3-second sound, characterized by elevated amplitude, was created by the author using Apple Logic Pro (v10.7.7) to serve as an exaggerated auditory cue.
    *   **Questionnaires:**: Questionnaires where presented digitally using a Python programm after the experiment.
        *   **Barratt Impulsiveness Scale (BIS-11):** Mention the version used (likely BIS-11 based on item count) and its purpose (measuring impulsivity). Reference the scoring method (total score).
            * **Citation**
                - Meule, A., Vögele, C., & Kübler, A. (2011). Psychometrische Evaluation der deutschen Barratt Impulsiveness Scale - Kurzversion (BIS-15). *Diagnostica, 57*(3), 126-133.

            * **Purpose**
                - The BIS-15 is a short version of the Barratt Impulsiveness Scale (BIS-11), designed to assess impulsivity as a personality trait in German-speaking populations.
                - Impulsivity is characterized by quick, thoughtless actions without considering potential negative consequences.
                - The study aimed to evaluate the psychometric properties of the German BIS-15, confirming its reliability and validity as a concise alternative to the 30-item BIS-11.

            * **Structure**
                - **Item Count:** 15 items (reduced from 30 in the BIS-11).
                - **Subdimensions:** Measures three facets of impulsivity:
                - **Non-planning impulsivity:** Lack of future orientation or foresight (e.g., "Ich plane für die Zukunft" [inverted]).
                - **Motor impulsivity:** Acting without thinking (e.g., "Ich handele spontan").
                - **Attentional impulsivity:** Difficulty focusing or concentrating (e.g., "Ich werde bei Vorlesungen oder Vorträgen schnell unruhig").
                - **Response Format:** 4-point Likert scale:
                    - 1 = selten/nie (rarely/never)
                    - 2 = gelegentlich (occasionally)
                    - 3 = oft (often)
                    - 4 = fast immer/immer (almost always/always)
                - **Scoring:** Total scores range from 15 to 60; higher scores indicate greater impulsivity. Subscale scores are also calculable (5 items per subscale).

            * **Psychometric Properties**
                - **Factor Structure:**
                    - Principal component analysis with Varimax rotation confirmed the three-factor structure (non-planning, motor, attentional).
                    - Eigenvalues: 4.36, 1.91, 1.69; explains 53% of variance.
                    - Congruence coefficients with the English BIS-15 (Spinella, 2007) ranged from 0.90 to 1.00, indicating near-identical factor loadings.
                - **Reliability (Cronbach's Alpha):**
                    - Total scale: 0.81
                    - Non-planning impulsivity: 0.82
                    - Motor impulsivity: 0.72
                    - Attentional impulsivity: 0.68
                - **Validity:**
                    - Significant correlations with the UPPS Impulsive Behavior Scale (total: *r* = .67, *p* < .001), particularly with Lack of Premeditation (*r* = .53) and Urgency (*r* = .37).
                    - Moderate correlations with the Sensation Seeking Scale (SSS-V) (total: *r* = .32, *p* < .05), especially Boredom Susceptibility (*r* = .26).
                - **Sample Details:**
                    - **Study 1:** 752 participants (77.4% female, mean age 23.1 years, mostly students), online survey.
                    - **Study 2:** 51 participants (80.4% female, mean age 30.0 years), broader age range.

                * **Implications for Research**
                    - The BIS-15 is a time-efficient, reliable, and valid tool for measuring impulsivity in German-speaking populations.
                    - Its brevity (15 items) reduces participant burden, making it ideal for studies with multiple measures or time constraints.
                    - The three-factor structure allows for detailed analysis of specific impulsivity components, useful for both research and clinical applications.

                * **Limitations**
                    - **Sample Bias:** Predominantly female and student-based samples (Study 1: 77.4% female, 94% students; Study 2: 80.4% female) limit generalizability to older or less-educated populations.
                    - **Online Format:** Forced responses in the online survey may have influenced data quality due to potential participant reactance.
                    - **Scope:** As a short form, it may not capture the full depth of impulsivity compared to the BIS-11.
                    - **Future Research Needs:** Test-retest reliability and validation in diverse populations are recommended.

                * **Suggested Use in Your Study**
                    - **Methods Section:** Describe the BIS-15 as a 15-item measure of impulsivity with three subscales, citing its good reliability (Cronbach's α = 0.81) and validity in German samples (Meule et al., 2011).
                    - **Limitations Section:** Note the potential generalizability issues due to the original sample composition and suggest caution when applying findings to non-student populations.
        *   **Sensation Seeking Scale (SSS):** Mention the version (e.g., SSS-V) and its purpose. Describe the format (forced-choice) and scoring (total score, subscales: TAS, ES, Dis, BS). *Note: Your rules list `SST`, `SSE`, `SSD`, `SSB` which likely correspond to Thrill and Adventure Seeking, Experience Seeking, Disinhibition, and Boredom Susceptibility.*
            - **Introduction to the Questionnaire:**
                - Used to measure sensation seeking, a personality trait characterized by the pursuit of varied, novel, and intense experiences.
                - Based on Marvin Zuckerman's original Sensation Seeking Scale (SSS; Zuckerman, 1979).
                - Employed the German short version developed by Jürgen Grimm (2015), as outlined in the working paper "Sensation Seeking nach Zuckerman. Deutsche Kurzversion. Test-Dokumentation" (Methodenforum der Universität Wien: MF-Working Paper 2015-02).

            - **Details of the Questionnaire:**
                - Comprises 8 items across four subdimensions:
                    - Thrill and Adventure Seeking (2 items).
                    - Experience Seeking (3 items).
                    - Disinhibition (2 items).
                    - Boredom Susceptibility (1 item).
                - Participants choose between two statements per item (e.g., one reflecting sensation-seeking behavior, the other not).
                - Scoring: Sensation-seeking responses are coded as 1, non-sensation-seeking as 0; subdimension scores are summed, converted to percentage agreement (0-100 scale), and averaged for an overall score: SS = (SST + SSE + SSD + SSB) / 4.

            - **Adaptation for Cultural Sensitivity:**
                - Modified one item in the Experience Seeking subdimension due to politically incorrect language.
                - Original item: "Ich würde gerne Freunde in Außenseitergruppen wie 'Skinheads' oder 'Zigeuner' kennen lernen" ("I would like to get to know friends in outsider groups like 'Skinheads' or 'Zigeuner'").
                - Adapted item: Replaced "Zigeuner" (a derogatory term) and "Skinheads" with "Punks" and Spirituelle," resulting in "Ich würde gerne Freunde in Außenseitergruppen wie 'Punks' oder 'Spirituelle' kennen lernen" ("I would like to get to know friends in outsider groups like 'Punks' or 'Spirituelle'").
                - Rationale: The term "Zigeuner" is offensive and culturally insensitive; "Punks und Spirituelle" (Punks and Spiritual individuals) was chosen to reflect contemporary, non-stigmatizing subcultures while preserving the intent of assessing openness to unconventional groups.

            - **Implications of the Adaptation:**
                - The change may have altered the item's context or connotation:
                    - "Zigeuner" historically refers to Romani people and carries negative stereotypes.
                    - "Punks und Spirituelle" represents modern subcultures (Punks for rebellion/non-conformity, Spirituelle for spiritual/new-age identities), potentially shifting the item's interpretation.
                - This adaptation could affect participants' responses, as the new terms may evoke different associations or levels of familiarity.
                - While the modification aligns with ethical research practices, it introduces a deviation from the original scale, potentially impacting comparability.

            - **Acknowledgment of Limitations:**
                - The German short version by Grimm (2015) lacks formal reliability and validity data, as noted in the original documentation.
                - The adaptation exacerbates this limitation, as the modified questionnaire has not been psychometrically evaluated.
                - Results should be interpreted cautiously due to the exploratory nature of the instrument.
                - Recommendation: Future research should validate this adapted version to confirm its reliability and validity in measuring sensation seeking.

            - **Citation and Reference:**
                - Primary citation: Grimm, J. (2015). *Sensation Seeking nach Zuckerman. Deutsche Kurzversion. Test-Dokumentation.* Methodenforum der Universität Wien: MF-Working Paper 2015-02.
                - Optional context: Zuckerman, M. (1979). *Sensation Seeking: Beyond the Optimal Level of Arousal.* Hillsdale, NJ: Erlbaum.
        *   **Experiment-Specific Questions:** Briefly mention the open-ended questions asked at the end, noting they are not part of the main quantitative analysis.
            - **Introduction to the Questionnaire:**
                - The evaluation questionnaire was designed to collect participants' feedback on their experience in the study.
                - Its primary purpose was to assess participants' understanding of the study's goal, their perceptions of the task, and their reactions to specific audiovisual feedback stimuli associated with win notifications.
                - Administered post-study, it aimed to explore whether these feedback stimuli influenced participants' decision-making and motivation.

            - **Details of the Questionnaire:**
                - The questionnaire consisted of nine questions in german language, blending open-ended and closed-ended formats. They can be translated to english like that:
                    - **Open-ended questions (1, 2, 4, 6, 9):**
                        - Q1: "What do you believe was the goal of this study?"
                        - Q2: "Which aspects of the task did you find particularly striking or noteworthy?"
                        - Q4: "If yes, how did you perceive the special audiovisual feedback stimuli?"
                        - Q6: "If yes, in what way did the special audiovisual feedback stimuli influence your decision-making behavior?"
                        - Q9: "Do you have any additional comments or feedback about the study?"
                        - These questions encouraged detailed, qualitative responses to capture participants' subjective experiences.
                    - **Closed-ended questions (3, 5, 7, 8):**
                        - Q3: "Did you notice during the task that some win notifications were associated with special audiovisual feedback stimuli?" (Options: Ja/Nein)
                        - Q5: "Do you believe the special audiovisual feedback stimuli influenced your decision-making behavior?" (Options: Ja/Nein/Unsicher)
                        - Q7: "Did you feel that the special feedback stimuli affected your assessment of which option was better?" (Options: Ja/Nein/Unsicher)
                        - Q8: "How motivated were you to achieve the highest possible total win during the task?" (Likert scale: Sehr motiviert/Moderat motiviert/Wenig motiviert/Gar nicht motiviert)
                        - These provided structured, quantifiable data for easier analysis.
                - The mix of question types balanced depth and breadth in participant feedback.

            - **Implications of the Questionnaire:**
                - Open-ended questions yielded rich qualitative insights, potentially revealing nuanced perceptions of the task and feedback stimuli.

            - **Acknowledgment of Limitations:**
                - Being a custom-designed tool for this study, the questionnaire lacks established reliability and validity metrics, which weakens its robustness.

            - **Suggested Implementation in Your Paper:**
                - **Methods Section:**
                    - "Participants completed a custom evaluation questionnaire post-study to provide feedback on their experience. Comprising nine questions, it included open-ended items to explore their understanding of the study's goal and perceptions of the task, alongside closed-ended items to assess awareness and subjective impact of special audiovisual feedback stimuli on decision-making and motivation."

3.  **Procedure**
    *   **Overall Flow:** Describe the sequence of events for each participant: informed consent -> task instructions -> practice trials -> main task -> questionnaires -> debriefing.
        *   Upon arrival, participants were greeted by the experimenter.
        *   **Informed Consent:** Participants were provided with an informed consent form, asked to read it thoroughly, and given the opportunity to ask questions. It was emphasized that participation was voluntary and could be discontinued at any time without penalty. Two copies were signed and dated; one was retained by the participant, the other by the experimenter.
        *   **Task Introduction:** Participants were instructed to put on headphones and were informed they would be following instructions displayed on the computer screen to complete the experimental task.
        *   **Task Execution (Automated):** The PsychoPy script then guided participants through:
            *   Detailed on-screen instructions for the bandit task.
            *   A practice phase consisting of 15 trials.
            *   The main experimental phase consisting of 200 trials.
        *   The experimenter remained available to address any technical difficulties, intervening only if necessary.
        *   **Post-Task Questionnaires:** Immediately following the bandit task, participants completed a series of digital questionnaires on the same computer, including the BIS-15, the SSS, and experiment-specific questions regarding their experience.
        *   **Debriefing:** After completing the questionnaires, participants were thanked for their participation. They were briefly debriefed about the study's general purpose, which was to investigate the effects of salient audiovisual feedback (referred to as 'special effects' paired with some wins) on learning processes and decision-making behavior. They were also informed that they would receive an email with a link to the study results once available.
        *   **Compensation:** Participants received their predetermined compensation (either monetary payment or course credit). Those receiving monetary compensation signed a receipt.
    *   **Instructions:** Briefly summarize the instructions given to participants regarding the task goal (e.g., maximize points/rewards).
        *   Participants received all instructions digitally on-screen via the PsychoPy interface.
        *   **Core Task Goal:** They were informed their primary objective was to accumulate as many points as possible by choosing between two options in each trial.
        *   **Choice Mechanism:** Instructions specified using the left ('←') and right ('→') arrow keys to make their selection.
        *   **Immediate Feedback:** Participants were told they would see immediately after each choice whether they had won points or not.
        *   **Dynamic Probabilities:** It was explained that the likelihood of winning points with either option would change throughout the game, and their task was to figure out which option was currently better.
        *   **Stimulus vs. Position:** A key instruction emphasized that the specific symbol chosen was important, not the side of the screen (left/right) on which it appeared, as positions were randomized.
        *   **Salient Cue Introduction:** Participants were informed that some wins would be accompanied by "special effects" (the salient audiovisual feedback).
        *   **Bonus Information:** The possibility of earning a performance-based monetary bonus (up to €3.00) tied to their total points was described.
    *   **Practice Phase:** Mention the purpose (familiarization) and number of practice trials (15). Specify if feedback was provided.
        *   A practice phase consisting of **15 trials** preceded the main task.
        *   **Purpose:** The primary aim was to familiarize participants with the task mechanics (making choices, feedback presentation) and to ensure they understood the dynamic nature of reward probabilities and the meaning of both non-salient and salient feedback types before the main data collection began.
        *   **Salient Feedback Exposure:** Salient audiovisual feedback for wins was included in the practice phase, following the same variable ratio reinforcement schedule logic as in the main task. This was to prevent novelty effects associated with salient cues during the main task and to ensure participants understood the full range of possible feedback.
        *   **Distinct Stimuli:** Different fractal stimuli (e.g., green and purple) were used for the practice trials compared to the main task stimuli (e.g., red and blue). This was a deliberate choice to prevent any stimulus-specific learning from the practice phase from transferring to or contaminating behavior in the main experimental trials, ensuring the practice focused on task rules rather than specific stimulus-reward associations.
    *   **Main Task Phase:** State the number of main trials (200).
        *   The main experimental phase consisted of **200 trials**.
        *   This number of trials was chosen to provide sufficient data for analyzing learning dynamics, choice behavior, and the impact of the different feedback conditions.
    *   **Questionnaire Administration:** How and when were the questionnaires administered (e.g., via computer after the task)?
        *   Questionnaires were administered digitally on the same computer immediately following the completion of the main bandit task.
        *   This process was managed by a separate Python script (`questionnaire_survey.py`).
        *   The questionnaires were presented in the following fixed order:
            1.  Barratt Impulsiveness Scale (BIS-15)
            2.  Sensation Seeking Scale (SSS)
            3.  Experiment-Specific Evaluation Questionnaire
        *   This order was chosen to assess stable personality traits first, before questions related to the specific experimental experience, to minimize potential carry-over effects between the questionnaire types.

4.  **Task Design (Restless Two-Armed Bandit)**
    *   **Basic Structure:** Describe the two-choice task structure. Participants chose between two options (stimulus 1/`0`, stimulus 2/`1`) on each trial.
        *   Participants engaged in a two-choice decision-making task on each trial, selecting between two visual stimuli (e.g., fractal images described in Materials) presented concurrently.
        *   The on-screen positions (left or right) of these two stimuli were randomized on each trial. Participants were explicitly instructed that the identity of the chosen stimulus, not its spatial position, was relevant for obtaining rewards.
        *   Internally, choices were coded as `0` (mapping to one specific stimulus, e.g., the red fractal in main trials) or `1` (mapping to the other specific stimulus, e.g., the blue fractal).
    *   **Trial Timing:** Specify timing details: stimulus presentation duration, response window, inter-trial interval (ITI), feedback duration. *This information might be in your PsychoPy script.*
        *   **Stimulus Presentation:** The two choice stimuli were displayed until the participant made a response or the response window expired.
        *   **Response Window:** Participants had a maximum of **5 seconds** to make their choice using the arrow keys. Otherwise, the trial will be marked as "missed".
        *   **Feedback Presentation & Duration:** Feedback was presented immediately following a choice. Visual feedback (and auditory feedback, if applicable for salient wins) remained on screen for a fixed duration of **3 seconds**.
        *   **Inter-Trial Interval (ITI):** After the feedback offset, a central fixation cross ('+') was displayed for a variable duration. This ITI was drawn from a normal distribution with a mean of **1.0 second** and a standard deviation of **0.5 seconds**, with an enforced minimum ITI of **0.5 seconds**.
    *   **Reward Structure:** Explain that rewards (`1`) or no rewards (`0`) were delivered probabilistically. Argue, why binary outcomes were chosen. Crucially, describe the "restless" nature – how did the reward probabilities associated with each arm change over time? Describe the Gaussian Random Walk and its generation in detail. *This is a key detail of the restless bandit paradigm.*
        *   **Binary Outcomes:** A choice on any given trial resulted in one of two outcomes: a reward (win, numerically coded as `1`) or no reward (loss, numerically coded as `0`).
            *   *Rationale for Binary Outcomes:* This binary outcome structure was chosen to simplify the learning problem for participants, providing a clear distinction between winning and not winning. It aligns with many standard paradigms in reinforcement learning research and facilitates straightforward computational modeling of learning (e.g., Q-learning models where prediction errors are based on these unambiguous outcomes). This design focuses the investigation on how salience modulates the processing of win versus no-win events, rather than varying magnitudes of reward.
        *   **"Restless" Probabilistic Rewards (Decaying Gaussian Random Walk):** The probability of receiving a reward from choosing either of the two options changed independently and continuously throughout the experiment. This "restless" nature of the reward contingencies required participants to constantly track and adapt their choices. The underlying reward probabilities for each arm were pre-generated using a separate Python script (`random_walk_generation.ipynb`) based on a decaying Gaussian random walk model, similar to that used by Najar et al. (2020)/Daw et al. (2006), and loaded from CSV files (`main_random_walk.csv`, `prac_random_walk.csv`) by the main task script (`task_logic.py`).
            *   **Initial Probabilities (t₀):** On the first trial, the true underlying reward probability for each of the two arms was drawn independently from a Gaussian distribution with a mean ($\theta$) of **0.5** and a standard deviation ($\sigma_o$) of **0.04**.
            *   **Probability Update Rule:** For each subsequent trial $t+1$, the reward probability for each arm $i$ ($\mu_{i,t+1}$) was updated based on its value in the previous trial ($\mu_{i,t}$) using the formula: $\mu_{i,t+1} = \lambda \cdot \mu_{i,t} + (1 - \lambda) \cdot \theta + \nu_t$.
                *   The decay parameter, $\lambda$, was set to **0.9836**, causing the probability to slowly revert towards the decay center, $\theta$.
                *   The decay center, $\theta$, was **0.5**.
                *   The diffusion noise, $\nu_t$, was a random value drawn from a Gaussian distribution with a mean of 0 and a standard deviation ($\sigma_d$) of **0.065**. *(Note: The Python script used for data generation, `random_walk_generation.ipynb`, implemented $\sigma_d = 0.065$.)*
            *   **Anticyclic Correlation:** To enhance the "restless" nature of the task, the diffusion noise terms ($\nu_t$) for the two arms were partially negatively correlated. The noise for the second arm was generated based on the noise for the first arm using the formula: `noise_2 = correlation_factor * (-noise_1) + (1 - correlation_factor) * N(0, sigma_d)`, with a `correlation_factor` of **0.35**. This created a tendency where an increase in one arm's reward probability was often accompanied by a decrease in the other's.
            *   **Clipping:** All generated reward probabilities were clipped to ensure they remained within the valid [0, 1] range.
            *   **Probability Resets:** To prevent probabilities from becoming static at the boundaries (0 or 1) and to maintain task engagement, the underlying reward probabilities for *both* arms were simultaneously reset to the central value $\theta$ (0.5) at unpredictable intervals. These reset points occurred after a variable number of trials, with the interval length drawn from `abs(round(N(30, 5)))`, meaning resets happened approximately every 30 trials on average, but variably.
            *   **Reward Delivery on Trial:** The actual delivery of a reward on a given trial for a chosen arm $i$ was stochastic, determined by comparing a randomly drawn number (from a uniform distribution between 0 and 1) against the arm's true underlying reward probability $\mu_{i,t}$ for that trial. This was implemented by referencing pre-generated binary `payoff_1` and `payoff_2` columns in the loaded random walk CSV files.
    *   **Feedback Conditions:** Explain the manipulation:
        *   Rewards were followed by either non-salient (`condition == 0`) or salient (`condition == 1`) audiovisual feedback according to a variable reinforcement pattern (explain this pattern if possible, e.g., was it randomized, counterbalanced?).
            *   When a choice resulted in a reward (a "win"), the feedback provided was either **non-salient** (coded as `condition == 0` in the data) or **salient** (coded as `condition == 1`).
            *   **Reinforcement Schedule for Salient Feedback:** The type of feedback (salient or non-salient) delivered upon a win was determined by a predefined binary sequence (e.g., `[0, 0, 0, 1]`, where `1` represented a trial eligible for salient feedback upon win, and `0` for non-salient). This sequence was read from a CSV file (`vr_schedule.csv`) by `task_logic.py`. The script iterated through this sequence for each win trial. If the sequence element for the current win was `1`, salient feedback was delivered; otherwise, non-salient feedback was delivered. This created a variable ratio-like schedule for the occurrence of salient feedback specifically on rewarded trials, with (for example, using the `[0,0,0,1]` sequence) salient feedback being delivered on average for every fourth win.
        *   Specify feedback for non-rewarded trials.
            *   If a participant's choice did not result in a reward (a "loss"):
                *   The visual feedback was always **non-salient** (a static border appeared around the chosen, non-rewarded stimulus, and the text "+0 Punkte" was displayed).
                *   No specific "loss" sound effect was played; only the continuous background music was audible.
        *   Mention how missed trials (`choice == empty`, `condition == 2`) were handled (e.g., brief message, proceeded to next trial).
            *   If a participant failed to respond within the 5-second response window:
                *   The trial was recorded as a **missed trial** (coded as `condition == 2`).
                *   The message "Zu spät" (German for "Too late") was displayed on the screen for 2 seconds.
                *   No reward was delivered.
                *   The data for this trial included `choice = None` and `reaction_time = None`.
                *   Following the "Zu spät" message, the standard ITI was presented before the next trial began.
    *   **Data collection** Explain what data wer collected and how.
        *   **Trial-Level Data Logging:** The `task_logic.py` script meticulously recorded comprehensive data for each trial. This included:
            *   `participant_id`: The unique identifier for the participant.
            *   `mode`: The current phase of the experiment (`practice` or `main`).
            *   `trial`: The trial number (1-indexed, specific to its mode).
            *   `choice`: The participant's selection, coded as `0` for one stimulus (e.g., mapping to the 'red' fractal image in main trials) and `1` for the other (e.g., 'blue' fractal), or `None` if the trial was missed.
            *   `reaction_time`: The time in seconds from stimulus onset to the participant's response (`None` for missed trials).
            *   `reward`: The outcome of the choice, `1` for a win (reward received) and `0` for no win/loss.
            *   `condition`: An integer code indicating the feedback condition: `0` for non-salient feedback, `1` for salient feedback (only on win trials), and `2` for a missed trial.
            *   `reward_prob_1`, `reward_prob_2`: The true underlying reward probabilities for stimulus 1 and stimulus 2, respectively, on that specific trial, as per the pre-generated random walk data.
            *   `payoff_1`, `payoff_2`: The potential binary outcome (0 or 1) determined by the random walk for stimulus 1 and stimulus 2 on that trial, indicating if choosing that arm would have resulted in a win, irrespective of the participant's actual choice. reward probs and payoffs are just copied from the random_walk_data input
        *   **Data Storage:** All collected trial-by-trial data were saved in real-time to a comma-separated values (CSV) file. Each participant's data was stored in a uniquely named file, `{participant_id}_task_data.csv`, within the `bandit_task/collected_data/` directory. Each CSV file began with a header row listing all the recorded data fields.

5.  **Data Analysis**
    *   **Software:** Analyses will be primarily conducted using **R** (specific version to be reported alongside results). Bayesian hierarchical modeling will be performed using **Stan**, interfaced via the `rstan` package in R. Key R packages will include `rstan` for model fitting, `loo` for model comparison (e.g., LOOIC) and diagnostics, `bayesplot` and `ggplot2` for visualization of posteriors and model checks, `dplyr` and `tidyr` for data manipulation, and potentially `lme4`, `effsize`, and `car`/`performance` for frequentist analyses (like t-tests or ANOVAs for win-stay and PRP analyses if included) and their assumption checks.
    *   **Data Preprocessing:**
        *   All primary analyses will use data from the **main experimental trials** (200 trials per participant); practice trials will be excluded.
        *   The R script `StanList.R` (specifically the `StanCleaned` function) prepares the data for Stan. It selects main trials and structures choice, reward, and salient feedback condition data into arrays.
        *   **Handling of Missed Trials for RL Modeling:** Trials where the participant failed to make a choice within the 5-second window (recorded as `choice = None` or an equivalent placeholder like -9 in the prepared Stan data) are effectively excluded from the likelihood calculation within the Stan model by conditional checks on valid choice inputs. This means that the Q-values are not updated, and these trials do not directly contribute to the parameter estimation for those specific missed instances.
        *   No other systematic data cleaning steps, such as reaction time outlier removal, were applied prior to the reinforcement learning model fitting reported here.
    *   **Hypothesis 1.1: Reinforcement Learning Model**
        *   **Goal:** To investigate the influence of salient reward-paired cues on learning rates and choice consistency using a hierarchical Bayesian reinforcement learning (RL) model.
        *   **Model Structure:** A Q-learning model was implemented. For each participant, the model tracks the expected value (Q-value) of choosing each of the two options.
            *   **Q-value Update:** After each choice, the Q-value of the chosen option \(c\) is updated based on the Rescorla-Wagner learning rule: \( Q_{t+1}(c) = Q_t(c) + \alpha_{eff} \cdot \delta_t \).
            *   **Prediction Error (PE):** The prediction error \(\delta_t\) is calculated as the difference between the actual reward received \(R_t\) (1 for a win, 0 for no win/loss) and the expected Q-value of the chosen option: \( \delta_t = R_t - Q_t(c) \).
            *   **Choice Rule:** Participant choices are modeled using a softmax decision rule, where the probability of selecting an option is a function of its Q-value relative to the other option, scaled by an inverse temperature parameter \(\beta\).
        *   **Key Parameters (Estimated Hierarchically):**
            *   **\(\alpha\) (Base Learning Rate):** A subject-specific parameter (Phi-transformed to lie between 0 and 1) that quantifies the extent to which PEs update Q-values following **non-salient** feedback.
            *   **\(\alpha_{shift}\) (Alpha Shift by Salience):** A subject-specific parameter representing the additional change in learning rate due to **salient** feedback. The model estimates a raw, unbounded shift. An interpretable shift (the difference in the 0-1 learning rate when feedback is salient compared to non-salient, i.e., \(\alpha_{salient} - \alpha_{non-salient}\)) is derived in the generated quantities block of the Stan model. The effective learning rate (\(\alpha_{eff}\)) applied to the PE is thus \(\alpha\) for non-salient feedback, and conceptually \(\alpha + \alpha_{shift}\) (after appropriate transformations) for salient feedback. A positive \(\alpha_{shift}\) would indicate enhanced learning from salient feedback.
            *   **\(\beta\) (Inverse Temperature):** A subject-specific parameter (transformed to be positive, e.g., scaled between 0 and 10) reflecting choice consistency or the degree of exploration versus exploitation. Higher \(\beta\) values lead to more deterministic choices of the option with the highest Q-value.
        *   **Hierarchical Structure:** All subject-level parameters (\(\alpha\), \(\alpha_{shift}\), \(\beta\)) are drawn from group-level distributions (characterized by a mean and standard deviation), allowing for individual differences while also providing regularization through shared information across participants.
        *   **Implementation:** The model was specified in Stan (model file: `rl_cp_shift_uniform.stan`) and fitted using R (via `rstan`), with data preparation handled by `StanList.R` and model execution by `runModels.R`.
        *   **Priors:** Weakly informative priors were used for group-level parameters to allow the data to primarily drive the posterior estimates. For instance, group-level means for raw (untransformed) \(\alpha\) and \(\beta\) were given `uniform(-4, 4)` priors, the group-level mean for raw \(\alpha_{shift}\) was given a `normal(0, 1)` prior, and group-level standard deviations were given `uniform(0.001, 3)` priors.
        *   **Model Fitting Procedure:** The model was fitted using Hamiltonian Monte Carlo with multiple chains (e.g., 4 chains, each with 8000 iterations including 4000 warmup iterations). Specific sampler settings (e.g., `adapt_delta = 0.98`, `max_treedepth = 12`) were used to optimize sampling efficiency and address potential divergences. Sensible initial values for parameters were provided to each chain.
        *   **Model Evaluation:** Model convergence will be thoroughly assessed (e.g., by examining \(\hat{R}\) statistics, trace plots, and effective sample sizes). Posterior predictive checks will be performed to evaluate how well the model can replicate key features of the observed data (e.g., learning curves, choice patterns).
        *   **Reporting:** Inferences will be based on the posterior distributions of the parameters. Specifically, 95% Credible Intervals (CIs) will be reported for the interpretable group-level and subject-level parameters, with a particular focus on \(\alpha_{shift}\) to test the hypothesis regarding the impact of salient cues.
    *   **Hypothesis 1.2: Win-Stay Probability**
        *   Define "win-stay": choosing the same option on trial N+1 after being rewarded for that option on trial N.
        *   Explain calculation: Calculate separately for trials following non-salient wins vs. salient wins for each participant.
        *   Statistical Test: Paired t-test (or Wilcoxon signed-rank test if normality violated).
        *   Assumption Check: Shapiro-Wilk test for normality of differences.
        *   Reporting: p-value (α = 0.05), effect size (Cohen's d for paired samples), 95% CI for the mean difference.
    *   **Interindividual Differences:** Briefly state that exploratory analyses will examine relationships between questionnaire scores (BIS, SSS totals/subscales) and key model parameters (α, α_shift, β) or behavioral measures (win-stay difference, PRP differences), likely using correlations.

This detailed outline should cover all necessary aspects. Remember to fill in the specifics, especially regarding participant details, precise stimulus descriptions, the exact mechanics of the "restless" probabilities, and the definition used for PRPs. Good luck with writing!
