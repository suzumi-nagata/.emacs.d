;;; init-org.el --- org-mode configurations
;;; Commentary:
;;; Code:

(require 'org)

(setq-default major-mode 'org-mode)
(setq initial-major-mode 'org-mode)

(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      org-clock-into-drawer t
      org-log-into-drawer t
      org-link-file-path-type 'adaptive
      org-src-fontify-natively t
      org-agenda-window-setup 'current-window
      org-ellipsis "⬎"
      org-archive-location (concat "archive/" "%s_archive::")
      org-cycle-separator-lines 0
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (dot . t)
   (calc . t)
   (ditaa . t)
   (R . t)
   (C . t)
   (gnuplot . t)
   (shell . t)
   (ledger . t)
   (org . t)
   (picolisp . t)
   (clojure . t)
   (lilypond . t)
   (plantuml . t)
   (latex . t))
 )

;; latex preview font size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("⬢" "✤" "◉" "❄" "✧" "♠" "♣" "♥" "♦"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(bind-keys*
  ("M-h" . org-metaleft)
  ("M-l" . org-metaright))

;; easy templates
(add-to-list 'org-structure-template-alist '("box" . "mdframed"))
(add-to-list 'org-structure-template-alist '("ditaa" . "src ditaa :file images/.png"))

;; Use yas-minor mode for org and latex on org buffers
(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in 'org-mode' buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'my-org-latex-yas)

;; Permit org-export local variable as safe
(add-to-list 'safe-local-variable-values '(eval . (add-hook 'after-save-hook 'org-html-export-to-html t t)))

;;----------------------------------------------------------------------------
;; Agenda config
;;----------------------------------------------------------------------------

(setq org-todo-keywords
      (quote ((sequence "☛ TODO(t)" "↻ REDO(r@)" "➥ IN PROGRESS(p!)" "|")
              (sequence "⚑ WAITING(w@)" "➤ FORWARD(f@)" "◷ HOLD(h@)" "|")
              (sequence "⚡ NEXT(n!)" "|" "✘ CANCELLED(c@)" "✔ DONE(d!)")
              (sequence "Ω SOMEDAY(s!)" "⬆ URGENT(u!)" "|")))
      )

(setq org-todo-keyword-faces
      '(("☛ TODO" . "tomato")
        ("↻ REDO" . "yellow")
        ("⚡ NEXT" . "orange1")
        ("➥ IN PROGRESS" . "deep sky blue")
        ("◷ HOLD" . "NavajoWhite1")
        ("⚑ WAITING" . "gray")
        ("➤ FORWARD" . "gold3")
        ("✘ CANCELLED" . "lawn green")
        ("✔ DONE" . "green")
        ("Ω SOMEDAY" ."LightGoldenrod")
        ("⬆ URGENT" ."red3")
        ))

(setq org-agenda-files (list "~/Common/Agenda/work.org"
                             "~/Common/Agenda/refile.org"
                             "~/Common/Agenda/unicamp.org"
                             "~/Common/Agenda/personal.org"
                             "~/Common/Agenda/projects.org"
                             "~/Common/Agenda/reading.org"
                             "~/Common/Agenda/courses.org"
                             "~/Common/Agenda/PIBIC.org"
                             "~/Documents/1_Unicamp/EM524/EM524.org"
                             "~/Documents/1_Unicamp/GT001/GT001.org"
                             ))

;; Auto schedule depending on the first header
(defun my/org-after-refile-insert-hook ()
  (pcase (substring (car (org-get-outline-path)) 0 5)
    ("IE300" (org-deadline nil "Monday"))
    ("EM524" (org-deadline nil "Monday"))
    ("GT001" (org-deadline nil "Tuesday"))
    ("IA012" (org-deadline nil "Wednesday"))
    ("IE509" (org-deadline nil "Wednesday"))
    ("EA080" (org-deadline nil "Thursday"))
    ("PIBIC" (org-deadline nil "+2d"))
    ("CompSci" (org-deadline nil "+5d"))
    ("NEWS" (org-deadline nil "+7d"))
    ("Security" (org-deadline nil "+30d"))
    ))

(add-hook 'org-after-refile-insert-hook #'my/org-after-refile-insert-hook)

;; Color tags depending on category
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (save-excursion
              (color-org-header "PIBIC:" "#E10600" "#00239C")
              (color-org-header "EM524:" "DimGrey" "LawnGreen")
              (color-org-header "GT001:" "DarkBlue" "green1")
              (color-org-header "IE509:" "sienna4" "seashell1seashell1")
              (color-org-header "EA072:" "#EEA47F" "#00539C")
              (color-org-header "IA012:" "#2C5F2D" "#97BC62")
              (color-org-header "EA080:" "#D6ED17" "#1C1C1B")
              (color-org-header "estagios:" "#343136" "#D7C49E")
              (color-org-header "personal:" "#F2AA4C" "#101820")
              (color-org-header "unicamp:" "#422057" "#FCF951")
              (color-org-header "E.E.:" "#ADEFD1" "#00203F")
              ;; Silver & Turkish Sea
              (color-org-header "Emacs:" "#A2A2A1" "#195190")
              ;; Electric Blue Lemonade (#0063B2FF) and Aquamarine (#9CC3D5FF)
              (color-org-header "O.S.:" "#9CC3D5" "#0063B2")
              )))

(defun color-org-header (tag backcolor forecolor)
  ""
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward tag nil t)
    (add-text-properties (match-beginning 0) (+ (match-beginning 0) 10)
                         `(face (:background, backcolor, :foreground, forecolor)))))

;; Priorities
(use-package org-fancy-priorities
  :ensure t
  :diminish
  :after (org)
  :commands (org-fancy-priorities-mode)
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("I∧U" "I¬U" "¬IU" "¬I¬U")
                             "Eisenhower Matrix of Importance and Urgency"))

(setq org-priority-faces '((?A . (:foreground "green" :weight 'bold))
                           (?B . (:foreground "cyan"))
                           (?C . (:foreground "orange1"))
                           (?D . (:foreground "tomato"))
                           ))

(setq org-default-priority ?A)
(setq org-lowest-priority ?D)

;; (setq org-highest-priority ?A)
;; (setq org-default-priority ?D)
;; (setq org-lowest-priority ?G)

;; (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
;;                            (?B . (:foreground "orange1"))
;;                            (?C . (:foreground "yellow"))
;;                            (?D . (:foreground "green"))
;;                            (?E . (:foreground "blue"))
;;                            (?F . (:foreground "DarkViolet"))
;;                            (?G . (:foreground "DarkMagenta"))))

;; (setq org-fancy-priorities-list '((?A . "❗")
;;                                   (?B . "⬆")
;;                                   (?C . "⮬")
;;                                   (?D . "↕")
;;                                   (?E . "⮮")
;;                                   (?F . "⬇")
;;                                   (?G . "☕")

;; super agenda
;; TODO: finish reading the docs: https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :ensure t
  :init
  (setq org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Important And Urgent"
                :priority "A")
         (:name "Important But Not Urgent"
                :priority "B")
         (:name "Not Important And Urgent"
                :priority "C")
         (:name "Not Important Nor Urgent"
                :priority "D")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         ))
  :config
  (org-super-agenda-mode))

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                                 :todo "⚡ NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :face error
                                 :order 7)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Issues"
                                 :tag "Issue"
                                 :order 12)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Research"
                                 :tag "Research"
                                 :order 15)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "⚑ WAITING"
                                 :order 28)
                          (:name "In Progress"
                                 :todo "➥ IN PROGRESS"
                                 :order 20)
                          (:name "On Hold"
                                 :todo "◷ HOLD"
                                 :order 27)
                          (:name "Trivial"
                                 :priority<= "E"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

;;----------------------------------------------------------------------------
;; Org capture
;;----------------------------------------------------------------------------
(use-package doct
  :ensure t
  ;;recommended: defer until calling doct
  :commands (doct))

;; Taken from https://tecosaur.github.io/emacs-config/config.html (lots of good stuff)
(setq org-capture-templates
      (doct '((" Personal Todo"
               :keys "t"
               :file "~/Common/Agenda/refile.org"
               :prepend t
               :type entry
               :headline "Inbox"
               :children
               (("☛ Todo"
                 :keys "t"
                 :template ("* ☛ TODO %?\n"))
                ("⚡ Next"
                 :keys "n"
                 :template ("* ⚡ NEXT %?\n"))
                ("Ω Someday"
                 :keys "s"
                 :template ("* Ω SOMEDAY %?\n"))
                ("⬆ Urgent"
                 :keys "u"
                 :template ("* ⬆ URGENT %?\n")))
               )
              (" Journal"
               :keys "j"
               :file "~/Common/Agenda/journal.org"
               :type entry
               :datetree t
               :children
               ((" Journal Entry"
                 :keys "e"
                 :template ("* %U %^{Title}\n  %?\n"))
                (" Journal Prompt"
                 :keys "p"
                 :time-prompt t
                 :template ("* %^{Title}\n %?\n")))
               )
              (" Compras"
               :keys "c"
               :file "~/Common/Agenda/compras.org"
               :type plain
               :template ("- %?\n")
               )
              (" University"
               :keys "u"
               :file "~/Common/Agenda/refile.org"
               :headline "University"
               :unit-prompt "%%^{Unit|EM524|GT001}"
               :prepend t
               :type entry
               :children ((" Test"
                           :keys "t"
                           :template ("* ☛ TODO [#B] %{unit-prompt} %? :uni:tests:"
                                      "SCHEDULED: %^{Test date:}T"
                                      "%i %a"))
                          (" Assignment"
                           :keys "a"
                           :template ("* ☛ TODO [#B] %{unit-prompt} %? :uni:assignments:"
                                      "DEADLINE: %^{Due date:}T"
                                      "%i %a"))
                          (" Lecture"
                           :keys "l"
                           :template ("* ☛ TODO [#B] %{unit-prompt} %? :uni:lecture:"
                                      "%i %a"))
                          (" Miscellaneous task"
                           :keys "m"
                           :template ("* ☛ TODO [#B] %{unit-prompt} %? :uni:"
                                      "%i %a")
                           )))
              (" Email"
               :keys "e"
               :file "~/Common/Agenda/refile.org"
               :prepend t
               :headline "Mail"
               :type entry
               :template ("* ☛ TODO %^{type|reply to|contact} %\\3 %? :email:"
                          "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                          "about %^{topic}"
                          "%U %i %a"))
              (" Interesting"
               :keys "i"
               :file "~/Common/Agenda/refile.org"
               :prepend t
               :headline "Interesting"
               :type entry
               :template ("* [ ] %{desc}%? :%{i-type}:"
                          "%i %a")
               :children ((" Web"
                           :keys "w"
                           :desc "%(org-cliplink-capture) "
                           :i-type "read:web"
                           )
                          (" Article"
                           :keys "a"
                           :desc ""
                           :i-type "read:reaserch"
                           )
                          (" Information"
                           :keys "i"
                           :desc ""
                           :i-type "read:info"
                           )
                          (" Idea"
                           :keys "I"
                           :desc ""
                           :i-type "idea"
                           )))
              (" Tasks"
               :keys "k"
               :file "~/Common/Agenda/refile.org"
               :prepend t
               :headline "Tasks"
               :type entry
               :template ("* TODO %? %^G%{extra}"
                          "%i")
               :children ((" General Task"
                           :keys "k"
                           :extra "")
                          (" Task with deadline"
                           :keys "d"
                           :extra "\nDEADLINE: %^{Deadline:}t")
                          (" Scheduled Task"
                           :keys "s"
                           :extra "\nSCHEDULED: %^{Start time:}t")))
              (" PIBIC"
               :keys "p"
               :prepend t
               :type entry
               :headline "Inbox"
               :file ""
               :template ("* %{time-or-todo} %?"
                          "%i"
                          "%a")
               :custom (:time-or-todo "")
               :children ((" Project todo"
                           :keys "t"
                           :time-or-todo "☛ TODO"
                           :file "~/Common/PIBIC/3_org_files/captures/todo.org")
                          (" Project note"
                           :keys "n"
                           :time-or-todo "%U"
                           :file "~/Common/PIBIC/3_org_files/captures/notes.org")
                          (" Project changelog"
                           :keys "c"
                           :time-or-todo "%U"
                           :heading "Unreleased"
                           :file "~/Common/PIBIC/3_org_files/captures/changelog.org"))
               ))))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-files :maxlevel . 9)))

;;----------------------------------------------------------------------------
;; Org wiki
;;----------------------------------------------------------------------------

(unless (package-installed-p 'org-wiki)
  (let ((url "https://raw.githubusercontent.com/caiorss/org-wiki/master/org-wiki.el"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (kill-whole-line)
      (package-install-from-buffer)))
  )

(require 'org-wiki)
(setq org-wiki-location-list '("~/Common/Wiki"))

;; Initialize first org-wiki-directory or default org-wiki
(setq org-wiki-location (car org-wiki-location-list))

(setq org-wiki-close-root-switch t)

(setq org-wiki-server-port "8000")

(setq org-wiki-server-host "127.0.0.1") ;; Listen only localhost

;;----------------------------------------------------------------------------
;; Usefull functions
;;----------------------------------------------------------------------------

;; ros takes a screenshot of a region and put in org format
(defun ros ()
  (interactive)
  (if buffer-file-name
      (progn
        (message "Waiting for region selection with mouse...")
        (let ((filename
               (concat "./images/"
                       (format-time-string "%Y%m%d_%H%M%S")
                       ".png")))
          (unless (file-exists-p "./images")
            (make-directory "./images"))
          (if (executable-find "scrot")
              (call-process "scrot" nil nil nil "-s" filename)
            (call-process "screencapture" nil nil nil "-s" filename))
          (insert (concat "[[file:" filename "]]"))
          )
        (message "File created and linked...")
        )
    (message "You're in a not saved buffer! Save it first!")))

;; Insert setupfiles
(defun bigblow-setupfile ()
  "Insert setupfile at point."
  (interactive)
  (insert "#+SETUPFILE: ./org-html-themes/setup/theme-bigblow-local.setup")
  (newline)
  )

(defun readtheorg-setupfile ()
  "Insert setupfile at point."
  (interactive)
  (insert "#+SETUPFILE: ./org-html-themes/setup/theme-readtheorg-local.setup")
  (newline)
  )

;; Create notes in a directory with date
(defun today-notes (folder-name)
  "Create a directory with today's date with FOLDER-NAME."
  (interactive "sFolder name: ")
  (let ((final-folder-name))
    (if (not (string= "" folder-name))
        (setq final-folder-name (concat "-" folder-name))
      (setq final-folder-name folder-name)
      )
    (let ((daily-name (format-time-string "%Y-%m-%d")))
      (make-directory (concat daily-name final-folder-name "/images") :parents)
      (find-file (expand-file-name (concat daily-name final-folder-name "/notes.org")))
      (insert (concat "#+TITLE:" folder-name))
      (newline)
      (insert "#+SETUPFILE: /home/nagata/Common/Templates/org-html-themes/setup/theme-readtheorg-local.setup")
      )
    )
  )

;; create workspace for assignment
(defun relatorio ()
  "Docstring."
  (interactive)
  (let ((dir-name "relatorio/images"))
    (make-directory dir-name :parents)
    (find-file (expand-file-name "relatorio/relatorio.org"))
    (insert-org-latex-macros)
    (save-buffer)
    )
  )

(provide 'init-org)
;;; init-org.el ends here
