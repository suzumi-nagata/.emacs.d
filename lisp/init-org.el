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

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-bullet-list
        '("⬢" "✤" "○" "◉" "✧" "♠" "♣" "♥" "♦"))
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
              (sequence "⚡ NEXT(n!)" "|" "✘ CANCELLED(c@)" "✔ DONE(d!)")))
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
(setq org-highest-priority ?A)
(setq org-default-priority ?D)
(setq org-lowest-priority ?G)

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "orange1"))
                           (?C . (:foreground "yellow"))
                           (?D . (:foreground "green"))
                           (?E . (:foreground "blue"))
                           (?F . (:foreground "DarkViolet"))
                           (?G . (:foreground "DarkMagenta"))))

;;----------------------------------------------------------------------------
;; Org capture
;;----------------------------------------------------------------------------
(setq org-capture-templates
      '(
        ("t" "Todo")
        ("tt" "Todo Entry"
         entry (file "~/Common/Agenda/refile.org")
         "* ☛ TODO %?\n")
        ("ts" "Todo with Clipboard"
         entry (file "~/Common/Agenda/refile.org")
         "* ☛ TODO %?\n  %x")
        ("tn" "NEWS"
         entry (file "~/Common/Agenda/refile.org")
         "* (NEWS) %?\n  %x")

        ("j" "Journal")
        ("jn" "Journal Entry"
         entry (file+datetree "~/Common/Agenda/journal.org")
         "* %U %^{Title}\n  %?\n")
        ("js" "Journal Clipboard"
         entry (file+datetree "~/Common/Agenda/journal.org")
         "* %U %?\n  %x\n")
        ("jc" "Journal with context" plain
         (file+datetree+prompt "~/Common/Agenda/journal.org")
         "%K - %a\n%i\n%?\n")

        ("c" "Compras"
         entry (file "~/Common/Agenda/compras.org")
         "* %?\n")

        ("p" "PIBIC"
         entry (file "~/Common/Agenda/PIBIC.org")
         "* ☛ TODO %?\n")

        ("ç" "Generic without todo"
         entry (file "~/Common/Agenda/refile.org")
         "* %?\n")

        ("n" "Note" entry (file "~/Common/Agenda/refile.org")
         "* NOTE %?\n%U" :empty-lines 1)
        ("N" "Note with Clipboard" entry (file "~/Common/Agenda/refile.org")
         "* NOTE %?\n%U\n   %x" :empty-lines 1)
        )
      )

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

(provide 'init-org)
;;; init-org.el ends here
