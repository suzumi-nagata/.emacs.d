;;; init-org.el --- org-mode configurations
;;; Commentary:
;;; Code:

(straight-override-recipe
   '(org :type git :host github :repo "emacsmirror/org" :no-build t))
(use-package org :straight t)

(require 'org)
(require 'org-habit)
(setq-default major-mode 'org-mode)
(setq initial-major-mode 'org-mode)

(setq org-directory "/home/nagata/Common/Backup/Org/"
      org-ellipsis "⬎"
      org-archive-location (concat "archive/" "%s_archive::")
      org-log-into-drawer t
      org-src-fontify-natively t
      org-export-coding-system 'utf-8
      org-use-sub-superscripts "{}"
      org-startup-folded "fold"
      org-agenda-window-setup 'current-window
      org-refile-targets '((org-agenda-files :level . 1))
      ;; org-tags-column 55
      org-noter-notes-window-location 'other-frame
      org-format-latex-options (plist-put org-format-latex-options :scale 1.7)
      org-roam-server-port 1784
      org-habit-graph-column 70
      org-display-remote-inline-images 'download
      org-adapt-indentation t
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      )

(defvar org-agenda-directory (concat org-directory "agenda/"))
(setq org-agenda-files (list org-agenda-directory))
(defvar org-capture-todo-file (concat org-agenda-directory "inbox.org"))
(defvar org-capture-reading-file (concat org-agenda-directory "reading.org"))
(defvar org-capture-email-file (concat org-agenda-directory "email.org"))
(defvar org-capture-groceries-file (concat org-agenda-directory "groceries.org"))

(use-package org-bullets :straight t
  :init (setq org-bullets-bullet-list '("⬢" "✤" "◉" "❄" "✧" "▶" "◆" "✿" "✸"))
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package doct
  :straight t
  :commands (doct))

;; Taken from https://tecosaur.github.io/emacs-config/config.html (lots of good stuff)
;; Note: uni-units is a list of names separated by newlines
(setq org-todo-keywords
      (quote ((sequence "☛ TODO(t)"
                        "➥ PROG(p!)"
                        "⚑ WAIT(w@)"
                        "◷ HOLD(h@)"
                        "|"
                        "✔ DONE(d!)"
                        "✘ CXLD(c@)"))))

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))))

(setq org-todo-keyword-faces
      '(("☛ TODO" . "tomato")
        ("➥ PROG" . "deep sky blue")
        ("◷ HOLD" . "NavajoWhite1")
        ("⚑ WAIT" . "gray")
        ("✔ DONE" . "green")
        ("✘ CXLD" . "lawn green")))

(set-face-attribute 'org-headline-done nil :strike-through t)

(setq org-capture-templates
      (doct '(("Personal todo"
               :keys "t"
               :file org-capture-todo-file
               :prepend t
               :type entry
               :headline "Inbox"
               :children
               (("Todo"
                 :keys "t"
                 :template ("* ☛ TODO %?"))
                ("Urgent"
                 :keys "u"
                 :template ("* ☛ TODO [#A] %^{Title} "
                            "DEADLINE: %^{Due date:}t"))))
              ("Web"
               :keys "w"
               :file org-capture-todo-file
               :prepend t
               :type entry
               :headline "Web Captures"
               :template ("* ☛ TODO [[%:link][%:description]]\n\n %i")
               :immediate-finish t
               )
              ("Email"
               :keys "e"
               :file org-capture-email-file
               :prepend t
               :type entry
               :immediate-finish t
               :children
               (("Send"
                 :keys "s"
                 :template ("* ☛ TODO [#A] Send %^{recipient}: %^{subject}"))
                ("Reply"
                 :keys "r"
                 :template ("* ☛ TODO [#A] Reply %^{recipient}: %^{subject}"))))
              ("Groceries"
               :keys "g"
               :file org-capture-groceries-file
               :type plain
               :template ("- %?")
               )
              )))

;TODO: Migrate to Super agenda
(setq-default org-agenda-custom-commands `((" " "Agenda"
                                            ((agenda ""
                                                     ((org-agenda-start-day "-2d")
                                                      (org-agenda-span 10)
                                                      (org-deadline-warning-days 365)))
                                             (todo "☛ TODO"
                                                   ((org-agenda-overriding-header "To Refile")
                                                    (org-agenda-files '(,(expand-file-name org-capture-todo-file)))))
                                             (todo "☛ TODO"
                                                   ((org-agenda-overriding-header "Emails")
                                                    (org-agenda-files '(,(expand-file-name org-capture-email-file)))))
                                             ;; (tags "CATEGORY=\"Reading\""
                                             ;;       ((org-agenda-overriding-header "Reading")
                                             ;;        (org-agenda-files '(,(expand-file-name org-capture-reading-file)))))
                                             ;; (todo "CATEGORY=\"To read\""
                                             ;;       ((org-agenda-overriding-header "To read")
                                             ;;        (org-agenda-files '(,(expand-file-name org-capture-reading-file)))))
                                             ;; (todo "CATEGORY=\"To write\""
                                             ;;       ((org-agenda-overriding-header "To write")
                                             ;;        (org-agenda-files '(,(expand-file-name org-capture-reading-file)))))
                                             ))
                                           ("r" "Review"
                                            ((agenda ""
                                                     ((org-agenda-start-day "-7d")
                                                      (org-agenda-span 8)
                                                      (org-deadline-warning-days 365)))))
                                           ))

;; (setq org-agenda-custom-commands `((" " "Agenda"
;;                                       ((agenda ""
;;                                                ((org-agenda-start-day "-2d")
;;                                                 (org-agenda-span 10)
;;                                                 (org-deadline-warning-days 365)))
;;                                        (todo "☛ TODO"
;;                                              ((org-agenda-overriding-header "Inbox")
;;                                               (org-agenda-files '(,(expand-file-name "inbox.org" jethro/org-agenda-directory)))))
;;                                        (todo "☛ TODO"
;;                                              ((org-agenda-overriding-header "Emails")
;;                                               (org-agenda-files '(,(expand-file-name "emails.org" jethro/org-agenda-directory)))))
;;                                        (todo "NEXT"
;;                                              ((org-agenda-overriding-header "In Progress")
;;                                               (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
;;                                        (todo "TODO"
;;                                              ((org-agenda-overriding-header "Active Projects")
;;                                               (org-agenda-skip-function #'jethro/skip-projects)
;;                                               (org-agenda-files '(,(expand-file-name "projects.org" jethro/org-agenda-directory)))))
;;                                        (todo "TODO"
;;                                              ((org-agenda-overriding-header "One-off Tasks")
;;                                               (org-agenda-files '(,(expand-file-name "next.org" jethro/org-agenda-directory)))
;;                                               (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))

(use-package org-pretty-tags
  :after all-the-icons
  :straight t
  :config
  (setq org-pretty-tags-surrogate-strings
        `(("UNICAMP"      . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-silver  :v-adjust 0.01))
          ("@home"        . ,(all-the-icons-material "home"           :face 'all-the-icons-purple  :v-adjust 0.01))
          ("cryptography" . ,(all-the-icons-material "vpn_key"        :face 'all-the-icons-lorange  :v-adjust 0.01))
          ("programming"  . ,(all-the-icons-faicon   "terminal"       :face 'all-the-icons-lsilver :v-adjust 0.01))
          ("lecture"      . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
          ("email"        . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
          ("read"         . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
          ("article"      . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
          ("web"          . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
          ("info"         . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
          ("issue"        . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
          ("someday"      . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
          ("idea"         . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
          ("emacs"        . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
  (org-pretty-tags-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          Agenda processing          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custom/org-agenda-bulk-mark-regexp-category (regexp)
  "Mark entries whose category match REGEXP for future agenda bulk action."
  (interactive "sMark entries with category matching regexp: ")
  (let ((entries-marked 0) txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (and (re-search-forward regexp nil t)
                  (setq category-at-point
                        (get-text-property (match-beginning 0) 'org-category)))
        (if (get-char-property (point) 'invisible)
            (beginning-of-line 2)
          (when (string-match-p regexp category-at-point)
            (setq entries-marked (1+ entries-marked))
            (call-interactively 'org-agenda-bulk-mark)))))
    (unless entries-marked
      (message "No entry matching this regexp."))))

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(setq org-tag-alist '(("@office" . ?o)
                      ("UNICAMP" . ?u)
                      ("@home" . ?h)
                      ("programming" . ?p)
                      ("projects" . ?ç)
                      ("cryptography" . ?c)
                      ;; (:newline)
                      ;; ("CANCELLED" . ?c)
                      ))

(setq org-refile-allow-creating-parent-nodes 'confirm)

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "✔ DONE" 'file))
(require 'find-lisp)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "f") 'jethro/org-process-inbox)))

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (custom/org-agenda-bulk-mark-regexp-category "inbox")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defvar nagata/org-current-effort-list '(("0:15" . "0:15")
                                         ("0:30" . "0:30")
                                         ("0:45" . "0:45")
                                         ("1:00" . "1:00")
                                         ("1:15" . "1:15")
                                         ("1:30" . "1:30")
                                         ("1:45" . "1:45")
                                         ("2:00" . "2:00")
                                         ("2:30" . "2:30")
                                         ("3:00" . "3:00")
                                         ("4:00" . "4:00")
                                         ("5:00" . "5:00")
                                         ("6:00" . "6:00")
                                         ("7:00" . "7:00")
                                         ("8:00" . "8:00")))

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  ;; (interactive
  ;;  (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (interactive (list (completing-read
                      "Effort: "
                      nagata/org-current-effort-list
                      nil nil)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the 'org-agenda'."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

;; (map! :map org-agenda-mode-map
;;       "i" #'org-agenda-clock-in
;;       "I" #'jethro/clock-in-and-advance
;;       "r" #'jethro/org-process-inbox
;;       "R" #'org-agenda-refile
;;       "c" #'jethro/org-inbox-capture)

(defun jethro/advance-todo ()
  (org-todo 'right)
  (remove-hook 'org-clock-in-hook #'jethro/advance-todo))

(defun jethro/clock-in-and-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook 'jethro/advance-todo)
  (org-agenda-clock-in))

;; (use-package org-clock-convenience
;;   :bind (:map org-agenda-mode-map
;;               ("<S-up>" . org-clock-convenience-timestamp-up)
;;               ("<S-down>" . org-clock-convenience-timestamp-down)
;;               ("o" . org-clock-convenience-fill-gap)
;;               ("e" . org-clock-convenience-fill-gap-both)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Org roam              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam :straight t
  :init
  (setq org-roam-directory "/home/nagata/Common/Backup/Org/roam"
        org-roam-db-location "/home/nagata/Common/Backup/Org/roam/org-roam.db"
        org-roam-index-file "/home/nagata/Common/Backup/Org/roam/20210117203010-index.org"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher '("Index")
        org-roam-buffer-width 0.3
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :unnarrowed t
           :head "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+DATE: %u\n
- links :: \n\n")
          ("c" "concept" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "concepts/${slug}"
           :unnarrowed t
           :head "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+DATE: %u\n
- links :: \n\n")
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private/${slug}"
           :unnarrowed t
           :head "#+title: ${title}\n")
          ("r" "recipes" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "recipes/${slug}"
           :unnarrowed t
           :head "#+TITLE: ${title}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+DATE: %u\n
- links :: \n
* Ingredients
* Directions
* Notes")))

  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :immediate-finish t
           :file-name "websites/%<%Y%m%d%H%M%S>-${slug}"
           :unnarrowed t
           :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}\n
#+ROAM_TAGS:
- links ::
- source :: ${ref}\n\n"))))

(use-package org-roam-server :straight t)

(require 'simple-httpd)
;; (setq httpd-port 1784)
(require 'org-protocol)
(require 'org-roam-protocol)

(defun org-roam-start-server()
  "Start Emacs server and roam server."
  (interactive)
  (server-start)
  (org-roam-server-mode))

(require 'server)
(unless (server-running-p)
  (org-roam-start-server))

(use-package deft :straight t
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-noter :straight t)
(use-package org-fragtog :straight t :hook (org-mode . org-fragtog-mode))
(use-package org-journal :straight t
  :custom
  (org-journal-dir "/home/nagata/Common/Backup/Org/roam/journal")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))
(use-package org-download :straight t)
(use-package org-ref :straight t
    :after org
    :init
    (setq org-ref-notes-directory "/home/nagata/Common/Backup/Org/bib/notes.org"
          org-ref-bibliography-notes "/home/nagata/Common/Backup/Org/bib/articles.org"
          org-ref-default-bibliography '("/home/nagata/Common/Backup/Org/bib/my_library.bib")
          org-ref-pdf-directory "/home/nagata/Common/Backup/Zotero"))

(use-package ivy-bibtex :straight t
  :after org
  :init
  (setq bibtex-format-citation-functions
        '((org-mode . (lambda (x) (insert (concat
                                           "\\cite{"
                                           (mapconcat 'identity x ",")
                                           "}")) ""))))
  (setq bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("/home/nagata/Common/Backup/Org/bib/my_library.bib")
        bibtex-completion-library-path '("/home/nagata/Common/Backup/Zotero/")
        bibtex-completion-notes-path "/home/nagata/Common/Backup/Org/bib/articles.org"
        ))

(use-package org-roam-bibtex :straight t
  :load-path "/home/nagata/Common/Backup/Org/bib/my_library.bib" ;Modify with your own path
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions))))
(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
(setq orb-preformat-keywords   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("n" "ref+noter" plain (function org-roam-capture--get-point)
         ""
         :file-name "bib/%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:

- links ::
- keywords :: ${keywords}\n
\* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:"
)))

(fset 'highlight-investment-table
   (kmacro-lambda-form [?\M-x ?h ?i ?g ?h ?l ?i ?g ?h ?t ?- ?r ?e ?g ?e ?x ?p return ?E ?T ?F return return ?\M-x ?h ?i ?g ?h ?l ?i ?g ?h ?t ?- ?r ?e ?g ?e ?\C-n return ?F ?I ?I return return ?\M-x ?h ?i ?g ?h ?l ?i ?i backspace backspace ?i ?g ?h ?t ?- ?r ?e ?g ?e ?\C-n return ?S ?t ?o ?c ?k return return ?\M-x ?h ?i ?g ?h ?l ?i ?g ?h ?t ?- ?r ?e ?g ?e ?\C-n return ?F ?i ?x ?e ?d return return] 0 "%d"))

;; (use-package org-transclusion
;;   :straight (:host github :repo "nobiot/org-transclusion" :branch "main" :files ("*.el"))
;;   :config
;;   (set-face-attribute
;;    'org-transclusion-fringe nil
;;    :foreground "DarkSlateBlue"
;;    :background "MidnightBlue")
;;   (org-transclusion-mode)
;;   )

(add-hook 'org-mode-hook #'(lambda ()
                             (ispell-change-dictionary "en_US")
                             (flyspell-mode)))

(provide 'init-org)
;;; init-org.el ends here
