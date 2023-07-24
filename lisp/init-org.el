;;; init-org.el --- org-mode configurations
;;; Commentary:
;;; Code:

(straight-override-recipe
   '(org :type git :host github :repo "emacsmirror/org"))
(use-package org :straight t)

(require 'org)
(require 'org-habit)
(setq-default major-mode 'org-mode)
(setq initial-major-mode 'org-mode)

(setq org-directory "/home/nagata/Org/"
      org-ellipsis "⬎"
      org-archive-location (concat "archive/" "%s_archive::")
      org-log-into-drawer t
      org-src-fontify-natively t
      org-export-coding-system 'utf-8
      org-use-sub-superscripts "{}"
      org-startup-folded t
      org-agenda-window-setup 'current-window
      org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :level . 1))
      org-tags-column 80
      org-noter-notes-window-location 'other-frame
      org-format-latex-options (plist-put org-format-latex-options :scale 1.8)
      org-roam-server-port 1784
      org-startup-with-inline-images 'inlineimages
      org-habit-graph-column 70
      org-display-remote-inline-images 'download
      org-adapt-indentation t
      org-edit-src-content-indentation 0
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-tags-sort-function 'org-string-collate-lessp
      )

(setq org-agenda-sorting-strategy
  '((agenda time-up priority-down category-keep)
    (todo   priority-down category-keep)
    (tags   priority-down category-keep)
    (search category-keep)))

(defvar org-agenda-directory (concat org-directory "agenda/"))
;; (defvar org-journal-last-eight-days-files (org-journal-last-files (concat org-directory "roam/journal/")))
;; (setq org-agenda-files (append org-agenda-directory org-journal-last-eight-days-files))
;; (setq org-agenda-files (append (list org-agenda-directory) org-journal-last-eight-days-files))

(defvar org-study-backlog-file (concat org-directory "roam/" "20221121131908-study_backlog.org"))

;; (defvar org-mo611-file (concat org-directory "roam/" "20230304135847-unicamp_mo611_teleprocessamento_e_redes.org"))
;; (defvar org-mo601-file (concat org-directory "roam/" "20230304142110-unicamp_mo601_arquitetura_de_computadores_ii.org"))

(setq org-agenda-files (append (directory-files-recursively org-agenda-directory ".org$")
                        (list org-study-backlog-file)))

(defvar org-capture-todo-file (concat org-agenda-directory "inbox.org"))
(defvar org-capture-wishlist-file (concat org-agenda-directory "3-resources/wishlist/wishlist.org"))
(defvar org-capture-email-file (concat org-agenda-directory "2-areas/emailing/email.org"))
(defvar org-capture-professional-file (concat org-agenda-directory "2-areas/professional/listings.org"))

(use-package org-bullets :straight t
  :init (setq org-bullets-bullet-list '("⬢" "✤" "◉" "❄" "✧" "▶" "◆" "✿" "✸"))
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package doct
  :straight t
  :commands (doct))

;; Taken from https://tecosaur.github.io/emacs-config/config.html (lots of good stuff)
;; Note: uni-units is a list of names separated by newlines
(setq org-todo-keywords
      (quote ((sequence "> TODO(t)"
                        "# PROG(p)"
                        "$ WAIT(w@)"
                        "& HOLD(h@)"
                        "|"
                        "V DONE(d)"
                        "X CXLD(c@)"))))

(setq org-priority-lowest ?E)

(setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                           (?B . (:foreground "yellow"))
                           (?C . (:foreground "green"))
                           (?D . (:foreground "blue"))
                           (?E . (:foreground "gray"))))

(setq org-todo-keyword-faces
      '(("> TODO" . "tomato")
        ("# PROG" . "deep sky blue")
        ("$ WAIT" . "gray")
        ("& HOLD" . "NavajoWhite1")
        ("V DONE" . "green")
        ("X CXLD" . "lawn green")))

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
                 :template ("* > TODO %?"))
                ("Urgent"
                 :keys "u"
                 :template ("* > TODO [#A] %^{Title} "
                            "DEADLINE: %^{Due date:}t"))))
              ("Web"
               :keys "w"
               :file org-capture-todo-file
               :prepend t
               :type entry
               :headline "Web Captures"
               :template ("* > TODO [[%:link][%:description]]\n\n %i")
               :immediate-finish t
               )
              ("org-html"
               :keys "h"
               :file org-capture-todo-file
               :prepend t
               :type entry
               :headline "Web Captures"
               :template ("* > TODO [[%:link][%:description]]\n\n%?%:initial")
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
                 :template ("* > TODO [#A] Send email to %^{recipient}: %^{subject}"))
                ("Reply"
                 :keys "r"
                 :template ("* > TODO [#A] Reply email to %^{recipient}: %^{subject}"))))
              )))

;TODO: Migrate to Super agenda
(setq-default org-agenda-custom-commands
              `((" " "Agenda"
                 ((agenda ""
                          ((org-agenda-start-day "-2d")
                           (org-agenda-span 10)
                           (org-deadline-warning-days 365)))
                  (todo ""
                        ((org-agenda-overriding-header "To Refile")
                         (org-agenda-files '(,(expand-file-name org-capture-todo-file)))))
                  (todo ""
                        ((org-agenda-overriding-header "Wishlist")
                         (org-agenda-files '(,(expand-file-name org-capture-wishlist-file)))))
                  (todo ""
                        ((org-agenda-overriding-header "Emails")
                         (org-agenda-files '(,(expand-file-name org-capture-email-file)))))
                  (todo ""
                        ((org-agenda-overriding-header "Carrer")
                         (org-agenda-files '(,(expand-file-name org-capture-professional-file)))))
                 ))
                ("r" "Review"
                 ((agenda ""
                          ((org-agenda-start-day "-7d")
                           (org-agenda-span 8)
                           (org-deadline-warning-days 365)
                           (org-agenda-archives-mode t)))))
                ))

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

(setq org-refile-allow-creating-parent-nodes 'confirm)

(defun jethro/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "V DONE" 'file))
(require 'find-lisp)

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "i") 'jethro/org-process-inbox)))

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

(defun jethro/advance-todo ()
  (org-todo 'right)
  (remove-hook 'org-clock-in-hook #'jethro/advance-todo))

(defun jethro/clock-in-and-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook 'jethro/advance-todo)
  (org-agenda-clock-in))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               Org roam              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam :straight t
  :init
  (setq org-roam-v2-ack t)
  (setq-default org-roam-directory (file-truename "/home/nagata/Org/roam"))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))

  :config
  ;; bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-key org-roam-mode-map (kbd "<f9>") 'org-hide-properties)
  ;; journal ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %t %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%A, %d %B %Y>
#+FILETAGS: Journal

\n")
           :unnarrowed t)))
  ;; templates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Normal templates
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+FILETAGS:
- links ::\n\n")
           :immediate-finish t
           :unnarrowed t)

          ("c" "course" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+FILETAGS: course
- equivalent course ::
- links ::

* Syllabus
* References
* Classes
  Tuesday: 21:00 - 23:00
  Thursday: 19:00 - 21:00
* Dates [/]
* Grades:
  | Grade | Range      |
  |-------+------------|
  | A     | 8.5 - 10.0 |
  | B     | 7.0 - 8.5  |
  | C     | 5.0 - 7.0  |
  | D     | < 5.0      |
")
           :immediate-finish t
           :unnarrowed t)

          ("r" "bibliography reference" plain
           (file "~/.emacs.d/templates/org-roam-bib-template.org")
           :if-new (file+head "bib/${citekey}.org" "#+TITLE: ${title}\n")
           :unnarrowed t)

          ("c" "cooking recipe" plain "%?"
           :if-new (file+head "recipes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+FILETAGS:
- links ::

* Ingredients
* Directions
* Tips
")
           :unnarrowed t)
          ))

  ;; Org roam protocol templates
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "websites/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+FILETAGS:
- source :: ${ref}
- links ::\n\n"
)
           ;; :immediate-finish t
           :unnarrowed t
           )))

  ;; roam buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; for org-roam-buffer-toggle
  ;; Recommendation in the official manual
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.3)
                 (window-height . fit-window-to-buffer)))

  ;; mini buffer prompt ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; pretty print
  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (format "(%s)" (car (f-split dirs)))
      ""))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                                  :from links
                                  :where (= dest $s1)
                                  :and (= type "id")]
                         (org-roam-node-id node)))))
      (format "[%d]" count)))

  (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
    "Return the file TITLE for the node."
    (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-filetitle node)))
      (concat
       (if (> level 0) (concat filetitle " > "))
       (if (> level 1) (concat (string-join olp " > ") " > "))
       title))
    )

  (setq org-roam-node-display-template "${directories:10} ${hierarchy:100} ${tags:40} ${backlinkscount:6}")

  ;; start org roam ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (org-roam-db-autosync-mode))

(require 'org-protocol)
(use-package org-roam-protocol
  :after org-protocol)

(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

(use-package websocket :after org-roam
  :straight (:host github :repo "ahyatt/emacs-websocket" :branch "main"))

(use-package simple-httpd :straight t
  :after org-roam)

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package deft :straight t
  :after org
  :config
  ;; Overwrite `deft-current-files` for the `deft-buffer-setup` and limit it to 30 entries
  (defun anks-deft-limiting-fn (orig-fun &rest args)
    (let
        ((deft-current-files (-take 30 deft-current-files)))
      (apply orig-fun args)))

  (advice-add 'deft-buffer-setup :around #'anks-deft-limiting-fn)
  (setq deft-file-limit 30)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))
(use-package org-noter :straight t)
(use-package org-fragtog :straight t :hook (org-mode . org-fragtog-mode))
(use-package org-download :straight t)

(use-package org-ref :straight t
    :after org
    :init
    (setq org-ref-notes-directory "/home/nagata/Org/bib/notes.org"
          org-ref-bibliography-notes "/home/nagata/Org/bib/articles.org"
          org-ref-default-bibliography '("/home/nagata/Org/bib/my_library.bib")
          org-ref-pdf-directory "/home/nagata/Backup/Zotero"))

(use-package ivy-bibtex :straight t
  :after org
  :init
  (setq bibtex-format-citation-functions
        '((org-mode . (lambda (x) (insert (concat
                                           "\\cite{"
                                           (mapconcat 'identity x ",")
                                           "}")) ""))))
  (setq bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("/home/nagata/Org/bib/my_library.bib")
        bibtex-completion-library-path '("/home/nagata/Backup/Zotero/")
        bibtex-completion-notes-path "/home/nagata/Org/bib/articles.org"
        bibtex-completion-notes-template-multiple-files
        (concat
         "#+title: ${title}\n"
         "#+roam_key: cite:${=key=}\n"
         "* TODO Notes\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":URL: ${url}\n"
         ":END:\n\n"
         )))

(use-package org-roam-bibtex :straight t
  :after org-roam
  :config
  (require 'org-ref))

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

(use-package mermaid-mode :straight t)

(setenv "PUPPETEER_EXECUTABLE_PATH" "/usr/bin/chromium")

(use-package ob-mermaid :straight t
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((java . t)
   (mermaid . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("C" "java" "sh" "mermaid"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(provide 'init-org)
;;; init-org.el ends here
