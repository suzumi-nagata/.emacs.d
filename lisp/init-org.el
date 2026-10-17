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
      org-habit-graph-column 120
      org-display-remote-inline-images 'download
      org-adapt-indentation t
      org-edit-src-content-indentation 0
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-tags-sort-function 'org-string-collate-lessp
      org-cite-global-bibliography '("~/garden/content/bibliography.bib")
      ;; Copied out of ~/Zotero/styles so org-cite export survives deleting Zotero.
      org-cite-csl-styles-dir "~/.emacs.d/csl-styles/"
      )

(add-hook 'org-mode-hook 'org-indent-mode)

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
(defvar org-capture-groceries-file (concat org-agenda-directory "2-areas/home/groceries.org"))

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
                        "> TO-READ(r)"
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
              ("Groceries"
               :keys "g"
               :file org-capture-groceries-file
               :headline "Inbox"
               :type entry
               :template ("* %?")
               )
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

          ("p" "presentation" plain "%?"
           :if-new (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+Author: Suzumi Nagata
#+Email: suzumi.nag@gmail.com
#+FILETAGS: presentation

#+REVEAL_INIT_OPTIONS: width:1920, height:1080, margin: 0.1, transition: 'slide',
#+OPTIONS: toc:1 timestamp:nil
#+REVEAL_THEME: dracula
#+REVEAL_PLUGINS: (markdown notes)

* Introduction
* Contents
* Conclusion
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

(use-package citar :straight t)

(use-package org-ref :straight t
    :after org
    :init
    ;; org-ref-notes-directory still points at the legacy org literature
    ;; notes; new ones are markdown in the vault.
    (setq org-ref-notes-directory "/home/nagata/Org/roam/bib/"
          ;; org-ref-bibliography-notes "/home/nagata/Org/bib/articles.org"
          org-ref-default-bibliography '("~/garden/content/bibliography.bib")
          org-ref-pdf-directory "~/pdfs"))

(use-package zotra :straight t)

(use-package ivy-bibtex :straight t
  :after org
  :init
  (setq bibtex-completion-bibliography
        '("~/garden/content/bibliography.bib")
        bibtex-completion-notes-path "~/garden/content/Literature/"
        bibtex-completion-notes-extension ".md"
        ;; Mirror of content/templates/literature.md in the vault: RET on an
        ;; entry creates the same markdown note citar and Obsidian create.
        bibtex-completion-notes-template-multiple-files
        (concat
         "---\n"
         "title: \"${title}\"\n"
         "citekey: \"${=key=}\"\n"
         "authors: \"${author}\"\n"
         "year: ${year}\n"
         "venue: \"${journal}\"\n"
         "doi: \"${doi}\"\n"
         "url: \"${url}\"\n"
         "tags:\n"
         "  - literature\n"
         "read: false\n"
         "---\n\n"
         "# Why I picked this up\n\n"
         "# What the source argues (in my words)\n\n"
         "# Key claims (with locations)\n-\n\n"
         "# Quotes (with locations)\n>\n\n"
         "# Where I disagree / what's missing\n\n"
         "# Permanent notes grown from this\n-\n")
        bibtex-completion-pdf-field "File"
        bibtex-completion-library-path '("~/pdfs"))

  (setq bibtex-completion-additional-search-fields '(keywords abstract))

  ;; No Zotero required: ~/Repos/zotra-server runs Zotero's translators as a
  ;; standalone service (port 1969) and zotra starts it on demand.
  (setq zotra-backend 'zotra-server
        zotra-local-server-directory "~/Repos/zotra-server/"
        zotra-default-bibliography "~/garden/content/bibliography.bib"
        zotra-download-attachment-default-directory "~/pdfs/")

  (setq bibtex-completion-format-citation-functions
        '((org-mode . bibtex-completion-format-citation-org-title-link-to-PDF)
          (latex-mode . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
          (rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
          (default . bibtex-completion-format-citation-default)))
  :config
  (ivy-set-prompt 'ivy-bibtex (lambda () "BibTeX entries [M-o: actions]: "))
  (setq ivy-bibtex-default-action 'ivy-bibtex-edit-notes)
  (ivy-add-actions
   'ivy-bibtex
   '(("p" ivy-bibtex-open-any "Open pdf, url or DOI")))
  )

(setq org-cite-export-processors '((t csl)))

(use-package org-roam-bibtex :straight t
  :after org-roam
  :config
  (require 'org-ref)
  (org-roam-bibtex-mode 1)
  (setq orb-preformat-keywords '("url" "title" "file" "keywords" "citekey" "author" "date"))
  (setq orb-abbreviate-file-name nil)
  (setq orb-insert-interface 'ivy-bibtex
        orb-note-actions-interface 'ivy)
  )

(use-package ebib :straight t
  :init
  (require 'ebib-reading-list)
  (require 'org-ebib)
  ;; The library UI: ebib browses and edits the same .bib that citar,
  ;; ivy-bibtex, Quartz and the Obsidian citation plugin read.
  (setq ebib-preload-bib-files '("~/garden/content/bibliography.bib"))
  (setq ebib-notes-directory "~/garden/content/Literature")
  (setq ebib-multiline-major-mode 'org-mode)

  ;; --- reading list (R a in the index buffer adds the current entry) ------
  ;; Priorities A-E on org headings; "> TO-READ" and "V DONE" are the literal
  ;; state names from `org-todo-keywords' above.
  (setq ebib-reading-list-file "~/Org/roam/reading_list.org"
        ebib-reading-list-todo-marker "> TO-READ"
        ebib-reading-list-done-marker "V DONE"
        ebib-reading-list-template "* %M %T\n:PROPERTIES:\n%K\n:REF: %C\n:END:\n%F\n"
        ebib-reading-list-template-specifiers
        '((?K . ebib-reading-list-create-org-identifier)
          (?T . ebib-create-org-title)
          (?M . ebib-reading-list-todo-marker)
          (?C . ebib-create-org-cite)
          (?F . ebib-create-org-file-link)
          (?D . ebib-create-org-doi-link)
          (?U . ebib-create-org-url-link)))

  ;; --- topic indexes (ebib "dependent databases") -------------------------
  ;; One .bib per topic sharing entries with the library: create with M c in
  ;; ebib, push entries with M a, then list the file name here.
  (defvar my/ebib-index-directory "~/Org/bib/index/"
    "Directory holding the topic index databases.")
  (defvar my/ebib-index-databases nil
    "Topic index .bib files to open alongside the library.")
  (defun my/ebib-open-indexes ()
    "Open every index database listed in `my/ebib-index-databases'."
    (interactive)
    (dolist (db my/ebib-index-databases)
      (ebib--load-bibtex-file-internal
       (expand-file-name db my/ebib-index-directory)))
    (message "Opened %d index database(s)" (length my/ebib-index-databases)))

  (add-hook 'ebib-entry-mode-hook 'visual-line-mode)

  (setq ebib-index-columns '(("Title" 60 t)
                             ("Author/Editor" 40 t)
                             ("Year" 6 t)
                             ("Entry Key" 40 t)
                             ("Note" 10 t)))

  (setq ebib-citation-description-function 'ebib-create-org-title)

  (defun ebib-edit-annote ()
    "Edit annote field of entry."
    (interactive)
    (ebib-edit-field "annote"))

  (defun ebib-list-recent (days)
    "List entries created in the last DAYS days."
    (interactive "nNumber of days: ")
    ;; Save the database's current filter, if there is one.
    (let ((filter (ebib-db-get-filter ebib--cur-db)))
      (when filter (setq ebib--filters-last-filter filter)))
    (let*
        ;; Calculate the from-date in Emacs' time format.
        ((date (time-subtract (current-time) (days-to-time days)))
         ;; Create a Lisp expression that will function as the filter.
         (filter `(ebib--newer-than (quote ,date))))
      ;; Install it as the current database's filter.
      (ebib-db-set-filter filter ebib--cur-db)
      ;; Update the current entry key.
      (ebib-db-set-current-entry-key (ebib--get-key-at-point) ebib--cur-db)
      ;; Update the display, so that only filtered entries are visible.
      (ebib--update-buffers)))

  (defun ebib--newer-than (date)
    "Function for use in filters.
Return t if the entry being tested is newer than DATE.  DATE must
be a list of the format returned by `current-time' and is
compared to the timestamp of the entry being tested.  If the
entry has no timestamp, or a timestamp that cannot be converted
into a date representation, return nil."
    (let ((timestamp (cdr (assoc-string "urldate" ebib-entry))))
      (when (and timestamp
                 (setq timestamp (ignore-errors (date-to-time timestamp))))
        (time-less-p date timestamp))))

  ;; Prompt for an A-E priority on every item added to the reading list.
  (defun my/ebib-reading-list-set-priority ()
    "Set an A-E priority on the reading-list item that was just added."
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading t)
        (call-interactively #'org-priority))))

  (add-hook 'ebib-reading-list-new-item-hook #'my/ebib-reading-list-set-priority)

  ;; From a reading-list item, create or open its markdown literature note in
  ;; the vault.  The :REF: property holds "cite:<citekey>".
  (defun my/reading-list-open-note ()
    "Create or open the vault literature note for the item at point."
    (interactive)
    (require 'citar)
    (let* ((raw (org-entry-get nil "REF"))
           (citekey (and raw (string-remove-prefix "cite:" raw))))
      (unless citekey (user-error "No :REF: citekey on this heading"))
      (citar-create-note citekey)))
)

;;; --- reference workflow entry points --------------------------------------
(defun my/citar-create-note ()
  "Create or open the vault literature note for a citekey."
  (interactive)
  (require 'citar)                      ; not autoloaded, unlike ebib
  (call-interactively #'citar-create-note))

(global-set-key (kbd "C-c n e") 'ebib)                  ; browse the library
(global-set-key (kbd "C-c n r") 'my/citar-create-note)  ; literature note

;; zotra appends to the library with a plain `insert' into whatever buffer
;; visits the .bib.  If that buffer is read-only -- e.g. the library was opened
;; with C-x C-r just to look at it -- the capture dies with "Buffer is
;; read-only", and when it is triggered from the browser that error only
;; reaches emacsclient's stderr, which nothing displays.  So: make the write
;; unconditional, and report any other failure where it can be seen.
(defun my/zotra-add-entry-writable (orig &rest args)
  "Run ORIG with read-only buffers writable."
  (let ((inhibit-read-only t))
    (apply orig args)))

(defun my/zotra-protocol-report-errors (orig info)
  "Run ORIG, reporting a failure in the echo area instead of losing it."
  (condition-case err
      (funcall orig info)
    (error (message "zotra capture failed: %S" err) nil)))

;; zotra appends blindly, so capturing something already in the library leaves
;; two entries sharing a citekey -- which is worse than a missing entry, since
;; every citation of that key silently becomes ambiguous.
(defun my/zotra--bibfile (args)
  "Return the file `zotra-add-entry' will write to, given ARGS."
  (let ((arg (nth 2 args)))
    (cond ((and (stringp arg) (not (equal arg "here"))) arg)
          ((stringp zotra-default-bibliography) zotra-default-bibliography)
          ((listp zotra-default-bibliography) (car zotra-default-bibliography)))))

(defun my/zotra-add-entry-warn-duplicate (orig &rest args)
  "Run ORIG, warning when the citekey it added is already in the library."
  (let ((key (apply orig args))
        (bib (ignore-errors (my/zotra--bibfile args))))
    (when (and key bib (file-readable-p (expand-file-name bib)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name bib))
        (when (> (count-matches (concat "^@[A-Za-z]+{" (regexp-quote key) ",")
                                 (point-min) (point-max))
                 1)
          (message "zotra: `%s' was already in the library -- it now holds two entries with that key" key))))
    key))

(with-eval-after-load 'zotra
  (advice-add 'zotra-add-entry :around #'my/zotra-add-entry-writable)
  (advice-add 'zotra-add-entry :around #'my/zotra-add-entry-warn-duplicate)
  (advice-add 'zotra-protocol :around #'my/zotra-protocol-report-errors))


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

(use-package ox-reveal :straight t
  :config
  (setq org-reveal-root "file:///home/nagata/Repos/reveal.js/")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    hugo                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-hugo :straight t
  :after ox
  :config
  (setq-default org-hugo-base-dir "/home/nagata/suzumi-nagata.github.io/"))

(with-eval-after-load 'ox-hugo
  (defun my/ox-hugo-export-block-latex (export-block _contents _info)
    "Export LATEX blocks as raw Markdown, ensuring no leading indentation."
    (when (string= (org-element-property :type export-block) "LATEX")
      (let ((value (org-element-property :value export-block)))
        ;; 1. Remove the common indentation from the block
        ;; 2. Wrap in newlines to ensure it's a separate Markdown block
        (concat "\n\n" (org-remove-indentation value) "\n\n"))))

  ;; Intercept the Hugo exporter for LATEX blocks
  (advice-add 'org-hugo-export-block :before-until #'my/ox-hugo-export-block-latex))

(with-eval-after-load 'oc-csl
  ;; Re-evaluating the strategy: :filter-return only gets the output string.
  ;; Let's use :around instead to have access to all arguments.
  (defun my/org-cite-csl-render-bibliography-hugo-around (orig-fun keys files style props backend info)
    "Restore inline CSS for Hugo exports, as it ignores the HTML head."
    (let ((output (funcall orig-fun keys files style props backend info)))
      (if (and (org-export-derived-backend-p backend 'hugo)
               (not (plist-get info :hugo-csl-styles-added)))
          (progn
            (plist-put info :hugo-csl-styles-added t)
            (concat (org-cite-csl--generate-html-head info) output))
        output)))

  (advice-add 'org-cite-csl-render-bibliography :around #'my/org-cite-csl-render-bibliography-hugo-around))

(provide 'init-org)
;;; init-org.el ends here
