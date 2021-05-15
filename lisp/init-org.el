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
      org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
      org-tags-column 55
      org-noter-notes-window-location 'other-frame
      org-format-latex-options (plist-put org-format-latex-options :scale 1.7)
      org-roam-server-port 1784
      org-habit-graph-column 70
      org-display-remote-inline-images 'download
      )

(defvar org-agenda-directory (concat org-directory "agenda/"))
(setq org-agenda-files (list org-agenda-directory))
(defvar org-capture-todo-file (concat org-agenda-directory "inbox.org"))
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
               :children
               (("Basic" :keys "b" :headline "Basic")
                ("Beverages" :keys "d" :headline "Beverages")
                ("Carbohydrates" :keys "c" :headline "Carbohydrates")
                ("Meat" :keys "m" :headline "Meat")
                ("Sweets" :keys "d" :headline "Sweets")
                ("Fruits" :keys "f" :headline "Fruits")
                ("Hygiene" :keys "h" :headline "Hygiene")))
              )))

(setq-default org-agenda-custom-commands `(("m" "Agenda"
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
                                     ;; (todo "☛ TODO"
                                     ;;       ((org-agenda-overriding-header "UNICAMP")
                                     ;;        (org-agenda-files '(org-capture-todo-file))))
                                     ;; (todo "☛ TODO"
                                     ;;       ((org-agenda-overriding-header "Personal")
                                     ;;        (org-agenda-files '(org-capture-todo-file))))
                                     )
                                    )))

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
           :head "#+title: ${title}\n")))

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
          org-ref-default-bibliography '("/home/nagata/Common/Backup/Org/bib/library.bib")
          org-ref-pdf-directory "/home/nagata/Common/Backup/Zotero"))

(use-package helm-bibtex :straight t
  :after org
  :init
  (setq bibtex-format-citation-functions
        '((org-mode . (lambda (x) (insert (concat
                                           "\\cite{"
                                           (mapconcat 'identity x ",")
                                           "}")) ""))))
  (setq bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("/home/nagata/Common/Backup/Org/bib/library.bib")
        bibtex-completion-library-path '("/home/nagata/Common/Backup/Zotero/")
        bibtex-completion-notes-path "/home/nagata/Common/Backup/Org/bib/articles.org"
        ))

(use-package org-roam-bibtex :straight t
  :load-path "/home/nagata/Common/Backup/Org/bib/library.bib" ;Modify with your own path
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

(provide 'init-org)
;;; init-org.el ends here
