;;; init-org.el --- org-mode configurations (Android optimized)

(straight-override-recipe
   '(org :type git :host github :repo "emacsmirror/org"))
(use-package org :straight t)

(require 'org)
(require 'org-habit)
(setq-default major-mode 'org-mode)
(setq initial-major-mode 'org-mode)

;; Use dynamic paths defined in init.el
(setq org-directory my/org-base-dir
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
      org-startup-with-inline-images 'inlineimages
      org-habit-graph-column 100
      org-display-remote-inline-images 'download
      org-adapt-indentation t
      org-edit-src-content-indentation 0
      org-agenda-block-separator nil
      org-agenda-start-with-log-mode t
      org-tags-sort-function 'org-string-collate-lessp
      org-cite-global-bibliography (list (concat org-directory "bib/zotero.bib"))
      org-cite-csl-styles-dir (expand-file-name "styles/" user-emacs-directory)
      )

(setq org-agenda-sorting-strategy
  '((agenda time-up priority-down category-keep)
    (todo   priority-down category-keep)
    (tags   priority-down category-keep)
    (search category-keep)))

(defvar org-agenda-directory (concat org-directory "agenda/"))
(defvar org-study-backlog-file (concat org-directory "roam/" "20221121131908-study_backlog.org"))

(setq org-agenda-files (append (when (file-exists-p org-agenda-directory)
                                 (directory-files-recursively org-agenda-directory ".org$"))
                               (when (file-exists-p org-study-backlog-file)
                                 (list org-study-backlog-file))))

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
              ("Groceries"
               :keys "g"
               :file org-capture-groceries-file
               :headline "Inbox"
               :type entry
               :template ("* %?")
               )
              )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  Org roam                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam :straight t
  :init
  (setq org-roam-v2-ack t)
  (setq-default org-roam-directory (file-truename (concat org-directory "roam")))

  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))

  :config
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %t %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%A, %d %B %Y>
#+FILETAGS: Journal

\n")
           :unnarrowed t)))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}
#+FILETAGS:
- links ::\n\n")
           :immediate-finish t
           :unnarrowed t)))

  (org-roam-db-autosync-mode))

(use-package deft :straight t
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory (concat org-directory "roam")))

(use-package org-noter :straight t)
(use-package org-fragtog :straight t :hook (org-mode . org-fragtog-mode))
(use-package org-download :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                    Hugo                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-hugo :straight t
  :after ox
  :config
  (setq-default org-hugo-base-dir my/hugo-base-dir))

(provide 'init-org)
;;; init-org.el ends here
