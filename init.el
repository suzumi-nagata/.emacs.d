;;; init.el --- my init config
;;
;; Copyright (C) 2021 Vitor Nagata
;;
;; Author: Vitor Nagata <http://github/nagatavit>
;; Maintainer: Vitor Nagata <nagatavit@gmail.com>
;; Created: January 15, 2021
;; Modified: January 15, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nagata/init
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;; Uncomment to debug
;; (setq debug-on-error t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; add ~/.emacs.d/lisp to load the rest of configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'custom-functions)
(my/autosave-backup-dirs)

(require 'modeline)
(require 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                             Global Keybindings                              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x 2") (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-x 3") (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(global-set-key (kbd "C-x M-f") 'close-wrong-buffer-and-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c k w") 'bk/kill-inner-word)
(global-set-key (kbd "C-c y y") 'bk/copy-whole-line)

(global-set-key (kbd "C-c ç") 'org-capture)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c q") 'mark-next-like-this)
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'query-replace)

(global-set-key (kbd "C-c b") (lambda() (interactive) (my-insert-pair  "\[" "\]")))
(global-set-key (kbd "C-c c") 'insert-curly-braces)
(global-set-key (kbd "C-c p") 'insert-parentheses)
(global-set-key (kbd "C-c e") (lambda() (interactive) (my-insert-pair  "\"" "\"")))

(global-set-key (kbd "C-c x i") 'ag-project)
(global-set-key (kbd "C-c x u") 'dumb-jump-go)
(global-set-key (kbd "C-c x ç") 'xref-find-references)
(global-set-key (kbd "C-c x o") 'xref-find-definitions)
(global-set-key (kbd "C-c x p") 'xref-pop-marker-stack)
(global-set-key (kbd "C-c x j") 'lsp-ivy-workspace-symbol)
(global-set-key (kbd "C-M-p") 'xref-pop-marker-stack)

(global-set-key (kbd "C-q") 'er/expand-region)
(global-set-key (kbd "C-ç") 'other-window)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "M-ç") 'avy-goto-char)

(global-set-key (kbd "M-H") 'windmove-left)
(global-set-key (kbd "M-J") 'windmove-down)
(global-set-key (kbd "M-K") 'windmove-up)
(global-set-key (kbd "M-L") 'windmove-right)

(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)

(global-set-key "\C-x\C-m" 'compile)

(global-set-key (kbd "<f1>") 'find-file-project-root)
(global-set-key (kbd "<f2>") 'display-line-numbers-mode)
(global-set-key (kbd "<f5>") 'x509-viewkey)
(global-set-key (kbd "<f6>") 'x509-viewasn1)
(global-set-key (kbd "<f7>") 'x509-viewcrl)
(global-set-key (kbd "<f8>") 'x509-viewcert)
(global-set-key (kbd "<f11>") 'find-magit-project-root)
(global-set-key (kbd "<f12>") 'org-agenda)

(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-o") 'open-line-with-reindent)

;; (global-set-key (kbd "<escape>") #'god-mode-all)
(global-set-key (kbd "<escape>") #'turn-on-evil-mode)

(global-set-key (kbd "C-c n l") 'org-roam)
(global-set-key (kbd "C-c n i") 'org-roam-insert)
(global-set-key (kbd "C-c n b") 'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n f") 'org-roam-find-file)
(global-set-key (kbd "C-c n k") 'org-roam-jump-to-index)
(global-set-key (kbd "C-c n ç") 'org-roam-capture)
(global-set-key (kbd "C-c n d") 'deft)
(global-set-key (kbd "C-c n j") 'org-journal-new-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  appearance                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default font
;; (set-frame-font "Input Mono Compressed 11" nil t)
(add-to-list 'default-frame-alist '(font . "Input Mono Compressed 11"))

;; Base theme
(if (display-graphic-p)
    (progn
      ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (use-package dracula-theme
        :straight (dracula-theme :type git :host github :repo "dracula/emacs"
                                 :fork (:host github
                                        :repo "nagatavit/dracula")))
      (load-theme 'dracula t))
  (progn
    (use-package monokai-theme :straight t)
    (load-theme 'monokai t))
  )

;; Faces color configurations
(when (display-graphic-p)
  (custom-set-faces
   '(highlight ((t (:background "orange" :foreground "black"))))
   '(bold ((t (:foreground "orange" :weight bold))))
   '(show-paren-match ((t (:background "orange" :foreground "black" :weight extra-bold))))
   '(default ((t (:background "#131417"))))
   ;; org mode
   '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-hide ((t (:background "#131417" :foreground "#131417"))))
   '(org-agenda-date ((t (:inherit org-agenda-date :background "gray4" :foreground "ForestGreen"))))
   '(org-agenda-date-today ((t (:inherit org-agenda-date :background "orange" :foreground "#300b66"))))
   '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "#97BC62" :foreground "NavyBlue"))))
   '(org-block-begin-line ((t (:background "#606c7d"))))
   '(org-block-end-line ((t (:background "#606c7d"))))
   '(org-block ((t (:background "#323f4f"))))
   '(org-verbatim ((t (:inherit shadow :foreground "DarkGoldenrod1" :box (:line-width 1 :color "grey75" :style pressed-button)))))
   '(org-roam-link ((t (:inherit org-link :foreground "dark orange"))))
   '(org-list-dt ((t (:foreground "#97FF62" :weight bold))))
   '(ediff-odd-diff-A ((t (:background "#0a2832"))))
   '(ediff-even-diff-A ((t (:background "#0a2832"))))
   '(ediff-odd-diff-B ((t (:background "#0a2832"))))
   '(ediff-even-diff-B ((t (:background "#0a2832"))))
   ))

(set-face-attribute  'mode-line
              nil
              :foreground "LawnGreen"
              :background "#0a2832"
              :box '(:line-width 1 :style released-button))
(set-face-attribute  'mode-line-inactive
              nil
              :foreground "ForestGreen"
              :background "gray4"
              :box '(:line-width 1 :style released-button))

;; GUI
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(background-color . "#131417"))
  (set-face-attribute 'fringe nil :background nil)
  )

;; Terminal
(unless (display-graphic-p)
  (set-face-attribute 'region nil :background "blue")
  )

; General Appearance ;;;;;;;;;;;;;;;;;;

;; Disable the scroll bar
(scroll-bar-mode -1)
;; Disable toolbar
(tool-bar-mode -1)
;; Disable menu bar
(menu-bar-mode -1)
;; Show parenthesis match
(show-paren-mode 1)
;; Disable the bell animation
(setq visible-bell nil
      ring-bell-function #'ignore)
;; Overwrite selected text
(delete-selection-mode t)
;; Disable the startup screen
(setq inhibit-startup-screen t)
;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)
;; Remember window layout
(winner-mode)
;; Raise undo-limit to 80Mb
(setq undo-limit 80000000)
;; Interpret CamelCase as two words
(setq-default global-subword-mode 1)
;; Smooth scrolling lines
(setq scroll-conservatively 100)
(setq split-height-threshold nil)
(setq split-width-threshold 80)
;; Use utf-8 as default coding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(require 'iso-transl)
;; Indentation Style
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq c-default-style "linux" c-basic-offset 4)
(setq kill-ring-max 100)
;; Makes *scratch* empty.
(setq initial-scratch-message "")
;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")
;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda () (let ((buffer "*Completions*"))
                        (and (get-buffer buffer)
                             (kill-buffer buffer)))))
;; Pretty way to show buffers with same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq parens-require-spaces nil)
(setq uniquify-ignore-buffers-re "^\\*")
;; If truncate-lines is true, there is no "visual line break"
;; If word-wrap is ture, words are wrapped on space
(add-hook 'org-mode-hook '(lambda () (setq truncate-lines nil
                                           word-wrap t)))
(add-hook 'prog-mode-hook '(lambda () (setq truncate-lines t
                                            word-wrap nil)))
;; Divide windows equaly when splitting
(setq window-combination-resize t)

;; Note, on a newly installed OS, there is a need to run
;; M-x all-the-icons-install-fonts
;; To install the required fonts
(use-package all-the-icons :straight t)

(use-package emojify :straight t :hook (after-init . global-emojify-mode))

(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;        produuctivity packages       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy :straight t
  :init (ivy-mode 1)
  :config
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (setq ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil))
;; IDO-style directory navigation
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-partial-or-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))
(use-package ws-butler :straight t :config (add-hook 'prog-mode-hook #'ws-butler-mode))
(use-package expand-region :straight t)
(use-package mark-multiple :straight t)
(use-package swiper :straight t)
(use-package avy :straight t)
(use-package sudo-edit :straight t)
(use-package subword :straight t)
(use-package rainbow-mode :straight t :hook prog-mode)
(use-package rainbow-delimiters :straight t :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package dumb-jump :straight t)
(use-package multiple-cursors :straight t)
;; (use-package beacon :straight t :config (beacon-mode 1))
(use-package f :straight t)
(use-package sudo-edit :straight t)
(use-package projectile :straight t
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("M-s" . projectile-command-map)))

(use-package ibuffer-vc :straight t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package golden-ratio-scroll-screen :straight t)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

(use-package hl-todo :straight t
  :init (setq hl-todo-keyword-faces
              '(("TODO"   . "#FF0000")
                ("FIXME"  . "#FF0000")
                ("DEBUG"  . "#A020F0")
                ("GOTCHA" . "#FF4500")
                ("STUB"   . "#1E90FF")))
  :hook (prog-mode . hl-todo-mode))

(use-package vlf :straight t)
(use-package ag :straight t)
(use-package xclip :straight t :init (xclip-mode 1))
(use-package hl-line :straight t :init (when (display-graphic-p) (global-hl-line-mode 1)))
(use-package yasnippet :straight t
  :config
  (use-package yasnippet-snippets :straight t)
  (yas-reload-all)
  :init (yas-global-mode 1))
(use-package flycheck :straight t :config (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package magit :straight t
  :config
  (setq git-commit-summary-max-length 50)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (add-hook 'git-commit-mode-hook #'(lambda ()
                                    (ispell-change-dictionary "en_US")
                                    (flyspell-mode))))
(require 'ispell)
(setq ispell-program-name "/usr/bin/aspell")

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))
(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))
(use-package vterm :straight t)

;; (use-package god-mode
;;   :straight t
;;   :init
;;   (setq god-mode-enable-function-key-translation nil)
;;   :config
;;   (setq god-exempt-major-modes nil)
;;   (setq god-exempt-predicates nil)
;;   (define-key god-local-mode-map (kbd "C-x C-k") #'kill-current-buffer)
;;   (define-key god-local-mode-map (kbd "C-x C-1") #'delete-other-windows)
;;   (define-key god-local-mode-map (kbd "C-x C-2") #'split-window-below)
;;   (define-key god-local-mode-map (kbd "C-x C-3") #'split-window-right)
;;   (define-key god-local-mode-map (kbd "C-x C-0") #'delete-window)

;;   (define-key god-local-mode-map (kbd "C-x C-b") 'ivy-switch-buffer)
;;   (define-key god-local-mode-map (kbd "C-x C-S-b") 'ibuffer)

;;   (define-key god-local-mode-map (kbd "C-c C-u") 'dumb-jump-go)
;;   (define-key god-local-mode-map (kbd "C-c C-i") 'ag-project)
;;   (define-key god-local-mode-map (kbd "C-c C-o") 'xref-find-definitions)
;;   (define-key god-local-mode-map (kbd "C-c C-p") 'xref-pop-marker-stack)
;;   (define-key god-local-mode-map (kbd "C-c C-ç") 'xref-find-references)
;;   (define-key god-local-mode-map (kbd "C-c C-j") 'lsp-ivy-workspace-symbol)

;;   (define-key god-local-mode-map (kbd "C-<f12>") 'org-agenda)

;;   (define-key god-local-mode-map (kbd "z") 'repeat)
;;   (define-key god-local-mode-map (kbd "i") 'god-mode-all)
;;   (define-key god-local-mode-map (kbd "I") 'mortal-mode)

;;   (god-mode)
;;   )

;; (define-minor-mode mortal-mode
;;   "Allow temporary departures from god-mode."
;;   :lighter " mortal"
;;   :keymap '(([return] . (lambda ()
;;                           "Exit mortal-mode and resume god mode." (interactive)
;;                           (god-local-mode-resume)
;;                           (mortal-mode 0))))
;;   (when mortal-mode
;;     (god-local-mode-pause)))

;; (defun my-god-mode-update-cursor ()
;;   (setq cursor-type (if (or god-local-mode buffer-read-only)
;;                         'box
;;                       'bar)))

;; (add-hook 'god-mode-enabled-hook #'my-god-mode-update-cursor)
;; (add-hook 'god-mode-disabled-hook #'my-god-mode-update-cursor)

(defun close-wrong-buffer-and-magit ()
  "Close current buffer and find file from current path."
  (interactive)
  (let ((dirname (buffer-name)))
  (magit-status)
  (kill-buffer dirname)))
(defun find-magit-project-root (project)
  "Open magit starting from a PROJECT root."
  (interactive (list (completing-read
                      "Select a project: "
                      projects-roots-path
                      nil t)))
  (find-file (cdr (assoc project projects-roots-path)))
  (close-wrong-buffer-and-magit))

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :init
  ;; use emacs bindings in insert-mode
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-keybinding nil)
  :config
  (evil-define-key 'normal global-map "q" 'er/expand-region)
  (evil-define-key 'normal global-map "ç" 'other-window)
  (evil-define-key 'normal global-map "a" 'evil-first-non-blank)
  (evil-define-key 'normal global-map "e" 'evil-end-of-visual-line))

(use-package framemove
  :config (setq framemove-hook-into-windmove t)
  :straight t)

(use-package s :straight t)

(use-package google-this :straight t
  :config
  (google-this-mode 1))

(use-package evil-surround :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :straight t
  :config
  (setq-default evil-collection-company-use-tng nil)
  (setq-default evil-collection-calendar-want-org-bindings t)
  (setq-default evil-collection-outline-bind-tab-p t)
  (evil-collection-init))

(use-package evil-easymotion
  :straight t
  :config
  (evilem-default-keybindings "SPC"))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           end productivity          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               company               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company :straight t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers t))

;; Remap company navigation keys
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                latex                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools :straight t)

(use-package tex
  :defer t
  :straight auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(use-package company-reftex :straight t)
(use-package company-auctex :straight t)
;; (use-package latex-preview-pane :straight t)

(pdf-loader-install)

(use-package magic-latex-buffer :straight t :init (add-hook 'latex-mode-hook 'magic-latex-buffer))

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(use-package company-bibtex :straight t
  :config (add-to-list 'company-backends 'company-bibtex))

(use-package auctex-latexmk :straight t
  :init (with-eval-after-load 'tex (auctex-latexmk-setup))
  :config
  ;; Use Latexmk as the default command.
  ;; (We have to use a hook instead of `setq-default' because AUCTeX sets this variable on mode activation.)
  (defun my-tex-set-latexmk-as-default ()
    (setq TeX-command-default "LatexMk"))
  (add-hook 'TeX-mode-hook #'my-tex-set-latexmk-as-default)

  ;; Compile to PDF when `TeX-PDF-mode' is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package insert-shebang :straight t :defer 1)

(use-package x509-mode :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                 lsp                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode :straight t
  :init
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (setq gc-cons-threshold 100000000)
  (setq lsp-file-watch-threshold 20000)
  :commands (lsp lsp-deferred))

(use-package lsp-ui :straight t
  :commands lsp-ui-mode)

(use-package lsp-ivy :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;               dap-mode              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dap-mode :straight t)
;TODO: add dap support to c, c++
;; (require 'dap-lldb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                golang               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode :straight t
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package exec-path-from-shell :straight t
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "/home/nagata/go/bin")
  (setenv "GOPATH" "/home/nagata/go"))

(lsp-register-custom-settings
 '(("gopls.completeUnimported" t t)
   ("gopls.staticcheck" t t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                python               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-python-ms :straight t
  :after lsp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                ispell               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun spell (language)
  "Enable flyspell in this buffer for LANGUAGE as en or pt."
  (interactive "sLanguage: ")
  (if (string= "pt" language)
      (ispell-change-dictionary "pt_BR"))
  (if (string= "en" language)
      (ispell-change-dictionary "en_US"))
  (if (string= "" language)
      (flyspell-mode 0)
    (flyspell-mode 1))
  )

(use-package flyspell-correct :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy :straight t
  :after flyspell-correct)

(use-package flyspell :straight t
  :config
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;             global hooks            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;         end of init commands        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-file org-roam-index-file)
(org-roam)
(kill-buffer "*scratch*")

(provide 'init)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
