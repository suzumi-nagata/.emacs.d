;;; init.el --- stripped version for Android (Org-mode only)
;;
;; This version is optimized for mobile devices (Android) and focuses on Org-mode.
;; It avoids heavy packages like LSP and hardcoded paths.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 Bootstrap                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Path Settings                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define base paths dynamically
(defvar my/org-base-dir
  (if (file-exists-p "/sdcard/Org")
      "/sdcard/Org/" ;; Common Termux/Android path
    (expand-file-name "~/Org/")))

(defvar my/hugo-base-dir
  (if (file-exists-p "/sdcard/Documents/suzumi-nagata.github.io")
      "/sdcard/Documents/suzumi-nagata.github.io/"
    (expand-file-name "~/Documents/suzumi-nagata.github.io/")))

;; add ~/.emacs.d/lisp to load the rest of configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Disable native comp warnings
(setq-default warning-minimum-level :emergency)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               Core Components                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'custom-functions)
(my/autosave-backup-dirs)

(require 'modeline)
(require 'init-org)

(use-package s :straight t)

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
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-c R") 'query-replace)
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
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-o") 'open-line-with-reindent)
(global-set-key (kbd "<escape>") #'turn-on-evil-mode)

;; Org Roam Bindings
(global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c n i") 'org-roam-node-insert)
(global-set-key (kbd "C-c n f") 'org-roam-node-find)
(global-set-key (kbd "C-c n ç") 'org-roam-capture)
(global-set-key (kbd "C-c n d") 'deft)
(global-set-key (kbd "C-c n j") 'org-roam-dailies-capture-today)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 Appearance                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base theme
(use-package dracula-theme
  :straight (dracula-theme :type git :host github :repo "dracula/emacs"
                           :fork (:host github :repo "suzumi-nagata/dracula")))
(load-theme 'dracula t)

;; General Appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(delete-selection-mode t)
(setq inhibit-startup-screen t)
(fset 'yes-or-no-p 'y-or-n-p)
(winner-mode)
(setq undo-limit 80000000)
(setq scroll-conservatively 100)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq kill-ring-max 100)
(setq initial-scratch-message "")

(add-hook 'org-mode-hook '(lambda () (setq truncate-lines nil
                                           word-wrap t)))

(use-package all-the-icons :straight t)
(use-package emojify :straight t :hook (after-init . global-emojify-mode))

(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Productivity                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy :straight t
  :init (ivy-mode 1)
  :config
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))
  (setq ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil))
(use-package counsel :straight t
  :bind (("M-x" . counsel-M-x)))

(use-package expand-region :straight t)
(use-package mark-multiple :straight t)
(use-package swiper :straight t)
(use-package avy :straight t)
(use-package multiple-cursors :straight t)
(use-package f :straight t)

(use-package hl-line :straight t :init (global-hl-line-mode 1))

(use-package evil
  :straight t
  :hook (after-init . evil-mode)
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-keybinding nil)
  :config
  (evil-define-key 'normal global-map "q" 'er/expand-region)
  (evil-define-key 'normal global-map "ç" 'other-window)
  (evil-define-key 'normal global-map "m" 'back-to-indentation)
  (evil-define-key 'normal global-map (kbd "C-a") 'evil-beginning-of-line)
  (evil-define-key 'normal global-map (kbd "C-e") 'evil-last-non-blank)
  (evil-define-key 'normal global-map (kbd "p") 'yank)
  (define-key org-agenda-mode-map "h" 'evil-backward-char)
  (define-key org-agenda-mode-map "j" 'evil-next-line)
  (define-key org-agenda-mode-map "k" 'evil-previous-line)
  (define-key org-agenda-mode-map "l" 'evil-forward-char)
  )

(use-package evil-collection
  :straight t
  :config
  (setq-default evil-collection-calendar-want-org-bindings t)
  (setq-default evil-collection-outline-bind-tab-p t)
  (setq evil-collection-mode-list '(calc calendar comint company compile custom dashboard dired ebdb ediff eglot elisp-mode eshell eww flycheck help ibuffer image info ivy magit man org replace simple term vterm which-key xref))
  (evil-collection-init))

(use-package evil-org :straight t
  :after org
  :config
  (evil-org-set-key-theme '(textobjects navigation todo))
  :hook (org-mode . evil-org-mode))

(use-package evil-surround :straight t
  :config
  (global-evil-surround-mode 1))

(save-place-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq global-auto-revert-mode t)

(use-package smex :straight t
  :config
  (smex-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                Final Setup                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (server-running-p)
  (server-start))

(org-roam-db-autosync-enable)
(org-agenda nil " ")

(provide 'init)
;;; init.el ends here
