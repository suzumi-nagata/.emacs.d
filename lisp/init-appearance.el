;;; init-appearance.el -- Theme configurations
;;; Commentary:
;;; Code:

;; Default font
(set-frame-font "Input Mono Compressed 11" nil t)

;; Base theme
(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (use-package dracula-theme :ensure t)
      (load-theme 'dracula t))
  (progn
    (use-package monokai-theme :ensure t)
    (load-theme 'monokai t))
  )

;; Faces color configurations
(when (display-graphic-p)
  (custom-set-faces
   '(highlight ((t (:background "orange" :foreground "black"))))
   '(bold ((t (:foreground "orange" :weight bold))))
   ;; '(font-lock-comment-face ((t (:foreground "#9acd32"))))
   '(default ((t (:inherit nil :stipple nil :background "#131417" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width extra-condensed :foundry "FBI " :family "Input Mono Compressed"))))
   '(show-paren-match ((t (:background "orange" :foreground "black" :weight extra-bold))))

   ;; Org mode
   '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.05))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-hide ((t (:background "#131417" :foreground "#131417"))))
   '(org-agenda-date-today ((t (:inherit org-agenda-date :background "orange" :foreground "#300b66"))))
   '(org-agenda-date-weekend ((t (:inherit org-agenda-date :background "#97BC62" :foreground "NavyBlue"))))
   '(org-block-begin-line ((t (:foreground "#9acd32" :background "#3d4551"))))
   '(org-block-end-line ((t (:foreground "#9acd32" :background "#3d4551"))))
   '(org-verbatim ((t (:inherit shadow :foreground "DarkGoldenrod1" :box (:line-width 1 :color "grey75" :style pressed-button)))))
   )
  )
;;----------------------------------------------------------------------------
;; Some color changes to the base themes
;;----------------------------------------------------------------------------
;; GUI
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(background-color . "#131417"))
  (set-face-attribute 'fringe nil :background nil)
  )

;; Terminal
(unless (display-graphic-p)
  (set-face-attribute 'region nil :background "blue")
  )

;;----------------------------------------------------------------------------
;; General Appearance
;;----------------------------------------------------------------------------

;; Disable the scroll bar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Show parenthesis match
(show-paren-mode 1)

;; Disable the sound bell
(setq visible-bell t)

;; Overwrite selected text
(delete-selection-mode t)

;; Disable the startup screen
(setq inhibit-startup-screen t)

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

;; Indentation Style
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent t)
(setq c-default-style "linux" c-basic-offset 4)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable highlighting current line
(use-package hl-line
  :ensure t
  ;; :init
  ;; (when (display-graphic-p) (global-hl-line-mode 1))
  :hook
  (prog-mode . hl-line-mode))

;; Pretty way to show buffers with same name
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Divide window equaly when splitting
(setq window-combination-resize t)

;; If truncate-lines is true, there is no "visual line break"
;; If word-wrap is ture, words are wrapped on space
(add-hook 'org-mode-hook
          '(lambda ()
             (setq truncate-lines nil
                   word-wrap t)))

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq truncate-lines t
                   word-wrap nil)))

;; Note, on a newly installed OS, there is a need to run
;; M-x all-the-icons-install-fonts
;; To install the required fonts
(use-package all-the-icons
  :ensure t)

(use-package diminish
  :ensure t)

(provide 'init-appearance)
;;; init-appearance.el ends here
