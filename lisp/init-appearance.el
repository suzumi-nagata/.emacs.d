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
(custom-set-faces
 '(highlight ((t (:background "orange" :foreground "black"))))
 '(bold ((t (:foreground "orange" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#9acd32"))))

 '(org-block-begin-line ((t (:foreground "#9acd32" :background "#3d4551"))))
 '(org-block-end-line ((t (:foreground "#9acd32" :background "#3d4551"))))
 )

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
  :init
  (when (display-graphic-p)
    (global-hl-line-mode 1)
    ))

;; Pretty way to show buffers with same name
(require 'uniquify)

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; Modeline
(setq line-number-mode t)
(setq column-number-mode t)
(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")
(setq display-time-default-load-average nil)
(display-time-mode 1)

(set-face-attribute  'mode-line
              nil
              :foreground "#8aff2b"
              :background "DimGrey"
              :box '(:line-width 1 :style released-button))

(set-face-attribute  'mode-line-inactive
              nil
              :foreground "ForestGreen"
              :background "gray4"
              :box '(:line-width 1 :style released-button))

(setq-default transient-mark-mode 'nil)

;; Divide window equaly when splitting
(setq window-combination-resize t)

(provide 'init-appearance)
;;; init-appearance.el ends here
