;;; init-productivity.el --- packages that help better my workflow
;;; Commentary:
;;; Code:

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  :config
  (setq ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil))

;; IDO-style directory navigation
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
(dolist (k '("C-j" "C-RET"))
  (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

;; Enable deleting trailing whitespace on all programming modes
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  )

(use-package expand-region
  :ensure t)

(use-package mark-multiple
  :ensure t
  :bind
  ("C-c q" . 'mark-next-like-this))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char-2))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c m" . mc/mark-all-like-this))

(use-package google-translate
  :ensure t
  :bind
  ("C-c g" . 'google-translate-at-point)
  ("C-c G" . 'google-translate-query-translate))

(use-package golden-ratio-scroll-screen
  :ensure t)

(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

;; If you have a ansi colored file
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package sudo-edit
  :ensure t)

;; I'm installing this just to use on org-wiki, I don't like helm that much
(use-package helm
  :ensure t)

(use-package subword
  :ensure t
  :diminish)

(use-package beacon
  :ensure t
  :diminish
  :config
  (beacon-mode 1))

(use-package f
  :ensure t
  :diminish)

(use-package sudo-edit
  :ensure t)

(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
  :hook
  (prog-mode . hl-todo-mode)
  )

(winner-mode)

;; Raise undo-limit to 80Mb
(setq undo-limit 80000000)

;; Interpret CamelCase as two words
(setq global-subword-mode 1)

(use-package vlf
  :ensure t)

(use-package rg
  :ensure t
  :bind
  ("C-c s" . #'rg-menu)
  :hook
  (rg-mode . (lambda()
               (local-set-key (kbd "C-n") 'compilation-next-error)
               (local-set-key (kbd "C-p") 'compilation-previous-error))))

(provide 'init-productivity)
;;; init-productivity.el ends here
