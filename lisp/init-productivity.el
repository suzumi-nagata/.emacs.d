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
  :ensure t
  :bind
  ("C-q" . er/expand-region))

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

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))

(setq kill-ring-max 100)

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all)
  :init
  (yas-global-mode 1))

;; Use nabla instead of TAB
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(global-set-key '[8711] 'yas-expand)

(provide 'init-productivity)
;;; init-productivity.el ends here
