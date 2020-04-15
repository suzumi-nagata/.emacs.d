;;; init-programming-utils.el -- packages that help programming in general
;;; Commentary:
;;; Code:

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook prog-mode)

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

(use-package flycheck
  :ensure t)

(provide 'init-programming-utils)
;;; init-programming-utils.el ends here
