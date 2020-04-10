;;; init-appearance.el -- Theme configurations
;;; Commentary:
;;; Code:

(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (use-package dracula-theme :ensure t)
      (load-theme 'dracula t))
  (progn
    (use-package monokai-theme :ensure t)
    (load-theme 'monokai t))
  )

(provide 'init-appearance)
;;; init-appearance.el ends here
