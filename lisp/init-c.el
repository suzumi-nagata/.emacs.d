;;; init-c -- c and c++ configuration
;;; Commentary:
;;; Code:

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; (use-package company-irony
;;   :ensure t
;;   :config
;;   (require 'company)
;;   (add-to-list 'company-backends 'company-irony))

;; (use-package company-c-headers
;;   :ensure t)

;; (use-package irony
;;   :ensure t
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (with-eval-after-load 'company
;;   (add-hook 'c++-mode-hook 'company-mode)
;;   (add-hook 'c-mode-hook 'company-mode))

(provide 'init-c)
;;; init-c.el ends here
