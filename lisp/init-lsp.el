;;; init-lsp --- Language server protocol configurations
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp-mode)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :ensure t)

(provide 'init-lsp)
;;; init-lsp.el ends here
