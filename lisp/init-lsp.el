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

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))
(setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")

(provide 'init-lsp)
;;; init-lsp.el ends here
