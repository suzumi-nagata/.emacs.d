;;; init-c -- c and c++ configuration
;;; Commentary:
;;; Code:

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "M-o") 'ff-find-other-file)))

(provide 'init-c)
;;; init-c.el ends here
