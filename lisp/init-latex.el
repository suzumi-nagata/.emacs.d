;;; init-latex --- Latex related stuff
;;; Commentary:
;;; Code:

(use-package pdf-tools
  :ensure t)

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

;; set Okular as the default pdf viewer
;; (custom-set-variables
;;  '(TeX-view-program-selection
;;    (quote
;;     ((output-pdf "Okular")))))

(use-package company-reftex
  :ensure t)

(use-package latex-preview-pane
  :ensure t)

(pdf-loader-install)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(provide 'init-latex)
;;; init-latex.el ends here
