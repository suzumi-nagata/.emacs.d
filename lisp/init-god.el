;;; init-god --- emacs modal editing mode
;;; Commentary:
;;; Code:

(use-package god-mode
  :ensure t
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "i") 'god-local-mode)

  (define-key god-local-mode-map (kbd "C-c C-o") 'xref-find-definitions)
  (define-key god-local-mode-map (kbd "C-c C-p") 'xref-pop-marker-stack)

  (define-key god-local-mode-map (kbd "C-x C-b") 'ivy-switch-buffer)
  (define-key god-local-mode-map (kbd "C-x C-S-b") 'ibuffer)
  (define-key god-local-mode-map (kbd "C-<f12>") 'org-agenda)
  (define-key god-local-mode-map (kbd "I") 'mortal-mode))

(define-minor-mode mortal-mode
  "Allow temporary departures from god-mode."
  :lighter " mortal"
  :keymap '(([return] . (lambda ()
                          "Exit mortal-mode and resume god mode." (interactive)
                          (god-local-mode-resume)
                          (mortal-mode 0))))
  (when mortal-mode
    (god-local-mode-pause)))

(provide 'init-god)
;;; init-god.el ends here
