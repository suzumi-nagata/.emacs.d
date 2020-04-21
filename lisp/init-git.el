;;; init-git --- git management on emacs
;;; Commentary:
;;; Code:

;; git 50/70 rule
(use-package magit
  :ensure t
  :config
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-x g" . magit-status))

(add-hook 'git-commit-mode-hook #'(lambda ()
                                    (ispell-change-dictionary "en_US")
                                    (flyspell-mode)))

(provide 'init-git)
;;; init-git.el ends here
