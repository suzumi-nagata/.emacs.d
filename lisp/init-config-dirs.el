;;; init-config-dirs.el
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun my/autosave-backup-dirs()
  (let ((emacs-bkp-dir (expand-file-name "emacs-backups/" user-emacs-directory)))
      (unless (file-exists-p emacs-bkp-dir)
        (make-directory emacs-bkp-dir))
      (setq backup-directory-alist '(("." . emacs-bkp-dir)))
      )
  (let ((emacs-save-dir (expand-file-name "emacs-saves/" user-emacs-directory)))
    (unless (file-exists-p emacs-save-dir)
      (make-directory emacs-save-dir))
    (setq auto-save-file-name-transforms `((".*", emacs-save-dir t)))
    )
  )

(my/autosave-backup-dirs)

(provide 'init-config-dirs)
;;; init-config-dirs.el
