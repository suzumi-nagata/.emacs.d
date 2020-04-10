;;; init-config-dirs.el
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq backup-directory-alist '(("." . "~/Common/.emacs-backups")))
(setq auto-save-file-name-transforms `((".*" ,"~/Common/.emacs-saves/" t)))

(provide 'init-config-dirs)
;;; init-config-dirs.el
