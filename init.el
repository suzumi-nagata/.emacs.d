;;; init.el
;;; Commentary:

;; This config file is inspired by https://github.com/purcell/emacs.d where the
;; configuration is split into multiple files to better organize them

;;; Code:

;; Uncomment to debug
;;(setq debug-on-error t)

;; Add ~/.emacs.d/lisp to load the rest of configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Adjust garbage collection during startup and restores original afterwards
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Startup configurations
(require 'init-startup-packages)
(require 'init-config-dirs)
