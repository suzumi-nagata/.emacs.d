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
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))

;;----------------------------------------------------------------------------
;; Configurations (files located at ~/.emacs.d/lisp)
;;----------------------------------------------------------------------------

;; Initialize packages
(require 'init-startup-packages)

;; Configure some default directories like auto-save
(require 'init-config-dirs)
;; Theme and colors
(require 'init-appearance)
(require 'init-utils)
(require 'init-global-keybinds)
(require 'init-productivity)
