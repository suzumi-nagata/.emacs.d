;;; init-ibuffer.el -- Better buffer management
;;; Commentary:
;;; Code:

(setq ibuffer-saved-filter-groups
      '(("home"
         ;; configs
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")))
         ;; Programming
         ("c / c++" (or (mode . c-mode)
                        (mode . c++-mode)))
         ("Python" (mode . python-mode))
         ("go" (mode . go-mode))
         ("Makefile" (mode . makefile-mode))
         ;; Certs
         ("Keys / Certs" (or (filename . "\.key")
                             (filename . "\.crt")
                             (filename . "\.pem")
                             (filename . "\.der")))
         ;; Tools
         ("Google Translate" (name . "*Google Translate*"))
         ("Keys / Certs" (or (filename . ".key")
                             (filename . ".crt")
                             (filename . ".pem")
                             (filename . ".der")))
         ("Magit" (or (name . "\*magit")
                      (mode . magit-process-mode)
                      (mode . magit-log-mode)
                      (mode . magit)
                      (mode . magit-status-mode)
                      (mode . magit-revision-mode)
                      ))
         ("Subversion" (name . "\*svn"))
         ("ag-search" (mode . ag-mode))
         ("Dired" (mode . dired-mode))
         ("shell" (mode . shell-mode))
         ("Eglot" (name . "EGLOT"))
         ("Flycheck / Flymake" (name . "Flymake"))
         ("ag-search" (mode . ag-mode))
         ;; Org mode
         ("Org-roam" (or (filename . "roam")
                         (name . "*org-roam*")))
         ("Agenda" (or (filename . "Agenda")
                       (mode . org-agenda-mode)))
         ("Unicamp" (filename . "Unicamp"))
         ("Wiki" (filename . "Wiki"))
         ("Org" (or (mode . org-mode)
                    (filename . "OrgMode")))
         ;; Buffers to ignore but I do not want to close them
         ("httpd" (name . "*httpd*"))
         ("Help / Warning / Messages" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")
                     (name . "\*Warning\*"))))))

(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups "home")))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
