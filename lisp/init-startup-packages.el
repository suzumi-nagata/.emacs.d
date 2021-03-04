;;; init-startup-packages -- Initial packages configuration
;;; Commentary:
;;; Code:

(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Auto update packages every 4 days
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(provide 'init-startup-packages)
;;; init-startup-packages.el ends here
