;;; init-startup-packages -- Initial packages configuration
;;; Commentary:
;;; Code:

(package-initialize)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install use-package if not installed
(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-refresh-contents)
     (package-install package)))

(provide 'init-startup-packages)
;;; init-startup-packages ends here
