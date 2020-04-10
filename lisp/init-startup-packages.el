;;; init-startup-packages -- Initial packages configuration
;;; Commentary:
;;; Code:

(package-initialize)

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'init-startup-packages)
;;; init-startup-packages ends here
