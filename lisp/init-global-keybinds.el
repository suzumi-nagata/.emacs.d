;;; init-global-keybinds.el -- Global keybinds used on every buffer
;;; Commentary:
;;; Code:

;; Toggles the relative line numbering
(global-set-key (kbd "C-c 1") 'linum-mode)

;; Replace string keystroke
(global-set-key (kbd "C-c s") 'replace-string)

;; Query replace string keystroke
(global-set-key (kbd "C-c S") 'query-replace)

;; Adjust window size keystrokes
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Change window keystroke
(global-set-key (kbd "C-ç") 'other-window)

;; Compile Command
(global-set-key "\C-x\C-m" 'compile)

;; Fast visit buffers
(global-set-key (kbd "<f1>")
                (lambda()
                  (interactive)
                  (org-wiki-index)))

(global-set-key (kbd "<f10>") '
                (lambda()
                  (interactive)
                  (find-file "~/Common/PIBIC/3_org_files/Index.org")))

(global-set-key (kbd "<f11>") '
                (lambda()
                  (interactive)
                  (find-file "~/Common/SelfLearning/SelfLearning.org")))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(provide 'init-global-keybinds)
;;; init-global-keybinds.el ends here
