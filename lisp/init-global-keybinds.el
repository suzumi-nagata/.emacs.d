;;; init-global-keybinds.el -- Global keybinds used on every buffer
;;; Commentary:
;;; Code:

;; Toggles the relative line numbering
(global-set-key (kbd "C-c 1") 'linum-mode)

;; Replace string keystroke
(global-set-key (kbd "C-c r") 'replace-string)

;; Query replace string keystroke
(global-set-key (kbd "C-c R") 'query-replace)

;; Adjust window size keystrokes
(global-set-key (kbd "M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-S-<down>") 'shrink-window)
(global-set-key (kbd "M-S-<up>") 'enlarge-window)

;; Switch selected window
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<right>") 'windmove-right)

;; Vim keybindings
(global-set-key (kbd "M-s-h") 'windmove-left)
(global-set-key (kbd "M-s-j") 'windmove-down)
(global-set-key (kbd "M-s-k") 'windmove-up)
(global-set-key (kbd "M-s-l") 'windmove-right)

;; Change window keystroke
(global-set-key (kbd "C-ç") 'other-window)

;; Compile Command
(global-set-key "\C-x\C-m" 'compile)

;; Fast visit buffers
(global-set-key (kbd "<f1>")
                (lambda()
                  (interactive)
                  (org-wiki-index)))

(global-set-key (kbd "<f10>")
                '(lambda()
                   (interactive)
                   (find-file "~/Common/PIBIC/3_org_files/Index.org")))

(global-set-key (kbd "<f11>")
                '(lambda()
                   (interactive)
                   (find-file "~/Common/SelfLearning/SelfLearning.org")))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; split window and change focus
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Kill current buffer instead of asking
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key (kbd "C-c d") 'duplicate-line)

(global-set-key (kbd "C-c c") 'insert-curly-braces)

(global-set-key (kbd "C-o") 'open-line-with-reindent)

(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)

(global-set-key (kbd "C-c p") 'insert-parenthesis)
(global-set-key (kbd "C-c n") 'insert-curl-brackets)
(global-set-key (kbd "C-c b") 'insert-brackets)
(global-set-key (kbd "C-c e") 'insert-double-quotes)
(global-set-key (kbd "C-c x") 'insert-simple-quotes)
(global-set-key (kbd "C-c ,") 'insert-tag)
(global-set-key (kbd "C-c v") 'insert-verbatim-equals)

(global-set-key (kbd "C-x M-f") 'close-wrong-buffer-and-find-file)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)

;; Org mode
(define-key global-map "\C-c l l" 'org-store-link)
(global-set-key (kbd "C-c C-ç") 'org-capture)
(global-set-key (kbd "C-<f1>") 'vterm-other-window)
;; (global-set-key (kbd "C-<f1>") (lambda () (interactive) (org-preview-latex-fragment '(16))))
;; (global-set-key (kbd "C-<f2>") 'org-toggle-inline-images)
(global-set-key (kbd "<f12>") 'org-agenda)

;; expand region
(global-set-key (kbd "C-q") 'er/expand-region)

;; ispell
(global-set-key (kbd "C-<f11>") 'check-previous-spelling-error)
(global-set-key (kbd "C-<f12>") 'check-next-spelling-error)

(global-set-key (kbd "C-c o") 'xref-find-definitions)
(global-set-key (kbd "C-M-p") 'xref-pop-marker-stack)

;; god mode
(global-set-key (kbd "<escape>") 'god-mode-all)
(global-set-key (kbd "C-x C-2") (lambda ()
                          (interactive)
                          (split-window-vertically)
                          (other-window 1)))
(global-set-key (kbd "C-x C-3") (lambda ()
                          (interactive)
                          (split-window-horizontally)
                          (other-window 1)))

(global-set-key (kbd "C-x C-1") 'delete-other-windows)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-0") 'delete-window)

(global-set-key (kbd "<f6>") 'spray-mode)

(global-set-key (kbd "C-c f") 'find-file-project-root)

;; dumb-jump
(global-set-key (kbd "C-c u") 'dumb-jump-go)

(provide 'init-global-keybinds)
;;; init-global-keybinds.el ends here
