;;; modeline --- custom modeline (Android optimized)

;; flycheck lighter (safe)
(defun d/flycheck-lighter (state)
  (when (and (bound-and-true-p flycheck-mode)
             (fboundp 'flycheck-count-errors))
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errorp (flycheck-has-current-errors-p state))
           (err (or (cdr (assq state counts)) "?"))
           (running (eq 'running flycheck-last-status-change)))
      (if (or errorp running) (format "•%s" err)))))

(setq-default mode-line-format
              (list
               "[" (propertize "%I" 'face 'font-lock-constant-face) "] "
               "[" '(:eval (propertize "%b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name))) "] "

               '(:eval (when (vc-mode)
                         (concat "[" (propertize (substring vc-mode 1)
                                      'face 'font-lock-string-face) "] ")))

               "[" '(:eval (propertize "%m" 'face 'font-lock-string-face)) "] "

               ;; Colors based on state (simplified)
               '(:eval (progn
                         (set-face-attribute  'mode-line nil
                                              :foreground "LawnGreen"
                                              :background "#0a2832"
                                              :box '(:line-width 1 :style released-button))
                         (set-face-attribute  'mode-line-inactive nil
                                              :foreground "ForestGreen"
                                              :background "gray4"
                                              :box '(:line-width 1 :style released-button))
                         ""))

               "[" (propertize "%01l" 'face 'font-lock-type-face) ","
               (propertize "%01c" 'face 'font-lock-type-face) ":"
               '(:eval (propertize "%q" 'face 'font-lock-type-face))
               '(:eval (if (use-region-p)
                           (concat ":" (propertize (number-to-string (abs (- (point) (mark))))
                                                   'face 'font-lock-type-face))))
               "] "

               ;; flycheck errors (safe)
               '(:eval
                 (when (and (bound-and-true-p flycheck-mode)
                            (fboundp 'flycheck-count-errors)
                            (or flycheck-current-errors
                                (eq 'running flycheck-last-status-change)))
                   (concat "["
                           (cl-loop for state in '((error . "#FB4933")
                                                   (warning . "#FABD2F")
                                                   (info . "#83A598"))
                                    as lighter = (d/flycheck-lighter (car state))
                                    when lighter
                                    concat (propertize lighter 'face `(:foreground ,(cdr state))))
                           "] ")))

               ;; flyspell indicator
               '(:eval (when (bound-and-true-p flyspell-mode)
                         (propertize "[SPL] " 'face 'font-lock-string-face)))

               ;; lsp indicator (safe)
               '(:eval (when (and (derived-mode-p 'prog-mode)
                                  (bound-and-true-p lsp-mode)
                                  (fboundp 'lsp-mode-line))
                         (concat "[" (substring (lsp-mode-line) 1) "] ")))

               "[" '(:eval (propertize (format-time-string "%H:%M - %d/%B/%Y")
                                       'face 'font-lock-builtin-face)) "] "

               '(:eval (when (buffer-modified-p)
                         (propertize "[MOD]" 'face 'font-lock-warning-face)))
               '(:eval (when buffer-read-only
                         (propertize "[RO]" 'face 'font-lock-type-face)))
               ))

(provide 'modeline)
;;; modeline.el ends here
