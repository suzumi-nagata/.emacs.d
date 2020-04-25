;;; init-modeline --- custom modeline
;;; Commentary:
;;; Code:

(set-face-attribute  'mode-line
              nil
              :foreground "LawnGreen"
              :background "#0a2832"
              :box '(:line-width 1 :style released-button))

(set-face-attribute  'mode-line-inactive
              nil
              :foreground "ForestGreen"
              :background "gray4"
              :box '(:line-width 1 :style released-button))

(setq-default transient-mark-mode 't)

;; flycheck
(defun d/flycheck-lighter (state)
  "Return flycheck information for the given error type STATE.

Source: https://git.io/vQKzv"
  (let* ((counts (flycheck-count-errors flycheck-current-errors))
         (errorp (flycheck-has-current-errors-p state))
         (err (or (cdr (assq state counts)) "?"))
         (running (eq 'running flycheck-last-status-change)))
    (if (or errorp running) (format "•%s" err))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               custom modeline                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Thanks to https://www.reddit.com/r/emacs/comments/701pzr/flycheck_error_tally_in_custom_mode_line/
(setq-default mode-line-format
              (list

               "["  ;; Size of file
               (propertize "%I" 'face 'font-lock-constant-face)
               "] "

               "["  ;; Buffer name with file name as tooltip
               '(:eval (propertize "%b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
               "] "

               '(:eval (when (vc-mode)
                         (concat
                          "["
                          (propertize (substring vc-mode 1)
                                      'face 'font-lock-string-face)
                          "] "
                          )))

               "["  ;; Major mode
               '(:eval (propertize "%m" 'face 'font-lock-string-face)) "] "

               ;; god mode
               '(:eval (if (bound-and-true-p god-local-mode)
                           (progn
                             (set-face-attribute  'mode-line
                                                  nil
                                                  :foreground "#209d6e"
                                                  :background "#490f25"
                                                  :box '(:line-width 1 :style released-button))

                             (set-face-attribute  'mode-line-inactive
                                                  nil
                                                  :foreground "#c57b57"
                                                  :background "#080340"
                                                  :box '(:line-width 1 :style released-button)))
                         (progn
                           (set-face-attribute  'mode-line
                                                nil
                                                :foreground "LawnGreen"
                                                :background "#0a2832"
                                                :box '(:line-width 1 :style released-button))

                           (set-face-attribute  'mode-line-inactive
                                                nil
                                                :foreground "ForestGreen"
                                                :background "gray4"
                                                :box '(:line-width 1 :style released-button))
                           )
                         ))

               "[" ;; Line, column and percentage
               (propertize "%01l" 'face 'font-lock-type-face)
               ","
               (propertize "%01c" 'face 'font-lock-type-face)
               ":"
               '(:eval (propertize "%q" 'face 'font-lock-type-face))
               '(:eval (if (use-region-p)
                           (concat
                            ":"
                            (propertize (number-to-string (abs (- (point) (mark)))) 'face 'font-lock-type-face)
                            )))
               "] "

               ;; flycheck errors
               '(:eval
                 (when (and (bound-and-true-p flycheck-mode)
                            (or flycheck-current-errors
                                (eq 'running flycheck-last-status-change)))
                   (concat "["
                           (cl-loop for state in '((error . "#FB4933")
                                                   (warning . "#FABD2F")
                                                   (info . "#83A598"))
                                    as lighter = (d/flycheck-lighter (car state))
                                    when lighter
                                    concat (propertize
                                            lighter
                                            'face `(:foreground ,(cdr state))))
                           "] ")))

               ;; "["  ;; minor modes (for now until I decide what is needed or not)
               ;; 'minor-mode-alist
               ;; "] "

               ;; flyspell indicator
               '(:eval (when (bound-and-true-p flyspell-mode)
                         (propertize
                          "[SPL]"
                          'face 'font-lock-string-face)))

               ;; lsp indicator on prog mode
               '(:eval (when (derived-mode-p 'prog-mode)
                         (concat
                          "["
                          (substring (lsp-mode-line) 1)
                          "] ")))

               "[" ;; time and date
               '(:eval (propertize
                        (format-time-string "%H:%M - %d/%B/%Y")
                        'face 'font-lock-builtin-face))
               "] "

               ;; Modified/Read only
               '(:eval (when (buffer-modified-p)
                         (propertize "[MOD]"
                                     'face 'font-lock-warning-face
                                     'help-echo "Buffer has been modified")))
               '(:eval (when buffer-read-only
                         (propertize "[RO]"
                                     'face 'font-lock-type-face
                                     'help-echo "Buffer is read only")))
               ))

(provide 'init-modeline)
;;; init-modeline.el ends here
