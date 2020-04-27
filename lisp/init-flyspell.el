;;; init-flyspell --- Spell checking
;;; Commentary:
;;; Code:

(require 'ispell)
(setq ispell-program-name "/usr/bin/aspell")

(use-package helm-flyspell
  :ensure t)

(defun flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))


(defun check-previous-spelling-error ()
  "Jump to previous spelling error and correct it."
  (interactive)
  (push-mark-no-activate)
  (flyspell-goto-previous-error 1)
  (call-interactively 'helm-flyspell-correct))

(defun check-next-spelling-error ()
  "Jump to next spelling error and correct it."
  (interactive)
  (push-mark-no-activate)
  (flyspell-goto-next-error)
  (call-interactively 'helm-flyspell-correct))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region.
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(use-package flyspell-lazy
  :ensure t
  :init
  ;; If you are using aspell instead of ispell on the backend, the following setting may improve performance:
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  :config
  (flyspell-lazy-mode 1))

(provide 'init-flyspell)
;;; init-flyspell.el ends here
