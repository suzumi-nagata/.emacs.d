;;; custom-functions.el --- custom util functions -*- lexical-binding: t; -*-

(defun bk/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))
    (message "Whole line copied!")))

(defun bk/kill-inner-word ()
  "Kill the entire word your cursor is in.  Equivalent to ciw in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (let (sline)
    (setq sline (buffer-substring (point-at-bol) (point-at-eol)))
    (end-of-line)
    (newline)
    (insert sline)))

(defun move-line-up ()
  "Move current line up one line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move current line down one line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun my-insert-pair (open close)
  (if (region-active-p)
      (progn
        (let* ((mark-start (region-beginning))
               (mark-end (region-end)))
          (goto-char mark-end)
          (insert close)
          (goto-char mark-start)
          (insert open)))
    (insert open close))
  (backward-char))

(defun insert-curly-braces()
  (interactive)
  (end-of-line)
  (insert " {")
  (indent-according-to-mode)
  (newline)
  (indent-according-to-mode)
  (newline)
  (insert "}")
  (indent-according-to-mode)
  (forward-line -1)
  (end-of-line))

(defun open-line-with-reindent (n)
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun my/autosave-backup-dirs()
  (let ((emacs-bkp-dir (expand-file-name "emacs-backups/" user-emacs-directory)))
    (unless (file-exists-p emacs-bkp-dir)
      (make-directory emacs-bkp-dir))
    (setq backup-directory-alist `(("." . ,emacs-bkp-dir)))
    )
  (let ((emacs-save-dir (expand-file-name "emacs-saves/" user-emacs-directory)))
    (unless (file-exists-p emacs-save-dir)
      (make-directory emacs-save-dir))
    (setq auto-save-file-name-transforms `((".*", emacs-save-dir t)))
    )
  )

(defun close-wrong-buffer-and-find-file ()
  "Close current buffer and find file from current path."
  (interactive)
  (let ((dirname (buffer-name)))
  (call-interactively #'find-file)
  (kill-buffer dirname))
  )

(provide 'custom-functions)
;;; custom-functions.el ends here
