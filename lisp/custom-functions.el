;;; custom-functions.el --- custom util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

;;----------------------------------------------------------------------------
;; Proper C-w behavior (borrowed from https://stackoverflow.com/a/14047437/3581311)
;;----------------------------------------------------------------------------
(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'.
Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))

;;----------------------------------------------------------------------------
;; duplicate line
;;----------------------------------------------------------------------------
(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (let (sline)
    (setq sline (buffer-substring (point-at-bol) (point-at-eol)))
    (end-of-line)
    (newline)
    (insert sline)))

;;----------------------------------------------------------------------------
;; Move the current line up (and down)
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; Insert pairs: insert a pair from the arguments. If the region is selected
;; put the pair around the region.
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; Curly brackets insertion
;;----------------------------------------------------------------------------
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

;;----------------------------------------------------------------------------
;; Open line and tabs
;;----------------------------------------------------------------------------
(defun open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
  If there is a fill prefix and/or a `left-margin', insert them
  on the new line if the line would have been blank.
  With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
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

;; If you have a ansi colored file
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun my/autosave-backup-dirs()
  (let ((emacs-bkp-dir (expand-file-name "emacs-backups/" user-emacs-directory)))
    (unless (file-exists-p emacs-bkp-dir)
      (make-directory emacs-bkp-dir))
    ;; don't know why this variable isn't setting properly, so it is hardcoded
    ;; for now
    (setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backups/")))
    )
  (let ((emacs-save-dir (expand-file-name "emacs-saves/" user-emacs-directory)))
    (unless (file-exists-p emacs-save-dir)
      (make-directory emacs-save-dir))
    (setq auto-save-file-name-transforms `((".*", emacs-save-dir t)))
    )
  )

;;----------------------------------------------------------------------------
;; Close compilation buffer if there is no errors
;;----------------------------------------------------------------------------

(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

;;----------------------------------------------------------------------------
;; If you have open a wrong buffer but want to keep searching the right one
;;----------------------------------------------------------------------------

(defun close-wrong-buffer-and-find-file ()
  "Close current buffer and find file from current path."
  (interactive)
  (let ((dirname (buffer-name)))
  (call-interactively #'find-file)
  (kill-buffer dirname))
  )

;;----------------------------------------------------------------------------
;; Find a file from a list of projects
;;----------------------------------------------------------------------------

(require 'cl-lib)

(defun alist-project-files (full-file-path )
  "Build an alist from the files in FULL-FILE-PATH."
  (interactive)
  (let ((full-path-name (expand-file-name full-file-path)))
    (cl-pairlis (directory-files full-path-name nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
                (directory-files full-path-name 1 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(defvar projects-roots-path)
(setq projects-roots-path '(("downloads" . "~/Downloads")
                            ("backup" . "~/Common/Backup")
                            ("init" . "~/.emacs.d")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Common/Projects/Programming/c")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Common/Projects/Programming/python")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Common/Projects/Programming/js")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/.config")))

(defun find-file-project-root (project)
  "Find file starting from a PROJECT root."
  (interactive (list (completing-read
                      "Select a project: "
                      projects-roots-path
                      nil t)))
  (find-file (cdr (assoc project projects-roots-path)))
  (close-wrong-buffer-and-find-file))

(provide 'custom-functions)
;;; custom-functions.el ends here
