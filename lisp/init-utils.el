;;; init-utils.el -- Usefull functions for editing
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

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
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;----------------------------------------------------------------------------
;; Kill current buffer instead of asking what buffer to kill
;;----------------------------------------------------------------------------
(defun kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;----------------------------------------------------------------------------
;; close all open buffers
;;----------------------------------------------------------------------------
(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

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

;; Move the current line up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

;; Move the current line down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; tag insertion
(defun insert-tag()
  (interactive)
  (my-insert-pair "<" ">"))

;; Parenthesis insertion
(defun insert-parenthesis()
  (interactive)
  (my-insert-pair ?( ?)))

;; Double quotes insertion
(defun insert-double-quotes()
  (interactive)
  (my-insert-pair  "\"" "\""))

;; Simple quotes insertion
(defun insert-simple-quotes()
  (interactive)
  (my-insert-pair  "'" "'"))

;; Square Braket insertion
(defun insert-brackets()
  (interactive)
  (my-insert-pair ?[ ?]))

;; Curly Braces insertion
(defun insert-curl-brackets()
  (interactive)
  (my-insert-pair ?{ ?}))

;; Verbatim Insertion
(defun insert-verbatim-equals()
  (interactive)
  (my-insert-pair ?\= ?\=))

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
  (previous-line)
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
;; Find file starting from a PROJECT root.
;; (Usefull if the project is inside many directories)
;;----------------------------------------------------------------------------
; TODO: Fazer um jeito de ler os nomes e paths a partir de um arquivo
;; (split-string (f-read-text org-uni-units-file-location) "\n")
(defvar projects-roots-path '(("PIBIC" . "~/Common/PIBIC/")
                              ("go" . "~/Common/Projects/go/src/")
                              ("elisp" . "~/.emacs.d/lisp")))
(defun find-file-project-root (project)
  "Find file starting from a PROJECT root."
  (interactive (list (completing-read
                      "Select a project: "
                      projects-roots-path
                      nil t)))
  (find-file (cdr (assoc project projects-roots-path)))
  (close-wrong-buffer-and-find-file))

;;----------------------------------------------------------------------------
;; Activate flyspell with pt or en
;;----------------------------------------------------------------------------

(defun spell (language)
  "Enable flyspell in this buffer for LANGUAGE as en or pt."
  (interactive "sLanguage: ")
  (if (string= "pt" language)
      (ispell-change-dictionary "pt_BR"))
  (if (string= "en" language)
      (ispell-change-dictionary "en_US"))
  (if (string= "" language)
      (flyspell-mode 0)
    (flyspell-mode 1))
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

(provide 'init-utils)
;;; init-utils.el ends here
