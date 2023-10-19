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
(load custom-file)

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

(defun alist-project-files (full-file-path)
  "Build an alist from the files in FULL-FILE-PATH."
  (interactive)
  (let ((full-path-name (expand-file-name full-file-path)))
    (cl-pairlis (directory-files full-path-name nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
                (directory-files full-path-name 1 "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))

(defvar projects-roots-path)
(setq projects-roots-path '(("downloads" . "~/Downloads")
                            ("init" . "~/.emacs.d")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/.config")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Programming")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Programming/go")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Programming/c")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Programming/java")))
(setq projects-roots-path (append projects-roots-path (alist-project-files "~/Org")))

(defun find-file-project-root (project)
  "Find file starting from a PROJECT root."
  (interactive (list (completing-read
                      "Select a project: "
                      projects-roots-path
                      nil t)))
  (find-file (cdr (assoc project projects-roots-path)))
  (close-wrong-buffer-and-find-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;      Org agenda last day files      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-journal-last-files (org-journal-file-path)
  "Return a list with the last 8 days from the files in ORG-JOURNAL-FILE-PATH."
  (interactive)
  (let ((full-path-name (expand-file-name org-journal-file-path))
        (current-date-delta 0)
        (journal-files-regex "^\\("))
    (while (> current-date-delta -8)
      (setq journal-files-regex
            (concat journal-files-regex
                    (format-time-string "%Y-%m-%d"
                                        ;; multiply current-date-delta with seconds in a day
                                        (time-add (current-time) (* current-date-delta 86400)))))
      (setq current-date-delta (1- current-date-delta))
      (if (not (eq current-date-delta -8))
          (setq journal-files-regex (concat journal-files-regex "\\|"))
        (setq journal-files-regex (concat journal-files-regex "\\)"))
        )
      )
    (directory-files full-path-name 1 journal-files-regex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reverse characters in a line. Taken from:
;; https://emacs.stackexchange.com/questions/38156/reversing-the-order-of-letters-characters-of-a-selected-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun horizontal-reverse-region (beg end)
  "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                             ; Make the bg transparent (for pretty terminals) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-bg-transparent ()
  "Make the background of Emacs transparent."
  (interactive)
  (set-face-background 'default "unspecified-bg" (selected-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Copy current file name and line (stolen from https://gist.github.com/kristianhellquist/3082383) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; adapted function to generate the org noter skeleton ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-org-noter-create-skeleton ()
  "Create notes skeleton with the PDF outline or annotations.
Only available with PDF Tools."
  (interactive)
  (org-noter--with-valid-session
   (cond
    ((eq (org-noter--session-doc-mode session) 'pdf-view-mode)
     (let* ((ast (org-noter--parse-root))
            (top-level (org-element-property :level ast))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type  (alist-get 'type item))
                   (page  (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top   (alist-get 'top item)))
               (when (and (eq type 'goto-dest) (> page 0))
                 (push (vector title (cons page top) (1+ depth) nil) output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type  (alist-get 'type item))
                      (page  (alist-get 'page item))
                      (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents)
                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight)  "Highlight")
                                      ((eq type 'underline)  "Underline")
                                      ((eq type 'squiggly)   "Squiggly")
                                      ((eq type 'text)       "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type  link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text)

                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push (vector heading-text (cons page top) 'inside nil) output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title          (aref data 0)
                     location       (aref data 1)
                     relative-level (aref data 2)
                     contents       (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (when (car contents)
                 (org-noter--insert-heading level (car contents)))

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))

               (when (cdr contents)
                 (insert (cdr contents)))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))))

    (t (user-error "This command is only supported on PDF Tools.")))))

(provide 'custom-functions)
;;; custom-functions.el ends here
