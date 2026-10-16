;;; obsidian-qmd.el --- Semantic vault search via qmd -*- lexical-binding: t; -*-

;; Thin async wrapper around the `qmd' CLI (https://github.com/tobi/qmd)
;; for use with obsidian.el.  Runs `qmd query' (hybrid BM25 + vector +
;; rerank) against the vault and presents results with `completing-read'.

;; Requires qmd to be installed and the vault indexed:
;;   qmd collection add ~/garden/content/ --name vault
;;   qmd update && qmd embed

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'obsidian nil t) ;; for `obsidian-directory' when available

(defgroup obsidian-qmd nil
  "Semantic search of an Obsidian vault using qmd."
  :group 'obsidian
  :prefix "obsidian-qmd-")

(defcustom obsidian-qmd-executable "qmd"
  "Path to the qmd executable."
  :type 'string)

(defcustom obsidian-qmd-command "query"
  "Qmd subcommand used for searches.
\"query\" is the full hybrid pipeline (expansion + BM25 + vectors +
reranking).  \"vsearch\" is semantic-only, \"search\" is instant BM25
keyword search."
  :type '(choice (const "query") (const "vsearch") (const "search")))

(defcustom obsidian-qmd-num-results 20
  "Maximum number of results to request."
  :type 'integer)

(defcustom obsidian-qmd-vault-root
  (if (boundp 'obsidian-directory)
      (expand-file-name obsidian-directory)
    "~/garden/content/")
  "Filesystem root of the indexed vault.
Used to resolve the qmd:// URIs qmd returns into real paths."
  :type 'directory)

(defvar obsidian-qmd--process nil
  "Currently running qmd search process, if any.")

(defun obsidian-qmd--result-file (result)
  "Resolve the file of RESULT (a qmd JSON alist) to a local path."
  (let ((file (alist-get 'file result)))
    (cond
     ;; qmd://collection/relative/path.md
     ((and (stringp file) (string-prefix-p "qmd://" file))
      (let* ((rest (substring file (length "qmd://")))
             (rel (if (string-match "/" rest)
                      (substring rest (match-end 0))
                    rest)))
        (expand-file-name rel obsidian-qmd-vault-root)))
     ((stringp file) (expand-file-name file))
     (t nil))))

(defun obsidian-qmd--format-candidate (result)
  "Build a display string for qmd RESULT."
  (let* ((title (or (alist-get 'title result)
                    (file-name-nondirectory
                     (or (obsidian-qmd--result-file result) "?"))))
         (snippet (or (alist-get 'snippet result)
                      (alist-get 'context result)
                      (alist-get 'body result)
                      ""))
         (snippet (replace-regexp-in-string "[\n\r]+" " " snippet))
         (snippet (truncate-string-to-width snippet 80 nil nil "…"))
         (score (alist-get 'score result)))
    (format "%s  %s  %s"
            (propertize (if score (format "%.2f" score) "  -  ")
                        'face 'shadow)
            (propertize title 'face 'bold)
            (propertize snippet 'face 'font-lock-comment-face))))

(defun obsidian-qmd--parse-results (proc-buffer)
  "Extract the JSON result list from PROC-BUFFER.
Tolerates progress/warning lines qmd may print before the JSON."
  (with-current-buffer proc-buffer
    (goto-char (point-min))
    (when (re-search-forward "^[ \t]*\\([\[{]\\)" nil t)
      (goto-char (match-beginning 1))
      (condition-case nil
          (let ((json-array-type 'list)
                (json-object-type 'alist))
            (json-read))
        (error nil)))))

(defun obsidian-qmd--handle-output (proc-buffer query)
  "Parse qmd JSON output in PROC-BUFFER and offer to visit a result."
  (let ((raw (with-current-buffer proc-buffer (buffer-string)))
        (results (obsidian-qmd--parse-results proc-buffer)))
    (kill-buffer proc-buffer)
    (if (null results)
        (progn
          (message "qmd: no usable results for \"%s\" (see *qmd-output* buffer)"
                   query)
          (with-current-buffer (get-buffer-create "*qmd-output*")
            (erase-buffer)
            (insert raw)))
      (kill-buffer (get-buffer "*qmd-output*")))
    (if (null results)
        (message "qmd: no results for \"%s\"" query)
      (let* ((candidates (mapcar #'obsidian-qmd--format-candidate results))
             (table (lambda (s pred action)
                      (if (eq action 'metadata)
                          '(metadata (display-sort-function . identity)
                                     (cycle-sort-function . identity))
                        (complete-with-action action candidates s pred))))
             (choice (completing-read (format "qmd \"%s\": " query)
                                      table nil t))
             (idx (cl-position choice candidates :test #'equal))
             (result (and idx (nth idx results)))
             (file (and result (obsidian-qmd--result-file result)))
             (line (and result (alist-get 'line result))))
        (if (and file (file-exists-p file))
            (progn
              (find-file file)
              (when (integerp line)
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter)))
          (message "qmd: could not resolve file for \"%s\"" choice))))))

;;;###autoload
(defun obsidian-qmd-search (query)
  "Search the vault semantically for QUERY using qmd.
Runs asynchronously; results are offered via `completing-read' and
selecting one visits the file."
  (interactive "Mqmd query: ")
  (unless (executable-find obsidian-qmd-executable)
    (user-error "qmd executable not found (install with: npm install -g @tobilu/qmd)"))
  (when (process-live-p obsidian-qmd--process)
    (when (yes-or-no-p "A qmd search is already running; kill it? ")
      (kill-process obsidian-qmd--process))
    (when (process-live-p obsidian-qmd--process)
      (user-error "Previous search still running")))
  (message "qmd: searching for \"%s\"… (cold queries load local models and can take a while)"
           query)
  (let ((buf (generate-new-buffer " *qmd-output*"))
        (err-buf (generate-new-buffer " *qmd-errors*")))
    (setq obsidian-qmd--process
          (make-process
           :name "obsidian-qmd"
           :buffer buf
           :command (list obsidian-qmd-executable
                          obsidian-qmd-command
                          query
                          "--json"
                          "-n" (number-to-string obsidian-qmd-num-results))
           :stderr err-buf
           :noquery t
           :sentinel
           (lambda (proc event)
             (cond
              ((string-prefix-p "finished" event)
               (kill-buffer err-buf)
               (obsidian-qmd--handle-output (process-buffer proc) query))
              ((memq (process-status proc) '(exit signal))
               (let ((errs (with-current-buffer err-buf (buffer-string))))
                 (kill-buffer err-buf)
                 (kill-buffer (process-buffer proc))
                 (message "qmd %s failed: %s — %s"
                          obsidian-qmd-command
                          (string-trim event)
                          (truncate-string-to-width
                           (string-trim errs) 200 nil nil "…"))))))))))

;;;###autoload
(defun obsidian-qmd-search-region-or-query ()
  "Search qmd for the active region, or prompt for a query."
  (interactive)
  (if (use-region-p)
      (obsidian-qmd-search (buffer-substring-no-properties
                            (region-beginning) (region-end)))
    (call-interactively #'obsidian-qmd-search)))

(provide 'obsidian-qmd)
;;; obsidian-qmd.el ends here
