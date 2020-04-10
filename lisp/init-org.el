;;; init-org.el --- org-mode configurations
;;; Commentary:
;;; Code:

;; ros takes a screenshot of a region and put in org format
(defun ros ()
  (interactive)
  (if buffer-file-name
      (progn
        (message "Waiting for region selection with mouse...")
        (let ((filename
               (concat "./images/"
                       (format-time-string "%Y%m%d_%H%M%S")
                       ".png")))
          (unless (file-exists-p "./images")
            (make-directory "./images"))
          (if (executable-find "scrot")
              (call-process "scrot" nil nil nil "-s" filename)
            (call-process "screencapture" nil nil nil "-s" filename))
          (insert (concat "[[file:" filename "]]"))
          )
        (message "File created and linked...")
        )
    (message "You're in a not saved buffer! Save it first!")))

(provide 'init-org)
;;; init-org.el ends here
