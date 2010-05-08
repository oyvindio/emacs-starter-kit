;;; custom-defuns.el --- my custom defuns

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

;; source: http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun gist-buffer-confirm (&optional private)
  "Gists the current buffer (optionally creating a private gist), after asking
 for confirmation."
  (interactive "P")
  (when (yes-or-no-p "Are you sure you want to Gist this buffer? ")
    (gist-region-or-buffer private)))

(defun diw-word-count ()
  "Count words in buffer, excluding (X)HTML markup."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "diw-wc.py"))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun move-file-and-buffer (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil) t))))

(provide 'custom-defuns)
