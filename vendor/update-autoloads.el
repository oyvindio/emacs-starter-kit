; put this path into the load-path automatically
;;;###autoload
(progn
  (setq load-path (cons (file-name-directory load-file-name) load-path)))

;;;###autoload
(defun update-vendor-autoloads (&optional file)
  "Update autoloads for files in the diretory containing this file."
  (interactive)
  (let ((base (file-truename
       (file-name-directory 
        (symbol-file 'update-vendor-autoloads 'defun)))))
    (require 'autoload)         ;ironic, i know
    (let ((generated-autoload-file (concat base "loaddefs.el")))
      (when (not (file-exists-p generated-autoload-file))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";;") ;; create the file with non-zero size to appease autoload
          (save-buffer)))
      (cd base)
      (if file
          (update-file-autoloads file)
        (update-directory-autoloads base)))))

;;;###autoload
(defun update-vendor-autoloads-for-file (file)
  (interactive "f")
  (update-vendor-autoloads file))
