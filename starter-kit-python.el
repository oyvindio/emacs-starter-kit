;;; starter-kit-python.el

;; For this to work properly, you need:
;; * the ipython executable on your $PATH (for ipython buffer)
;; * pep8, pylint and pyflakes on your $PATH (for
;; pylint_etc_wrapper.py). these can be installed with pip.
;;
;; Note: To explicitly tell pymacs which python it should use,
;; set the PYMACS_PYTHON environment variable to the full path to the
;; python executable.

;; Load python-mode for .py and .wsgi files
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Run starter-kit coding hooks for some useful features
(add-hook 'python-mode-hook 'run-coding-hook)

;; Fix broken tab completion in ipython buffer
(setq ipython-completion-command-string
      "print(';'.join(__IP.Completer.all_completions('%s')))\n")

;;; Flymake
;; Run `pyflymake` (pylint, pyflakes and pep8) on python files with flymake
(setq flymake-executable (concat dotfiles-dir "/bin/" "pylint_etc_wrapper.py"))
(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list flymake-executable (list local-file))))
  (push '(".+\\.py$" flymake-python-init) flymake-allowed-file-name-masks))

;;; Pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(when (eq system-type 'darwin)
  (eval-after-load "pymacs"
    (setq pymacs-python-command "/usr/local/bin/python")))



(defun lazy-load-ropemacs-mode ()
  "Activates ropemacs-mode. Loads ropemacs through pymacs if not
loaded yet."
  (when (not (fboundp 'ropemacs-mode))
    (pymacs-load "ropemacs" "rope-"))
  (ropemacs-mode t))

(eval-after-load "ropemacs"
  (lambda ()
    ;; don't ask permission to save buffer before running refactorings
    (setq ropemacs-confirm-saving nil)
    ;; try to open ropemacs project files automagically
    (setq ropemacs-guess-project t)))

(add-hook 'python-mode-hook
          (lambda ()
            ; load ropemacs, but only once!
            (lazy-load-ropemacs-mode)
            ; Activate flymake unless buffer is a tmp buffer for the interpreter
            (unless (eq buffer-file-name nil) (flymake-mode t))
            ;; Bind a few keys for navigating errors
            (local-set-key (kbd "C-c w") 'show-fly-err-at-point)
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
            (load-library "flymake-no-cursor")))

;;; Use ipython
(require 'ipython)

;; TODO: bindings for pydoc, a-la ri in starter-kit-ruby.el
;; TODO: self-contained pymacs+ropemacs?

(provide 'starter-kit-python)
