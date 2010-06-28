;;; starter-kit-python.el

;; For this to work properly, you need:
;; * the ipython executable on your $PATH (for ipython buffer)
;; * pep8, pylint and pyflakes on your $PATH (for
;; pylint_etc_wrapper.py). these can be installed with pip.

;; Load python-mode for .py files
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
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

(add-hook 'python-mode-hook
          (lambda ()
            ; Activate flymake unless buffer is a tmp buffer for the interpreter
            (unless (eq buffer-file-name nil) (flymake-mode t))
            ;; Bind a few keys for navigating errors
            (local-set-key (kbd "C-c w") 'show-fly-err-at-point)
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))

;;; Pymacs + ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-confirm-saving 'nil)

;;; Use ipython
(require 'ipython)

;; TODO: bindings for pydoc, a-la ri in starter-kit-ruby.el
;; TODO: self-contained pymacs+ropemacs?

(provide 'starter-kit-python)
