;;; starter-kit-python.el

;; Load python-mode for .py and .wsgi files
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Run starter-kit coding hooks for some useful features
(add-hook 'python-mode-hook 'run-coding-hook)

;; look for .py and .wsgi files with ffip
(eval-after-load 'find-file-in-project
  '(progn
    (add-to-list 'ffip-patterns "*.py")
    (add-to-list 'ffip-patterns "*.wsgi")))

;;; Flymake
(setq flymake-executable (concat dotfiles-dir "bin/python-flymake.sh"))
(when (load "flymake" t)
  (defun flymake-python-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file
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
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
            (load-library "flymake-no-cursor")))

;; jedi
(add-to-list 'load-path (concat dotfiles-dir "opt/emacs-jedi-03d26925d0/"))
(setq jedi:setup-keys t)
(setq jedi:key-complete (kbd "M-/"))
(setq jedi:key-goto-definition (kbd "M-g d"))

(autoload 'jedi:setup "jedi" nil t)
(add-hook 'auto-complete-mode-hook (lambda ()
                                     (define-key ac-completing-map (kbd "C-n") 'ac-next)
                                     (define-key ac-completing-map (kbd "C-p") 'ac-previous)))
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)

(require 'nose)
(eval-after-load 'python-mode
  '(progn
     (define-key py-mode-map (kbd "M-?") 'hippie-expand)
     (define-key py-mode-map (kbd "C-c t") 'nosetests-all)))

(provide 'starter-kit-python)
