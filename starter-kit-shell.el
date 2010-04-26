;;; starter-kit-shell.el

;; Enable ANSI colors for shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Bindings for navigating the commandline history
(eval-after-load 'shell
  '(progn
     (define-key shell-mode-map [up] 'comint-previous-input)
     (define-key shell-mode-map [down] 'comint-next-input)
     (define-key shell-mode-map "\C-p" 'comint-previous-input)
     (define-key shell-mode-map "\C-n" 'comint-next-input)))

(provide 'starter-kit-shell)
