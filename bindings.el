;;; bindings.el --- my custom key bindings

(global-set-key (kbd "C-x r s") 'replace-string)
(global-set-key (kbd "C-x r r") 'replace-rectangle)
(global-set-key (kbd "C-x d") 'duplicate-line)
(global-set-key (kbd "C-c C-g") 'gist-buffer-confirm)
(global-set-key (kbd "C-x a a") 'ack)
(global-set-key (kbd "M-i") 'ido-goto-symbol)
(when (eq system-type 'darwin)
  (global-set-key (kbd "<f12>") 'ns-toggle-fullscreen))

(global-set-key  [C-tab] 'other-window)
; don't hijack my keybinding, org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [C-tab] 'other-window)))
(provide 'bindings)
