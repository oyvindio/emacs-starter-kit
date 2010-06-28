;;; custom-bindings.el --- my custom key bindings

(global-set-key (kbd "C-x r s") 'replace-string)
(global-set-key (kbd "C-x r r") 'replace-rectangle)
(global-set-key (kbd "C-x d") 'duplicate-line)
(global-set-key (kbd "C-c C-g") 'gist-buffer-confirm)
(global-set-key (kbd "C-x a a") 'ack)

(global-set-key  [C-tab] 'other-window)
; don't hijack my keybinding, org-mode
(eval-after-load "org-mode"
  '(progn (define-key org-mode-map [C-tab] 'other-window)))

(provide 'custom-bindings)
