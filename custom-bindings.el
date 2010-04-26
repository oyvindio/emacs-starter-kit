;;; custom-bindings.el --- my custom key bindings

(global-set-key  [C-tab] 'other-window)
(global-set-key (kbd "C-r") 'replace-string)
(global-set-key (kbd "C-c C-g") 'gist-buffer-confirm)
(global-set-key (kbd "M-s") 'ack)

(provide 'custom-bindings)
