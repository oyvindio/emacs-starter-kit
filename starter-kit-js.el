;;; starter-kit-js.el --- Some helpful Javascript helpers
;;
;; Part of the Emacs Starter Kit

(autoload 'js2-mode "js2-mode" "Start js2-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-hook 'js2-mode-hook 'moz-minor-mode)
(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook 'idle-highlight)
(setq js2-basic-offset 2)

(provide 'starter-kit-js)
;;; starter-kit-js.el ends here
