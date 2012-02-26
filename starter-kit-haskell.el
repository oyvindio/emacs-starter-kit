;;; starter-kit-haskell.el
(add-to-list 'auto-mode-alist '("*.hs" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(provide 'starter-kit-haskell)
