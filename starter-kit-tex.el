;;; starter-kit-tex.el

(defun auctex-add-and-default-make ()
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t
                                     :help "Run Make"))
  (setq TeX-command-default "Make"))

(when (boundp 'LaTeX-mode-hook)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'add-watchwords)
  (add-hook 'LaTeX-mode-hook 'auctex-add-and-default-make))

(provide 'starter-kit-tex)
