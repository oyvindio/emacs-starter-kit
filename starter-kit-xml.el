(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(setq sgml-basic-offset 4)
(add-hook 'nxml-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-n") 'pretty-print-xml-region)))

(provide 'starter-kit-xml)
