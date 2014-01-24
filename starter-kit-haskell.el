;;; starter-kit-haskell.el
(add-to-list 'auto-mode-alist '("*.hs" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(defun is-header (line)
  (string-match "^.*\\.hs:" line))

(defun file-name-and-info-from-header (line)
  (let ((separated (split-string line ":")))
    (list (car separated) (cadr separated) (caddr separated))))


(defun parse-until-next-header (lines acumulator)
  (if (or (null lines) (is-header (car lines)))
      (list (reverse acumulator) lines)
    (parse-until-next-header (cdr lines) (cons (car lines) acumulator))))

(defun parse-all-errors (lines)
  (let ((next (parse-until-next-header lines (list))))
    (if (null (cadr next))
        '()
      (gather-errors (cadr next)))))

(defun gather-errors (lines)
  (if (null lines) (list)
    (let ((first (file-name-and-info-from-header (car lines)))
          (next (parse-until-next-header (cdr lines) (list))))
      (cons (flycheck-error-new
             :filename (car first)
             :line (string-to-number (cadr first))
             :column (string-to-number (caddr first))
             :message (mapconcat 'identity (car next) "\n")
             :level (if (string-match ".*Warning.*" (car (car next)))
                        'warning 'error))
            (gather-errors (cadr next))))))

(defun parse-ghc-output (ghc-output checker buffer)
  (parse-all-errors (split-string ghc-output "\n")))

(defun parse-hlint-output (hlint-output checker buffer)
  (if (string-match ".*No suggestions.*" hlint-output)
      (list)
    (gather-errors (split-string hlint-output "\n"))))

(flycheck-declare-checker haskell-ghc-checker
  "A simple syntax checker using ghc"
  :command '("ghc" "-Wall" "-fno-warn-name-shadowing" source-inplace)
  :error-parser 'parse-ghc-output
  :modes 'haskell-mode
  :next-checkers '(haskell-hlint-checker))

(flycheck-declare-checker haskell-hlint-checker
  "A simple syntax checker using hlint"
  :command '("hlint" source-inplace)
  :error-parser 'parse-hlint-output
  :modes 'haskell-mode)

(add-to-list 'flycheck-checkers 'haskell-hlint-checker)

(provide 'starter-kit-haskell)
