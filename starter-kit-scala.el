;;; starter-kit-scala.el
(add-to-list 'load-path (concat dotfiles-dir "/opt/scala-mode"))
(add-to-list 'load-path (concat dotfiles-dir "/opt/ensime_2.9.1-0.7.6/elisp"))

(autoload 'scala-mode-auto "scala-mode-auto" "Scala editing mode." t)
(autoload 'scala-mode "scala-mode" "Scala editing mode." t)
(require 'ensime) ; ensime apparently doesn't have autoloads

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
(add-hook 'scala-mode-hook 'scala-mode-feature-electric-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(eval-after-load 'ensime
  (lambda ()
    (define-key ensime-mode-map (kbd "M-/") 'auto-complete)))

(eval-after-load 'scala-mode
  (lambda ()
    (define-key scala-mode-map [C-tab] 'other-window)))

(provide 'starter-kit-scala)
