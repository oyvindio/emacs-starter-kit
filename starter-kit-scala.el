;;; starter-kit-scala.el
(add-to-list 'load-path (concat dotfiles-dir "/opt/scala-mode"))
;;(add-to-list 'load-path (concat dotfiles-dir "/opt/ensime_2.9.1-0.7.6/elisp"))
;(add-to-list 'load-path (concat dotfiles-dir "/opt/ensime_2.9.2-RC1-0.9.3.RC4/elisp"))
;(autoload 'scala-mode-auto "scala-mode-auto" "Scala editing mode." t)
;(autoload 'scala-mode "scala-mode" "Scala editing mode." t)
(require 'scala-mode-auto)
;(require 'ensime) ; ensime apparently doesn't have autoloads

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))
(add-hook 'scala-mode-hook 'scala-mode-feature-electric-mode)
;(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(eval-after-load "ensime"
  '(progn
     (define-key ensime-mode-map (kbd "M-/") 'auto-complete)
     (setq ensime-sem-high-faces
      '(
        (var . (:foreground "#586e75"))
        (val . (:foreground "#002b36"))
        (varField . (:foreground "#586e75"))
        (valField . (:foreground "#002b36"))
        (functionCall . (:foreground "#586e75"))
        (param . (:foreground "#657b83"))
        (class . font-lock-type-face)
        (trait . (:foreground "#6c71c4"))
        (object . (:foreground "#d33682"))
        (package . (:foreground "#073642"))))))

(eval-after-load "auto-complete"
  '(progn
     (setq ac-use-menu-map t)
     (define-key ac-menu-map "\C-n" 'ac-next)
     (define-key ac-menu-map "\C-p" 'ac-previous)))

(eval-after-load "scala-mode"
  '(progn
     (define-key scala-mode-map [C-tab] 'other-window)))

(provide 'starter-kit-scala)
