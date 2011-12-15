;;; bindings.el --- my custom key bindings

(global-set-key (kbd "C-x r s") 'replace-string)
(global-set-key (kbd "C-x r r") 'replace-rectangle)
(global-set-key (kbd "C-x d") 'duplicate-line)
(global-set-key (kbd "C-c C-g") 'gist-buffer-confirm)
(global-set-key (kbd "C-x a a") 'ack)
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; free up M-c for other key bindings; capitalize-word isn't THAT
;; important
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c c") 'capitalize-word)

;; C-w should do backward-kill-word, like in the shell
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-M-s") 'isearch-current-symbol)
(global-set-key (kbd "C-M-r") 'isearch-backward-current-symbol)

(when (eq system-type 'darwin)
  (global-set-key (kbd "<f12>") 'ns-toggle-fullscreen)

  ;; I'm using US keyboard layout on a Norwegian keyboard, with s-';[
  ;; mapped to the Norwegian letters æøå, and needed to get this to
  ;; work in emacs as well.
  (global-set-key (kbd "s-[") (lambda () (interactive) (insert "å")))
  (global-set-key (kbd "s-{") (lambda () (interactive) (insert "Å"))) 

  (global-set-key (kbd "s-'") (lambda () (interactive) (insert "æ")))
  (global-set-key (kbd "s-\"") (lambda () (interactive) (insert "Æ")))

  (global-set-key (kbd "s-;") (lambda () (interactive) (insert "ø")))
  (global-set-key (kbd "s-:") (lambda () (interactive) (insert "Ø")))

  ;; In Emacs 23 (Cocoa) in Snow Leopard, Apple delete key deletes
  ;; backward, not forward as is usual. This fixes this behaviour.
  (if (display-graphic-p)
      (normal-erase-is-backspace-mode 1)))

(global-set-key  [C-tab] 'other-window)
; don't hijack my keybinding, org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map [C-tab] 'other-window)))

(eval-after-load 'browse-kill-ring
  (global-set-key (kbd "C-M-y") 'browse-kill-ring))
(provide 'bindings)
