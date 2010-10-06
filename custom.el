;;; custom.el

;; Set color theme and font if we're running emacs in a gui frame
(when window-system
  (set-face-attribute 'default nil :font "Droid Sans Mono Slashed 12")
  (color-theme-railscasts)
  ;; Don't pop up new frames when opening files via drag n drop or
  ;; open with in OS X
  (when (eq system-type 'darwin)
    (setq ns-pop-up-frames nil)))

;; Make kill-region (C-w)  and kill-ring-save (M-w) use the line at
;; point if no region is selected, like cut and copy in IDEA
(transient-mark-mode t)
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; load my custom key bindings and defuns
(require 'custom-bindings)
(require 'custom-defuns)
