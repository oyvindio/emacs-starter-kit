;;; conf.el

;; Set color theme and font if we're running emacs in a gui frame
(when window-system
  (set-face-attribute 'default nil :font "Source Code Pro 12")
  ;(color-theme-railscasts)
  ;(color-theme-sanityinc-solarized-dark)
  (color-theme-sanityinc-solarized-light)
  ;; Don't pop up new frames when opening files via drag n drop or
  ;; open with in OS X
  (when (eq system-type 'darwin)
    (setq ns-pop-up-frames nil))

  ;; Start emacs daemon
  (server-start))

(when (eq system-type 'darwin)
  ;; Move to trash on delete
  (setq delete-by-moving-to-trash t)
  ;; Ignore .DS_Store files with ido mode
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  ;; use spotlight for locate
  (setq locate-command "mdfind"))

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

;; Make ansi-term colors suck less
(setq ansi-term-color-vector [unspecified ; ?
                              "#4E4E4E"   ; black
                              "#FF6C60"   ; red
                              "#A8FF60"   ; green
                              "#FFFFB6"   ; yellow
                              "#0070E3"   ; blue
                              "#FF73FD"   ; magenta
                              "#C6C5FE"   ; cyan
                              "#EEEEEE"]) ; white

(eval-after-load "flyspell-mode"
  (setq flyspell-issue-message-flag nil))

;; always hightlight current line
(global-hl-line-mode t)

;; load my custom key bindings and defuns
(require 'bindings)
(require 'defuns)

(provide 'conf)
