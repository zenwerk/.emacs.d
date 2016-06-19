;; Font size in 1/10pt (120 = 12 pt).
(set-face-attribute 'default nil :height 120)

;; Default font.
;(set-default-font "Hack")

;; Fringe.
(setq-default left-fringe-width 16)

;; Resize frames pixel-by-pixel.
(setq frame-resize-pixelwise t)

;; Scrolling with the mouse.
(setq mouse-wheel-scroll-amount '(1))    ;; one line at a time
(setq mouse-wheel-progressive-speed t)   ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't)       ;; scroll window under mouse

(provide 'my-gui)
