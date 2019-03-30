(defun my/theming-load-theme (theme)
  "Set `theme' as the current theme."
  (interactive
   (list
    (intern (completing-read "Load theme: " my/gui-themes nil t))))
  (when (my/theming--theme-set-p)
    (disable-theme my/theming-current-theme))
  (setq my/theming-current-theme theme)
  (load-theme theme t)
  (message "Loaded theme %s" theme))

(defun my/theming-load-next-theme ()
  "Load the next theme in the `my/gui-themes' list of themes."
  (interactive)
  (let* ((current-idx (if (my/theming--theme-set-p)
                          (cl-position my/theming-current-theme my/gui-themes)
                        -1))
         (theme (my/theming--next-element current-idx my/gui-themes)))
    (my/theming-load-theme theme)))

(defun my/theming-load-prev-theme ()
  (interactive)
  "Load the previous theme in the `my/gui-themes' list of themes."
  (let* ((current-idx (if (my/theming--theme-set-p)
                          (cl-position my/theming-current-theme my/gui-themes)
                        1))
         (theme (my/theming--prev-element current-idx my/gui-themes)))
    (my/theming-load-theme theme)))

;; Tells whether there's a currently set theme.
(defun my/theming--theme-set-p ()
  (boundp 'my/theming-current-theme))

;; Returns the element after `current-idx' in `list' (wrapping around the list).
(defun my/theming--next-element (current-idx list)
  (let ((next-idx (% (+ 1 current-idx) (length list))))
    (nth next-idx list)))

;; Returns the element before `current-idx' in `list' (wrapping around the
;; list).
(defun my/theming--prev-element (current-idx list)
  (let ((next-idx (% (- (+ current-idx (length list)) 1) (length list))))
    (nth next-idx list)))

(provide 'my-theming)
