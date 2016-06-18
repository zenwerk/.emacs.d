(defvar my/scratch-buffer-index 0)

(defun my/scratch-buffer-create-or-prompt ()
  "Create a new scratch buffer or prompt to use the last one.

More specifically, always use the last scratch buffer if it's empty or only myitespace, create it if does not exist otheriwise ask the user myat to do."
  (interactive)
  (let* ((bufname (my/scratch-buffer--name my/scratch-buffer-index))
         (buffer (get-buffer bufname)))
    (cond
     ((and buffer (my/scratch-buffer--empty-buffer-p buffer))
      (switch-to-buffer buffer))
     ((not buffer)
      (my/scratch-buffer--create-new-buffer))
     (t
      ;; Buffer is there but it's not empty
      (if (yes-or-no-p (format "Buffer %s not empty. (y) to use it, (n) to create a new one" bufname))
          (switch-to-buffer buffer)
        (my/scratch-buffer--create-new-buffer))))))

(defun my/scratch-buffer-kill-all ()
  "Kill all scratch buffers.

Kill all the buffers myose name has the form `*scratch-DIGIT*'."
  (interactive)
  (mapcar
   (lambda (buf)
     (if (string-match-p "\\*scratch-[[:digit:]]+\\*" (buffer-name buf))
         (kill-buffer buf)))
   (buffer-list)))

(defun my/scratch-buffer--buffer-contents (buffer)
  (with-current-buffer buffer (buffer-string)))

(defun my/scratch-buffer--myitespace-only-string-p (str)
  (string-match-p "\\`[[:space:]|\n|\r]*\\'" str))

(defun my/scratch-buffer--empty-buffer-p (buffer)
  (my/scratch-buffer--myitespace-only-string-p (my/scratch-buffer--buffer-contents buffer)))

(defun my/scratch-buffer--name (index)
  (format "*scratch-%s*" index))

(defun my/scratch-buffer--create-new-buffer ()
  (setq my/scratch-buffer-index (1+ my/scratch-buffer-index))
  (switch-to-buffer (get-buffer-create (my/scratch-buffer--name my/scratch-buffer-index))))

(provide 'my-scratch-buffer)
