;; Non interactive functions.

(defun my/under-tmux-p ()
  "Returns non-nil if the current Emacs instance is running under tmux."
  (and (not (display-graphic-p)) (getenv "TMUX")))

(defun my/toggle-tmux-status-bar (hide?)
  "Hides the tmux status bar if `HIDE?' is non-nil, otherwise shows it."
  (if (my/under-tmux-p)
      (shell-command (if hide? "tmux set status off" "tmux set status on"))
    (message "Emacs is not running under tmux")))

(defun my/magit-status-buffer-switch-function (buf)
  "Open the the Magit status in another frame if there's one.

If there's only one frame, then use the function that magit uses by default."
  (if (= 1 (length (frame-list)))
      (funcall (eval (car (get 'magit-status-buffer-switch-function 'standard-value))) buf)
    (select-frame-set-input-focus (next-frame))
    (switch-to-buffer buf)))

(defun my/random-element (elems)
  "Return a random element from the given list."
  (let ((random-index (random (length elems))))
    (nth random-index elems)))


;; Interactive functions.

(defun my/edit-init-file ()
  "Edit the init file, usually ~/.emacs.d/init.el."
  (interactive)
  (find-file (or user-init-file "")))

(defun my/newline-and-indent-like-previous-line ()
  "Create a newline and indent at the same level of the previous line."
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun my/split-window-horizontally-and-focus-new ()
  "Splits the window horizontally and focus the new one."
  (interactive)
  (my/--split-window-and-focus-new 'split-window-horizontally))

(defun my/split-window-vertically-and-focus-new ()
  "Splits the window vertically and focus the new one."
  (interactive)
  (my/--split-window-and-focus-new 'split-window-vertically))

(defun my/--split-window-and-focus-new (splitting-fun)
  (funcall splitting-fun)
  (other-window 1))

(defun my/eval-surrounding-sexp ()
  "Eval the sexp myich surrounds the current point."
  (interactive)
  (save-excursion
    (up-list)
    (eval-last-sexp nil)))

(defun my/create-bash-script (name)
  "Create a bash script in ~/bin.

The script will be called `NAME'. A bash shebang will be inserted on the first
line and the script will be made executable for the user."
  (interactive "sName: ")
  (let ((path (concat "~/bin/" name)))
    (find-file path)
    (insert "#!/bin/bash\n\n\n")
    (end-of-buffer)
    (save-buffer)
    (shell-script-mode)
    (shell-command (format "chmod u+x %s" path))))

(defun my/projectile-open-todo ()
  "Open TODO.md in the root of the (projectile) project if such file exists."
  (interactive)
  (let ((file (concat (projectile-project-root) "TODO.md")))
    (if (file-exists-p file)
        (find-file file)
      (when (y-or-n-p "TODO.md does not exist. Create one?")
        (find-file file)))))

(defun my/alchemist-new-exs-buffer ()
  "Create a scratch buffer in Elixir mode."
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode)
  (alchemist-mode))

(defun my/switch-to-previous-buffer ()
  "Switche to the previously visited buffer.

If called multiple times, basically alternate between two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(provide 'my-functions)
