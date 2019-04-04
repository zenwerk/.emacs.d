;;; Evil related things -*- lexical-binding: t -*-
(use-package evil
  ;; :ensure t
  :config
  (evil-mode 1)
  ;; Modes that don't use evil.
  (setq evil-emacs-state-modes (append evil-emacs-state-modes
                                       '(eshell-mode)))
  ;; j,k で物理行移動, gj,gk で論理行移動
  (defun evil-swap-key (map key1 key2)
    "MAP中のKEY1とKEY2を入れ替え"
    (let ((def1 (lookup-key map key1))
          (def2 (lookup-key map key2)))
      (define-key map key1 def2)
      (define-key map key2 def1)))
  (evil-swap-key evil-motion-state-map "j" "gj")
  (evil-swap-key evil-motion-state-map "k" "gk"))

;; evil-leader-mode
(use-package evil-leader
  :config
  (setq evil-leader/in-all-status 1)
  (global-evil-leader-mode)
  (evil-leader/set-leader "\\"))

;; evil-visualstar-mode
(use-package evil-visualstar
  :config
  ;(evil-visualstar/persistant 1)
  (global-evil-visualstar-mode 1))

;; evil-soround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; evil-matchit
(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; evil-nerd-commenter
;; Emacs key bindings
(use-package evil-nerd-commenter
  :config
  (global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
  ;; Vim key bindings
  (evil-leader/set-key
    "c<SPC>" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line))
    ;"\\" 'evilnc-comment-operator ; if you prefer backslash key

;; evil-search-highlight-persist
;;(require 'highlight)
(use-package evil-search-highlight-persist
  ;; :ensure t
  :config
  ; escape でハイライトを消す
  (define-key evil-normal-state-map [escape] 'evil-search-highlight-persist-remove-all)
  (global-evil-search-highlight-persist t))

(provide 'init-evil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; old stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package evil
;  :ensure t
;  :init
;  (use-package evil-leader
;    :ensure t
;    :init
;    (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "dired-mode" "gist-list-mode"))
;    :config
;    (progn
;      (evil-leader/set-leader "<SPC>")
;      (global-evil-leader-mode 1)
;      (evil-leader/set-key
;        "!" 'shell-command
;        ":" 'eval-expression
;        "K" 'kill-this-buffer
;        "b" 'switch-to-buffer
;        "B" 'my/switch-to-previous-buffer)
;      (evil-leader/set-key-for-mode 'emacs-lisp-mode
;        "e d" 'eval-defun
;        "e b" 'eval-buffer
;        "e s" 'my/eval-surrounding-sexp)))
;  :config
;  (progn
;    (evil-mode 1)
;    ;; Use Emacs keybindings when in insert mode.
;    (setcdr evil-insert-state-map nil)
;    (define-key evil-insert-state-map [escape] 'evil-normal-state)
;    (define-key evil-insert-state-map (kbd "<RET>") 'newline-and-indent)
;    ;; Evil keybindings.
;    (define-key evil-normal-state-map (kbd "-") 'dired-jump)
;    (define-key evil-normal-state-map (kbd "H") 'back-to-indentation)
;    (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)

;    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
;    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;    (define-key evil-normal-state-map (kbd "C-p") 'previous-line)
;    (define-key evil-normal-state-map (kbd "C-n") 'next-line)
;    (define-key evil-visual-state-map (kbd "a") 'align-regexp)
;    ;; Modes that don't use evil.
;    (setq evil-emacs-state-modes (append evil-emacs-state-modes
;                                         '(alchemist-iex-mode
;                                           cider-repl-mode
;                                           cider-stacktrace-mode
;                                           git-rebase-mode
;                                           haskell-error-mode
;                                           haskell-interactive-mode
;                                           inferior-emacs-lisp-mode
;                                           magit-popup-mode
;                                           magit-popup-sequence-mode
;                                           xkcd-mode)))))

;(use-package evil-commentary
;  :ensure t
;  :config
;  (evil-commentary-mode))

;(use-package evil-terminal-cursor-changer
;  :ensure t)

;(use-package evil-surround
;  :ensure t
;  :config
;  (progn
;    (global-evil-surround-mode 1)
;    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))
