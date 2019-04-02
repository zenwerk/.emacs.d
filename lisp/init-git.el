;;; Git related things -*- lexical-binding: t -*-

;(use-package magit
;  :ensure t
;  :commands (magit-status magit-checkout)
;  :bind (("C-x g" . magit-status)
;         ("C-c g b" . magit-checkout)
;         ("C-c g B" . magit-blame))
;  :init
;  (use-package magit-gh-pulls
;    :ensure t
;    :config
;    (add-hook 'magit-mode-hook 'magit-gh-pulls-mode))
;  (setq magit-revert-buffers 'silent
;        magit-push-always-verify nil
;        git-commit-summary-max-length 70)
;  ;; Use flyspell in the commit buffer
;  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
;  (add-hook 'magit-blame-mode 'evil-insert-state)
;  (add-hook 'magit-blame-mode (lambda () (message "hello"))))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (progn
    (global-git-gutter+-mode)
    (use-package git-gutter-fringe+ :defer t)
    (define-key evil-normal-state-map "[h" 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map "]h" 'git-gutter+-next-hunk)
    (evil-leader/set-key "g +" 'git-gutter+-stage-hunks)))

(use-package git-messenger
  ;; :ensure t
  :commands git-messenger:popup-message
  :init
  (setq git-messenger:show-detail t)
  (evil-leader/set-key "g p" 'git-messenger:popup-message)
  :config
  (progn
    (define-key git-messenger-map (kbd "j") 'git-messenger:popup-close)
    (define-key git-messenger-map (kbd "k") 'git-messenger:popup-close)
    (define-key git-messenger-map (kbd "RET") 'git-messenger:popup-close)))

(use-package git-timemachine
  ;; :ensure t
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "g t" 'git-timemachine-toggle)
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package gitignore-mode :defer t)

;; 指定した行を`github'で開く
(use-package browse-at-remote
  :commands browse-at-remote/browse
  :init
  (evil-leader/set-key "g b" 'browse-at-remote/browse))

(provide 'init-git)
