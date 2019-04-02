;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the package system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(setq package-enable-at-startup nil)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; 指定したディレクトリを `load-path' に加える
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 全体設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-h をバックスペースにする (?\C-? は DEL のシーケンス)
(define-key
  key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; ビープ音の無効化
(setq ring-bell-function 'ignore)

;; カラム番号も表示
(column-number-mode t)

;; 行番号表示
(line-number-mode t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;;左側に行番号の表示
(global-linum-mode t)

;; タブ幅の設定
(setq-default tab-width 4)

;; ミニバッファで C-w で単語区切りで削除
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;; マッチする()を強調
(show-paren-mode 1)

;; モードラインのカラム数を表示
(column-number-mode 1)

;; Don't display the start messages when Emacs starts.
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; ツールバー, メニューバー, スクロールバー非表示
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; 保存時に末尾空白文字削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ファイル行末改行
(setq require-final-newline t)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

;; Scroll output in compile buffers.
(setq compilation-scroll-output t)

;; Don't backup/autosave files and don't protect from locks.
(setq backup-inhibited t
      auto-save-default nil
      create-lockfiles nil)

;; Indentation is two spaces wide, with spaces instead of tabs.
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Wrap at 80 characters.
(setq-default fill-column 120)

;; Scrolling:
;; - `scroll-margin': always have a margin of 8 lines on top/bottom
;; - `scroll-conservatively': jump abruptedly every this lines. If set to very
;;   high, basically never jumps :)
(setq scroll-margin 8
      scroll-conservatively 100000)

;; utf-8 を基本的に使用する
(prefer-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Global Custom keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<f8>") 'my/edit-init-file)
(global-set-key (kbd "C-x %") 'my/split-window-horizontally-and-focus-new)
(global-set-key (kbd "C-x -") 'my/split-window-vertically-and-focus-new)
;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `straight.el'.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Fallback to `use-package'
(straight-use-package 'use-package)
;; `use-package' のバックエンドで `straight.el' をデフォルトで使用する
(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  ;; :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Functions that will be used also throughout this file.
;(use-package my-functions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package doom-themes :defer t)
(load-theme 'doom-one t)

;(use-package my-theming
;:demand t
;:bind (((kbd "C-c t n") . my/theming-load-next-theme)
;      ((kbd "C-c t p") . my/theming-load-prev-theme))
;:init
;(setq my/gui-themes '(doom-one
;                      doom-dracula
;                      material
;                      zenburn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  ;; :ensure t
  :config
  (evil-mode 1)
  ;; Modes that don't use evil.
  (setq evil-emacs-state-modes (append evil-emacs-state-modes
                                       '(alchemist-iex-mode)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package my-gui
  :straight nil
  :if (display-graphic-p))

(use-package my-osx
  :straight nil
  :if (eq system-type 'darwin))

(use-package my-windows
  :straight nil
  :if (eq system-type 'windows-nt))

;; (use-package my-scratch-buffer
;;   :straight nil
;;   :commands my/scratch-buffer-create-or-prompt
;;   :init
;;   (evil-leader/set-key "S" 'my/scratch-buffer-create-or-prompt))

;; (use-package my-smarter-beginning-of-line
;;   :straight nil
;;   :bind ("C-a" . my/smarter-beginning-of-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treemacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("s-1"       . treemacs) ; 表示/非表示の toggle
        ("M-0"       . treemacs-select-window) ; treemacs window へ直接移動するショートカットキー
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git-related things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ivy-related things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :diminish ivy-mode
  :bind (("C-S-a" . counsel-M-x)
         ("C-S-o" . counsel-find-file))
  :config
  (ivy-mode 1))

(use-package projectile
  ;; :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile :defer t)
  (evil-leader/set-key
    "p" 'helm-projectile-switch-project
    "f" 'helm-projectile-find-file
    "T" 'my/projectile-open-todo)
  :config
  (projectile-global-mode)
  (when (executable-find "gtags")
    ;; gtags の再生成コマンド
    (setq projectile-tags-file-name "GTAGS")
    (setq projectile-tags-command "gtags --gtagslabel=pygments")
    (defun regenerate-tags ()
      (interactive)
      (let ((tags-directory (directory-file-name (projectile-project-root))))
        (async-shell-command
         (format "gtags --gtagslabel=pygments --debug" tags-file-name tags-directory))))
    ))

(use-package flyspell
  ;; built-in
  :init
  (setq ispell-program-name "aspell"))

(use-package company
  :defer 4
  :diminish company-mode
  :config
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-show-numbers t
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case t)
    (global-set-key (kbd "C-<tab>") 'company-manual-begin)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
    (define-key company-active-map (kbd "RET") 'company-complete-selection)
    (global-company-mode t)))

(use-package hl-todo
  ;; :ensure t
  :defer 1
  :config
  (global-hl-todo-mode))

(use-package web-mode
  ;; :ensure t
  :mode (("\\.html\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package yaml-mode
  :defer t
  :mode "\\.e?ya?ml$")

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


;; Built-in packages.
;(use-package savehist
;  :init
;  (setq savehist-file "~/.emacs.d/etc/savehist")
;  (setq history-length 1000)
;  :config
;  (savehist-mode))

;(use-package dired-x
;  :config
;  (define-key dired-mode-map (kbd "-") 'dired-up-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm-related things.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package helm
;;   :diminish helm-mode
;;   :bind (("C-S-a" . helm-M-x)
;;          ("C-S-n" . helm-find-files)
;;          ("C-x b" . helm-buffers-list))
;;   :init
;;   (setq helm-M-x-fuzzy-match t
;;         helm-buffers-fuzzy-matching t
;;         helm-display-header-line nil)
;;   :config
;;   ;; No idea why here find-file is set to nil (so it uses the native find-file
;;   ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
;;   ;; Helm again.
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1)
;;   (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil)))

;; (use-package helm-ag
;;   ;; :ensure t
;;   :commands (helm-do-ag helm-do-ag-project-root)
;;   :init
;;   (evil-leader/set-key
;;     "a g" 'helm-do-ag-project-root
;;     "a G" 'helm-do-ag)
;;   :config
;;   (progn
;;     (evil-make-overriding-map helm-ag-mode-map 'normal)
;;     (add-hook 'helm-ag-mode-hook #'evil-normalize-keymaps)))

;(use-package ggtags
;  :init
;  (add-hook 'c-mode-common-hook
;            (lambda ()
;              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;                (ggtags-mode 1))))
;  (add-hook 'clojure-mode-hook (lambda () (ggtags-mode 1)))
;  (add-hook 'elixir-mode-hook (lambda () (ggtags-mode 1)))
;  (add-hook 'erlang-mode-hook (lambda () (ggtags-mode 1)))
;
;  :config
;  ;; use helm
;  (setq ggtags-completing-read-function nil)
;
;  ;; use eldoc
;  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
;
;  ;; imenu
;  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
;
;  (define-key ggtags-mode-map (kbd "s-b") 'ggtags-find-definition)
;  (define-key ggtags-mode-map (kbd "s-r") 'ggtags-find-reference)
;  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
;  (define-key ggtags-mode-map (kbd "C-c g m") 'ggtags-find-tag-dwim)
;  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))

;(use-package popwin
;  :ensure t
;  :defer 2
;  :config
;  (progn
;    (mapcar (lambda (el) (add-to-list 'popwin:special-display-config el))
;            '(helm-mode
;              ("*Help*" :stick t)
;              ("*rspec-compilation*" :position bottom :stick t :noselect t)
;              ("*alchemist help*" :position right :stick t :width 80)
;              ("*alchemist mix*" :position bottom :noselect t)
;              ("*alchemist elixir*" :position bottom :noselect t)
;              ("*alchemist test report*" :position bottom :stick t :noselect t)
;              ("*alchemist-eval-mode*" :position bottom :height 4 :stick t)
;              ("*GHC Info*" :position bottom :stick t :noselect t)))
;    (global-set-key (kbd "C-l") popwin:keymap)
;    (popwin-mode 1)))

;(use-package yasnippet
;  :ensure t
;  :defer 4
;  :diminish yas-minor-mode
;  :config
;  (progn
;    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
;    (yas-global-mode t)))

;(use-package writeroom-mode
;  :ensure t
;  :commands writeroom-mode
;  :init
;  (evil-leader/set-key "m w" 'writeroom-mode)
;  :config
;  (setq writeroom-restore-window-config t
;        writeroom-width 100)
;  (add-to-list 'writeroom-global-effects 'my/toggle-tmux-status-bar))

;; Correctly load $PATH and $MANPATH on OSX (GUI).
;(use-package reveal-in-osx-finder
;  :ensure t
;  :if (eq system-type 'darwin))

;(use-package ace-window
;  :ensure t
;  :bind ("M-o" . ace-window)
;  :config
;  (setq aw-keys '(?a ?s ?d ?f ?h ?j ?k ?l)))

;(use-package smartscan
;  :ensure t
;  :bind (("M-p" . smartscan-symbol-go-backward)
;         ("M-n" . smartscan-symbol-go-forward))
;  :config
;  (smartscan-mode t))

;(use-package drag-stuff
;  :ensure t
;  :bind (("M-J" . drag-stuff-down)
;         ("M-K" . drag-stuff-up))
;  :config
;  (drag-stuff-global-mode))

;(use-package default-text-scale
;  :ensure t
;  :bind (("s-=" . default-text-scale-increase)
;         ("s--" . default-text-scale-decrease)))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
;(use-package beacon
;  :ensure t
;  :defer 2
;  :config
;  (beacon-mode 1))

;(use-package perspective
;  :ensure t
;  :init
;  (use-package persp-projectile
;    :ensure t
;    :defer t)
;  :config
;  (progn
;    (persp-mode)
;    (require 'persp-projectile)
;    (evil-leader/set-key
;      "w n" 'persp-next
;      "w p" 'persp-prev)))

;; Modes for programming languages and such.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang / Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package erlang
;  :ensure t
;  :defer t
;  ;; We need to specify erlang-mode explicitely as the package is not called
;  ;; erlang-mode.
;  :mode (("\\.erl\\'" . erlang-mode)
;         ("\\.hrl\\'" . erlang-mode)
;         ("\\.xrl\\'" . erlang-mode)
;         ("sys\\.config\\'" . erlang-mode)
;         ("rebar\\.config\\'" . erlang-mode)
;         ("\\.app\\(\\.src\\)?\\'" . erlang-mode))
;  :config
;  (setq erlang-indent-level 4))

;(use-package elixir-mode
;  :ensure t
;  :defer t
;  :load-path "~/Code/emacs-elixir"
;  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
;  :config
;  (use-package flycheck-elixir)
;  ;; for elixir do-end
;  (use-package ruby-end
;    :config
;    (add-to-list 'elixir-mode-hook
;                 (defun auto-activate-ruby-end-mode-for-elixir-mode ()
;                   (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
;                        "\\(?:^\\|\\s-+\\)\\(?:do\\)")
;                   (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
;                   (ruby-end-mode +1)
;                   )))
;  (use-package alchemist
;    :ensure t
;    :load-path "~/Code/alchemist.el"
;    :diminish alchemist-mode
;    :init
;    (setq alchemist-test-status-modeline nil)
;    :config
;    (exec-path-from-shell-copy-env "MIX_ARCHIVES")
;    (progn
;      (evil-define-key 'normal alchemist-test-mode-map "]t" 'alchemist-test-mode-jump-to-next-test)
;      (evil-define-key 'normal alchemist-test-mode-map "[t" 'alchemist-test-mode-jump-to-previous-test)
;      (define-key evil-normal-state-map "]d" 'alchemist-goto-jump-to-next-def-symbol)
;      (define-key evil-normal-state-map "[d" 'alchemist-goto-jump-to-previous-def-symbol)
;      (define-key evil-normal-state-map "]T" '(lambda () (interactive)
;                                                (popwin:select-popup-window)
;                                                (alchemist-test-next-result)))
;      (define-key evil-normal-state-map "[T" '(lambda () (interactive)
;                                                (popwin:select-popup-window)
;                                                (alchemist-test-previous-result)))
;      (define-key alchemist-mode-map (kbd "C-c a g d") 'my/alchemist-generate-docs)
;      (define-key alchemist-mode-map (kbd "C-c a d g") 'my/alchemist-mix-deps-get)
;      (define-key alchemist-mode-map (kbd "C-c a S") 'my/alchemist-new-exs-buffer)
;      (evil-leader/set-key-for-mode 'elixir-mode
;        "t b" 'alchemist-mix-test-this-buffer
;        "t t" 'alchemist-mix-test
;        "t r" 'alchemist-mix-rerun-last-test
;        "t p" 'alchemist-mix-test-at-point
;        "e b" 'alchemist-eval-buffer
;        "a d" 'alchemist-goto-list-symbol-definitions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure and Related things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(use-package clojure-mode
;  :ensure t
;  :defer t
;  :init
;  (add-hook 'clojure-mode-hook #'yas-minor-mode)
;  (add-hook 'clojure-mode-hook #'subword-mode))

;(use-package cider
;  :ensure t
;  :defer t
;  :init
;  (add-hook 'cider-mode-hook #'clj-refactor-mode)
;  (add-hook 'cider-mode-hook #'company-mode)
;  (add-hook 'cider-repl-mode-hook #'company-mode)
;  :diminish subword-mode
;  :config
;  (setq nrepl-log-messages t
;        cider-repl-display-in-current-window t
;        cider-repl-use-clojure-font-lock t
;        cider-prompt-save-file-on-load 'always-save
;        cider-font-lock-dynamically '(macro core function var)
;        cider-overlays-use-font-lock t)
;  (cider-repl-toggle-pretty-printing))

;(use-package evil-paredit)

;(use-package cider-eval-sexp-fu)

;(use-package clj-refactor
;  :ensure t
;  :diminish clj-refactor-mode
;  :config (cljr-add-keybindings-with-prefix "C-c j"))

;; ;;; Buffer, Windows and Frames
;; (setq frame-title-format
;;       '(:eval (if (buffer-file-name)
;;                   (abbreviate-file-name (buffer-file-name)) "%b"))
;;       ;; Size new windows proportionally wrt other windows
;;       window-combination-resize t)

;; Configure `display-buffer' behaviour for some special buffers.
;; (setq display-buffer-alist
;;       `(
;;         ;; Nail Helm to the side window
;;         (,(rx bos "*" (* nonl) "helm" (* nonl) "*" eos)
;;          (display-buffer-in-side-window)
;;          (side . bottom)
;;          (window-height . 0.4)
;;          (window-width . 0.6))
;;         ;; Put REPLs and error lists into the bottom side window
;;         (,(rx bos (or "*Flycheck errors*" ; Flycheck error list
;;                       "*compilation"      ; Compilation buffers
;;                       "*Warnings*"        ; Emacs warnings
;;                       "*sbt"              ; SBT REPL and compilation buffer
;;                       "*SQL"              ; SQL REPL
;;                       "*shell"            ; Shell window
;;                       "*Help"             ; Help buffers
;;                       ))
;;          (display-buffer-reuse-window
;;           display-buffer-in-side-window)
;;          (side            . bottom)
;;          (reusable-frames . visible)
;;          (window-height   . 0.33))
;;         ;; Let `display-buffer' reuse visible frames for all buffers.  This must
;;         ;; be the last entry in `display-buffer-alist', because it overrides any
;;         ;; later entry with more specific actions.
;;         ("." nil (reusable-frames . visible))))

;;## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;;## end of OPAM user-setup addition for emacs / base ## keep this line
