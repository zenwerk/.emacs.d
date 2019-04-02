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
;; My stuff.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package my-functions
;;   :straight nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package init-evil
  :straight nil)

(use-package init-treemacs
  :straight nil)

(use-package init-git
  :straight nil)

(use-package init-ivy
  :straight nil)

(use-package init-completion
  :straight nil)

(use-package projectile
  ;; :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
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

(use-package hl-todo
  :defer 1
  :config
  (global-hl-todo-mode))

(use-package web-mode
  :mode (("\\.html\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package yaml-mode
  :defer t
  :mode "\\.e?ya?ml$")

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
