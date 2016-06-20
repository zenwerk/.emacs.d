;; refs. https://github.com/whatyouhide/emacs.d

;; Initialize the package system.
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Add custom code to the load path. `ext' contains Lisp code that I didn't
;; write but that is not in melpa, while `lisp' is for List code I wrote.
(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Make use-package available.
(require 'use-package)

;; Functions that will be used also throughout this file.
(use-package my-functions)

;; C-h をバックスペースにする (?\C-? は DEL のシーケンス)
(keyboard-translate ?\C-h ?\C-?)

;; 簡単にウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)

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


;; Theming
(use-package badwolf-theme      :ensure t :defer t)
(use-package darktooth-theme    :ensure t :defer t)
(use-package material-theme     :ensure t :defer t)
(use-package monokai-theme      :ensure t :defer t)
(use-package mustang-theme      :ensure t :defer t)
(use-package solarized-theme    :ensure t :defer t)
(use-package zenburn-theme      :ensure t :defer t)

(use-package my-theming
  :demand t
  :bind (("C-c t n" . my/theming-load-next-theme)
         ("C-c t p" . my/theming-load-prev-theme))
  :init
  (setq my/term-theme 'monokai
        my/gui-themes '(badwolf
                        darktooth
                        dichromacy
                        leuven
                        material
                        monokai
                        solarized-dark
                        solarized-light
                        zenburn))
  :config
  (if (memq window-system '(mac ns))
      (my/theming-load-random-theme)
    (load-theme my/term-theme t)))

;; Global keyboarding

(global-set-key (kbd "<f8>") 'my/edit-init-file)
(global-set-key (kbd "C-x \\") 'my/split-window-horizontally-and-focus-new)
(global-set-key (kbd "C-x -") 'my/split-window-vertically-and-focus-new)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-x B") 'my/switch-to-previous-buffer)
(global-set-key (kbd "C-k") 'kill-whole-line)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)


;; Evil.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :init
  ;; evil-leader-mode
  (use-package evil-leader
    :config
    (setq evil-leader/in-all-status 1)
    (global-evil-leader-mode)
    (evil-leader/set-leader "\\"))
  :config
  (evil-mode 1)
  ;; Modes that don't use evil.
  (setq evil-emacs-state-modes (append evil-emacs-state-modes
                                         '(alchemist-iex-mode))))

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
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
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
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
  ))

;; evil-search-highlight-persist
;;(require 'highlight)
(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t))
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
;    (define-key evil-normal-state-map (kbd "L") 'move-end-of-line)
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
;
;(use-package evil-commentary
;  :ensure t
;  :config
;  (evil-commentary-mode))
;
;(use-package evil-terminal-cursor-changer
;  :ensure t)
;
;(use-package evil-surround
;  :ensure t
;  :config
;  (progn
;    (global-evil-surround-mode 1)
;    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))

;; My stuff.

(use-package my-tmux
  :if (not (window-system)))

(use-package my-appearance)

(use-package my-gui
  :if (display-graphic-p))

(use-package my-osx
  :if (eq system-type 'darwin))

(use-package my-windows
  :if (eq system-type 'windows-nt))

(use-package my-configs)

(use-package my-scratch-buffer
  :commands my/scratch-buffer-create-or-prompt
  :init
  (evil-leader/set-key "S" 'my/scratch-buffer-create-or-prompt))

;(use-package my-notes
  ;;; We need this package as it exports a variable that we'll use later on.
  ;:demand t
  ;:commands my/notes-open-or-create
  ;:init
  ;(evil-leader/set-key "N" 'my/notes-open-or-create))

(use-package my-smarter-beginning-of-line
  :bind ("C-a" . my/smarter-beginning-of-line))

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

;; Misc packages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NeoTree
;(require 'dired)
;; https://github.com/andrewmcveigh/emacs.d/blob/master/lisp/init-neotree.el
(defun neotree-copy-file ()
  (interactive)
  (let* ((current-path (neo-buffer--get-filename-current-line))
         (msg (format "Copy [%s] to: "
                      (neo-path--file-short-name current-path)))
         (to-path (read-file-name msg (file-name-directory current-path))))
    (dired-copy-file current-path to-path t))
  (neo-buffer--refresh t))

(use-package neotree
  :config
  (evil-leader/set-key
  "n" 'neotree-toggle)
  ;; 隠しファイルをデフォルトで表示
  (setq neo-show-hidden-files t)
  ;; neotree でファイルを新規作成した後、自動的にファイルを開く
  (setq neo-create-file-auto-open t)
  ;; delete-other-window で neotree ウィンドウを消さない
  (setq neo-persist-show t)
  ;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
  (setq neo-smart-open t)
  (define-minor-mode neotree-evil
    "Use NERDTree bindings on neotree."
    :lighter " NT"
    :keymap (progn
              (evil-make-overriding-map neotree-mode-map 'normal t)
              (evil-define-key 'normal neotree-mode-map
                "C" 'neotree-change-root
                "U" 'neotree-select-up-node
                "r" 'neotree-refresh
                "o" 'neotree-enter
                (kbd "<return>") 'neotree-enter
                "i" 'neotree-enter-horizontal-split
                "s" 'neotree-enter-vertical-split
                "n" 'evil-search-next
                "N" 'evil-search-previous
                "ma" 'neotree-create-node
                "mc" 'neotree-copy-file
                "md" 'neotree-delete-node
                "mm" 'neotree-rename-node
                "gg" 'evil-goto-first-line
                "gi" (lambda ()
                       (interactive)
                       (if (string= pe/get-directory-tree-external-command
                                    nt/gitignore-files-cmd)
                           (progn (setq pe/get-directory-tree-external-command
                                        nt/all-files-cmd))
                         (progn (setq pe/get-directory-tree-external-command
                                      nt/gitignore-files-cmd)))
                       (nt/refresh))
                "I" (lambda ()
                      (interactive)
                      (if pe/omit-enabled
                          (progn (setq pe/directory-tree-function
                                       'pe/get-directory-tree-async)
                                 (pe/toggle-omit nil))
                        (progn (setq pe/directory-tree-function
                                     'pe/get-directory-tree-external)
                               (pe/toggle-omit t)))))
              neotree-mode-map))

  (setq neo-hidden-files-regexp "^\\.\\|~$\\|^#.*#$\\|^target$\\|^pom\\.*"))

(use-package dash
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'evil-commentary-mode)))

;; Git-related things.

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
  :ensure t
  :diminish git-gutter+-mode
  :config
  (progn
    (global-git-gutter+-mode)
    (use-package git-gutter-fringe+ :ensure t)
    (define-key evil-normal-state-map "[h" 'git-gutter+-previous-hunk)
    (define-key evil-normal-state-map "]h" 'git-gutter+-next-hunk)
    (evil-leader/set-key "g +" 'git-gutter+-stage-hunks)))

(use-package git-messenger
  :ensure t
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
  :ensure t
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "g t" 'git-timemachine-toggle)
  :config
  (progn
    (evil-make-overriding-map git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package gitignore-mode
  :ensure t)

(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote/browse
  :init
  (evil-leader/set-key "g b" 'browse-at-remote/browse))

;; Helm-related things.

;(use-package helm
;  :ensure t
;  :diminish helm-mode
;  :bind (("M-x" . helm-M-x)
;         ("C-x C-f" . helm-find-files)
;         ("C-x b" . helm-buffers-list))
;  :init
;  (setq helm-M-x-fuzzy-match t
;        helm-buffers-fuzzy-matching t
;        helm-display-header-line nil)
;  (evil-leader/set-key "<SPC>" 'helm-M-x)
;  :config
;  ;; No idea why here find-file is set to nil (so it uses the native find-file
;  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
;  ;; Helm again.
;  (helm-mode 1)
;  (helm-autoresize-mode 1)
;  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-S-a" . helm-M-x)
         ("C-S-n" . helm-find-files)
         ("C-x b" . helm-buffers-list))
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-display-header-line nil)
  :config
  ;; No idea why here find-file is set to nil (so it uses the native find-file
  ;; for Emacs. This makes stuff like (find-file (read-file-name ...)) work with
  ;; Helm again.
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . helm-completing-read-symbols)))

(use-package helm-ag
  :ensure t
  :commands (helm-do-ag helm-do-ag-project-root)
  :init
  (evil-leader/set-key
    "a g" 'helm-do-ag-project-root
    "a G" 'helm-do-ag)
  :config
  (progn
    (evil-make-overriding-map helm-ag-mode-map 'normal)
    (add-hook 'helm-ag-mode-hook #'evil-normalize-keymaps)))

;(use-package swiper-helm
;  :ensure t
;  :bind ("C-s" . swiper-helm))

;(use-package avy
;  :ensure t
;  :bind ("C-;" . avy-goto-char))

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :diminish projectile-mode
  :init
  (use-package helm-projectile
    :ensure t)
  (evil-leader/set-key
    "p" 'helm-projectile-switch-project
    "f" 'helm-projectile-find-file
    "T" 'my/projectile-open-todo)
  :config
  (projectile-global-mode))

(use-package flyspell
  ;; built-in
  :init
  (setq ispell-program-name "aspell"))

(use-package guide-key
  :ensure t
  :init
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence t)
    (setq guide-key/idle-delay 0.4)
    (guide-key-mode 1)))

(use-package popwin
  :ensure t
  :defer 2
  :config
  (progn
    (mapcar (lambda (el) (add-to-list 'popwin:special-display-config el))
            '(helm-mode
              ("*Help*" :stick t)
              ("*rspec-compilation*" :position bottom :stick t :noselect t)
              ("*alchemist help*" :position right :stick t :width 80)
              ("*alchemist mix*" :position bottom :noselect t)
              ("*alchemist elixir*" :position bottom :noselect t)
              ("*alchemist test report*" :position bottom :stick t :noselect t)
              ("*alchemist-eval-mode*" :position bottom :height 4 :stick t)
              ("*GHC Info*" :position bottom :stick t :noselect t)))
    (global-set-key (kbd "C-l") popwin:keymap)
    (popwin-mode 1)))

(use-package company
  :ensure t
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
    (define-key company-active-map (kbd "RET") nil)
    (global-company-mode t)))

(use-package yasnippet
  :ensure t
  :defer 4
  :diminish yas-minor-mode
  :config
  (progn
    (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode t)))

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
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (progn
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

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

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

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

(use-package hl-todo
  :ensure t
  :defer 1
  :config
  (global-hl-todo-mode))

;(use-package xkcd
;  :ensure t
;  :commands xkcd)

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

(use-package web-mode
  :ensure t
  :mode (("\\.html\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;(use-package js
;  ;; built-in
;  :init
;  (setq js-indent-level 2))

;(use-package scss-mode
;  :ensure t
;  :mode "\\.scss\\'"
;  :init
;  (setq css-indent-offset 2))

(use-package erlang
  :ensure t
  ;; We need to specify erlang-mode explicitely as the package is not called
  ;; erlang-mode.
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode)
         ("sys\\.config\\'" . erlang-mode)
         ("rebar\\.config\\'" . erlang-mode)
         ("\\.app\\(\\.src\\)?\\'" . erlang-mode))
  :config
  (setq erlang-indent-level 4))

(use-package elixir-mode
  :load-path "~/Code/emacs-elixir"
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
  :config
  (use-package alchemist
    :load-path "~/Code/alchemist.el"
    :diminish alchemist-mode
    :init
    (setq alchemist-test-status-modeline nil)
    :config
    (exec-path-from-shell-copy-env "MIX_ARCHIVES")
    (progn
      (evil-define-key 'normal alchemist-test-mode-map "]t" 'alchemist-test-mode-jump-to-next-test)
      (evil-define-key 'normal alchemist-test-mode-map "[t" 'alchemist-test-mode-jump-to-previous-test)
      (define-key evil-normal-state-map "]d" 'alchemist-goto-jump-to-next-def-symbol)
      (define-key evil-normal-state-map "[d" 'alchemist-goto-jump-to-previous-def-symbol)
      (define-key evil-normal-state-map "]T" '(lambda () (interactive)
                                                (popwin:select-popup-window)
                                                (alchemist-test-next-result)))
      (define-key evil-normal-state-map "[T" '(lambda () (interactive)
                                                (popwin:select-popup-window)
                                                (alchemist-test-previous-result)))
      (define-key alchemist-mode-map (kbd "C-c a g d") 'my/alchemist-generate-docs)
      (define-key alchemist-mode-map (kbd "C-c a d g") 'my/alchemist-mix-deps-get)
      (define-key alchemist-mode-map (kbd "C-c a S") 'my/alchemist-new-exs-buffer)
      (evil-leader/set-key-for-mode 'elixir-mode
        "t b" 'alchemist-mix-test-this-buffer
        "t t" 'alchemist-mix-test
        "t r" 'alchemist-mix-rerun-last-test
        "t p" 'alchemist-mix-test-at-point
        "e b" 'alchemist-eval-buffer
        "a d" 'alchemist-goto-list-symbol-definitions))))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" "\\.mkd\\'" "\\.markdown\\'"))

;(use-package ruby-mode
;  ;; built-in
;  :init
;  ;; Don't insert the "coding utf8" comment when saving Ruby files.
;  (setq ruby-insert-encoding-magic-comment nil))

;(use-package rspec-mode
;  :ensure t
;  :defer t
;  :init
;  (setq rspec-use-rake-when-possible nil))

;(use-package rbenv
;  :ensure t
;  :init
;  (setq rbenv-installation-dir "/usr/local/Cellar/rbenv/0.4.0"))

(use-package projectile-rails
  :ensure t
  :defer t
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")

;(use-package sh-script
;  ;; built-in
;  :demand t
;  :mode (("\\.zsh\\'" . shell-script-mode)))

;(use-package haskell-mode
;  :ensure t
;  :mode ("\\.hs\\'" "\\.lhs\\'")
;  :init
;  (setq haskell-process-suggest-remove-import-lines t
;        haskell-process-log t
;        haskell-stylish-on-save t)
;  (use-package ghc
;    :ensure t
;    :init
;    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;    (use-package company-ghc
;      :ensure t
;      :config
;      (add-to-list 'company-backends 'company-ghc)))
;  :config
;  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file))

;(use-package rust-mode
;  :ensure t
;  :mode "\\.rs\\'")

;(use-package go-mode
;  :ensure t
;  :mode "\\.go\\'"
;  :config
;  (add-hook 'go-mode-hook
;            (lambda ()
;              (setq-local tab-width 4)
;              (setq gofmt-command "goimports")
;              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package org-mode
  :mode "\\.org\\'"
  :config
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . auto))))


;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))

;; Custom file handling.
;(setq custom-file "~/.emacs.d/etc/custom.el")
;(when (not (file-exists-p custom-file))
;  (with-temp-buffer (write-file custom-file)))
;(load custom-file)


;; ;;; Buffer, Windows and Frames
;; (setq frame-title-format
;;       '(:eval (if (buffer-file-name)
;;                   (abbreviate-file-name (buffer-file-name)) "%b"))
;;       ;; Size new windows proportionally wrt other windows
;;       window-combination-resize t)


;; ;; Configure `display-buffer' behaviour for some special buffers.
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
