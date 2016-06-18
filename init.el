;; 参考URL
;; https://github.com/whatyouhide/emacs.d/blob/master/init.el
;;
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; パッケージシステムの初期化
(when (require 'package nil t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("MELPA" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  ;(add-to-list 'package-archives '("melpa" . "http://mepla.milkbox.net/packages/") t)
  ;; インストールしたパッケージにロードパスを通して読み込む
  (package-initialize))

;; Bootstrap `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; ext には他人の書いた elispコードを入れる
;; lisp には自分で書いた elisp コードを入れる
(add-to-list 'load-path (expand-file-name "ext" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; テーマ設定
;(load-theme 'manoj-dark t)

;;; スタートアップスクリーンを表示しない
(setq inhibit-splash-screen t)

;;リージョンに色をつける
(transient-mark-mode t)

;; インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

;; Only maximize the window now because doing so earlier causes weird
;; behaviours.
(when (display-graphic-p)
  (toggle-frame-maximized))

;; init.el で使用される共通関数を読み込み
(use-package my-functions)

;; TODO: テーマ
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

;; 返答は常に "y or n" にする.  "yes or no" は使用しない
(defalias 'yes-or-no-p 'y-or-n-p)

;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-language-environment  'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evil
(use-package evil
  :init
  ;; evil-leader-mode
  (use-package evil-leader
    :config
    (setq evil-leader/in-all-status 1)
    (global-evil-leader-mode)
    (evil-leader/set-leader "\\"))
  :config
  (evil-mode 1))

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

(use-package my-smarter-beginning-of-line
  :bind ("C-a" . my/smarter-beginning-of-line))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-mode
(use-package company
  :config
  (use-package company-quickhelp)

  (global-company-mode) ; 全バッファで有効にする
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  ;;(company-quickhelp-mode +1) ; バグるのでコメントアウト
  ;; 候補選択のキーバインドを変更
  ;(define-key company-active-map (kbd "M-n") nil)
  ;(define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-h") nil)
  ;; TABキーの挙動を変更
  ;;(defun company--insert-candidate2 (candidate)
  ;;  (when (> (length candidate) 0)
  ;;    (setq candidate (substring-no-properties candidate))
  ;;    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
  ;;        (insert (company-strip-prefix candidate))
  ;;      (if (equal company-prefix candidate)
  ;;          (company-select-next)
  ;;          (delete-region (- (point) (length company-prefix)) (point))
  ;;        (insert candidate))
  ;;      )))

  ;;(defun company-complete-common2 ()
  ;;  (interactive)
  ;;  (when (company-manual-begin)
  ;;    (if (and (not (cdr company-candidates))
  ;;             (equal company-common (car company-candidates)))
  ;;        (company-complete-selection)
  ;;      (company--insert-candidate2 company-common))))

  ;;(define-key company-active-map [tab] 'company-complete-common2)
  ;;(define-key company-active-map [backtab] 'company-select-previous) ; おまけ

  ;; 候補窓の色設定
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; helm
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar
;; http://keisanbutsuriya.hateblo.jp/entry/2015/11/22/214457
(use-package tabbar
  :config
  (tabbar-mode)
  (tabbar-mwheel-mode nil)                  ; マウスホイール無効
  (setq tabbar-buffer-groups-function nil)  ; グループ無効
  (setq tabbar-use-images nil)              ; 画像を使わない
  ;; キー割り当て
  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
  ;; 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))
  ;; タブのセパレーターの長さ
  (setq tabbar-separator '(1.0))
  ;; タブの色（CUIの時。GUIの時は後でカラーテーマが適用）
  (set-face-attribute
   'tabbar-default nil
   :background "brightblue"
   :foreground "white"
   )
  (set-face-attribute
   'tabbar-selected nil
   :background "#ff5f00"
   :foreground "brightwhite"
   :box nil
   )
  (set-face-attribute
   'tabbar-modified nil
   :background "brightred"
   :foreground "brightwhite"
   :box nil
   )
  ;; 表示するバッファ
  (defun my-tabbar-buffer-list ()
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((buffer-file-name b) b)
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                       ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                       ((equal "*shell*" (buffer-name b)) b) ; *shell*バッファは表示する
                       ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                       ((buffer-live-p b) b)))
                  (buffer-list))))
  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package flycheck-pos-tip
  :config
  (progn
    (custom-set-variables
     '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir
(use-package ruby-end)
(defun auto-activate-ruby-end-mode-for-elixir-mode ()
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
          "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode +1))
(use-package elixir-mode
  ;:load-path "~/Code/emacs-elixir"
  :init
  (use-package flycheck-elixir)
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
  :config
  (setq company-minimum-prefix-length 3) ; デフォルトは4
  (use-package alchemist
    :diminish alchemist-mode
    :init
    (setq alchemist-test-status-modeline nil)
    :config
    ;;(exec-path-from-shell-copy-env "MIX_ARCHIVES")
    ;;(progn
    ;;  (evil-define-key 'normal alchemist-test-mode-map "]t" 'alchemist-test-mode-jump-to-next-test)
    ;;  (evil-define-key 'normal alchemist-test-mode-map "[t" 'alchemist-test-mode-jump-to-previous-test)
    ;;  (define-key evil-normal-state-map "]d" 'alchemist-goto-jump-to-next-def-symbol)
    ;;  (define-key evil-normal-state-map "[d" 'alchemist-goto-jump-to-previous-def-symbol)
    ;;  (define-key evil-normal-state-map "]T" '(lambda () (interactive)
    ;;                                            (popwin:select-popup-window)
    ;;                                            (alchemist-test-next-result)))
    ;;  (define-key evil-normal-state-map "[T" '(lambda () (interactive)
    ;;                                            (popwin:select-popup-window)
    ;;                                            (alchemist-test-previous-result)))
    ;;  (define-key alchemist-mode-map (kbd "C-c a g d") 'my/alchemist-generate-docs)
    ;;  (define-key alchemist-mode-map (kbd "C-c a d g") 'my/alchemist-mix-deps-get)
    ;;  (define-key alchemist-mode-map (kbd "C-c a S") 'my/alchemist-new-exs-buffer)
    ;;  (evil-leader/set-key-for-mode 'elixir-mode
    ;;    "t b" 'alchemist-mix-test-this-buffer
    ;;    "t t" 'alchemist-mix-test
    ;;    "t r" 'alchemist-mix-rerun-last-test
    ;;    "t p" 'alchemist-mix-test-at-point
    ;;    "e b" 'alchemist-eval-buffer
    ;;    "a d" 'alchemist-goto-list-symbol-definitions))))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他
(use-package yaml-mode
  :ensure t
  :mode "\\.e?ya?ml$")
