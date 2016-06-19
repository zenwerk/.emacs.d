;; ファイル名の設定
(set-file-name-coding-system 'cp932)
(setq locale-coding-system 'cp932)

;; 左 Win キーを Super モディファイアキーに
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)


(provide 'my-windows)
