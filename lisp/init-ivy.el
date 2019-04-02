;;; Ivy-related things -*- lexical-binding: t -*-
(use-package counsel
  :diminish ivy-mode
  :bind (("C-S-a" . counsel-M-x)
         ("C-S-o" . counsel-find-file))
  :config
  (ivy-mode 1))

(provide 'init-ivy)
