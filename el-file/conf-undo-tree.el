;;
;; undo-tree
;;

(require-package 'undo-tree)

(use-package undo-tree
  :bind
  ("C-M-u" . undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist (list (cons "." (mel/expand-auto-dir "undo-tree-hist")))))

(provide 'conf-undo-tree)
