;;
;; undo-tree
;;

(require-package 'undo-tree)

(use-package undo-tree
  :bind
  ("C-c u" . undo-tree-mode))
;;  :config
;;  (global-set-key (kbd "C-x u" 'undo-tree-mode)))

(provide 'conf-undo-tree)
