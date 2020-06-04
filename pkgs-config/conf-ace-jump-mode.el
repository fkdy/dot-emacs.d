;;
;; ace-jump-mode
;;

(require-package 'ace-jump-mode)

(use-package ace-jump-mode
  :config
  (global-set-key (kbd "C-c j") 'ace-jump-mode) ;; globally bind "C-c ." with ace-jump-mode
  (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i)) ;; use lower case char
  (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" t))

(provide 'conf-ace-jump-mode)
