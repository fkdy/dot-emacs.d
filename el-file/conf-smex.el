;;
;; config smex
;;

(require-package 'smex)

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (mel/expand-auto-file "smex-items"))
  (global-set-key (kbd "M-x") 'smex))

(provide 'conf-smex)
