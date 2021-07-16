;;
;; config god mode
;;

(mel/require-package 'god-mode)

(use-package god-mode
  :init
  (global-set-key (kbd "<escape>") 'god-mode-all))

(provide 'conf-god-mode)
