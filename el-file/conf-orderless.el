;;
;; conf orderless
;;

;(mel/require-package 'orderless)

(use-package orderless
  :config
  (setq completion-styles '(flex orderless)
        orderless-matching-styles '(orderless-flex orderless-literal)))

(provide 'conf-orderless)
