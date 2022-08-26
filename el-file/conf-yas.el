;;
;; config yasnippet
;;

;(mel/require-package 'yasnippet)

(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))
  ;;:config
  ;;(eval-after-load 'warnings
  ;;    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))

(provide 'conf-yas)
