;;
;; config selectrum
;;

;(mel/require-package 'selectrum)

(use-package selectrum
  :config
  (selectrum-mode +1)
  ;; optional performance optimization with orderless
  ;; by highlighting only the visible candidates.
  ;;(setq orderless-skip-highlighting (lambda() selectrum-is-active)
  ;;      selectrum-highlight-candidates-function #'orderless-highlight-matches))
  )

(provide 'conf-selectrum)
