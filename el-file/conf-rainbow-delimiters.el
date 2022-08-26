;;
;; config rainbow-delimiters
;;

;(mel/require-package 'rainbow-delimiters)

(use-package rainbow-delimiters
  :defer t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'conf-rainbow-delimiters)
