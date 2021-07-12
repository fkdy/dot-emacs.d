;;
;; config avy
;;

(mel/require-package 'avy)

(use-package avy
  :init
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (global-set-key (kbd "M-g c") 'avy-goto-char))

(provide 'conf-avy)
