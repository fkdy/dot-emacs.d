;;
;; config pyim
;;

(mel/require-package 'pyim)
(mel/require-package 'pyim-wbdict)

(use-package pyim
  :ensure nil
  :init
  (setq default-input-method "pyim")
  :defer t
  :config
  (use-package pyim-wbdict
    :ensure nil
    :config (pyim-wbdict-gbk-enable))
  (setq pyim-default-scheme 'wubi)
  (setq pyim-page-length 5))

(provide 'conf-pyim)
