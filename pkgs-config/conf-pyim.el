;;
;; config pyim
;;

(require-package 'pyim)
(require-package 'pyim-wbdict)

(use-package pyim
  :ensure nil
  :defer t
  :config
  (use-package pyim-wbdict
    :ensure nil
    :config (pyim-wbdict-gbk-enable))
  (setq pyim-default-scheme 'wubi)
  (setq default-input-method "pyim")
  (setq pyim-page-length 5))

(provide 'conf-pyim)
