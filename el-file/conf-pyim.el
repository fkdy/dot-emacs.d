;;
;; config pyim
;;

;;(mel/require-package 'pyim)
;;(mel/require-package 'pyim-wbdict)

(use-package pyim
  :defer t
  :config
  (use-package pyim-wbdict
    :ensure nil
    :config (pyim-wbdict-v86-enable))
  (setq pyim-default-scheme 'wubi
        pyim-page-length 5
        default-input-method "pyim"
        pyim-dcache-directory (mel/expand-auto-dir "pyim/dcache")))

(provide 'conf-pyim)
