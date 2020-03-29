;;
;; config elpy
;;

(require-package 'elpy)

(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable)
  :config
  (setq python-shell-interpreter "ipython")
  (setq elpy-rpc-python-command "python"))

(provide 'conf-elpy)
