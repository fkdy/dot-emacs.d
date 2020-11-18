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
  (setq elpy-rpc-virtualenv-path (mel/expand-auto-dir "elpy"))
  (unless (getenv "WORKON_HOME")
    (setenv "WORKON_HOME" elpy-rpc-virtualenv-path)
    (mel/mkdir elpy-rpc-virtualenv-path))
  (mel/mkdir elpy-rpc-virtualenv-path)
  (setq python-shell-interpreter "ipython")
  (setq elpy-rpc-python-command "python"))

(provide 'conf-elpy)
