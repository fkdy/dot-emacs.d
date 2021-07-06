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
  (progn
    (unless (getenv "WORKON_HOME")
      (setenv "WORKON_HOME" (mel/expand-auto-dir "elpy"))
      (mel/mkdir (getenv "WORKON_HOME")))
    (setq elpy-rpc-virtualenv-path
          (expand-file-name "emacs" (getenv "WORKON_HOME")))
    (mel/mkdir elpy-rpc-virtualenv-path))
  (setq python-shell-interpreter "ipython")
  (setq elpy-rpc-python-command "python"))

(provide 'conf-elpy)
