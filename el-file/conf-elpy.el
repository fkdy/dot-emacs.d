;;
;; config elpy
;;

;;(mel/require-package 'elpy)

(use-package elpy
  :defer t
  :init
  (with-eval-after-load 'python
    (elpy-enable))
  :config
  (setq elpy-rpc-virtualenv-path
        (expand-file-name "emacs" (mel/expand-auto-dir "elpy")))
  ;; set $WORKON_HOME env variable to the directory containing all the
  ;; virtual env, so pyvenv-workon can select which virutal env to use.
  (setenv "WORKON_HOME" (file-name-directory elpy-rpc-virtualenv-path))
  (setq elpy-rpc-python-command "python")
  (when (file-directory-p elpy-rpc-virtualenv-path)
    ;; the 'emacs' virtualenv is the default virtualenv(assume the
    ;; 'emacs' virutalenv has been installed in
    ;; `(mel/expand-auto-dir "elpy/emacs")'
    (pyvenv-workon "emacs")))

(provide 'conf-elpy)
