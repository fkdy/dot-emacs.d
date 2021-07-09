;;
;; ido mode
;;

(require 'ido)

(use-package ido
  :init
  (setq ido-save-directory-list-file (mel/expand-auto-file "ido.last")
        ;; show any name that has the chars you typed
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-create-new-buffer 'always
        confirm-nonexistent-file-or-buffer t
        ;; use current pane for newly opened file
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-everywhere t)
  :config
  (add-hook 'after-init-hook 'ido-mode))

(provide 'conf-ido)
