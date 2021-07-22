;;
;; configure find-file-in-project
;;

(require 'find-file-in-project)

(use-package find-file-in-project
  :config
  (if (eq system-type 'windows-nt)
      ;; use rust fd instead of GNU find for windows
      (setq ffip-use-rust-fd t
            ;; set emacs user directory as project root
            ffip-project-root "~/.emacs.d")
    ;; exclude hiden file/dir for GNU find
    (setq ffip-find-options "-not -path \"*/\.*\""))
  ;; bind key
  (global-set-key (kbd "C-x f") 'find-file-in-project))

(provide 'conf-ffip)
