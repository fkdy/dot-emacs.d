;;
;; configure find-file-in-project
;;

;(mel/require-package 'find-file-in-project)

(use-package find-file-in-project
  :defer t
  :config
  (if (eq system-type 'windows-nt)
      ;; use rust fd instead of GNU find for windows
      (setq ffip-use-rust-fd t)
    ;; exclude hiden file/dir for GNU find
    (setq ffip-find-options "-not -path \"*/\.*\""))
  ;; set emacs-user-directory as project root
  (setq ffip-project-root user-emacs-directory)
  ;; bind key
  (global-set-key (kbd "C-x f") 'find-file-in-project))

(provide 'conf-ffip)
