;;
;; configure find-file-in-project
;;

(require 'find-file-in-project)

(use-package find-file-in-project
  :config
  (setq ffip-find-options "-not -path \"*/\.*\"")
  (global-set-key (kbd "C-x f") 'find-file-in-project))

(provide 'conf-ffip)
