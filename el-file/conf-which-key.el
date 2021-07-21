;;
;; config which-key
;;

(mel/require-package 'which-key)

(use-package which-key
  :config
  ;; enable globally
  (which-key-mode)
  ;; Hide/Modify some function prefix in which-key show menu
  (setq which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("which-key-show-next-page" . "wk next pg")
          ("\\`calc-" . "") ; Hide "calc-" prefixes when listing M-x calc keys
          ("/body\\'" . "") ; Remove display the "/body" portion of hydra fn names
          ("modi/" . "m/") ; The car is intentionally not "\\`modi/" to cover
                                        ; cases like `hydra-toggle/modi/..'.
          ("\\`hydra-" . "+h/")
          ("\\`org-babel-" . "ob/")
          ("\\`my/" . ""))))

(provide 'conf-which-key)
