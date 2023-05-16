;;
;; ggtags
;;

;;(mel/require-package 'ggtags)
;(require 'ggtags)

(when (executable-find "gtags")
  (use-package ggtags
    :defer t
    :config
    (setq ggtags-update-on-save nil) ;Don't try to update GTAGS on each save; makes the system sluggish for huge projects.
    (setq ggtags-highlight-tag nil)  ;Don't auto-highlight tag at point.. makes the system really sluggish!
    (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
    (setq ggtags-navigation-mode-lighter nil)
    (setq ggtags-mode-line-project-name nil)
    ;(setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB
    :hook
    ((c-mode c++-mode verilog-mode) . (lambda () (ggtags-mode 1)))
    :bind
    (:map ggtags-mode-map
          ("C-c g c" . ggtags-create-tags)
          ("C-c g u" . ggtags-update-tags)
          ("C-c g r" . ggtags-find-reference)
          ("C-c g d" . ggtags-find-definition)
          ("C-c g s" . ggtags-find-other-symbol)
          ("C-c g g" . ggtags-grep)
          ("M-." . ggtags-find-tag-dwim)))
  )

(provide 'conf-ggtags)
