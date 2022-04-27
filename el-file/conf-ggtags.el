;;
;; ggtags
;;

;;(mel/require-package 'ggtags)
;(require 'ggtags)

(use-package ggtags
  :hook
  ((c-mode c++-mode verilog-mode) . (lambda () (ggtags-mode 1)))
  :bind
  (:map ggtags-mode-map
        ("C-c g c" . ggtags-create-tags)
        ("C-c g u" . ggtags-update-tags)
        ("C-c g r" . ggtags-find-reference)
        ("C-c g d" . ggtags-find-definition)
        ("C-c g s" . ggtags-find-other-symbol)
        ("M-." . ggtags-find-tag-dwim)))

(provide 'conf-ggtags)
