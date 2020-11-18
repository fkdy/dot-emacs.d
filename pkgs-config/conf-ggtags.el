;;
;; ggtags
;;

(require-package 'ggtags)

(use-package ggtags-mode
  :hook (c-mode c++-mode verilog-mode)
  :config (ggtags-mode 1))

(provide 'conf-ggtags)
