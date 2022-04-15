;;
;; config dashboard
;;

;(mel/require-package 'dashboard)

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome"
        dashboard-startup-banner '3
        dashboard-set-init-info t
        dashboard-items '((recents . 5)
                          (bookmarks . 5)))
  (dashboard-setup-startup-hook))

(provide 'conf-dashboard)
