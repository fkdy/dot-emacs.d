;; on-going settings for emacs

;; config org-file
(eval-after-load 'org
  '(setq mel/org-file-dir (mel/expand-emacs-d "org-file")
         mel/org-inbox-dir (mel/expand-emacs-d "inbox/work")
         org-default-notes-file (mel/expand-org-file "node.org")))

(require 'conf-zenburn)

(require 'conf-orderless)

(require 'conf-selectrum)

(require 'conf-ffip)

(require 'conf-avy)

(require 'conf-which-key)

(require 'conf-rainbow-delimiters)

(require 'conf-pyim)

(require 'conf-dashboard)

(require 'conf-verilog)

(require 'org-mmap)

(require 'conf-ggtags)

;(require 'conf-yas)

;(require 'conf-god-mode)
