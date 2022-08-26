;;
;; config verilog-mode
;;

(use-package verilog-mode
  :load-path
  "pkgs/non-elpa/verilog-mode/"
  :defer t
  :config
  (setq
   ;; Indentation of Verilog statements with respect to containing block
   verilog-indent-level 2
   ;; Indentation of Module level Verilog statements (eg always, initial)
   verilog-indent-level-module 0
   ;; Indentation of declarations with respect to containing block
   verilog-indent-level-declaration 2
   ;; Absolute indentation of first begin in a task or function block
   verilog-indent-level-behavioral 0
   ;; Indentation to add to each level of `ifdef declarations
   verilog-indent-level-directive 0
   ;; Indentation of Verilog statements split across lines
   verilog-cexp-indent 2
   ;; Indentation for case statements
   verilog-case-indent 2
   ;; How to treat indenting items in a list
   verilog-indent-lists t
   ;; Non-nil means TAB should always re-indent the current line
   verilog-tab-always-indent nil
   ;; Non-nil means indent begin statements following if, else, while, etc
   verilog-indent-begin-after-if t
   ;; Non-nil means automatically newline after semicolons
   verilog-auto-newline nil
   ;; Type of statements to lineup across multiple lines
   verilog-auto-lineup 'all
   ;; nil means lined up with first character on line holding matching if
   verilog-align-ifelse nil))

(provide 'conf-verilog)
