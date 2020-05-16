;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(add-to-list 'load-path (expand-file-name "pkgs-config" user-emacs-directory))

(require 'conf-preference)

(require 'conf-packages)

(require 'conf-use-package)

(require 'conf-smex)

(require 'conf-ace-jump-mode)

(require 'conf-undo-tree)

(require 'conf-ggtags)

(require 'conf-elpy)

(require 'conf-org)

(require 'conf-zenburn)
