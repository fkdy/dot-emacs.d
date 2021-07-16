;;; -*- lexical-binding: t -*-

;; Author: mel <chuenshengm@gmail.com>
;; Homepage:
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see https://www.gnu.org/licenses.

;;; Commentary:

;; Help keeping ~/.emacs.d as I expected.

;; Usage:

;; Load the feature `mel-collect-auto-file' as early as possible in your
;; init file.  Make sure you load it at least before you change any path
;; variables using some other method.
;;
;;   (require 'mel-collect-auto-file)

;; If you would like to use base directories different from what
;; `mel-collect-auto-file' uses by default, then you have to set the
;; respective variables before loading the feature.
;;
;;   (setq mel-collect-auto-file-etc-directory
;;         (expand-file-name "config/" user-emacs-directory))
;;   (setq mel-collect-auto-file-var-directory
;;         (expand-file-name "data/" user-emacs-directory))
;;   (require 'mel-collect-auto-file)

;;; Code:

(require 'cl-lib)

(defvar mel-collect-auto-directory mel/auto-dir-n-file
  "The directory where packages place their configuration and
  persistent data files. This variable has to be set before
  `mel-collect-auto-file' is loaded. use the same value as
  `mel/auto-dir-n-file' defined in conf.el")

;;;###autoload
(defun mel-collect-auto-file-expand-file-name (file)
  "Expand filename FILE relative to `mel-collect-auto-directory'."
  (expand-file-name (convert-standard-filename file)
                    mel-collect-auto-directory))

(cl-letf (((symbol-function 'autos)
           (symbol-function #'mel-collect-auto-file-expand-file-name)))
  (unless (file-exists-p mel-collect-auto-directory)
    (make-directory mel-collect-auto--directory t))
  (with-no-warnings ; many of these variables haven't been defined yet

;;; Built-in packages

    (setq abbrev-file-name                 (autos "abbrev.el"))
    (setq auto-insert-directory            (autos "auto-insert/"))
    ;(setq auto-save-list-file-prefix       (autos "auto-save/sessions/"))
    ;(setq backup-directory-alist           (list (cons "." (autos "backup/"))))
    (setq bookmark-default-file            (autos "bookmark-default.el"))
    (setq calc-settings-file               (autos "calc-settings.el"))
    (setq custom-file                      (autos "custom.el"))
    (eval-after-load 'desktop
      `(make-directory ,(autos "desktop/") t))
    (setq desktop-dirname                  (autos "desktop/"))
    (setq desktop-path                     (list desktop-dirname))
    (setq diary-file                       (autos "diary"))
    (setq ecomplete-database-file          (autos "ecomplete-database.el"))
    (setq ede-project-placeholder-cache-file (autos "ede-projects.el"))
    (eval-after-load 'erc
      `(make-directory ,(autos "erc/dcc/") t))
    (setq erc-dcc-get-default-directory    (autos "erc/dcc/"))
    (setq erc-log-channels-directory       (autos "erc/log-channels/"))
    (eval-after-load 'eshell
      `(make-directory ,(autos "eshell/") t))
    (setq eshell-aliases-file              (autos "eshell/aliases"))
    (setq eshell-directory-name            (autos "eshell/"))
    (setq eudc-options-file                (autos "eudc-options.el"))
    (eval-after-load 'eww
      `(make-directory ,(autos "eww/") t))
    (setq eww-bookmarks-directory          (autos "eww/"))
    (setq filesets-menu-cache-file         (autos "filesets-menu-cache.el"))
    (setq gamegrid-user-score-file-directory (autos "gamegrid-user-score/"))
    (setq ido-save-directory-list-file     (autos "ido-save-directory-list.el"))
    (setq image-dired-db-file              (autos "image-dired/db.el"))
    (setq image-dired-dir                  (autos "image-dired/"))
    (setq image-dired-gallery-dir          (autos "image-dired/gallery/"))
    (setq image-dired-temp-image-file      (autos "image-dired/temp-image"))
    (setq image-dired-temp-rotate-image-file (autos "image-dired/temp-rotate-image"))
    (setq kkc-init-file-name               (autos "kkc-init.el"))
    (eval-after-load 'newsticker
      `(make-directory ,(autos "newsticker/") t))
    (setq newsticker-cache-filename        (autos "newsticker/cache.el"))
    (setq newsticker-dir                   (autos "newsticker/data/"))
    (setq nsm-settings-file                (autos "nsm-settings.el"))
    (eval-after-load 'org
      `(make-directory ,(autos "org/") t))
    (setq org-clock-persist-file           (autos "org/clock-persist.el"))
    (setq org-id-locations-file            (autos "org/id-locations.el"))
    (setq org-publish-timestamp-directory  (autos "org/timestamps/"))
    (setq project-list-file                (autos "project-list.el"))
    (setq quickurl-url-file                (autos "quickurl-url.el"))
    (setq rcirc-log-directory              (autos "rcirc-log/"))
    (setq recentf-save-file                (autos "recentf-save.el"))
    (setq remember-data-file               (autos "remember/data"))
    (setq remember-data-directory          (autos "remember/data.d/"))
    (setq save-place-file                  (autos "save-place.el"))
    (setq savehist-file                    (autos "savehist.el"))
    (setq srecode-map-save-file            (autos "srecode-map.el"))
    (setq semanticdb-default-save-directory (autos "semantic/"))
    (setq shared-game-score-directory      (autos "shared-game-score/"))
    (setq timeclock-file                   (autos "timeclock"))
    (setq tramp-auto-save-directory        (autos "tramp/auto-save/"))
    (setq tramp-persistency-file-name      (autos "tramp/persistency.el"))
    (setq type-break-file-name             (autos "type-break.el"))
    (setq url-cache-directory              (autos "url/cache/"))
    (setq url-configuration-directory      (autos "url/"))
    (setq url-cookie-file                  (autos "url/cookies.el"))
    (setq url-history-file                 (autos "url/history.el"))

;;; Third-party packages

    (setq pyim-dcache-directory            (autos "pyim/dcache/"))
    (setq smex-save-file                   (autos "smex-save.el"))
    (setq undo-tree-history-directory-alist (list (cons "." (autos "undo-tree-hist/"))))
    (eval-after-load 'yasnippet
      `(make-directory ,(autos "yasnippet/") t))
    (setq yas-snippet-dirs                 (list (autos "yasnippet/")))
    ))

;;; _
(provide 'mel-collect-auto-file)
;; Local Autosiables:
;; indent-tabs-mode: nil
;; End:
;;; mel-collect-auto-file.el ends here
