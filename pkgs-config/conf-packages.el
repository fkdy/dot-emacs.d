;;
;; config packages.el
;;

(require 'package)

;; package dirs
(let ((versioned-pkg-dirs
	   (expand-file-name
		(format "pkgs/elpa-%s.%s" emacs-major-version emacs-minor-version)
		user-emacs-directory)))
  (setq package-user-dir versioned-pkg-dirs))

;; package repositories
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
						 ("melpa" . "http://mirrors.163.com/elpa/melpa-stable/")))

;; do not activate installed packages
(setq package-enable-at-startup nil)

;; initialize packages
(package-initialize)

;; refresh package list
(when (not package-archive-contents)
  (package-refresh-contents))

;; install packages if not installed
(defun require-package (pkg)
  "Install given PACKAGE"
  (condition-case err
	  (unless (package-installed-p pkg)
		(package-install pkg))
	(error (message "Couldn't install package: `%s': %S" pkg err) nil)))

(provide 'conf-packages)
