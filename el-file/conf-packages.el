;;
;; config packages.el
;;

(require 'package)

;; package dirs
(let ((versioned-pkg-dirs
	   (expand-file-name
		(format "pkgs/elpa-%s.%s" emacs-major-version emacs-minor-version)
		user-emacs-directory)))
  ;; set up package-user-dir
  (setq package-user-dir versioned-pkg-dirs)
  ;; set up package-gnupghome-dir for elpa gnupg
  (setq package-gnupghome-dir (expand-file-name "gnupg" versioned-pkg-dirs)))

;; package repositories
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
						 ("melpa" . "http://mirrors.163.com/elpa/melpa-stable/")))

;; do not activate installed packages
(setq package-enable-at-startup nil)

;; initialize packages
(package-initialize)

;; install packages if not installed
(defun require-package (pkg)
  "Install given PACKAGE"
  (condition-case err
	  (unless (package-installed-p pkg)
		(package-install pkg))
	(error (message "Couldn't install package: `%s': %S" pkg err) nil)))

;; refresh package list
;; unconditionally update keyring for all versions
(when (not package-archive-contents)
  ;; disable signature checking
  (setq package-check-signature nil)
  ;; install gnu-elpa-keyring-update
  (package-refresh-contents)
  (require-package 'gnu-elpa-keyring-update)
  ;; set signature checking to default value
  (setq package-check-signature 'allow-unsigned)
  ;; import keyring to versioned-pkg-dirs/gnupg
  (package-import-keyring (expand-file-name "package-keyring.gpg" data-directory))
  ;; update keyring
  (gnu-elpa-keyring-update))

(provide 'conf-packages)
