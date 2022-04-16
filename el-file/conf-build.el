;;;; Site-lisp

;; The site-lisp directory is where we put our own packages.  We byte-compile
;; and generate an autoload file for them. We only do this when a package is
;; newer than its byte-compiled version.

;; This is needed, or `generated-autoload-file' will be not defined as a
;; variable at byte-compile time.  See the comments in
;; `straight--generate-package-autoloads'.
(eval-and-compile
  (require 'autoload)
  (require 'bytecomp))

(defun gen-auto()
  "Generate autoload files"
  (interactive)
  (let* (;; Dir & files
         (build-dir (progn (make-directory (mel/expand-non-elpa-dir "build") t)
                           (mel/expand-non-elpa-file "build")))
         (build-files (directory-files build-dir nil ".*\.el"))
         (newer-lisp-file nil)
         ;; Don't bother me.
         ;(inhibit-message t)
         ;; Prevent `update-directory-autoloads' from running hooks when
         ;; visiting the autoload file.
         (find-file-hook nil)
         (write-file-functions nil)
         ;; Prevent `update-directory-autoloads' from creating backup files.
         (backup-inhibited t)
         (version-control 'never)
         ;; set autoload-file
         (generated-autoload-file (expand-file-name "autoloads.el" build-dir)))
    (cl-letf (((symbol-function #'byte-compile-log-1) #'ignore)
              ((symbol-function #'byte-compile-log-file) #'ignore)
              ((symbol-function #'byte-compile-log-warning) #'ignore))
      ;; add build-dir to load-path
      (add-to-list 'load-path build-dir)
      (dolist (file (directory-files mel/non-elpa nil ".*\.el$"))
        (unless (string-prefix-p "." file)
          ;; Make symlinks of site-lisp files in build-dir. This is needed
          ;; for `byte-compile-file' and `update-directory-autoloads'.
          (unless (member file build-files)
            (make-symbolic-link (mel/expand-non-elpa-file file)
                                (expand-file-name file build-dir)))
          ;; Byte compile
          (let ((byte-file
                 (concat build-dir (file-name-sans-extension file) ".elc")))
            (when (file-newer-than-file-p
                   (concat mel/non-elpa file) byte-file)
              (setq newer-lisp-file t)
              (byte-compile-file (concat build-dir file))))))
      ;; Generate autoload file
      (when newer-lisp-file
        (unless (file-exists-p generated-autoload-file)
          (with-current-buffer (find-file-noselect generated-autoload-file)
            (insert ";; -*- lexical-binding: t -*-\n")
            (save-buffer)))
        (update-directory-autoloads build-dir)
        (byte-compile-file (concat build-dir "autoloads.el")))
      ;; Load autoload file
      (load (concat build-dir "autoloads") 'noerror 'nomessage))))

(provide 'conf-build)
