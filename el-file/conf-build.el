;; update mel/autoload-path
(defun mel/search-el-dir-recursivly (el-path)
  "Get autoload-path with top dir as el-path"
  (let (el-dirs nil)
    (if (file-directory-p el-path)
        ;; recursively goto child dir
        (dolist (entry (directory-files el-path t))
          (unless (string-match-p "\\(?:\\(?:\\.\\(?:\\.\\|git\\)?\\)$\\)" entry)
            (mel/update-autoload-path-recursivly entry)))
      (when (string-match-p ".*\\.el$" el-path)
        ;; there is an elisp file in the dir, add the dir to autoload path
        (add-to-list el-dirs (file-name-directory el-path))
        el-dirs))))

;; set mel/autoload-path
;(setq mel/autoload-path (mel/search-el-dir-recursivly mel/non-elpa))

;; generate autoload file
(defun mel/gen-autoload-file (&optional el-paths)
  "Generate autoload files recursively in the dir list `el-paths'"
  (interactive)
  (eval-and-compile
    (require 'autoload)
    (require 'bytecomp))
  (let (
        ;; Don't bother me.
        ;;(inhibit-message t)
        ;; Prevent `update-directory-autoloads' from running hooks when
        ;; visiting the autoload file.
        (find-file-hook nil)
        (write-file-functions nil)
        ;; Prevent `update-directory-autoloads' from creating backup files.
        (backup-inhibited t)
        (version-control 'never)
        ;; set autoload-file
        (generated-autoload-file (mel/expand-non-elpa-file "autoloads.el")))
    ;; rm original autoload-file
    (if (file-exists-p generated-autoload-file)
        (delete-file generated-autoload-file))
    ;; create autoload -file
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (insert ";; -*- lexical-binding: t -*-\n")
      (save-buffer))
    ;; update autoload file
    (dolist (entry el-paths)
      (if (and (file-exists-p entry)
               (file-directory-p el-paths))
          (if (fboundp 'make-directory-autoloads)
              (make-directory-autoloads entry generated-autoload-file)
            (and (fboundp 'update-directory-autoloads)
                 (update-directory-autoloads entry)))))
    ;; compile autoload file
    (byte-compile-file generated-autoload-file)
    ;; Load autoload file
    (load generated-autoload-file 'noerror 'nomessage)))

(provide 'conf-build)
