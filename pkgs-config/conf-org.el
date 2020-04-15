;;
;; configure org
;;

(global-set-key (kbd "C-c c") 'org-capture)

(let* ((org-file-dir (file-name-as-directory (expand-file-name "org-file" user-emacs-directory)))
       (org-idea-file (expand-file-name "idea.org" org-file-dir))
       (org-note-file (expand-file-name "note.org" org-file-dir))
       (org-journal-file (expand-file-name "journal.org" org-file-dir)))
  (unless (file-exists-p org-file-dir)
    (make-directory org-file-dir))
  (setq org-directory org-file-dir)
  (setq org-default-notes-file org-note-file)
  (setq org-capture-templates `(("j" "Journal" entry (file+olp+datetree ,org-journal-file)
                                 "* %U %?")

                                ("n" "Note" entry (file ,org-note-file)
                                 "* %T %?")

                                ("i" "Idea" entry (file ,org-idea-file)
                                 "* %T %?"))))

(provide 'conf-org)
