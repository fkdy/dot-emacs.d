;;
;; configure org
;;

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; org-agenda configuration
(let* ((org-file-dir (file-name-as-directory (expand-file-name "org-file" user-emacs-directory))))
  ;; make dir in case not existing
  (unless (file-exists-p org-file-dir)
    (make-directory org-file-dir))
  ;; set org-agenda-files
  (setq org-agenda-files (list org-file-dir))
  ;; set org-refile-targets
  (setq org-refile-targets (quote (;; use current buffer
                                   (nil :maxlevel . 6)
                                   ;; use org-agenda-files for targets
                                   (org-agenda-files :maxlevel . 6))))
  ;; provide refile target as path
  (setq org-refile-use-outline-path (quote file))
  ;; complete the outline path in a single go
  (setq org-outline-path-complete-in-steps nil)
  ;; create new parents
  (setq org-refile-allow-creating-parent-nodes (quote confirm)))

;; refile configuration
;; todo keywords
(setq org-todo-keywords (quote ((sequence
                                 ;; todo item that need to clarify outcome or process immediately
                                 "UNREAD(u!)"
                                 ;; making project plan which leads to the outcome
                                 "TODO(t@)"
                                 ;; interrupted by something, need to resume when the context is proper
                                 "WAIT(w@)"
                                 "|"
                                 "DONE(d!)"
                                 "CANCELED(c@)"))))

;; log drawer for state change
(setq org-log-into-drawer t)

;; global tags list
(setq org-tag-alist (quote (;; daily input
                            ("inbox" . ?i)
                            ;; temporary idea
                            ("note" . ?n)
                            ;; stuff that needs more than five minutes to process
                            ("project" . ?p)
                            ;; canceled todo item
                            ("canceled" . ?c)
                            ;; archived todo item
                            ("ARCHIVE" . ?a))))

;; todo state triggered tag change
(setq org-todo-state-tags-triggers
      `(;; no todo keywords
        (("note" . t))
        ;; add inbox tag to unread items
        ("UNREAD" ("inbox" . t))
        ;; add project tag to todo items
        ("TODO" ("project" . t))
        ;; add canceled tag to canceled items
        ("CANCELED" ("canceled" . t))
        ;; add archive tag to done items
        ("DONE" ("ARCHIVE" . t))))

;; org-capture configuration
(let* ((org-file-dir (file-name-as-directory (expand-file-name "org-file" user-emacs-directory)))
       (org-idea-file (expand-file-name "idea.org" org-file-dir))
       (org-note-file (expand-file-name "note.org" org-file-dir))
       (org-journal-file (expand-file-name "journal.org" org-file-dir)))
  (unless (file-exists-p org-file-dir)
    (make-directory org-file-dir))
  (setq org-directory org-file-dir)
  (setq org-default-notes-file org-note-file)
  (setq org-capture-templates `(
                                ;; ("j" "Journal" entry (file+datetree+olp ,org-journal-file)
                                ;; ; "* %U %?")

                                ("e" "itEm" item (file ,org-note-file)
                                 "- %?")

                                ("n" "Note" entry (file ,org-note-file)
                                 "* %T %?")

                                ("i" "Idea" entry (file ,org-idea-file)
                                 "* %T %?"))))

(provide 'conf-org)
