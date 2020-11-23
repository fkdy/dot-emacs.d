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
                                 ;; remove starting time stamp logging, 'cause there's a post-command-hook bug
                                 "UNREAD(u/!)"
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
                            ;; reference material
                            ("material" . ?m)
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
        ;; add project tag to todo items, rm inbox tag
        ("TODO" ("inbox") ("project" . t))
        ;; add canceled tag to canceled items, rm inbox tag
        ("CANCELED" ("inbox") ("canceled" . t))
        ;; add archive tag to done items, rm inbox and canceled
        ("DONE" ("inbox") ("canceled") ("ARCHIVE" . t))))

;; add inbox tags
(defun mel/org-capture-add-note-tags ()
  (if (string-match "\\\(note\\\)" (buffer-name))
      (let (pmin pmax)
        ;; set point min
        (setq pmin
              (- (point-max)
                 (org-capture-get :captured-entry-size)))
        ;; set point max
        (setq pmax (point-max))
        ;; make region
        (goto-char pmin)
        (push-mark pmax t t)
        ;; map func on each entries
        (org-map-entries
         (lambda()
           (let* ((tags (org-get-tags)))
             ;; check if tag is empty
             (if (string= "" (car tags))
                 ;; set to inbox
                 (org-set-tags-to "note")
               ;; add inbox to tag
               (org-set-tags-to (add-to-list 'tags "note")))
             ;; align tags
             (org-set-tags nil t)))
         ;; match
         t
         ;; scope
         'region))))

;; add todo state
(defun mel/org-capture-add-unread-state ()
  (if (string-match "\\\(input\\\|inbox\\\|idea\\\)" (buffer-name))
      (let (pmin pmax)
        ;; set point min
        (setq pmin
              (- (point-max)
                 (org-capture-get :captured-entry-size)))
        ;; set point max
        (setq pmax (point-max))
        ;; make region
        (goto-char pmin)
        (push-mark pmax t t)
        ;; map func on each entries
        (org-map-entries
         (lambda()
           ;; check if tag is empty
           (unless (org-entry-get (point) "TODO")
             ;; set to UNREAD
             (org-todo "UNREAD")))
         ;; match
         t
         ;; scope
         'region))))

(add-hook 'org-capture-before-finalize-hook 'mel/org-capture-add-note-tags 'append)
(add-hook 'org-capture-before-finalize-hook 'mel/org-capture-add-unread-state 'append)

;; org-capture configuration
(defun mel/org-cap-add-temp (key desc type target temp)
  (push `(,key ,desc ,type ,target ,temp) org-capture-templates))

(setq mel/org-file-dir
      (file-name-as-directory (expand-file-name "org-file" user-emacs-directory)))

(let* ((mel/org-inbox-file (expand-file-name "inbox.org" mel/org-file-dir))
       (mel/org-note-file (expand-file-name "note.org" mel/org-file-dir))
       (mel/org-today-file (expand-file-name "today.org" mel/org-file-dir))
       (mel/org-journal-file (expand-file-name "journal.org" mel/org-file-dir)))
  (unless (file-exists-p mel/org-file-dir)
    (make-directory mel/org-file-dir))
  (setq org-directory mel/org-file-dir)
  (setq org-default-notes-file mel/org-note-file)
  (setq org-capture-templates nil)
  (mel/org-cap-add-temp "n" "Note" 'entry `(file ,mel/org-note-file) "* %T %?")
  (mel/org-cap-add-temp "t" "Today" 'entry `(file ,mel/org-today-file) "* %U %?")
  (mel/org-cap-add-temp "j" "Journal" 'entry `(file+olp+datetree ,mel/org-journal-file) "* %U %?")
  (mel/org-cap-add-temp "i" "Inbox" 'entry `(file ,mel/org-inbox-file) "* %T %?"))

;; add more template below this line

(provide 'conf-org)
