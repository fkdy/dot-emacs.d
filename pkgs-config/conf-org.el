;;
;; configure org
;;

(require 'org-id)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

;; show effect when requested to edit on invisible position
(setq org-catch-invisible-edits 'show)

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

;; org link configuration
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun mel/org-custom-id-set (&optional pom)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. The function
   create a CUSTOM_ID if none is present already. The CUSTOM_ID
   of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (unless (and id (stringp id) (string-match "\\S-" id))
        (save-match-data
          ;; get timestamp in headline
          (setq headline-string (nth 4 (org-heading-components)))
          ;; set id format
          (setq id-format "node-%Y-%m-%d-%H-%M")
          ;; subtract time string in timestamp
          (unless (not (string-match "^[[<]\\([^>]+?\\)[]>]" headline-string))
            ;; convert to internal time
            (setq internal-time (org-time-string-to-time (match-string 1 headline-string)))
            ;; set custom id
            (setq id (format-time-string id-format internal-time))
            (org-entry-put pom "CUSTOM_ID" id)
            (org-entry-put pom "ID" id)
            (org-id-add-location id (buffer-file-name (buffer-base-buffer)))))
        id))))

;; set org-archive-sibling-heading
(setq org-archive-sibling-heading "_ARCHIVE_")

;; archive done state entries to `org-archive-sibling-heading'
(defun mel/org-archive-all (&optional done-state)
  "archive all entries with TODO state set to DONE"
  (interactive)
  (save-excursion
    ;; check done-state
    (unless done-state
      (setq done-state "DONE"))
    ;; map func on each entries
    (org-map-entries
     (lambda()
       (if (and (string= done-state (org-entry-get (point) "TODO"))
                (= (car (org-heading-components)) 1))
           ;; archive to archive sibling
           (org-archive-to-archive-sibling)
         ;; move to end of current subtree
         (org-end-of-subtree))
       ;; continue mapping from end of the subtree or previous archived
       ;; position
       (setq org-map-continue-from (point)))
     ;; match
     t
     ;; scope
     'file)))

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
        ("CANCELED" ("inbox") ("canceled" . t) ("ARCHIVE" . t))
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
           (org-with-point-at (point)
             (let ((tags (org-get-tags))
                   (id (org-entry-get nil "CUSTOM_ID")))
               ;; check if tag is empty
               (if (string= "" (car tags))
                   ;; set to inbox
                   (org-set-tags-to "note")
                 ;; add inbox to tag
                 (org-set-tags-to (add-to-list 'tags "note")))
               ;; align tags
               (if (functionp 'org-align-tags)
                   (org-align-tags nil)
                 ;; Org version < 9.2
                 (org-set-tags nil t))

               ;; add custom-id
               (mel/org-custom-id-set (point)))))
         ;; match
         t
         ;; scope
         'region-start-level))))

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
       (mel/org-journal-file (expand-file-name "journal.org" mel/org-file-dir))
       (mel/org-review-file (expand-file-name "review.org" mel/org-file-dir)))
  (unless (file-exists-p mel/org-file-dir)
    (make-directory mel/org-file-dir))
  (setq org-directory mel/org-file-dir)
  (setq org-default-notes-file mel/org-note-file)
  (setq org-capture-templates nil)
  (mel/org-cap-add-temp "n" "Note" 'entry `(file ,mel/org-note-file) "* %T %?")
  (mel/org-cap-add-temp "t" "Today" 'entry `(file ,mel/org-today-file) "* %U %?")
  (mel/org-cap-add-temp "j" "Journal" 'entry `(file+olp+datetree ,mel/org-journal-file) "* %U %?")
  (mel/org-cap-add-temp "i" "Inbox" 'entry `(file ,mel/org-inbox-file) "* %T %?")
  (mel/org-cap-add-temp
   "r" "Review" 'entry `(file ,mel/org-review-file) "* %T %(format-time-string \"%W\")-th review\n%?")
  )

;; add more template below this line

;; get time stamp info in the heading
(defun mel/org-get-head-ts ()
  (save-excursion
    (let ((re org-ts-regexp3)
          match)
      (unless (org-at-heading-p) (org-back-to-heading t))
      (if (and (setq match (re-search-forward re (point-at-eol) t))
               (goto-char (- (match-beginning 1) 1)))
          (cadr (org-element-timestamp-parser))))))

;; get DATE from timestamp plist
(defun mel/org-get-date-from-ts (ts)
  (list (plist-get ts :month-start)
        (plist-get ts :day-start)
        (plist-get ts :year-start)))

;; sort entries to another file
(defun mel/org-refile-to (new-buf)
  "refile current entry at `point' to `new-buf', `new-buf' must
be an accessible file/buffer name"
  (if (and (get-buffer new-buf)
           (setq ts (mel/org-get-head-ts))
           (setq d (mel/org-get-date-from-ts ts)))
      ;; refile this entry to `new-buf'
      (progn
        ;; cut current subtree from the buffer
        (org-copy-subtree 1 nil nil nil)
        ;; find or create the datetree with ts info in `new-buf'
        (with-current-buffer new-buf
          (goto-char (point-min))
          (org-datetree-find-date-create d nil)
          ;; go to the end of this subtree
          (org-end-of-subtree t nil)
          ;; check time stamp
          (setq n (mel/org-get-head-ts))
          ;; delete whitespace
          (backward-delete-char (skip-chars-backward " \t\n"))
          ;; add new line, so the subtree located at the end of inserted subtree
          (unless n (insert "\n"))
          ;; paste the subtree to `new-buf'
          (org-paste-subtree nil nil nil t)
          ;; demote the subtree to adjust the heading level
          (unless n (org-demote-subtree))))))

;; copy entries to another file
(defun mel/org-cp-entry (old-buf new-buf)
  "copy entries in `old-buf' to `new-buf'"
  (with-current-buffer old-buf
    (save-excursion
      (goto-char (point-min))
      (if (and (get-buffer new-buf)
               (get-buffer old-buf))
          ;; map func on each entries
          (org-map-entries
           (lambda()
             ;; refile to `new-buf'
             (if (mel/org-refile-to new-buf)
                 ;; continue mapping from end of the subtree or previous position
                 (setq org-map-continue-from (point))))
           ;; all headline
           t
           ;; scope
           'file)))))


(provide 'conf-org)
