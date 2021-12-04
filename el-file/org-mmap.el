(require 'cl-lib)
(require 'org-macs)

;;; Customization

(defcustom org-mmap-map-pos "MAP_POS"
  "property to store map position.

For example, set to \"MAP_POS\"."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'string
  :safe #'stringp)

(defcustom org-mmap-format-function 'org-mmap-default-function
  "Function used to display numbering.
It is called with one argument, a list of numbers, and should
return a string, or nil.  When nil, no numbering is displayed.
Any `face' text property on the returned string overrides
`org-num-face'."
  :group 'org-appearance
  :package-version '(Org . "9.3")
  :type 'function
  :safe nil)

;;; Internal Variables

(defvar-local org-mmap-headline "^\\(\\*+ \\)"
  "Matches a headline, putting stars and text into groups.
Stars are put in group 1 and the trimmed body in group 2.")

;;; Internal Functions

(defun org-mmap-clear ()
  "Remove all overlays in current buffer."
  (remove-overlays (point-min) (point-max) 'org-mmap t))

(defun org-mmap-make-overlay (map-pos-str ov-pos)
  "Make and set overlay properties.

OV-POS is the overlay position."
  (when map-pos-str
    (let ((ov (make-overlay (car ov-pos) (cdr ov-pos))))
      ;; set `org-mmap' property
      (overlay-put ov 'org-mmap t)
      ;; evaporate when empty
      (overlay-put ov 'evaporate t)
      ;; set MAP-POS-STR after headline string
      (overlay-put ov
                   'after-string
                   (funcall org-mmap-format-function map-pos-str))
      ov)))

(defun org-mmap-map-region (start end _)
  "Add overlays between START and END positions.
When START and END are nil, use buffer boundaries. Return the
list of created overlays, newest first."
  (save-match-data
    (org-with-point-at (or start (point-min))
      (save-excursion
        ;; go back to nearest heading
        (org-with-limited-levels
         (ignore-errors (org-back-to-heading t)))
        (let ((regexp org-mmap-headline))
          ;; search ORG-MMAP-HEADLINE from nearest headline to END
          (while (re-search-forward regexp end t)
            ;; point is at the end of the occurrance
            (let* ((pos-beg (match-beginning 0))
                   (pos-end (match-end 0))
                   (map-pos-str (org-entry-get pos-beg org-mmap-map-pos)))
              (pcase (get-char-property-and-overlay pos-beg 'org-mmap)
                ;; got an overlay, whose org-mmap property is t
                (`(t . ,o)
                 (cond
                  ((or (not map-pos-str) (string-empty-p map-pos-str))
                   ;; no 'MAP-POS' found, but we got overlay, del it
                   (delete-overlay o))
                  (t
                   ;; 'MAP-POS' property is updated
                   (unless (string-equal
                            (funcall org-mmap-format-function map-pos-str)
                            (overlay-get o 'after-string))
                     (overlay-put o 'after-string (funcall org-mmap-format-function map-pos-str)))
                   )))
                (`(nil)
                 ;; no overlay found, but we got 'MAP-POS' property
                 (when (and map-pos-str (not (string-empty-p map-pos-str)))
                   (org-mmap-make-overlay map-pos-str (cons pos-beg pos-end))))
                (`(_ nil))))))
        ))))

;;; Public Functions

;;;###autoload
(defun org-mmap-default-function (map-pos)
  "Default map-pos display function.
MAP-POS is the topic node to display."
  (concat "|" map-pos "|> "))

;;;###autoload
(define-minor-mode org-mmap-mode
  "Dynamic numbering of headlines in an Org buffer."
  :lighter " omm"
  (cond
   (org-mmap-mode
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot overlay mind-map position outside Org mode"))
    (org-mmap-map-region nil nil 0)
    (add-hook 'after-change-functions #'org-mmap-map-region nil t)
    (add-hook 'change-major-mode-hook #'org-mmap-clear nil t))
   (t
    (org-mmap-clear)
    (remove-hook 'after-change-functions #'org-mmap-map-region t)
    (remove-hook 'change-major-mode-hook #'org-mmap-clear t))))

(provide 'org-mmap)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-mmap.el ends here
