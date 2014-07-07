;;; pbxproj.el --- Xcode pbxproj file parser.

;; Copyright (c) 2014 Robert S. Jones

;; Author: Robert S. Jones <robert.jones.sv@gmail.com>
;; Version: 0.01
;; Keywords: Xcode

;;; Commentary:

;; This is a library for parsing Xcode project files.

;; Much of this code was informed by `json.el' as the Xcode pbxproj file is
;; structurally similar to JSON.

;;; History:

;; 2014-04-06 - Initial version.

;;; Code:

;; Parameters

(defvar pbxproj-object-type 'hash-table
  "Type to convert pbxproj dictionaries to.
Only support `alist' for now.")

(defvar pbxproj-array-type 'vector
  "Type to convert pbxproj arrays to.
Only support `vector' for now.")

(defvar pbxproj-key-type 'string
  "Type to convert pbxproj keys to.
Only support `symbol' for now.")

;; Reader utilities

(defsubst pbxproj-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defsubst pbxproj-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :pbxproj-eof)))

(defsubst pbxproj-pop ()
  "Advance past the character at point, returning it."
  (let ((char (pbxproj-peek)))
    (if (eq char :pbxproj-eof)
        (signal 'end-of-file nil)
      (pbxproj-advance)
      char)))

(defun pbxproj-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))

;; Error conditions

(put 'pbxproj-error 'error-message "Unknown PBXProj error")
(put 'pbxproj-error 'error-conditions '(pbxproj-error error))

(put 'pbxproj-readtable-error 'error-message "PBXProj readtable error")
(put 'pbxproj-readtable-error 'error-conditions
     '(pbxproj-readtable-error pbxproj-error error))

(put 'pbxproj-string-format 'error-message "Bad string format")
(put 'pbxproj-string-format 'error-conditions
     '(pbxproj-string-format pbxproj-error error))

(put 'pbxproj-object-format 'error-message "Bad PBXProj object")
(put 'pbxproj-object-format 'error-conditions
     '(pbxproj-object-format pbxproj-error error))

;;; PBXProj Objects

(defun pbxproj-new-object ()
  "Create a new Elisp object corresponding to a PBXProj object."
  (make-hash-table :test 'equal))

(defun pbxproj-add-to-object (object key value)
  "Add a new KEY -> VALUE association to OBJECT.
Returns the updated object."
  (puthash key value object)
  object)

;;; PBX Project reader.

(defun pbxproj-read-symbol ()
  "Read the PBX key at point."
  (let ((characters '())
        (char (pbxproj-peek)))
    (while (not (or (char-equal char ?\t)
                    (char-equal char ?\r)
                    (char-equal char ?\n)
                    (char-equal char ?\f)
                    (char-equal char ?\b)))
      (push (pbxproj-pop) characters)
      (setq char (pbxproj-peek)))
    (if characters
        (apply 'string (nreverse characters))
      "")))

(defun pbxproj-read-string ()
  "Read the PBXProj string at point."
  (unless (char-equal (pbxproj-peek) ?\")
    (signal 'pbxproj-string-format (list "doesn't start with '\"'!")))
  ;; Skip over the '"'
  (pbxproj-advance)
  (let ((characters '())
        (char (pbxproj-peek)))
    (while (not (char-equal ?\"))
      (push (pbxproj-pop) characters))
    ;; Skip over the '"'
    (pbxproj-advance)
    (if characters
        (apply 'string (nreverse characters))
      "")))

(defun pbxproj-read-object ()
  "Read the PBXProj object at point."
  (unless (char-equal (pbxproj-peek) ?{)
    signal 'pbxproj-string-format (list "doesn't start with '{'!"))
  (let ((elements (pbxproj-new-object)))
    (while (not (char-equal (pbxproj-peek) ?}))
      (pbxproj-skip-whitespace)
      (setq key (pbxproj-read-symbol))
      (pbxproj-skip-whitespace)
      (if (char-equal (pbxproj-peek) ?=)
          (pbxproj-advance)
          (signal 'pbxproj-object-format (list "=" (pbxproj-peek))))
      (setq value (pbxproj-read))
      (setq elements (pbxproj-add-to-object elements key value))
      (pbxproj-skip-whitespace)
      (unless (char-equal (pbxproj-peek) ?})
        (if (char-equal (pbxproj-peek) ?\;)
            (pbxproj-advance)
          (signal 'pbxproj-object-format (list ";" (pbxproj-peek))))))
    (pbxproj-advance)
    elements))

(defun pbxproj-read-array ()
  "Read the PBXProj array at point."
  ;; Skip over the "("
  (pbxproj-advance)
  (pbxproj-skip-whitespace)
  (let (elements)
    (while (not (char-equal (pbxproj-peek) ?\)))
      (push (pbxproj-read) elements)
      (pbxproj-skip-whitespace)
      (unless (char-equal (pbxproj-peek) ?\))
        (if (char-equal (pbxproj-peek) ?,)
            (pbxproj-advance)
          (signal 'pbxproj-error (list "," (pbxproj-peek))))))
    ;; Skip over the ")"
    (pbxproj-advance)
    (apply pbxproj-array-type (nreverse elements))))

(defvar pbxproj-readtable
  (let ((table
         '((?{ pbxproj-read-object)
           (?\( pbxproj-read-array)
           (?\" pbxpron-read-string))))
    (mapc (lambda (char)
          (push (list char 'pbxproj-read-symbol) table))
          '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n
               ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
               ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
               ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
               ?\/))
    (mapc (lambda (char)
            (push (list char 'pbxproj-read-number) table))
          '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
    table)
  "Readtable for PBXProj reader.")

(defun pbxproj-read ()
  "Parse and return the pbxproj object following point.
Advances point just past the pbxproj object."
  (pbxproj-skip-whitespace)
  (let ((char (pbxproj-peek))
        (if (not (eq char :pbxproj-eof))
            (let ((record (cdr (assq char pbxproj-readtable))))
              (if (functionp (car record))
                  (apply (car record) (cdr record))
                (signal 'pbxproj-readtable-error record)))
          (signal 'end-of-file nil)))))

(defun pbxproj-read-file (file)
  "Read pbxproj in FILE and return it."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "/\\*" nil t)
        (let ((beg (match-beginning 0))
              (end (re-search-forward "\\*/")))
          (delete-region beg end))))
    (goto-char (point-min))
    (pbxproj-read)))

(defun pbxproj-strip-comments ()
  "Strip comments from pbxproj file."
  (interactive)
  (with-current-buffer
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (delete-region (point-min) (point))
        (while (re-search-forward "/\\*" nil t)
          (let ((beg (match-beginning 0))
                (end (re-search-forward "\\*/")))
            (delete-region beg end))))))

(provide 'pbxproj-strip-comments)
(provide 'pbxproj)

;;; pbxproj.el ends here
