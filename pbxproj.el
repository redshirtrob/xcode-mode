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

(defvar pbxproj-object-type 'alist
  "Type to convert pbxproj dictionaries to.
Only support `alist' for now.")

(defvar pbxproj-array-type 'vector
  "Type to convert pbxproj arrays to.
Only support `vector' for now.")

(defvar pbxproj-key-type 'symbol
  "Type to convert pbxproj keys to.
Only support `symbol' for now.")

;; Reader utilities

(defun pbxproj-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defun pbxproj-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :pbxproj-eof)))

(defun pbxproj-pop ()
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

;;; PBX Project reader.

(defun pbxproj-read ()
  "Parse and return the pbxproj object following point.
Advances point just past the pbxproj object.")

(defun pbxproj-read-file (file)
  "Read pbxproj in FILE and return it."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "/\\*" nil t)
        (let ((beg (match-beginning 0))
              (end (re-search-forward "\\*/")))
          (delete-region beg end))))))

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
