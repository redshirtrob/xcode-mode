;;; xcode.el --- Major mode for handling Xcode project and workspace files

;; Copyright (c) 2014 Robert S. Jones

;; Author: Robert S. Jones <robert.jones.sv@gmail.com>
;; Version: 0.01
;; Keywords: Xcode

;;; Commentary:

;; This is a library for interacting with Xcode project and workspace files.

;; Much of this code was informed by `json.el' as the Xcode pbxproj file is
;; structurally similar to JSON.  Many other pieces were informed by
;; `python-django.el'.

;;; History:

;; 2014-04-05 - Initial version.

;;; Code:

(require 'tree-widget)
(require 'wid-edit)
(require 'widget)

(defgroup xcode-mode nil
  "Xcode project settings."
  :group 'convenience
  :version "24.3")

;;; Keymaps

(defvar xcode-mode-map
  (let ((map (make-keymap)))
    ;; Customize keymap
    map)
    "Keymap for `xcode-mode'.")

;;; Main vars

(defvar xcode-mode-project-root nil
  "Xcode project root directory.")

(defvar xcode-mode-project-name nil
  "Xcode project name.")

;; Parameters

(defvar xcode-mode-object-type 'alist
  "Type to convert pbxproj dictionaries to.
Only support `alist' for now.")

(defvar xcode-mode-array-type 'vector
  "Type to convert pbxproj arrays to.
Only support `vector' for now.")

(defvar xcode-mode-key-type 'symbol
  "Type to convert pbxproj keys to.
Only support `symbol' for now.")

;; Faces

(defgroup xcode-mode-faces nil
  "Customize the appearance of Xcode buffers."
  :prefix "xcode-mode-"
  :group 'faces
  :group 'xcode-mode)

(defface xcode-mode-face-header
  '((t :inherit font-lock-function-name-face))
  "Face for generic header lines.

Many Xcode faces inherit from this one by default."
  :group 'xcode-mode-faces)

(defface xcode-mode-face-title
  '((t :inherit font-lock-keyword-face))
  "Face for titles."
  :group 'xcode-mode-faces)

(defface xcode-mode-face-project-name
  '((t :inherit xcode-mode-face-header))
  "Face for project name.")

;; Reader utilities

(defun xcode-mode-advance (&optional n)
  "Skip past the following N characters."
  (forward-char n))

(defun xcode-mode-peek ()
  "Return the character at point."
  (let ((char (char-after (point))))
    (or char :xcode-mode-eof)))

(defun xcode-mode-pop ()
  "Advance past the character at point, returning it."
  (let ((char (xcode-mode-peek)))
    (if (eq char :xcode-mode-eof)
        (signal 'end-of-file nil)
      (xcode-mode-advance)
      char)))

(defun xcode-mode-skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "\t\r\n\f\b "))

;; Error conditions

;; UI

(defun xcode-mode-ui-clean ()
  "Empty current UI buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun xcode-mode-ui-insert-header ()
  "Draw header information."
  (insert
   (format "%s\t\t%s\n"
           (propertize
            "Project:"
            'face 'xcode-mode-face-title)
           (propertize
            xcode-mode-project-name
            'face 'xcode-mode-face-project-name))
   "\n\n\n"))

(defcustom xcode-mode-ui-buffer-switch-function 'switch-to-buffer
  "Function for switching to the project buffer.
The function receives one argument, the status buffer."
  :group 'xcode-mode
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function-item display-buffer)
                (function :tag "Other")))

(defun xcode-mode-ui-show-buffer (buffer)
  "Show the Project BUFFER."
  (funcall xcode-mode-ui-buffer-switch-function buffer))

;;; Xcode Mode

(defvar xcode-mode-hook nil)

(defun xcode-mode-project-basename (dir)
  "Get project name for given DIR."
  (car (split-string (car (last (split-string dir "/" t))) "\\." t)))

(define-derived-mode xcode-mode special-mode "Xcode"
  "Major mode to manage Xcode projects and workspaces.
\\{xcode-mode-map}}")

(defun xcode-mode-open-project (directory)
  "Open an Xcode project at given DIRECTORY."
  (interactive "sProject Directory: ")
  (let* ((project-name (xcode-mode-project-basename directory))
         (buffer-name (format "Xcode: %s" project-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (xcode-mode)
        (xcode-mode-ui-clean)
        (set (make-local-variable
              'xcode-mode-project-name) project-name)
        (xcode-mode-ui-insert-header)
        (xcode-mode-ui-show-buffer (current-buffer))))))

(provide 'xcode-mode)

;;; xcode.el ends here
