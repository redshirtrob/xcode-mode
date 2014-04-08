;;; xcode-mode.el --- Major mode for handling Xcode project and workspace files

;; Copyright (c) 2014 Robert S. Jones

;; Author: Robert S. Jones <robert.jones.sv@gmail.com>
;; Version: 0.01
;; Keywords: Xcode

;;; Commentary:

;; This is a library for interacting with Xcode project and workspace files.

;; Much of this project was informed by `python-django.el'.

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

(defvar xcode-mode-project-filename nil
  "Xcode project filename.")

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
  (interactive
   (list
    (read-directory-name "Project Root:" xcode-mode-project-root nil t)))
  (let* ((project-name (xcode-mode-project-basename directory))
         (project-file (concat directory "project.pbxproj"))
         (buffer-name (format "Xcode: %s" project-name)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (xcode-mode)
        (xcode-mode-ui-clean)
        (set (make-local-variable
              'xcode-mode-project-name) project-name)
        (set (make-local-variable
              'xcode-mode-project-filename) project-file)
        (xcode-mode-ui-insert-header)
        (xcode-mode-ui-show-buffer (current-buffer))))))

(provide 'xcode-mode)

;;; xcode-mode.el ends here
