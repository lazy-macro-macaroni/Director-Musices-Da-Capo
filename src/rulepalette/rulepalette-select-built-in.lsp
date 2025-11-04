
(globals:standard-package :rulepalette-select-built-in
  create-menu
  create-popup-menu)

(defun open-rulepalette (file)
  (file-utils:check-type-is-file file)
  (globals:println "Not Implemented: rulepalette-select:open-rulepalette"))

(defparameter *file-tree* nil)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (folder-menu:create-tree (file-utils:jfile "." "dm" "rulepalettes")))))

(defun create-menu (menu)
  (ensure-file-tree)

  (folder-menu:create-menu *file-tree* menu "Open Built-In Rulepalette" #'open-rulepalette))

(defun create-popup-menu ()
  (ensure-file-tree)

  (folder-menu:create-popup-menu *file-tree* #'open-rulepalette))
