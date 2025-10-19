
(globals:standard-package :rulepalette-select-built-in create-menu)

(defun open-rulepalette (file)
  (java-utils:jcheck-type file "java.io.File")
  (globals:println "Not Implemented: rulepalette-select:open-rulepalette"))

(defparameter *file-tree* nil)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (folder-menu:create-tree (file-utils:jfile "." "dm" "rulepalettes")))))

(defun create-menu (menu)
  (ensure-file-tree)

  (folder-menu:create-menu *file-tree* menu "Open Built-In Rulepalette" #'open-rulepalette))
