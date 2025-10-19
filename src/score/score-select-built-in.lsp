
(globals:standard-package :score-select-built-in create-menu)

(defun open-score (file)
  (java-utils:jcheck-type file "java.io.File")
  (globals:println "Not Implemented: score-select:open-score"))

(defparameter *file-tree* nil)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (folder-menu:create-tree (file-utils:jfile "." "dm" "scores")))))

(defun create-menu (menu)
  (ensure-file-tree)

  (folder-menu:create-menu *file-tree* menu "Open Built-In Score" #'open-score))
