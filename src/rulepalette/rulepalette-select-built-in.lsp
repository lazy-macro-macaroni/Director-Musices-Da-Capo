
(globals:standard-package :rulepalette-select-built-in
  create-menu
  create-popup-menu)

(defparameter *file-tree* nil)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (folder-menu:create-tree (file-utils:jfile "." "dm" "rulepalettes")))))

(defun create-menu (menu)
  (ensure-file-tree)

  (folder-menu:create-menu *file-tree* menu "Open Built-In Rulepalette" #'rulepalette-manage:new-from-file))

(defun create-popup-menu ()
  (ensure-file-tree)

  (folder-menu:create-popup-menu *file-tree* #'rulepalette-manage:new-from-file))
