
(globals:standard-package :score-select-built-in
  create-menu
  create-popup-menu)

(defun open-score (file)
  (file-utils:check-type-is-file file)
  (score-manage:new-from-file file))

(defparameter *file-tree* nil)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (folder-menu:create-tree (file-utils:jfile "." "dm" "scores")))))

(defun create-menu (menu)
  (ensure-file-tree)

  (folder-menu:create-menu *file-tree* menu "Open Built-In Score" #'open-score))

(defun create-popup-menu ()
  (ensure-file-tree)

  (folder-menu:create-popup-menu *file-tree* #'open-score))
