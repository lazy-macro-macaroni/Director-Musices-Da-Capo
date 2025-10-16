
(globals:standard-package :project-data load-project save-project create-project)

(defparameter *ini-def*
  (ini-definition:create
    '()
    '()))

(defclass project () ())

(defmethod load-from-string ((p project) in-string)
)

(defmethod save-to-string ((p project))
  "")

(defmethod load-project ((p project) file)
  (load-from-string p (file-utils:read-from-file file)))

(defmethod save-project ((p project) file)
  (file-utils:save-to-file file (save-to-string p)))

(defun create-project ()
  (make-instance 'project))
