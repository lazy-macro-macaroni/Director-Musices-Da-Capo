
(globals:standard-package :project-data get-current-file create-project load-from-file save-to-file)

(defparameter *ini-def*
  (ini-definition:create
    "dm-project"
    '()
    '()))

(defclass project ()
  ((ini :accessor a-get-ini :initarg :ini)
   (current-file :accessor a-get-current-file :initform (data-utils:create-data-value "project-current-file" nil "java.io.File" :allow-nil t))))

(defun create-project ()
  (make-instance 'project :ini (ini-file:create *ini-def*)))

(defmethod get-current-file ((p project))
  (a-get-current-file p))

(defmethod load-from-file ((p project) file)
  (java-utils:jcheck-type file "java.io.File")
  (ini-file:read-ini-from-file (a-get-ini p) file)
  (setf (a-get-current-file p) file))

(defmethod save-to-file ((p project) file)
  (java-utils:jcheck-type file "java.io.File")
  (ini-file:save-ini-to-file (a-get-ini p) file)
  (setf (a-get-current-file p) file))
