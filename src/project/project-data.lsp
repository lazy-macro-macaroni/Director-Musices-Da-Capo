
(globals:standard-package :project-data
  get-current-file
  create-project
  reset-project
  load-from-file
  save-to-file)

(defparameter *ini-def*
  (ini-definition:create
    "dm-project"
    '()
    '()))

(defclass project ()
  ((ini :accessor ini-a :initarg :ini)
   (scores :accessor scores-a :initform (data-value-list:create-data-value-list "project-scores" 'integer))
   (rulepalettes :accessor rulepalettes-a :initform (data-value-list:create-data-value-list "project-rulepalettes" 'integer))
   (current-file :accessor current-file-a :initform (data-value:create-data-value "project-current-file" nil "java.io.File" :allow-nil t))))

(defun create-project ()
  (make-instance 'project :ini (ini-file:create *ini-def*)))

(defmethod reset-project ((p project))
  (setf (ini-a p) (ini-file:create *ini-def*))
  (data-value-list:set-list (scores-a p) '())
  (data-value-list:set-list (rulepalettes-a p) '())
  (data-value:set-value (current-file-a p) nil))

(defmethod get-current-file ((p project))
  (current-file-a p))

(defmethod load-from-file ((p project) file)
  (file-utils:check-type-is-file file)
  (ini-file:read-ini-from-file (ini-a p) file)
  (data-value:set-value (current-file-a p) file))

(defmethod save-to-file ((p project) file)
  (file-utils:check-type-is-file file)
  (ini-file:save-ini-to-file (ini-a p) file)
  (data-value:set-value (current-file-a p) file))
