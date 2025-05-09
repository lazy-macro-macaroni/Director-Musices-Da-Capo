(globals:standard-package :settings)

(defvar *settings* (make-hash-table))
; (defvar *validators* (make-hash-table))

(defclass setting ()
  (value)
  (validator))

(defmethod validate ((obj setting) value)
  (funcall (slot-value obj :validator) value))

(defun get-value (name)
  (gethash name *settings*))

(defun set-value (name value)
  (let ((validator (gethash name *validators*)))
    (if (not validator)
      (error (globals:format-string "Setting with name

  (setf (gethash name *settings*) value))
