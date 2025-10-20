
(globals:standard-package :data-utils data-value-bad-type-error create-data-value get-value set-value add-listener)

(globals:custom-error data-value-bad-type-error)

(defun check-value-type (value value-type allow-nil)
  (if (and allow-nil (eq value nil))
    t
    (if (stringp value-type)
      (progn
        (java-utils:jcheck-type value value-type)
        t)
      (if (not (typep value value-type))
        (globals:throw-custom-error data-value-bad-type-error (globals:format-string "Value ~S is not of type ~S" value value-type))))))

(defclass data-value ()
  ((name :initarg :name)
   (allow-nil :initarg :allow-nil)
   (value-type :initarg :value-type)
   (value :initarg :value)
   (listeners :initform '())))

(defun create-data-value (name initial-value value-type &key allow-nil)
  (check-type name string)
  (check-value-type initial-value value-type allow-nil)
  (make-instance 'data-value :name name :value initial-value :value-type value-type :allow-nil allow-nil))

(defmethod get-value ((obj data-value))
  (slot-value obj 'value))

(defmethod set-value ((obj data-value) value)
  (let ((value-type (slot-value obj 'value-type)))
    (check-value-type value value-type (slot-value obj 'allow-nil))
    (setf (slot-value obj 'value) value)
    (loop for listener in (slot-value obj 'listeners)
      do (funcall listener value))))

(defmethod add-listener ((obj data-value) listener)
  (check-type listener function)
  (let ((current (slot-value obj 'listeners)))
    (setf (slot-value obj 'listeners) (if (eq current nil) (list listener) (cons listener current))))
  (funcall listener (slot-value obj 'value)))
