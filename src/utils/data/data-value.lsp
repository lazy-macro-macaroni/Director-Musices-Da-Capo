
(globals:standard-package :data-value
  create-data-value
  get-value
  set-value
  add-listener)

(defclass data-value ()
  ((name :initarg :name)
   (allow-nil :initarg :allow-nil)
   (value-type :initarg :value-type)
   (value :initarg :value)
   (listeners :initform '())))

(defun create-data-value (name initial-value value-type &key allow-nil)
  (check-type name string)
  (data-value-common:check-value-type initial-value value-type allow-nil)
  (make-instance 'data-value :name name :value initial-value :value-type value-type :allow-nil allow-nil))

(defmethod get-value ((obj data-value))
  (slot-value obj 'value))

(defmethod set-value ((obj data-value) value)
  (let ((value-type (slot-value obj 'value-type)))
    (data-value-common:check-value-type value value-type (slot-value obj 'allow-nil))
    (setf (slot-value obj 'value) value)
    (loop for listener in (slot-value obj 'listeners)
      do (funcall listener value))))

(defmethod add-listener ((obj data-value) listener)
  (check-type listener function)
  (let ((current (slot-value obj 'listeners)))
    (setf (slot-value obj 'listeners) (if (eq current nil) (list listener) (cons listener current))))
  (funcall listener (slot-value obj 'value)))
