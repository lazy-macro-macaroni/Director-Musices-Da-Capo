
(globals:standard-package :data-value-list
  create-data-value-list
  add-listener
  get-list
  set-list
  add-value
  remove-value
  get-value)

;; DATA LIST VALUE ;;

(defclass data-value-list ()
  ((name :accessor name-a :initarg :name)
   (allow-nil :accessor allow-nil-a :initarg :allow-nil)
   (value-type :accessor value-type-a :initarg :value-type)
   (value-list :accessor value-list-a :initform '())
   (listeners :accessor listeners-a :initform '())))

(defun create-data-value-list (name value-type &key allow-nil)
  (check-type name string)
  (make-instance 'data-value-list :name name :value-type value-type :allow-nil allow-nil))

(defmethod add-listener ((obj data-value-list) listener)
  (check-type listener function)
  (let ((current (listeners-a obj)))
    (setf (listeners-a obj) (if (eq current nil) (list listener) (cons listener current))))
  (funcall listener (value-list-a obj)))

(defmethod get-list ((obj data-value-list))
  (value-list-a obj))

(defmethod set-list-2 ((obj data-value-list) new-list)
  (setf (value-list-a obj) new-list)

  (loop for listener in (listeners-a obj)
    do (funcall listener (value-list-a obj))))

(defmethod set-list ((obj data-value-list) new-list)
  (loop for item in new-list
    do  (data-value-common:check-value-type value (value-type-a obj) (allow-nil-a obj)))
  (set-list-2 obj new-list))

(defmethod add-value ((obj data-value-list) value)
  (data-value-common:check-value-type value (value-type-a obj) (allow-nil-a obj))
  (set-list-2 obj (append (get-list obj) (list value))))

(defmethod remove-value ((obj data-value-list) value eq-fn)
  (set-list-2 obj (loop for item in (get-list obj) unless (funcall eq-fn item value) collect item)))

(defmethod get-value ((obj data-value-list) index)
  (check-type index integer)
  (nth index (value-list-a obj)))
