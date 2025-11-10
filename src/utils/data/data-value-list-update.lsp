
(globals:standard-package :data-value-list-update
  type-add-value
  type-remove-value
  type-set-list
  data-value-list-update
  create
  get-update-type
  get-index)

(globals:custom-error bad-update-type-error)

(defparameter type-add-value :type-add-value)
(defparameter type-remove-value :type-remove-value)
(defparameter type-set-list :type-set-list)
(defparameter update-types (list type-add-value type-remove-value type-set-list))

(defclass data-value-list-update ()
  ((update-type :accessor update-type-a :initarg :update-type)
   (index :accessor index-a :initarg :index)
   (value :accessor value-a :initarg :value)))

(defun create (update-type index value)
  (check-type index integer)

  (unless (member update-type update-types)
    (globals:throw-custom-error bad-update-type-error "Bad update type: ~S. Possible values: ~S." update-type update-types))

  (let ((u (make-instance 'data-value-list-update :update-type update-type :index index :value value)))
    u))

(defun get-update-type (dvlu)
  (check-type dvlu data-value-list-update)
  (update-type-a dvlu))

(defun get-index (dvlu)
  (check-type dvlu data-value-list-update)
  (index-a dvlu))
