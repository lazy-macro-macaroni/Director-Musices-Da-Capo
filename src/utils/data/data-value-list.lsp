
(globals:standard-package :data-value-list
  data-value-list
  create-data-value-list
  add-listener
  get-name
  get-list
  get-length
  set-list
  add-value
  remove-value
  get-value
  mapped-data-value-list)

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

;; Listeners ;;

(defun trigger-update (obj update)
  (check-type obj data-value-list)
  (check-type update data-value-list-update:data-value-list-update)
  (loop for listener in (listeners-a obj)
    do (funcall listener update)))

(defun trigger-listeners (obj update-type index value)
  (check-type obj data-value-list)
  (trigger-update obj (data-value-list-update:create update-type index value)))

(defun add-listener (obj listener)
  (check-type obj data-value-list)
  (check-type listener function)
  (let ((current (listeners-a obj)))
    (setf (listeners-a obj) (if (eq current nil) (list listener) (cons listener current))))
  (funcall listener (data-value-list-update:create data-value-list-update:type-set-list 0 (value-list-a obj))))

;; ;;

(defun get-name (obj)
  (check-type obj data-value-list)
  (name-a obj))

(defun get-list (obj)
  (check-type obj data-value-list)
  (value-list-a obj))

(defun get-length (obj)
  (check-type obj data-value-list)
  (list-length (get-list obj)))

(defun set-list-2 (obj new-list)
  (check-type obj data-value-list)
  (setf (value-list-a obj) new-list))

(defun set-list (obj new-list)
  (check-type obj data-value-list)
  (loop for value in new-list
    do  (data-value-common:check-value-type value (value-type-a obj) (allow-nil-a obj)))
  (set-list-2 obj new-list)
  (trigger-listeners obj data-value-list-update:type-set-list 0 new-list))

(defun add-value (obj value)
  (check-type obj data-value-list)
  (data-value-common:check-value-type value (value-type-a obj) (allow-nil-a obj))
  (set-list-2 obj (append (get-list obj) (list value)))
  (trigger-listeners obj data-value-list-update:type-add-value (- (list-length (get-list obj)) 1) value))

(defun remove-value (obj value eq-fn)
  (check-type obj data-value-list)
  (set-list-2 obj (loop for item in (get-list obj) unless (funcall eq-fn item value) collect item))
  (globals:println "Not implemented: Index when removing value in data-value-list.")
  (trigger-listeners obj data-value-list-update:type-remove-value 0 value))

(defun get-value (obj index)
  (check-type obj data-value-list)
  (check-type index integer)
  (nth index (value-list-a obj)))

;; Mapped ;;

(defun mapped-data-value-list (name value-type in-list map-fn)
  "Create a data-value-list that listens to another data-value-list and runs MAP-FN on each value."
  (let ((dvl (create-data-value-list name value-type))
        (mapped-cache (make-hash-table :test #'eq)))

    (add-listener in-list
      (globals:safe-lambda (globals:format-string "Mapped Value List: ~S" name)
        (update)
        (let ((out-updates '()))
          (set-list-2 dvl
            (loop for item in (get-list in-list)
              collect
              (if (gethash item mapped-cache)
                (gethash item mapped-cache)
                (progn
                  (setf (gethash item mapped-cache) (funcall map-fn item))
                  (gethash item mapped-cache)))))

          (let ((update-type (data-value-list-update:get-update-type update))
                (index (data-value-list-update:get-index update)))
            (case update-type
              (:type-add-value (trigger-listeners dvl :type-add-value index (nth index (get-list dvl))))
              (:type-set-list (trigger-listeners dvl :type-set-list 0 (get-list dvl)))
              (otherwise
                (error "Unhandled update type: ~S, in mapped-data-value-list." update-type)))))

        (clrhash mapped-cache)

        (let ((cur-list (get-list dvl)))
          (loop for item in (get-list in-list)
            for i from 0
            do (setf (gethash item mapped-cache) (nth i cur-list))))))

    dvl))
