
(globals:standard-package :ini-definition
  :create :name :setting-type :default-value :settings :files
  :add-setting :get-setting :get-setting-type :get-setting-default-value
  :add-file :has-file
  :parse-settings :convert-string :validate-value)

(defclass ini-definition-setting ()
  ((name :initarg :name :accessor name)
   (setting-type :initarg :setting-type :accessor setting-type)
   (default-value :initarg :default-value :accessor default-value)))

(defclass ini-definition ()
  ; (type-validators :initform
  ;   (let ((validators (make-hash-table))
  ;         (int-matcher (string-utils:regex-matcher "\\d+"))
  ;         (float-matcher (string-utils:regex-matcher "\\d+\\.?\\d*")))
  ;     (setf (gethash :string validators) (lambda (val) t))
  ;     (setf (gethash :int validators) (lambda (val) ))
  ;     validators))
  ((settings :accessor settings :initform (make-hash-table))
   (files :accessor files :initform '())))

(defun create (settings files)
  (let ((def (make-instance 'ini-definition)))
    (parse-settings def settings)
    (parse-files def files)
    def))

;; Settings ;;

(defmethod parse-settings ((obj ini-definition) settings)
  (loop for (setting setting-type default-value) in settings
    do (add-setting obj setting setting-type default-value)))

(defmethod add-setting ((obj ini-definition) name setting-type default-value)
  (setf name (misc-utils:to-keyword name))
  (when (gethash name (settings obj))
    (error "Setting with name ~S already exists." name))

  (setf
    (gethash name (settings obj))
    (make-instance 'ini-definition-setting
      :name name :setting-type setting-type :default-value default-value))
)

(defmethod get-setting ((obj ini-definition) name)
  (setf name (misc-utils:to-keyword name))
  (let ((setting (gethash name (settings obj))))
    (unless setting
      (error "Setting with name ~S doesn't exist." name))
    setting))

(defmethod get-setting-type ((obj ini-definition) name)
  (setf name (misc-utils:to-keyword name))
  (setting-type (get-setting obj name)))

(defmethod get-setting-default-value ((obj ini-definition) name)
  (setf name (misc-utils:to-keyword name))
  (default-value (get-setting obj name)))

;; Embedded Files ;;

(defmethod parse-files ((obj ini-definition) file-names)
  (loop for file-name in file-names
    do (add-file obj (misc-utils:to-keyword file-name))))

(defmethod add-file ((obj ini-definition) file-name)
  (setf file-name (misc-utils:to-keyword file-name))
  (setf (files obj) (cons file-name (files obj))))

(defmethod has-file ((obj ini-definition) file-name)
  (setf file-name (misc-utils:to-keyword file-name))
  (if (member file-name (files obj))
    t
    (error "File with name ~S isn't defined." file-name)))

;; Conversion / Validation ;;

(string-utils:define-matcher match-int-string "\\s*[0-9]+\\s*")
(string-utils:define-matcher match-float-string "\\s*[0-9]+\\.[0-9]+\\s*")

(defmethod convert-string ((obj ini-definition) name str)
  (setf name (misc-utils:to-keyword name))
  (let ((wanted-type (setting-type (get-setting obj name))))
    (case wanted-type
      (:string str)
      (:float
        (if (match-int-string str)
          (coerce (parse-integer str) 'float)
          (if (match-float-string str)
            (read-from-string str)
            (error "Setting ~A expects a float value, but ~S can't be converted." name str))))
      (:int
        (if (match-int-string str)
          (parse-integer str)
          (error "Setting ~A expects an integer value, but ~S can't be converted." name str)))
      (:form (read-from-string str))
      (otherwise (error "Type is unknown: ~A, for name: ~A" wanted-type name)))))

(defmethod validate-value ((obj ini-definition) name value)
  (setf name (misc-utils:to-keyword name))
  (let ((wanted-type (setting-type (get-setting obj name))))
    (unless
      (case wanted-type
        (:string (stringp value))
        (:float (floatp value))
        (:int (integerp value))
        (:form t)
        (otherwise (error "Type is unknown: ~A, for name: ~A" wanted-type name)))
      (error "Setting: ~A. Expected type: ~A. Instead got type: ~A. Value: ~A." name wanted-type (type-of value) value))))
