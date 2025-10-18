
(globals:standard-package :ini-definition
  ini-error ini-bad-file-type-error ini-bad-type-error ini-bad-setting-name-error ini-bad-file-name-error
  :create get-file-type validate-file-type :name :setting-type :default-value :settings :files
  :add-setting :get-setting :get-setting-keys :get-setting-type :get-setting-default-value
  :add-file :has-file :get-file-keys
  :parse-settings :string-to-value :value-to-string validate-name :validate-value)

;; Error Types ;;

(globals:custom-error ini-error)

(globals:custom-error ini-bad-file-type-error :parent ini-error)
(globals:custom-error ini-bad-type-error :parent ini-error)
(globals:custom-error ini-bad-setting-name-error :parent ini-error)
(globals:custom-error ini-bad-file-name-error :parent ini-error)

;; Ini Def ;;

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
  ((file-type :initarg :file-type)
   (settings :accessor settings :initform (make-hash-table))
   (files :accessor files :initform '())))

(defun create (file-type settings files)
  (check-type file-type string)
  (let ((def (make-instance 'ini-definition :file-type file-type)))
    (parse-settings def settings)
    (parse-files def files)
    def))

(defmethod get-file-type ((obj ini-definition))
  (slot-value obj 'file-type))

(defmethod validate-file-type ((obj ini-definition) file-type)
  (let ((def-file-type (get-file-type obj)))
    (unless (string= file-type def-file-type)
      (globals:throw-custom-error ini-bad-file-type-error (globals:format-string "Expected file type: ~S, but instead got: ~S." def-file-type file-type)))))

;; Settings ;;

(defmethod parse-settings ((obj ini-definition) settings)
  (loop for (setting setting-type default-value) in settings
    do (add-setting obj setting setting-type default-value)))

(defmethod add-setting ((obj ini-definition) name setting-type default-value)
  (setf name (misc-utils:to-keyword name))
  (when (not (eq nil (gethash name (settings obj))))
    (globals:throw-custom-error ini-bad-setting-name-error "Setting with name ~S already exists in ini file type ~S." name (slot-value obj 'file-type)))

  (setf
    (gethash name (settings obj))
    (make-instance 'ini-definition-setting
      :name name :setting-type setting-type :default-value default-value)))

(defmethod get-setting ((obj ini-definition) name)
  (setf name (misc-utils:to-keyword name))
  (let ((setting (gethash name (settings obj))))
    (when (eq nil setting)
      (globals:throw-custom-error ini-bad-setting-name-error "Setting with name ~S doesn't exist in ini file type ~S." name (slot-value obj 'file-type)))
    setting))

(defmethod get-setting-keys ((obj ini-definition))
  (let (keys)
    (maphash (lambda (key value) (push key keys)) (settings obj))
    keys))

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
    (globals:throw-custom-error ini-bad-file-name-error "Embedded file with name ~S isn't defined in ini file type ~S." file-name (slot-value obj 'file-type))))

(defmethod get-file-keys ((obj ini-definition))
  (files obj))

;; Conversion / Validation ;;

(string-utils:define-matcher match-int-string "\\s*[0-9]+\\s*")
(string-utils:define-matcher match-float-string "\\s*[0-9]+\\.[0-9]+\\s*")

(defmethod string-to-value ((obj ini-definition) name str)
  (setf name (misc-utils:to-keyword name))
  (let ((wanted-type (setting-type (get-setting obj name))))
    (case wanted-type
      (:string str)
      (:float
        (if (match-int-string str)
          (coerce (parse-integer str) 'float)
          (if (match-float-string str)
            (read-from-string str)
            (globals:throw-custom-error ini-bad-type-error "Setting ~S expects a float value, but ~S can't be converted." name str))))
      (:int
        (if (match-int-string str)
          (parse-integer str)
          (globals:throw-custom-error ini-bad-type-error "Setting ~S expects an integer value, but ~S can't be converted." name str)))
      (:form (read-from-string str))
      (otherwise (error "Type is unknown: ~S, for name: ~S" wanted-type name)))))

(defmethod value-to-string ((obj ini-definition) name value)
  (setf name (misc-utils:to-keyword name))
  (validate-value obj name value)

  (let ((wanted-type (setting-type (get-setting obj name))))
    (case wanted-type
      (:string
        (if (string-utils:contains-p value string-utils:*newline*)
          (globals:format-string "{{{~%~A~%}}}" value)
        value))
      (:float (globals:format-string "~S" value))
      (:int (globals:format-string "~S" value))
      (:form (globals:format-string "~S" value))
      (otherwise (error "Type is unknown: ~S, for name: ~S" wanted-type name)))))

(defmethod validate-name ((obj ini-definition) name)
  (get-setting obj name))

(defmethod validate-value ((obj ini-definition) name value)
  (setf name (misc-utils:to-keyword name))
  (let ((wanted-type (setting-type (get-setting obj name))))
    (unless
      (case wanted-type
        (:string (stringp value))
        (:float (floatp value))
        (:int (integerp value))
        (:form t)
        (otherwise (error "Type is unknown: ~S, for name: ~S" wanted-type name)))
      (globals:throw-custom-error ini-bad-type-error "Setting: ~S. Expected type: ~S. Instead got type: ~S. Value: ~S." name wanted-type (type-of value) value))))
