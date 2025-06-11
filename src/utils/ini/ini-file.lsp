
(globals:standard-package :ini-file :create :get-setting :set-setting :close-file :read-ini-from-file :read-ini-from-string :save-ini-to-file :save-ini-to-string :set-files :get-files :add-file)

;; Matchers

(string-utils:define-matcher match-empty "\\s*")

(string-utils:define-matcher match-setting "\\s*([A-Za-z0-9\\-]+?)\\s*=\\s*(.*?)\\s*")

(string-utils:define-matcher match-multiline-start "\\s*\\{\\{\\{\\s*")
(string-utils:define-matcher match-multiline-end "\\s*\\}\\}\\}\\s*")

(string-utils:define-matcher match-file-start "(?i)====[ ]*FILE:[ ]*([A-Za-z0-9\\-]+)[ ]*====") ; (?i) means case insensitive in java regex
(string-utils:define-matcher match-file-end "(?i)====[ ]*ENDFILE:[ ]*([A-Za-z0-9\\-]+)[ ]*====")

;; Class

(defclass ini-file ()
  ((buffered-reader :initarg :buffered-reader :initform nil :accessor buffered-reader)
  ;  (line-number :initform 0 :accessor line-number)
  ;  (state :initform :settings :accessor state)

   (definition :initarg :definition :accessor definition)
   (settings :initform (make-hash-table) :accessor settings)
   (files :initform (make-hash-table) :accessor files))
)

(defun create (definition)
  (let ((obj (make-instance 'ini-file :definition definition)))
    ; Set default values
    (maphash
      (lambda (name setting)
        (setf (gethash name (settings obj)) t)
        (set-setting obj name (ini-definition:default-value setting)))
      (ini-definition:settings definition))

    (loop for file-name in (ini-definition:files definition)
      do (set-files obj file-name '()))

    obj))

;; Settings ;;

(defmethod get-setting ((obj ini-file) name)
  (setf name (misc-utils:to-keyword name))
  (let ((value (gethash name (settings obj))))
    (unless value
      (error "Error getting setting. Setting with name ~A doesn't exist." name))
    value))

(defmethod set-setting ((obj ini-file) name value)
  (setf name (misc-utils:to-keyword name))
  (unless (gethash name (settings obj))
    (error "Error setting setting. Setting with name ~A doesn't exist. Value: ~S" name value))
  (ini-definition:validate-value (definition obj) name value)
  (setf (gethash name (settings obj)) value))

;; Files ;;

(defmethod get-files ((obj ini-file) name)
  (setf name (misc-utils:to-keyword name))
  (gethash name (files obj)))

(defmethod set-files ((obj ini-file) name value)
  (setf name (misc-utils:to-keyword name))
  (setf (gethash name (files obj)) value))

(defmethod add-file ((obj ini-file) name value)
  (setf name (misc-utils:to-keyword name))
  (check-type value string)
  (ini-definition:has-file (definition obj) name)

  (set-files obj name (append (get-files obj name) (list value))))

;; File Reading

(defmethod next-line ((obj ini-file))
  (let ((result (jcall "readLine" (buffered-reader obj))))
    (if result
      (car (string-utils:split-string-on-char result #\; :from-end t)))))

(defmethod close-file ((obj ini-file))
  (jcall "close" (buffered-reader obj))
  (setf (buffered-reader obj) nil))

(defmethod read-ini-from-file ((obj ini-file) file)
  (java-utils:jcheck-type file "java.io.File")

  (setf (buffered-reader obj) (jnew "java.io.BufferedReader" (jnew "java.io.FileReader" file)))
  (unwind-protect
    (parse obj)
    (close-file obj)))

(defmethod read-ini-from-string ((obj ini-file) str)
  (check-type str string)

  (setf (buffered-reader obj) (jnew "java.io.BufferedReader" (jnew "java.io.StringReader" str)))
  (unwind-protect
    (parse obj)
    (close-file obj)))

(defmethod save-ini-to-file ((obj ini-file) file)
  (java-utils:jcheck-type file "java.io.File")

  (let ((file-writer (jnew "java.io.FileWriter" file)))
    (save obj file-writer)))

(defmethod save-ini-to-string ((obj ini-file))
  (let ((string-writer (jnew "java.io.StringWriter")))
    (save obj string-writer)
    (jcall "toString" string-writer)))

;; Parsing

(defmethod get-lines-until-match ((obj ini-file) matcher)
  (let ((out nil))
    (loop for line = (next-line obj)
      do
      (progn
        (unless line
          (error "Reached end of file reading multiline setting."))
        (if (funcall matcher line) (return))
        (if (eq out nil)
          (setf out line)
          (setf out (globals:format-string "~A~%~A" out line)))))
    out))

(defmethod parse-file ((obj ini-file) name)
  (setf name (misc-utils:to-keyword name))
  (let ((start-matcher (string-utils:regex-matcher "(?i)====[ ]*FILE:[ ]*([A-Za-z0-9\\-]+)[ ]*===="))
        (end-matcher ((string-utils:regex-matcher "(?i)====[ ]*ENDFILE:[ ]*([A-Za-z0-9\\-]+)[ ]*====")))
        (count 1)
        (out '()))
    (loop for line = (next-line obj)
          while line
          do
          (progn
            (let ((m (string-utils:match start-matcher line)))
              (if (and m (equalp (car m) name))
                (setf count (+ count 1))))

            (let ((m (string-utils:match end-matcher line)))
              (if (and m (equalp (car m) name))
                (setf count (- count 1))))

            (when (= count 0)
              (return (string-utils:join-strings out #\newline)))

            (setf out (append out '(line))))
          finally (error "Reached end of file while parsing file section with name: ~A" name))))

(defmethod parse-file-line ((obj ini-file) line)
  (let ((m (match-file-start line)))
    (when m
      (let ((name (car m))
            (out (get-lines-until-match obj
                  (lambda (str)
                    (let ((m2 (match-file-end str)))
                      (and m2 (string= (string-utils:uppercase (car m)) (string-utils:uppercase (car m2)))))))))
        (add-file obj name out))
      t)))

(defmethod parse-setting ((obj ini-file) name value-str)
  (setf name (misc-utils:to-keyword name))

  (when (match-multiline-start value-str)
    (setf value-str (get-lines-until-match obj #'match-multiline-end)))

  (set-setting obj name (ini-definition:string-to-value (definition obj) name value-str)))

(defmethod parse-setting-line ((obj ini-file) line)
  (let ((m (match-setting line)))
    (when m
      (parse-setting obj (car m) (car (cdr m)))
      t)))

(defmethod parse ((obj ini-file))
  (let ((first-line (next-line obj)))
    (when (eq first-line nil)
      (error "File is empty."))

    (unless (string= "INIFILE" first-line)
      (error "File is not INI file."))

    (loop for line = (next-line obj)
          while line
          when (not (match-empty line))
          do
          (or
            (parse-setting-line obj line)
            (parse-file-line obj line)
            (error "INI file contains unreadable line: ~S" line)))))
          ; (progn
          ;   (globals:println "Got line = ~S" line)
          ;   (let ((m (match-setting line)))
          ;     (if m
          ;       (parse-setting obj (car m) (car (cdr m)))))))

;; Saving

(defun write-line (writer line)
  (jcall "write" writer line)
  (jcall "newLine" writer))

(defmethod save-settings ((obj ini-file) writer)
  (loop for key in (ini-definition:get-setting-keys (definition obj))
    do
    (write-line writer (globals:format-string "~A=~A" key (ini-definition:value-to-string (definition obj) key (get-setting obj key))))
  ))

    ; (globals:println "Got key: ~S" key)))

(defmethod save-files ((obj ini-file) writer)
  (loop for key in (ini-definition:get-file-keys (definition obj))
    do
    (loop for file in (get-files obj key)
      do
      (write-line writer "")
      (write-line writer (globals:format-string "==== FILE: ~A ====" key))
      (write-line writer file)
      (write-line writer (globals:format-string "==== ENDFILE: ~A ====" key)))))

(defmethod save ((obj ini-file) in-writer)
  (let ((writer (jnew "java.io.BufferedWriter" in-writer)))
    (unwind-protect
      (progn
        (write-line writer "INIFILE")
        (write-line writer "")

        (save-settings obj writer)
        (save-files obj writer)

        (write-line writer ""))
      (progn
        (jcall "flush" writer)
        (jcall "close" writer)))))
