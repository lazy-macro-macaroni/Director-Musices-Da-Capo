
(globals:standard-package :rulepalette-data
  rulepalette
  create-rulepalette
  get-name
  load-from-file
  save-to-file
  parse-legacy-rulepalette)

(defparameter *ini-def*
  (ini-definition:create
    "dm-rulepalette"
    '((:name :string "Unnamed"))
    '()))

(defclass rulepalette ()
  ((ini :accessor ini-a :initarg :ini)
   (name :accessor name-a :initform
    (data-value:create-data-value "rulepalette-name" "" 'string))
  ))

(defun create-rulepalette ()
  (let ((rp (make-instance 'rulepalette :ini (ini-file:create *ini-def*))))
    (data-value:connect-to-ini (name-a rp) (ini-a rp) :name)
    (data-value:add-listener (name-a rp)
      (lambda (value)
        (swing-threads:invoke-later (globals:println "Rulepalette name set to: ~S, ini name is: ~S" value (ini-file:get-setting (ini-a rp) :name)))))
    rp))

(defun get-name (rp)
  (check-type rp rulepalette)
  (data-value:get-value (name-a rp)))

(defun load-from-file (rp file)
  (check-type rp rulepalette)
  (file-utils:check-type-is-file file)
  (ini-file:read-ini-from-file (ini-a rp) file))

(defun save-to-file (rp file)
  (check-type rp rulepalette)
  (file-utils:check-type-is-file file)
  (ini-file:save-ini-to-file (ini-a rp) file))

(defun parse-legacy-rulepalette (rp file)
  (check-type rp rulepalette)
  (file-utils:check-type-is-file file)

  (data-value:set-value (name-a rp) (file-utils:file-name-no-extension file))
  (globals:println "Not Implemented: rulepalette-data:parse-legacy-rulepalette"))
