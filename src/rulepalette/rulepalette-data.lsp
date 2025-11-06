
(globals:standard-package :rulepalette-data
  rulepalette
  create-rulepalette
  get-name
  load-from-file
  save-to-file)

(defparameter *ini-def*
  (ini-definition:create
    "dm-rulepalette"
    '((:name :string ""))
    '()))

(defclass rulepalette ()
  ((ini :accessor ini-a :initarg :ini)
   (name :accessor name-a :initform
    (data-value:create-data-value "rulepalette-name" "Unnamed" 'string))
  ))

(defun create-rulepalette ()
  (let ((rp (make-instance 'rulepalette :ini (ini-file:create *ini-def*))))
    (data-value:connect-to-ini (name-a rp) (ini-a rp) :name)
    (data-value:add-listener (name-a rp)
      (lambda (value)
        (globals:println "Name set to: ~S, ini name is: ~S" value (ini-file:get-setting (ini-a rp) :name))))
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
