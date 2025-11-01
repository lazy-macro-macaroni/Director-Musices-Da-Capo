
(globals:standard-package :data-value-common data-value-bad-type-error check-value-type)

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
