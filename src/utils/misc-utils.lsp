
(globals:standard-package :misc-utils
  :booleanp)

(defun booleanp (x)
  (or (eq x t) (eq x nil)))
