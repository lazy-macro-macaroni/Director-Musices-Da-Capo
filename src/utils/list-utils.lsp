
(globals:standard-package :list-utils
  :flatten)

(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
    (t (cons (car lst) (flatten (cdr lst))))))
