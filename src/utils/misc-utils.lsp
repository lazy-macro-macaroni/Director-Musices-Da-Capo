
(globals:standard-package :misc-utils
  :boolean-p :to-keyword)

(defun boolean-p (x)
  (or (eq x t) (eq x nil)))

(defun to-keyword (x)
  "Converts a string, symbol, or keyword to a keyword."
  (cond
    ((keywordp x) x) ; Already a keyword
    ((stringp x) (intern (string-upcase x) :keyword))
    ((symbolp x) (intern (string-upcase (symbol-name x)) :keyword))
    (t (error "Unsupported type: ~S" x))))
