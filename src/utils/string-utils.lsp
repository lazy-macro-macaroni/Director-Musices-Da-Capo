
(globals:standard-package :string-utils :*newline* :starts-with-p :ends-with-p :contains-p :alphanumeric-p :remove-suffix :split :split-string-on-char :uppercase :join-strings :regex-matcher :match :match-once :define-matcher)

(defparameter *newline* (globals:format-string "~%"))

(defun starts-with-p (str prefix)
  "Checks if STR starts with PREFIX."
  (check-type str string)
  (check-type prefix string)

  (let ((plen (length prefix)))
    (and (<= plen (length str))
         (string= prefix (subseq str 0 plen)))))

(defun ends-with-p (str suffix)
  "Checks if STR ends with SUFFIX."
  (check-type str string)
  (check-type suffix string)

  (let ((suffix-length (length suffix)))
    (and (>= (length str) suffix-length) ; Ensure str is long enough
         (string= (subseq str (- (length str) suffix-length)) suffix))))

(defun contains-p (string substring)
  "Return T if SUBSTRING is found in STRING, else NIL."
  (search substring string :test #'char-equal))

(defun alphanumeric-char-p (char)
  "Checks if a character is alphanumeric."
  (or (alpha-char-p char)
      (digit-char-p char)))

(defun alphanumeric-p (str)
  "Return T if STR contains only alphanumeric characters (A-Z, a-z, 0-9), NIL otherwise."
  (check-type str string)
  (every (lambda (ch) (alphanumeric-char-p ch)) str))

(defun remove-prefix (str prefix)
  "Removes PREFIX from STR if STR starts with PREFIX. Returns NIL if PREFIX wasn't in STR."
  (check-type str string)
  (check-type prefix string)

  (if (starts-with-p str prefix)
      (subseq str (length prefix))
      nil))

(defun remove-suffix (str suffix)
  "Remove SUFFIX from STR if STR ends with SUFFIX. Returns NIL if SUFFIX wasn't in STR."
  (check-type str string)
  (check-type suffix string)

  (if (ends-with-p str suffix)
      (subseq str 0 (- (length str) (length suffix)))
      nil))

(defun split (str delimiter)
  "Splits STR into a list of substrings using the first occurrence of DELIMITER."
  (let ((pos (position (char delimiter 0) str)))
    (if pos
        (list (subseq str 0 pos) (subseq str (1+ pos)))
        (list str))))

(defun split-string-on-char (str char &key from-end)
  "Splits STR at the first or last occurrence of CHAR.
If FROM-END is true, splits at the last occurrence of CHAR.
Returns a cons cell (car . cdr) where car is the part before CHAR
and cdr is the part after CHAR."
  (let* ((index (if from-end
                    (position char str :from-end t)
                    (position char str))))
    (if index
        (cons (subseq str 0 index)
              (subseq str (1+ index)))
        (cons str nil))))  ;; If CHAR not found, return entire string in car

(defun uppercase (str)
  "Converts STR to upper case."
  (check-type str string)
  (jcall "toUpperCase" str))

(defun join-strings (strings separator)
  "Joins a list of strings into a single string, with SEPARATOR between them"
  (let ((sep (if (characterp separator)
               (string separator)
               separator)))
    (with-output-to-string (out)
      (when strings
        (write-string (first strings) out)
        (dolist (s (rest strings))
          (write-string sep out)
          (write-string s out))))))

;; Regexes

(defclass regex-matcher ()
  ((matcher
    :accessor matcher
    :initarg :matcher))
)

(defun regex-matcher (regex)
  (make-instance 'regex-matcher :matcher (jstatic "compile" "java.util.regex.Pattern" regex)))

(defmethod match ((obj regex-matcher) str)
  (let ((match (jcall "matcher" (matcher obj) str)))
    (if (jcall "matches" match)
      (if (>= (jcall "groupCount" match) 1)
        (loop for x from 1 upto (jcall "groupCount" match)
          collect (jcall "group" match x))
        T))))

(defun match-once (regex str)
  "Creates a regex matcher and runs match, then discards the matcher."
  (match (regex-matcher regex) str))

(defmacro define-matcher (fn-name regex)
  "Creates a function with name FN-NAME, that will match a string to the REGEX.
It will also cache the regex matcher for quicker use."
  (let ((var-symbol (intern (globals:format-string "*~A-matcher*" fn-name))))
    `(progn
      (defparameter ,var-symbol nil)
      (defun ,fn-name (str)
        (check-type str string)
        (unless ,var-symbol (setf ,var-symbol (regex-matcher ,regex)))

        (match ,var-symbol str)))))
