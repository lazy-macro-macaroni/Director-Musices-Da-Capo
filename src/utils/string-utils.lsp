
(globals:standard-package :string-utils :starts-with-p :ends-with-p :alphanumeric-p :remove-suffix :split :uppercase)

(defun starts-with-p (str prefix)
  "Checks if STR starts with PREFIX."
  (let ((plen (length prefix)))
    (and (<= plen (length str))
         (string= prefix (subseq str 0 plen)))))

(defun ends-with-p (str suffix)
  "Checks if STR ends with SUFFIX."
  (let ((suffix-length (length suffix)))
    (and (>= (length str) suffix-length) ; Ensure str is long enough
         (string= (subseq str (- (length str) suffix-length)) suffix))))

(defun alphanumeric-p (char)
  "Checks if a character is alphanumeric."
  (or (digit-char-p char)
      (and (char>= char #\A) (char<= char #\Z))
      (and (char>= char #\a) (char<= char #\z))))

(defun remove-suffix (string suffix)
  "Removes the SUFFIX from STRING, if it exists."
  (if (and (>= (length string) (length suffix))
           (string= (subseq string (- (length string) (length suffix))) suffix))
      (subseq string 0 (- (length string) (length suffix)))
      string))

(defun split (str delimiter)
  "Splits STR into a list of substrings using the first occurrence of DELIMITER."
  (let ((pos (position (char delimiter 0) str)))
    (if pos
        (list (subseq str 0 pos) (subseq str (1+ pos)))
        (list str))))

(defun uppercase (str)
  "Converts the STR to upper case."
  (check-type str string)
  (jcall "toUpperCase" str))
