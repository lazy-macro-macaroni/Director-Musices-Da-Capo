
(globals:standard-package :jinterop
  jinstance-of jcheck-type exit jpath jfile path-exists directory-exists file-exists file-to-string file-to-string file-is-same file-is-parent array-to-list)

;; Java

(defun jinstance-of (object jclass-name)
  (jcall "isInstance"  (jclass jclass-name) object))

(defmacro jcheck-type (object jclass-name)
  `(if (not (jinstance-of ,object ,jclass-name))
    (error (globals:format-string "Object ~S is not of type ~S" ',object ,jclass-name))))

(defun exit (&optional (exit-code 0))
  (jstatic "exit" "java.lang.System" exit-code))

;; Files

(defun jpath (path &rest paths)
  (let ((out (cond
               ((jinstance-of path "java.io.File") (jcall "toPath" path))
               ((stringp path) (jstatic "get" "java.nio.file.Paths" path (jnew-array-from-list "java.lang.String" '())))
               (t (error "Bad type ~A" (type-of path))))))
    (loop for item in paths do (setf out (jcall "resolve" out item)))
    (jcall "normalize" (jcall "toAbsolutePath" out))))

(defun jfile (path &rest paths)
  (jcall "toFile" (apply #'jpath path paths)))

(defun path-exists (path)
  (jcheck-type path "java.io.File")
  (jcall "exists" path))

(defun directory-exists (path)
  (jcheck-type path "java.io.File")
  (jcall "isDirectory" path))

(defun file-exists (path)
  (jcheck-type path "java.io.File")
  (jcall "isFile" path))

(defun file-to-string (path)
  (jcheck-type path "java.io.File")
  (jcall "getPath" path))

(defun file-is-same (file1 file2)
  (jcheck-type file1 "java.io.File")
  (jcheck-type file2 "java.io.File")

  (jstatic "isSameFile" "java.nio.file.Files" (jcall "toPath" file1) (jcall "toPath" file2)))

(defun file-is-parent (parent child)
  (jcheck-type parent "java.io.File")
  (jcheck-type child "java.io.File")

  (jcall "startsWith" (jcall "toPath" child) (jcall "toPath" parent)))

;; File I/O

(defun read-from-file (file)
  (jcheck-type file "java.io.File")
  (array-to-list (jcall-raw "toArray" (jstatic-raw "readAllLines" "java.nio.file.Files" (jpath file)))))

(defun save-to-file (file content)
  (jcheck-type file "java.io.File")
  (check-type content string)
  (jstatic "write" "java.nio.file.Files" (jpath file) (jcall-raw "getBytes" content) (jnew-array-from-list "java.nio.file.OpenOption" '())))

;; buh

(defun array-to-list (arr)
  (assert (jcall "isArray" (jcall "getClass" arr)) () "Argument is not array.")

  (loop for i from 0 upto (- (jarray-length arr) 1)
    for item = (jarray-ref arr i)
    collect item))
