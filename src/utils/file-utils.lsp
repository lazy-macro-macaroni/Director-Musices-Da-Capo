
(globals:standard-package :file-utils
  :jpath :jfile :path-exists :directory-exists :file-exists :file-to-string :file-is-same :file-is-parent :read-from-file :save-to-file)

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
  (java-utils:array-to-list (jcall-raw "toArray" (jstatic-raw "readAllLines" "java.nio.file.Files" (jpath file)))))

(defun save-to-file (file content)
  (jcheck-type file "java.io.File")
  (check-type content string)
  (jstatic "write" "java.nio.file.Files" (jpath file) (jcall-raw "getBytes" content) (jnew-array-from-list "java.nio.file.OpenOption" '())))
