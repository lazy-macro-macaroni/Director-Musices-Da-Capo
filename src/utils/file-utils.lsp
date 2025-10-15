
(globals:standard-package :file-utils
  :jpath :jfile :path-exists :directory-exists :file-exists :file-to-string :file-is-same :file-is-parent :file-name
  :file-name-no-extension
  :read-from-file read-lines-from-file :save-to-file :list-files)

(defun jpath (path &rest paths)
  (let ((out (cond
               ((java-utils:jinstance-of path "java.io.File") (jcall "toPath" path))
               ((stringp path) (jstatic "get" "java.nio.file.Paths" path (jnew-array-from-list "java.lang.String" '())))
               (t (error "Bad type ~A" (type-of path))))))
    (loop for item in paths do (setf out (jcall "resolve" out item)))
    (jcall "normalize" (jcall "toAbsolutePath" out))))

(defun jfile (path &rest paths)
  (jcall "toFile" (apply #'jpath path paths)))

(defun path-exists (path)
  (java-utils:jcheck-type path "java.io.File")
  (jcall "exists" path))

(defun directory-exists (path)
  (java-utils:jcheck-type path "java.io.File")
  (jcall "isDirectory" path))

(defun file-exists (path)
  (java-utils:jcheck-type path "java.io.File")
  (jcall "isFile" path))

(defun file-to-string (path)
  (java-utils:jcheck-type path "java.io.File")
  (jcall "getPath" path))

(defun file-is-same (file1 file2)
  (java-utils:jcheck-type file1 "java.io.File")
  (java-utils:jcheck-type file2 "java.io.File")

  (jstatic "isSameFile" "java.nio.file.Files" (jcall "toPath" file1) (jcall "toPath" file2)))

(defun file-is-parent (parent child)
  (java-utils:jcheck-type parent "java.io.File")
  (java-utils:jcheck-type child "java.io.File")

  (jcall "startsWith" (jcall "toPath" child) (jcall "toPath" parent)))

(defun file-name (path)
  (java-utils:jcheck-type path "java.io.File")

  (jcall "getName" path))

(defun file-name-no-extension (path)
  "Returns the file name without the extension from PATH."
  (java-utils:jcheck-type path "java.io.File")

  (let* ((name (file-name path))
         (last-dot (search "." name :from-end t)) ; Find the position of the last dot
         (before (if last-dot (subseq name 0 last-dot) name))) ; Substring before the dot
    before))

;; File I/O ;;

(defun read-from-file (file)
  (java-utils:jcheck-type file "java.io.File")
  (jstatic "readString" "java.nio.file.Files" (jpath file)))

(defun read-lines-from-file (file)
  (java-utils:jcheck-type file "java.io.File")
  (java-utils:array-to-list (jcall-raw "toArray" (jstatic-raw "readAllLines" "java.nio.file.Files" (jpath file)))))

(defun save-to-file (file content)
  (java-utils:jcheck-type file "java.io.File")
  (check-type content string)
  (jstatic "write" "java.nio.file.Files" (jpath file) (jcall-raw "getBytes" content) (jnew-array-from-list "java.nio.file.OpenOption" '())))

;; Find Files ;;

(defun list-files2 (parent path)
  (java-utils:jcheck-type parent "java.io.File")
  (java-utils:jcheck-type path "java.io.File")

  (assert (jcall "exists" path) (path) "Path doesn't exist: ~A" (jcall "getPath" path))

  ; Some extra protection to not delete whole system or something
  (when (not (file-utils:file-is-parent parent path))
    (globals:println "File \"~A\" is not a child of directory \"~A\". Skipping." (file-utils:file-to-string path) (file-utils:file-to-string parent))
    (return-from list-files2))

  (cond
    ((jcall "isFile" path) (list path))
    ((jcall "isDirectory" path)
      (let ((contents (jcall "listFiles" path)))
        (if (= (length contents) 0) (return-from list-files2))
        (loop
          for item across contents
          when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
          collect (list-files2 parent item))))
    (t
      (globals:println "WARNING: Somehow path \"~A\" is not a directory or file" (file-utils:file-to-string path))
      nil)))

(defun list-files (path)
  (java-utils:jcheck-type path "java.io.File")

  (list-utils:flatten (list-files2 path path)))
