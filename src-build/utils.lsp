
(globals:standard-package :build-utils :get-classpath-separator :get-path-separator :delete-path :copy-path :zip :tar-gz :with-task :write-to-file)

(defun get-classpath-separator ()
  (jfield "java.io.File" "pathSeparator"))

(defun get-path-separator ()
  (jfield "java.io.File" "separator"))

;;
;; Deleting file/folders
;;

(defun delete-path2 (parent path)
  (java-utils:jcheck-type parent "java.io.File")
  (java-utils:jcheck-type path "java.io.File")

  (assert (jcall "exists" path) (path) "Path doesn't exist: ~A" (jcall "getPath" path))

  ; (when (not (jcall "exists" path))

  ; Some extra protection to not delete whole system or something
  (when (not (file-utils:file-is-parent parent path))
    (globals:println "File \"~A\" is not a child of directory \"~A\". Skipping deletion." (file-utils:file-to-string path) (file-utils:file-to-string parent))
    (return-from delete-path2))

  (cond
    ((jcall "isFile" path)
      ; (globals:println "Delete file: ~A" (jcall "getPath" path))
      (jcall "delete" path))
    ((jcall "isDirectory" path)
      ; (globals:println "Deleting directory: ~A" (jcall "getPath" path))
      (let ((contents (jcall "listFiles" path)))
        (if (not contents) (return-from delete-path2))
        (loop
          for item across contents ;in (coerce contents 'list)
          when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
          do
          (delete-path2 parent item)))
      (jcall "delete" path))
    (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file" (file-utils:file-to-string path)))))

(defun delete-path (path)
  (delete-path2 path path))

;;
;; Copy file/folder
;;

(defun copy-path2 (input output current)
  (java-utils:jcheck-type input "java.io.File")
  (java-utils:jcheck-type output "java.io.File")
  ; (java-utils:jcheck-type current "java.io.File")

  (assert (jcall "exists" input) (input) "Input path doesn't exist: ~A" (jcall "getPath" input))
  ; (assert (jcall "exists" current) (current) "Current path doesn't exist: ~A" (jcall "getPath" current))

  (let* ((input-file (apply #'file-utils:jfile input current))
         (output-file (apply #'file-utils:jfile output current)))

    ; Some extra protection to not copy whole system or something
    (when (not (file-utils:file-is-parent input input-file))
      (globals:println "File \"~A\" is not a child of directory \"~A\". Skipping copy." (file-utils:file-to-string input-file) (file-utils:file-to-string input))
      (return-from copy-path2))

    (cond
      ((jcall "isFile" input-file)
       (jcall "mkdirs" (jcall "getParentFile" output-file))
       (jstatic "copy" "java.nio.file.Files" (jcall "toPath" input-file) (jcall "toPath" output-file) (jnew-array-from-list "java.nio.file.CopyOption" '())))
      ((jcall "isDirectory" input-file)
        ; (globals:println "Deleting directory: ~A" (jcall "getPath" path))
        (let ((contents (jcall "listFiles" input-file)))
          (if (not contents) (return-from copy-path2))
          (loop
            for item across contents ;in (coerce contents 'list)
            when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
            do
            (copy-path2 input output (append current (list (jcall "getName" item)))))))
      (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string input-file))))))

(defun copy-path (input output)
  (copy-path2 input output '()))

;;
;; Zipping
;;

(defun zip-file (zip-stream input-file process-file current)
  (when process-file
    (funcall process-file input-file))

  (let* ((buffer-size 1024)
         (input-stream (jnew "java.io.FileInputStream" input-file))
         (zip-path (globals:format-string "~{~A~^/~}" current))
         (data (jnew-array-from-list "byte" (make-list buffer-size :initial-element 0)))
         )
    (jcall "putNextEntry" zip-stream (jnew "java.util.zip.ZipEntry" zip-path))

    (loop
      (let* ((read-length (jcall "read" input-stream data 0 buffer-size)))
        (if (< read-length 0) (return))
        (jcall "write" zip-stream data 0 read-length)))

    ; (globals:println "Add file ~A, to path: ~A" (file-utils:file-to-string input-file) zip-path)
    (jcall "closeEntry" zip-stream)
    (jcall "close" input-stream)))

(defun zip2 (input-folder zip-stream process-file current)
  (java-utils:jcheck-type input-folder "java.io.File")

  (assert (jcall "exists" input-folder) (input-folder) "Input path doesn't exist: ~A" (file-utils:file-to-string input-folder))

  (let* ((input-file (apply #'file-utils:jfile input-folder current)))
    ; Some extra protection to not zip whole system or something
    (when (not (file-utils:file-is-parent input-folder input-file))
      (globals:println "File \"~A\" is not a child of directory \"~A\". Skipping zipping." (file-utils:file-to-string input-file) (file-utils:file-to-string input-folder))
      (return-from zip2))

    (cond
      ((jcall "isFile" input-file)
       (zip-file zip-stream input-file process-file current))
      ((jcall "isDirectory" input-file)
        (let ((contents (jcall "listFiles" input-file)))
          (if (not contents) (return-from zip2))
          (loop
            for item across contents ;in (coerce contents 'list)
            when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
            do
            (zip2 input-folder zip-stream process-file (append current (list (jcall "getName" item)))))))
      (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string input-file))))))

(defun zip (input-folder output-file &key process-file)
  (java-utils:jcheck-type input-folder "java.io.File")
  (java-utils:jcheck-type output-file "java.io.File")

  (when (file-utils:directory-exists output-file)
    (error "Output file is directory: ~A" (file-utils:file-to-string output-file)))

  (let* ((file-stream (jnew "java.io.FileOutputStream" output-file))
         (zip-stream (jnew "java.util.zip.ZipOutputStream" file-stream)))
    (zip2 input-folder zip-stream process-file '())
    (jcall "close" zip-stream)))

;;
;; Compress tar.gz
;;

(defun tar-file (tar-stream input-file process-file current)
  (let* ((buffer-size 1024)
         (input-stream (jnew "java.io.FileInputStream" input-file))
         (path (globals:format-string "~{~A~^/~}" current))
         (data (jnew-array-from-list "byte" (make-list buffer-size :initial-element 0)))
         (entry (jnew "org.apache.commons.compress.archivers.tar.TarArchiveEntry" input-file path))
         )
    (when process-file
      (funcall process-file path input-file entry))

    ; (jcall "setSize" entry (jfield "length" input-file))
    (jcall "putArchiveEntry" tar-stream entry)

    (loop
      (let* ((read-length (jcall "read" input-stream data 0 buffer-size)))
        (if (< read-length 0) (return))
        (jcall "write" tar-stream data 0 read-length)))

    ; (globals:println "Add file ~A, to path: ~A" (file-utils:file-to-string input-file) zip-path)
    (jcall "closeArchiveEntry" tar-stream)
    (jcall "close" input-stream)))

(defun tar-gz2 (input-folder tar-stream process-file current)
  (java-utils:jcheck-type input-folder "java.io.File")

  (assert (jcall "exists" input-folder) (input-folder) "Input path doesn't exist: ~A" (file-utils:file-to-string input-folder))

  (let* ((input-file (apply #'file-utils:jfile input-folder current)))
    ; Some extra protection to not compress whole system or something
    (when (not (file-utils:file-is-parent input-folder input-file))
      (globals:println "File \"~A\" is not a child of directory \"~A\". Skipping zipping." (file-utils:file-to-string input-file) (file-utils:file-to-string input-folder))
      (return-from tar-gz2))

    (cond
      ((jcall "isFile" input-file)
       (tar-file tar-stream input-file process-file current))
      ((jcall "isDirectory" input-file)
        (let ((contents (jcall "listFiles" input-file)))
          (if (not contents) (return-from tar-gz2))
          (loop
            for item across contents ;in (coerce contents 'list)
            when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
            do
            (tar-gz2 input-folder tar-stream process-file (append current (list (jcall "getName" item)))))))
      (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string input-file))))))

(defun tar-gz (input-folder output-file &key process-file)
  (java-utils:jcheck-type input-folder "java.io.File")
  (java-utils:jcheck-type output-file "java.io.File")

  (when (file-utils:directory-exists output-file)
    (error "Output file is directory: ~A" (file-utils:file-to-string output-file)))

  (let* ((file-stream (jnew "java.io.FileOutputStream" output-file))
         (buf-stream (jnew "java.io.BufferedOutputStream" file-stream))
         (gz-stream (jnew "org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream" buf-stream))
         (tar-stream (jnew "org.apache.commons.compress.archivers.tar.TarArchiveOutputStream" gz-stream)))
    (tar-gz2 input-folder tar-stream process-file '())
    (jcall "close" tar-stream)))

;;
;; Util
;;

(defmacro with-task (task-text &rest forms)
  `(globals:handle-errors
    (progn
      (globals:print "~A..." ,task-text)
      ,@forms)
    :error-prefix (globals:format-string "FAIL~%")
    :success (globals:println "OK")
    :failure
    (java-utils:exit 1)))

(defun write-to-file (output-file text)
  (java-utils:jcheck-type output-file "java.io.File")

  (let ((print-writer (jnew "java.io.PrintWriter" (file-utils:file-to-string output-file) "UTF-8")))
    (jcall "print" print-writer text)
    (jcall "close" print-writer)))
