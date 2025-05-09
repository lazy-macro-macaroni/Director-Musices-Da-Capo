
(globals:standard-package :build-build :build-windows :build-macos-intel :build-macos-m-chip :build-all)

; (defvar *build-version* "0.1")
; (defun run-command-to-string (&rest cmds)
;   (let* ((pb (jnew "java.lang.ProcessBuilder" (jnew-array-from-list "java.lang.String" cmds)))
;          (process (jcall "start" pb))
;          (br (jnew "java.io.BufferedReader" (jnew "java.io.InputStreamReader" (jcall "getInputStream" process))))
;          (builder (jnew "java.lang.StringBuilder")))
;     (loop
;       for line = (jcall "readLine" br)
;       while line
;       do
;       (jcall "append" builder line)
;       (jcall "append" builder (jstatic "getProperty" "java.lang.System" "line.separator")))
;     (jcall "toString" builder)))

(defun get-current-platform ()
  (let ((os-name (jcall "toLowerCase" (jstatic "getProperty" "java.lang.System" "os.name")))
        (os-arch (jcall "toLowerCase" (jstatic "getProperty" "java.lang.System" "os.arch"))))
  (cond
    ((jcall "startsWith" os-name "windows") :windows)
    ((jcall "startsWith" os-name "mac")
     (if (jcall "contains" os-arch "arm")
       :macos-m-chip
       :macos-intel))
    (t (error "Unknown platform.")))))

(defun get-java-command (command)
  (case (get-current-platform)
    (:windows (jinterop:jfile (build-paths:jdk-path :windows) "bin" (globals:format-string "~A.exe" command)))
    ((:macos-m-chip :macos-intel) (jinterop:jfile (build-paths:jdk-path :windows) "Home" "bin" command))
    (t (error "Unknown platform."))))

(defun run-java (java-command &rest arguments)
  (let* ((command-file (get-java-command java-command))
         (arguments2 (loop for arg in arguments collect (if (jinterop:jinstance-of arg "java.io.File") (jinterop:file-to-string arg) arg)))
         (arguments3 (cons (jinterop:file-to-string command-file) arguments2))
         (pb (jnew "java.lang.ProcessBuilder" (jnew-array-from-list "java.lang.String" arguments3))))
    (jcall "inheritIO" pb)
    (let ((result (jcall "waitFor" (jcall "start" pb))))
      (when (not (eq result 0))
        (Globals:println "Java command exited with non-zero result: ~A. See error messages above.~%Command: ~A" result arguments3)
        (jinterop:exit)))))

(defun build-java ()
  (globals:print "Building java...")
  (run-java "javac" "-cp"
    (concatenate 'string
      (jinterop:file-to-string (jinterop:jfile "." "lib" "abcl-1.9.2" "abcl.jar"))
      (build-utils:get-classpath-separator)
      (jinterop:file-to-string (jinterop:jfile "." "lib" "abcl-1.9.2" "abcl-contrib.jar")))
    "-d"
    (jinterop:jfile "." "build" "java")
    (concatenate 'string
      (jinterop:file-to-string (jinterop:jfile "." "src-java" "dm_java"))
      (build-utils:get-path-separator)
      "*.java"))
  (globals:println "OK"))

(defun create-jre (platform)
  (let ((jdk-path (build-paths:jdk-path platform))
        (out-path (build-paths:jre-path platform)))
    (when (jinterop:path-exists out-path)
      (build-utils:with-task "Deleting JRE"
        (build-utils:delete-path out-path)))

    (build-utils:with-task "Creating JRE"
      (run-java "jlink" "--compress" "2" "--strip-debug" "--no-header-files" "--no-man-pages"
        "--output" out-path
        "--module-path" (jinterop:file-to-string (build-paths:jmods-path platform))
        "--add-modules" "java.base,java.desktop"))))

(defun build (&rest platforms)
  (apply #'build-download:download-jdks (remove-duplicates (cons (get-current-platform) platforms) :test #'string=))
  (build-java)

  (loop
    for platform in platforms
    do
    (globals:println "~%Building for platform: ~A" platform)
    (create-jre platform)
    (build-bundle:bundle platform)))

(defun build-windows (&optional (is-build-all nil))
  (build :windows))

(defun build-macos-m-chip (&optional (is-build-all nil))
  (build :macos-m-chip))

(defun build-macos-intel (&optional (is-build-all nil))
  (build :macos-intel))

(defun build-all ()
  (build :windows :macos-m-chip :macos-intel))
