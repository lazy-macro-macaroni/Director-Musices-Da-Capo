
(globals:standard-package :build-bundle :bundle)

(defun get-platform-path-separator (platform)
  (case platform
    (:windows "\\")
    (otherwise "/")))

(defun get-platform-cp-separator (platform)
  (case platform
    (:windows ";")
    (otherwise ":")))

(defun java-command (platform)
  (let ((path-sep (get-platform-path-separator platform))
        (cp-sep (get-platform-cp-separator platform)))
    (concatenate 'string
      "-splash:resources/dm_splash.png "
      "-cp \""
      "." path-sep "lib" path-sep "abcl-1.9.2" path-sep "abcl.jar"
      cp-sep
      "." path-sep "lib" path-sep "abcl-1.9.2" path-sep "abcl-contrib.jar"
      cp-sep
      "." path-sep "java"
      "\" "
      "dm_java.Main --run")))

(defun get-out-path (platform)
  (case platform
    (:windows (file-utils:jfile "." "build" "windows"))
    (:macos-m-chip (file-utils:jfile "." "build" "macos-m-chip"))
    (:macos-intel (file-utils:jfile "." "build" "macos-intel"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun get-out-main (out-path platform)
  (case platform
    (:windows out-path)
    ((:macos-m-chip :macos-intel) (file-utils:jfile out-path "Director-Musices.app" "Contents" "MacOS"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun add-executable-windows (out-main)
  (build-utils:with-task "Copying exe"
    (build-utils:copy-path (file-utils:jfile "." "src-exe" "Director-Musices.exe") (file-utils:jfile out-main "Director-Musices.exe")))

  (build-utils:with-task "Creating java command file"
    (build-utils:write-to-file (file-utils:jfile out-main "resources" "java-command.txt") (java-command :windows))))

(defun add-executable-macos (out-main)
  (build-utils:with-task "Adding shell script"
    (let ((content (concatenate 'string
                     "#!/bin/sh" (string #\newline)
                     "cd \"$(dirname \"$0\")\"" (string #\newline)
                     "./jre/bin/java "
                     (java-command :macos-m-chip))))
      (build-utils:write-to-file (file-utils:jfile out-main "Director-Musices") content))))

(defun add-executable (platform out-main)
  (case platform
    (:windows (add-executable-windows out-main))
    ((:macos-m-chip :macos-intel) (add-executable-macos out-main))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun get-archive-file (platform)
  (let ((extension
          (case platform
            (:windows ".zip")
            ((:macos-m-chip :macos-intel) ".tar.gz")
            (otherwise
              (error "Bad platform: ~S." platform)))))
    (file-utils:jfile "." "build" (globals:format-string "Director-Musices-~A-~A~A" version:*dm-version* (string platform) extension))))

(defun bundle (platform)
  (let* ((out-path (get-out-path platform))
         (out-main (get-out-main out-path platform)))
    (when (file-utils:path-exists out-path)
      (build-utils:with-task "Deleting previous files"
        (build-utils:delete-path out-path)))

    (build-utils:with-task "Copying JRE"
      (build-utils:copy-path (build-paths:jre-path platform) (file-utils:jfile out-main "jre")))

    (build-utils:with-task "Copying DM"
      (build-utils:copy-path (file-utils:jfile "." "dm") (file-utils:jfile out-main "dm")))

    (build-utils:with-task "Copying sources"
      (build-utils:copy-path (file-utils:jfile "." "src") (file-utils:jfile out-main "src")))

    (build-utils:with-task "Copying compiled java"
      (build-utils:copy-path (file-utils:jfile "." "build" "java") (file-utils:jfile out-main "java")))

    (build-utils:with-task "Copying resources"
      (build-utils:copy-path (file-utils:jfile "." "resources") (file-utils:jfile out-main "resources")))

    (build-utils:with-task "Copying libraries"
      (build-utils:copy-path (file-utils:jfile "." "lib") (file-utils:jfile out-main "lib")))

    (add-executable platform out-main)

    (let ((archive-file (get-archive-file platform)))
      (when (file-utils:path-exists archive-file)
        (build-utils:with-task "Deleting previous archive"
          (build-utils:delete-path archive-file)))
      (build-utils:with-task "Creating archive"
        ; (build-utils:zip out-path archive-file)
        (case platform
          (:windows (build-utils:zip out-path archive-file))
          ((:macos-m-chip :macos-intel) (build-utils:tar-gz out-path archive-file :process-file
           (lambda (path file entry)
            (jcall "setMode" entry 6)
            (when (or (string= path "Director-Musices.app/Contents/MacOS/jre/bin/java")
                      (string= path "Director-Musices.app/Contents/MacOS/Director-Musices"))
              (globals:println "Adding exe permissions for path = ~A" path)
              (jcall "setMode" entry 7))
           )
           )))
           ))
    ))
