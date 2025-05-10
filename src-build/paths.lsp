
(globals:standard-package :build-paths :jdks-dir :jdk-path :jre-path :jmods-path)

(defun jdks-dir () (file-utils:jfile "." "build" "jdks"))

(defun jdk-path (platform)
  (case platform
    (:windows (file-utils:jfile (jdks-dir) "windows" "jdk-17.0.13+11"))
    (:macos-m-chip (file-utils:jfile (jdks-dir) "macos-m-chip" "jdk-17.0.13+11"))
    (:macos-intel (file-utils:jfile (jdks-dir) "macos-intel" "jdk-17.0.13+11"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun jre-path (platform)
  (case platform
    (:windows (file-utils:jfile "." "build" "jre" "windows"))
    (:macos-m-chip (file-utils:jfile "." "build" "jre" "macos-m-chip"))
    (:macos-intel (file-utils:jfile "." "build" "jre" "macos-intel"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun jmods-path (platform)
  (let ((jdk (jdk-path platform)))
    (case platform
      (:windows (file-utils:jfile jdk "jmods"))
      (:macos-m-chip (file-utils:jfile jdk "Contents" "Home" "jmods"))
      (:macos-intel (file-utils:jfile jdk "Contents" "Home" "jmods"))
      (otherwise
        (error "Bad platform: ~S." platform)))))
