
(globals:standard-package :build-paths :jdks-dir :jdk-path :jre-path :jmods-path)

(defun jdks-dir () (jinterop:jfile "." "build" "jdks"))

(defun jdk-path (platform)
  (case platform
    (:windows (jinterop:jfile (jdks-dir) "windows" "jdk-17.0.13+11"))
    (:macos-m-chip (jinterop:jfile (jdks-dir) "macos-m-chip" "jdk-17.0.13+11"))
    (:macos-intel (jinterop:jfile (jdks-dir) "macos-intel" "jdk-17.0.13+11"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun jre-path (platform)
  (case platform
    (:windows (jinterop:jfile "." "build" "jre" "windows"))
    (:macos-m-chip (jinterop:jfile "." "build" "jre" "macos-m-chip"))
    (:macos-intel (jinterop:jfile "." "build" "jre" "macos-intel"))
    (otherwise
      (error "Bad platform: ~S." platform))))

(defun jmods-path (platform)
  (let ((jdk (jdk-path platform)))
    (case platform
      (:windows (jinterop:jfile jdk "jmods"))
      (:macos-m-chip (jinterop:jfile jdk "Contents" "Home" "jmods"))
      (:macos-intel (jinterop:jfile jdk "Contents" "Home" "jmods"))
      (otherwise
        (error "Bad platform: ~S." platform)))))
