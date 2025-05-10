
(globals:standard-package :build-download download-windows-jdk download-macos-m-chip-jdk download-macos-intel-jdk download-all-jdks download-jdks)

(defvar *download-buffer-size* 1024)

(defun print-progress-bar ()
  (globals:println "~%Progress: |~A|" (apply #'concatenate 'string (make-list 100 :initial-element " ")))
  (globals:print     "           "))

;; DOWNLOAD

; output-path can be a java File object.
; From here: https://stackoverflow.com/a/22273319
(defun download (url output-path)
  (check-type url string)
  (java-utils:jcheck-type output-path "java.io.File")

  (when (not (jcall "isFile" output-path))
    (jcall "mkdirs" (jcall "getParentFile" output-path)))

  (print-progress-bar)

  (let* ((url (jcall "toURL" (jstatic "create" "java.net.URI" url)))
         (http-connection (jcall "openConnection" url))
         (file-size (jcall "getContentLength" http-connection))

         (in-stream (jnew "java.io.BufferedInputStream" (jcall "getInputStream" http-connection)))
         (file-stream (jnew "java.io.FileOutputStream" output-path))
         (out-stream (jnew "java.io.BufferedOutputStream" file-stream *download-buffer-size*))
         (data (jnew-array-from-list "byte" (make-list *download-buffer-size* :initial-element 0)))
         (downloaded-file-size 0)
         (previous-percent 0))
    (loop
      (let* ((read-length (jcall "read" in-stream data 0 *download-buffer-size*)))
        (if (< read-length 0) (return))
        (let* ((new-size (+ downloaded-file-size read-length))
               (new-progress (float (/ downloaded-file-size file-size)))
               (new-percent (round (* 100 new-progress))))
          (setf downloaded-file-size new-size)
          (if (> new-percent previous-percent)
            (progn (setf previous-percent new-percent) (Globals:print "^"))))
        (jcall "write" out-stream data 0 read-length)))
    (jcall "close" out-stream)
    (jcall "close" in-stream))
  (globals:println ""))

;; CHECKSUM

(defun check-sha256 (path checksum)
  (java-utils:jcheck-type path "java.io.File")
  (check-type checksum string)

  (globals:print "Checking checksum...")

  (let* ((digest (jstatic "getInstance" "java.security.MessageDigest" "SHA-256"))
         (file-stream (jnew "java.io.FileInputStream" path))
         (input-stream (jnew "java.io.BufferedInputStream" file-stream))
         (data (jnew-array-from-list "byte" (make-list *download-buffer-size* :initial-element 0))))
    (loop
      (let ((read-length (jcall "read" input-stream data 0 *download-buffer-size*)))
        (if (< read-length 0) (return))
        (jcall "update" digest data 0 read-length)))
    (let ((result (string= checksum (bytes-to-hex (jcall "digest" digest)))))
      (globals:println (if result "OK" "BAD"))
      result)))

(defun bytes-to-hex (bytes-array)
  ; (java-utils:jcheck-type bytes-array "java.lang.Array")

  (let ((sb (jnew "java.lang.StringBuilder")))
    (loop
      for i from 0 upto (- (jarray-length bytes-array) 1)
      for b = (jarray-ref-raw bytes-array i)
      do
      (jcall "append" sb (jstatic "format" "java.lang.String" "%02x"  (jnew-array-from-list "java.lang.Object" (list b)))))
    (jcall "toString" sb)))

;; UNZIP

; From here: https://stackoverflow.com/a/40050629

(defun unzip (file folder)
  (java-utils:jcheck-type file "java.io.File")
  (java-utils:jcheck-type folder "java.io.File")

  (globals:println "File = ~A~%Folder = ~A" (file-utils:file-to-string file) (file-utils:file-to-string folder))

  (print-progress-bar)

  (let* ((file-input (jnew "java.io.FileInputStream" (jcall "getCanonicalFile" file)))
         (channel (jcall "getChannel" file-input))
         (zip-input (jnew "java.util.zip.ZipInputStream" (jnew "java.io.BufferedInputStream" file-input)))
         (data (jnew-array-from-list "byte" (make-list *download-buffer-size* :initial-element 0)))
         (length (jcall "length" file))
         (previous-percent 0))
    (loop
      for ze = (jcall "getNextEntry" zip-input)
      while (not (eq ze nil))
      for f = (jnew "java.io.File" (jcall "getCanonicalPath" folder) (jcall "getName" ze))
      do
      (if (jcall "isDirectory" ze)
        (jcall "mkdirs" f)
        (progn
          (jcall "mkdirs" (jcall "getParentFile" f))
          (let ((fos (jnew "java.io.BufferedOutputStream" (jnew "java.io.FileOutputStream" f))))
            (loop
              for bytes-read = (jcall "read" zip-input data)
              while (> bytes-read -1)
              for new-percent = (round (* 100 (float (/ (jcall "position" channel) length))))
              do
              (jcall "write" fos data 0 bytes-read)
              (if (> new-percent previous-percent)
                (progn (setf previous-percent new-percent) (Globals:print "^"))))
            (jcall "close" fos)))))
    (jcall "close" zip-input)
    (when (not (eq previous-percent 100))
      (error "Extracting didn't reach 100%")))

  (globals:println ""))

;; Extract tar.gz

(defun extract-tar-gz (file folder)
  (java-utils:jcheck-type file "java.io.File")
  (java-utils:jcheck-type folder "java.io.File")

  (globals:println "File = ~A~%Folder = ~A" (file-utils:file-to-string file) (file-utils:file-to-string folder))

  (print-progress-bar)

  (let* ((file-input (jnew "java.io.FileInputStream" (jcall "getCanonicalFile" file)))
         (gzip-stream (jnew "org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream" file-input))
         (tar-stream (jnew "org.apache.commons.compress.archivers.tar.TarArchiveInputStream" gzip-stream))
         (data (jnew-array-from-list "byte" (make-list *download-buffer-size* :initial-element 0)))
         (channel (jcall "getChannel" file-input))
         (length (jcall "length" file))
         (previous-percent 0))
    (loop
      for entry = (jcall "getNextEntry" tar-stream)
      while (not (eq entry nil))
      when (not (jcall "isDirectory" entry))
      do

      ; (globals:println "Entry path = ~A" (file-utils:file-to-string (file-utils:jfile folder (jcall "getName" entry))))
      (let ((f (file-utils:jfile folder (jcall "getName" entry))))
        (jcall "mkdirs" (jcall "getParentFile" f))

        (let ((fos (jnew "java.io.BufferedOutputStream" (jnew "java.io.FileOutputStream" f))))
          (loop
            for bytes-read = (jcall "read" tar-stream data)
            while (> bytes-read -1)
            for new-percent = (round (* 100 (float (/ (jcall "position" channel) length))))
            do
            (jcall "write" fos data 0 bytes-read)
            (if (> new-percent previous-percent)
              (progn (setf previous-percent new-percent) (Globals:print "^"))))
          (jcall "close" fos))
        ))

    (globals:println "")

    (jcall "close" tar-stream)
    (when (not (eq previous-percent 100))
      (error "Extracting didn't reach 100%"))))

;; Download JDKs

; (defvar *jdks-dir* (file-utils:jfile "." "build" "jdks"))

(defun unzip-jdk (zip-file out-dir completed-file)
  (globals:println "Extracting...")

  (let ((file-name (file-utils:file-to-string zip-file)))
    (cond
      ((jcall "endsWith" file-name ".zip") (unzip zip-file out-dir))
      ((jcall "endsWith" file-name ".tar.gz") (extract-tar-gz zip-file out-dir))
      (t (error "Unknown file extension for file: ~A" file-name))))

  (jcall "mkdirs" (jcall "getParentFile" completed-file))
  (jcall "createNewFile" completed-file))

; url is download url. checksum is checksum :). zip-file is the resulting zip file. completed-file is an empty file that will be created when everything is completed.
(defun download-jdk (url checksum zip-file out-dir completed-file)
  (check-type url string)
  (check-type checksum string)
  (java-utils:jcheck-type zip-file "java.io.File")
  (java-utils:jcheck-type out-dir "java.io.File")
  (java-utils:jcheck-type completed-file "java.io.File")

  (when (jcall "isFile" completed-file)
    (globals:println "OK")
    (return-from download-jdk))

  (when (jcall "isFile" zip-file)
    (globals:print "ZIP...")
    (if (check-sha256 zip-file checksum)
      (progn
        (globals:println "OK")
        (unzip-jdk zip-file out-dir completed-file)
        (return-from download-jdk))
      (progn
        (globals:println "BAD"))))

  (let ((file (file-utils:jfile (build-paths:jdks-dir) "download.temp")))
    (globals:println "NO")
    (globals:println "Downloading...")
    (download url file)
    (jcall "renameTo" file zip-file)
    (if (check-sha256 zip-file checksum)
      (unzip-jdk zip-file out-dir completed-file)
      (progn
        (globals:println "Bad checksum after downloading. Program will exit.")
        (java-utils:exit)))))

(defun download-windows-jdk ()
  (Globals:print "Windows JDK...")
  (download-jdk
    "https://api.adoptium.net/v3/binary/latest/17/ga/windows/x64/jdk/hotspot/normal/eclipse?project=jdk"
    "6b64255e1bd690b09a135d44ac6b0d6bd4490728a8bad81904941d2789d394c0"
    (file-utils:jfile (build-paths:jdks-dir) "windows-jdk-17.zip")
    (file-utils:jfile (build-paths:jdks-dir) "windows")
    (file-utils:jfile (build-paths:jdks-dir) "windows-jdk-17-completed.txt")))

(defun download-macos-m-chip-jdk ()
  (Globals:print "MacOS m-chip JDK...")
  (download-jdk
    "https://api.adoptium.net/v3/binary/latest/17/ga/mac/aarch64/jdk/hotspot/normal/eclipse?project=jdk"
    "d8b2f77f755d06e81a540834c5be22ed86f3c8a51a20396606c074303f8f9e2d"
    (file-utils:jfile (build-paths:jdks-dir) "macos-m-chip-jdk-17.tar.gz")
    (file-utils:jfile (build-paths:jdks-dir) "macos-m-chip")
    (file-utils:jfile (build-paths:jdks-dir) "macos-m-chip-jdk-17-completed.txt")))

(defun download-macos-intel-jdk ()
  (Globals:print "MacOS intel JDK...")
  (download-jdk
    "https://api.adoptium.net/v3/binary/latest/17/ga/mac/x64/jdk/hotspot/normal/eclipse?project=jdk"
    "840535070200a944a6b582d258ee84608bd25c9f2b5d1cdddb58dfadb019675a"
    (file-utils:jfile (build-paths:jdks-dir) "macos-intel-jdk-17.tar.gz")
    (file-utils:jfile (build-paths:jdks-dir) "macos-intel")
    (file-utils:jfile (build-paths:jdks-dir) "macos-intel-jdk-17-completed.txt")))

(defun download-all-jdks ()
  (download-windows-jdk)
  (download-macos-m-chip-jdk)
  (download-macos-intel-jdk)
  (Globals:println "JDKs downloaded."))

(defun download-jdks (&rest platforms)
  (loop for platform in platforms
    do
    (case platform
      (:windows (download-windows-jdk))
      (:macos-m-chip (download-macos-m-chip-jdk))
      (:macos-intel (download-macos-intel-jdk))
      (otherwise
        (globals:println "Bad platform: ~S. Program will exit." platform)
        (java-utils:exit)))))
