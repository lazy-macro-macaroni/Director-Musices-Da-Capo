
(load "src/globals.lsp")
(load "src/version.lsp")
(load "src/utils/utils.lsp")

(load "src-build/paths.lsp")
(load "src-build/utils.lsp")
(load "src-build/download.lsp")
(load "src-build/bundle.lsp")
(load "src-build/build.lsp")

(load "src-test/test.lsp")

(globals:standard-package :build-main :main)

(defvar *build-option-min* 1)
(defvar *build-option-max* 8)

(defun run-program ()
  (load "src/init.lsp")
  (let ((init (find-symbol "INIT" (find-package 'init))))
    (funcall init)))

; (defun option-2 ()
;   (build-download:unzip (file-utils:jfile "." "testfile.zip") (file-utils:jfile "." "zip_out")))
  ; (Globals:println "Got sum: ~A"
  ;         (build-download:check-sha256
  ;           "testfile.zip" "6b64255e1bd690b09a135d44ac6b0d6bd4490728a8bad81904941d2789d394c0")))
            ;(build-download:download "https://api.adoptium.net/v3/binary/latest/17/ga/windows/x64/jdk/hotspot/normal/eclipse?project=jdk" "testfile.zip"))))

(defun clean-build ()
  (build-utils:delete-path (file-utils:jfile "." "build")))

(defun run-option2 (build-option)
  (case build-option
    (1 (run-program))
    (2 (build-build:build-all))
    (3 (clean-build))
    (4 (clean-build) (build-build:build-all))
    (5 (build-build:build-windows))
    (6 (build-build:build-macos-m-chip))
    (7 (build-build:build-macos-intel))
    (8 (test:main))
    (otherwise (globals:println "Unknown option: ~A" build-option))))

(defun run-option (build-option)
  (cond
    ((< build-option *build-option-min*) (globals:println "Build option number too small. Got ~A, expected at least ~A." build-option *build-option-min*))
    ((> build-option *build-option-max*) (globals:println "Build option number too big. Got ~A, expected at most ~A." build-option *build-option-max*))
    (t (run-option2 build-option))))

(defun main (build-option)
  ; (globals:handle-errors
  ;   (progn
  ;   ; (globals:println "Result = ~S" (jstatic "testLisp" "dm_java.Main"))
  ;   ; (build-utils:delete-path (file-utils:jfile "." "build" "java"))
  ;     ; (clean-build)
  ;     (build-build:build-all)
  ;   )
  ; )
  ; (return-from main)

  (globals:handle-errors
    (if (> build-option 0)
      (run-option build-option)
      (progn
        (format t
          (concatenate 'string
            "~%Director-Musices build script."
            (globals:format-string " Version = ~A" version:*dm-version*)
            "~%Options:"
            "~%"
            "~%  1. Run program."
            "~%  2. Build all platforms."
            "~%  3. Delete all build files."
            "~%  4. Delete all build files and rebuild all platforms."
            "~%  5. Build for windows."
            "~%  6. Build for macos (m-chip)."
            "~%  7. Build for macos (intel)."
            "~%  8. Run tests."
            "~%"
            (format nil "~%Please enter option (~A - ~A): " *build-option-min* *build-option-max*)))
        (finish-output)
        (let ((value (read)))
          (if (integerp value)
            (run-option value)
            (progn
              (format t "Not an integer value: ~S~%" value)
              (format t "Program will exit.~%"))))))))
