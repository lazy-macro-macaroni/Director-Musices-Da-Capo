
(globals:standard-package :test-main :main)

(defun reload-files ()
  (load "src/startup/load-files.lsp")

  (globals:run-fn "load-files" "load-files"
    nil
    (globals:run-fn "load-files" "dm-files")
    (globals:run-fn "load-files" "src-files")
    (globals:run-fn "load-files" "test-files")
    (globals:run-fn "load-files" "build-files")))

(defun get-yn (message yes-default)
  (loop do
    (print "~A (y/n) [~A] " message (if yes-default "y" "n"))
    (let ((input (read-line)))
      (cond
        ((string= input "") (return (if yes-default t nil)))
        ((string= input "y") (return t))
        ((string= input "n") (return nil))
        (t (globals:println "Invalid input."))))))

(defun main ()
  (loop
    do
    (globals:println "~%Running tests.")

    (globals:handle-errors
      (when (reload-files)
        (globals:run-fn "test-lib" "run-tests")
      )
    )

    (if (not (get-yn (globals:format-string "~%Again?") t))
      (return))))
