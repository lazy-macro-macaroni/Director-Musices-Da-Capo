
(globals:standard-package :test :main)

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
    (globals:println "Running tests.")
    (if (not (get-yn "Again?" t))
      (return))))
