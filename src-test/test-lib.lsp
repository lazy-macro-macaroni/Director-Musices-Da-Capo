
(globals:standard-package :test-lib :main :test-fn :run-tests)

(define-condition test-failed (error) ())

(defmacro tests (&rest forms)
  `(block run-until-false
     ,@(loop for form in forms
             collect `(unless ,form
                        (format t "~%FAILED: ~S~%~%" ',form)
                        (error 'test-failed)
                        (return-from run-until-false nil)))))

(defmacro test-fn (fn-name &rest forms)
  `(progn
     (globals:println "  Testing function ~A" (string-utils:uppercase ,fn-name))
     (tests ,@forms)))

(defun run-test-file2 (file pkg-name)
  (handler-case
    (progn
      (load (file-utils:file-to-string file))
      t)
    (test-failed (e) nil)))

(defun run-test-file (file)
  (let* ((pkg-name (string-upcase (file-utils:file-name-no-extension file))))
    (globals:println "Testing file ~A" pkg-name)
    (globals:handle-errors
      (run-test-file2 file pkg-name)
      :error-prefix "  Failed!~%"
      )))

(defun run-tests ()
  (loop for file in (file-utils:list-files (file-utils:jfile "." "src-test"))
    for name = (file-utils:file-name file)
    when (not (string= "test.lsp" name))
    when (not (string= "test-lib.lsp" name))
    when (string-utils:ends-with-p name ".lsp")
    do (run-test-file file)))
