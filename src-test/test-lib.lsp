(globals:standard-package :test-lib :test-equal :tests :test-fn :test-class :test-method :test-macro test-no-errors test-expect-error :run-tests)

(define-condition test-failed (error) ())

(defmacro test-equal (form1 form2)
  `(let ((val1 ,form1)
         (val2 ,form2))
    (if (equal val1 val2)
      T
      (progn
        (format t "~%FAILED: Values not equal.~%  Form 1: ~S~%   Value: ~S~%  Form 2: ~S~%   Value: ~S~%" ',form1 val1 ',form2 val2)
        (error 'test-failed)))))

(defmacro test-no-error (&rest forms)
  `(progn ,@forms t))

(defmacro test-expect-error (error-type &rest forms))

(defmacro tests (&rest forms)
  `(block run-until-false
    ,@(loop for form in forms
            collect `(let ((val ,form))
                      (unless val
                        (format t "~%FAILED: Didn't return a truthful value.~%   Form: ~S~%  Value: ~S~%" ',form val)
                          (error 'test-failed))))
    t)) ; Return t for easier nesting

(defmacro test-with-prefix (prefix name &rest forms)
  `(progn
     (globals:println "  Testing ~A: ~A" ,prefix (string-utils:uppercase ,name))
     (tests ,@forms)))

(defmacro test-fn (fn-name &rest forms)
  `(test-with-prefix "function" ,fn-name ,@forms))

(defmacro test-class (fn-name &rest forms)
  `(test-with-prefix "class" ,fn-name ,@forms))

(defmacro test-method (fn-name &rest forms)
  `(test-with-prefix "method" ,fn-name ,@forms))

(defmacro test-macro (fn-name &rest forms)
  `(test-with-prefix "macro" ,fn-name ,@forms))

(defmacro test-no-errors (name &rest forms)
  `(progn
     (globals:println "  Testing for errors: ~A" ,name)
     (tests (progn ,@forms t))))

(defmacro test-expect-error (err &rest forms)
  `(handler-case
    (progn
      (globals:println "  Testing expecting error: ~A" ',err)
      ,@forms
      (globals:println "~%FAILED: Expected error ~S, but instead completed successfully." ',err)
      (error 'test-failed))
    (,err (e) t)))

(defun run-test-file2 (file pkg-name)
  (load (file-utils:file-to-string file)))

(defun run-test-file (file)
  (let* ((pkg-name (string-upcase (file-utils:file-name-no-extension file))))
    (globals:println "Testing file ~A" pkg-name)
    (run-test-file2 file pkg-name)))

(defun run-tests ()
  (handler-case
    (loop for file in (file-utils:list-files (file-utils:jfile "." "src-test"))
      for name = (file-utils:file-name file)
      when (string-utils:ends-with-p name ".lsp")
      when (not (string= "test-lib.lsp" name))
      when (not (string= "test-main.lsp" name))
      do (run-test-file file))
    (test-failed (e) nil)))
