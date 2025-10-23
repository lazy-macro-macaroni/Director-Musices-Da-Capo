
(globals:standard-package :java-utils
  :array-to-list list-to-jlist :jinstance-of :jcheck-type :exit :runnable :thread :run-in-thread get-classpath-separator)

;; TYPES ;;

(defmacro jinstance-of (object jclass-name &rest rest-jclass-names)
  `(or ,@(loop for name in (cons jclass-name rest-jclass-names) collect `(jcall "isInstance" (jclass ,name) ,object))))

(defmacro jcheck-type (object jclass-name &rest rest-jclass-names)
  `(when (not (jinstance-of ,object ,jclass-name ,@rest-jclass-names))
    (if (eq 0 (list-length ',rest-jclass-names))
      (error (globals:format-string "Object ~S is not of type: ~A" ',object ,jclass-name))
      (error (globals:format-string "Object ~S is not any of types: ~{~a~^, ~}" ',object (cons ,jclass-name ',rest-jclass-names))))))

;; CONVERSION ;;

(defun array-to-list (arr)
  (assert (jcall "isArray" (jcall "getClass" arr)) () "Argument is not array.")

  (loop for i from 0 upto (- (jarray-length arr) 1)
    for item = (jarray-ref arr i)
    collect item))

(defun list-to-jlist (type-name l)
  (check-type type-name string)
  (check-type l list)

  (jstatic "asList" "java.util.Arrays"
  (jnew-array-from-list type-name l))
  )

;; THREADS ;;

(defmacro thread (&rest forms)
  `(jnew "java.lang.Thread" (runnable ,@forms)))

(defmacro run-in-thread (&rest forms)
  `(let ((thr (thread ,@forms)))
    (jcall "start" thr)
    thr))

;; MISC ;;

(defun exit (&optional (exit-code 0))
  (jstatic "exit" "java.lang.System" exit-code))

(defmacro runnable (name &rest forms)
  `(jinterface-implementation "java.lang.Runnable" "run"
    (globals:safe-lambda ,(globals:format-string "[Runnable] ~A" name) () (progn ,@forms)))) ;  (handler-case (progn ,@forms) (condition (c) (format t "[CL-ERR] Error in ~A: ~A~%" ,error-name c))))))

(defun wait (seconds)
  (jstatic "sleep" "java.lang.Thread" (* seconds 1000)))

(defun get-classpath-separator ()
  (jfield "java.io.File" "pathSeparator"))
