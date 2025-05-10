
(globals:standard-package :java-utils
  :array-to-list :jinstance-of :jcheck-type :exit :runnable :thread :run-in-thread)

;; TYPES ;;

(defun jinstance-of (object jclass-name)
  (jcall "isInstance"  (jclass jclass-name) object))

(defmacro jcheck-type (object jclass-name)
  `(if (not (jinstance-of ,object ,jclass-name))
    (error (globals:format-string "Object ~S is not of type ~S" ',object ,jclass-name))))

;; CONVERSION ;;

(defun array-to-list (arr)
  (assert (jcall "isArray" (jcall "getClass" arr)) () "Argument is not array.")

  (loop for i from 0 upto (- (jarray-length arr) 1)
    for item = (jarray-ref arr i)
    collect item))

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

(defmacro runnable (error-name &rest forms)
  `(jinterface-implementation "java.lang.Runnable" "run" (lambda () (handle-errors ,@forms)))) ;  (handler-case (progn ,@forms) (condition (c) (format t "[CL-ERR] Error in ~A: ~A~%" ,error-name c))))))
