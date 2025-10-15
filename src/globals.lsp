
; Load libraries that aren't supposed to be reloaded here.

; (load "src/lib/str.lsp")

; (let ((pkg (find-package :globals)))
;   (if pkg (delete-package pkg)))

(defpackage :globals
  (:use :cl :java)
  (:export
    format-string println print
    get-main-window set-main-window
    run-in-thread
    jfile
    jinstance-of jcheck-type
    handle-errors safe-lambda package standard-package del-package
    run-fn))

(in-package :globals)

;; PRINTING

(defmacro format-string (&rest forms)
  `(format nil ,@forms))

(defmacro println (&rest forms)
  `(let ((s (format nil ,@forms)))
    (format t (concatenate 'string s "~%"))
    s))

(defmacro print (&rest forms)
  `(let ((s (format nil ,@forms)))
    (format t s)
    (finish-output) ; flush output. Necessary when not printing newline
    s))

;; MAIN WINDOW ;;

(defparameter *main-window* nil)

(defun get-main-window ()
  *main-window*)

(defun set-main-window (window)
  (setf *main-window* window))

;; ERRORS

(defun print-trace-line (line)
  (if (string= line "(SYSTEM:BACKTRACE)")
    ""
    (format nil "    ~A~%" line)))

(defmacro handle-errors (form &key success &key failure &key error-prefix &key callback-name)
  `(let ((filename "")
         (backtrace ""))
     (handler-case
       (handler-bind ((condition (lambda (c)
                                   (setf filename (if (fboundp 'get-current-file-name) (get-current-file-name) "Unknown"))
                                   (setf backtrace "")
                                   (loop for bt in (sys:backtrace) do (setf backtrace (concatenate 'string backtrace (print-trace-line (jcall "toLispString" bt))))))))
         (progn ,form ,success))
       (error (c)
         (let* ((callback-message ,(if (not (eq callback-name nil)) (format-string "  Callback: ~A~%" callback-name) ""))
                (message (format-string "~AError: ~A~%  In file: ~A~%~A  Backtrace:~%~A" (format nil (or ,error-prefix "")) c filename callback-message backtrace)))
          (jcall "print" (jfield "java.lang.System" "err") message)
          (when (not (eq (get-main-window) nil))
            (jstatic "ShowError" "dm_java.ErrorDialog" "Error!" "Unknown Error" "There was an unknown error. See details below" message)))
         ,failure))))

(defmacro safe-lambda (name args &rest forms)
  (check-type name string)
  (check-type args list)

  `(lambda ,args
    (handle-errors (progn ,@forms) :callback-name ,name)))

;; PACKAGES

(defun del-package (name)
  (let ((pkg (find-package name)))
    (if pkg (delete-package pkg))))

(defmacro package (name &rest forms)
  `(progn
    (del-package ,name)
    (defpackage ,name ,@forms)
    (in-package ,name)
    (defun get-current-file-name () ,*LOAD-TRUENAME*)))

(defmacro standard-package (name &rest exports)
  `(package ,name (:use :cl :java) (:export ,@exports)))

(defun run-fn (pkg-name fn-name &rest args)
  "Looks up function FN-NAME in package PKG-NAME, then runs it."
  (check-type pkg-name string)
  (check-type fn-name string)

  (let ((pkg (find-package (jcall "toUpperCase" pkg-name))))
    (when (not pkg)
      (error (format-string "Couldn't find package: ~S")))
    (let ((func (intern (jcall "toUpperCase" fn-name) pkg)))
      (when (not func)
        (error (format-string "Couldn't find function: ~S")))
      (apply #'funcall func args))))
