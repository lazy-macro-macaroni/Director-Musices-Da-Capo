
; Load libraries that aren't supposed to be reloaded here.

; (load "src/lib/str.lsp")

; (let ((pkg (find-package :globals)))
;   (if pkg (delete-package pkg)))

(defpackage :globals
  (:use :cl :java)
  (:export
    format-string println print
    run-in-thread
    jfile
    jinstance-of jcheck-type
    invoke-and-wait
    handle-errors package standard-package del-package))

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

;; ERRORS

(defun print-trace-line (line)
  (if (string= line "(SYSTEM:BACKTRACE)")
    ""
    (format nil "    ~A~%" line)))

(defmacro handle-errors (form &key success &key failure &key error-prefix)
  `(let ((filename "")
         (backtrace ""))
     (handler-case
       (handler-bind ((condition (lambda (c)
                                   (setf filename (if (fboundp 'get-current-file-name) (get-current-file-name) "Unknown"))
                                   (setf backtrace "")
                                   (loop for bt in (sys:backtrace) do (setf backtrace (concatenate 'string backtrace (print-trace-line (jcall "toLispString" bt))))))))
         (progn ,form ,success))
       (error (c)
         (jcall "print" (jfield "java.lang.System" "err") (format nil "~A~%Error: ~A~%  In file: ~A~%  Backtrace:~%~A" ,error-prefix c filename backtrace))
         ,failure))))

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
