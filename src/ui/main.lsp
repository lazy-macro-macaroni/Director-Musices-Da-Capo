
(defpackage :main
  (:use :cl :java)
  (:export :start))

(in-package :main)

(defun combine-paths (first &rest paths)
  (jstatic "get" "java.nio.file.Paths" first (jnew-array-from-list "java.lang.String" paths)))

(defun load-score ()
  (let* (
         (path1 (combine-paths "." "dm" "scores" "polyphonic" "Bachsonat3.mus"))
        ;  (path1 (combine-paths "." "dm" "scores" "ekor.mus"))
         (path (jcall "normalize" (jcall "toAbsolutePath" path1)))
         (path-string (jcall "toString" path))
        ;  (content (jstatic "readString" "java.nio.file.Files" path))
        ; (path-string "D:\\Dropbox\\Programs\\DM\\dm\\scores\\ekor.mus")
         )
    (format t "Path = ~A~%" path-string)
    (ui-glue:load-active-score-from-file path-string)
    ))

(defun start2 ()
  (window-main-window:make-window))

  ; (format t "Window size = ~S~%" (window-calculate-window-size:set-window-size nil 0 0 0 0))


  ; (let* ((main-frame (jstatic "getMainFrame" "dm_java.CLManager"))
  ;        (panel (jnew "javax.swing.JPanel")))
  ;   (jcall "setJMenuBar" main-frame (ui-menu::create-menu))
  ;   (jcall "revalidate" main-frame)
  ;   (format t "HELLOOOO~%")
  ;   (load-score)))

(defun print-trace-line (line)
  (if (string= line "(SYSTEM:BACKTRACE)")
    ""
    (format nil "    ~A~%" line)))

(defun start ()
  (let ((filename "")
        (backtrace ""))
    (handler-case
      (handler-bind ((condition (lambda (c)
                                  (setq filename *LOAD-TRUENAME*)
                                  (setq backtrace "")
                                  (loop for bt in (sys:backtrace) do (setq backtrace (concatenate 'string backtrace (print-trace-line (jcall "toLispString" bt))))))))
        (start2))
      (error (c)
        (jcall "print" (jfield "java.lang.System" "err") (format nil "Error: ~A~%  In file: ~A~%  Backtrace:~%~A" c filename backtrace))))))
