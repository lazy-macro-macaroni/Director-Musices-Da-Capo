
(defpackage :ui-swing
  (:use :cl :java)
  (:export :choose-file))

(in-package :ui-swing)

; filetypes is list of extensions. (e.g. "jpg")
; Returns java.io.File
; Example: (choose-file "Image Files" '("jpg" "gif"))
(defun choose-file (filetype-description filetypes)
  (let* ((chooser (jnew "javax.swing.JFileChooser"))
         (combined-description (format nil "~A (.~{~A~^, ~})" filetype-description filetypes))
         (file-filter (jnew "javax.swing.filechooser.FileNameExtensionFilter" combined-description (java:jnew-array-from-list "java.lang.String" filetypes))))
    (jcall "setFileFilter" chooser file-filter)
    (if (eq (jcall "showOpenDialog" chooser JAVA:+NULL+) (jfield "javax.swing.JFileChooser" "APPROVE_OPTION"))
      (jcall "getSelectedFile" chooser)
      nil)))

(defmacro runnable (error-name &rest forms)
  `(jinterface-implementation "java.lang.Runnable" "run" (lambda () (handler-case (progn ,@forms) (condition (c) (format t "[CL-ERR] Error in ~A: ~A~%" ,error-name c))))))

; (format t "Runnable: ~S~%" (macroexpand-1 '(runnable (+ 1 2) (+ 3 4))))

(defmacro invoke-later (&rest forms)
  `(jstatic "invokeLater" (runnable "invoke-later" ,@forms)))

(defmacro invoke-and-wait (&rest forms)
  `(jstatic "invokeAndWait" (runnable "invoke-later" ,@forms)))

(defmacro run-thread (thread-name &rest forms)
  `(jnew "java.lang.Thread" (runnable ,(string thread-name))))
