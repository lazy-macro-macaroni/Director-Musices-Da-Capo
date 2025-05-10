
(globals:standard-package :swing-utils
  :invoke-later :invoke-and-wait :choose-file)

;; MULTITHREADING ;;

(defmacro invoke-later (&rest forms)
  "Runs FORMS on the ui thread. Necessary when accessing the ui. Does not run instantly."
  `(jstatic "invokeLater" "javax.swing.SwingUtilities" (runnable "invoke-later" ,@forms)))

(defmacro invoke-and-wait (&rest forms)
  "Runs FORMS on the ui thread. Necessary when accessing the ui. Runs instantly and the current thread is blocked until completion."
  `(jstatic "invokeAndWait" "javax.swing.SwingUtilities" (runnable "invoke-and-wait" ,@forms)))

;; DIALOGS ;;

(defun choose-file (filetype-description filetypes)
  "FILETYPES is list of extensions. (e.g. \"jpg\")
   Returns java.io.File
   Example: (choose-file \"Image Files\" '(\"jpg\" \"gif\"))"
  (let* ((chooser (jnew "javax.swing.JFileChooser"))
         (combined-description (format nil "~A (.~{~A~^, ~})" filetype-description filetypes))
         (file-filter (jnew "javax.swing.filechooser.FileNameExtensionFilter" combined-description (java:jnew-array-from-list "java.lang.String" filetypes))))
    (jcall "setFileFilter" chooser file-filter)
    (if (eq (jcall "showOpenDialog" chooser JAVA:+NULL+) (jfield "javax.swing.JFileChooser" "APPROVE_OPTION"))
      (jcall "getSelectedFile" chooser)
      nil)))
