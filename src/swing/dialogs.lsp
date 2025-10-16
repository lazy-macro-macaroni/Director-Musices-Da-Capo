
(globals:standard-package :swing-dialogs choose-file ok-dialog error-dialog ok-cancel-dialog)

(defun choose-file (filetype-description filetypes &key is-save)
  "Shows an open file dialog by default. Set IS-SAVE to t to get a save dialog.
   FILETYPES is list of extensions. (e.g. \"jpg\")
   Returns java.io.File
   Example: (choose-file \"Image Files\" '(\"jpg\" \"gif\"))"
  (let* ((chooser (jnew "javax.swing.JFileChooser"))
         (combined-description (format nil "~A (.~{~A~^, ~})" filetype-description filetypes))
         (file-filter (jnew "javax.swing.filechooser.FileNameExtensionFilter" combined-description (java:jnew-array-from-list "java.lang.String" filetypes))))
    (jcall "setFileFilter" chooser file-filter)
    (if (eq (jcall (if is-save "showSaveDialog" "showOpenDialog") chooser JAVA:+NULL+) (jfield "javax.swing.JFileChooser" "APPROVE_OPTION"))
      (jcall "getSelectedFile" chooser)
      nil)))

(defun ok-dialog (message)
  (check-type message string)

  (jstatic "showMessageDialog" "javax.swing.JOptionPane" java:+null+ message)
  t)

(defun error-dialog (message)
  (check-type message string)

  (jstatic "showMessageDialog" "javax.swing.JOptionPane" java:+null+ message "ERROR" (jfield "javax.swing.JOptionPane" "ERROR_MESSAGE"))
  t)

(defun ok-cancel-dialog (message title)
  (check-type message string)
  (check-type title string)

  (let ((result (jstatic "showConfirmDialog" "javax.swing.JOptionPane" java:+null+ message "ERROR"
          (jfield "javax.swing.JOptionPane" "OK_CANCEL_OPTION"))))
    (= result 0)))
