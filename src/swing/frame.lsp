
(globals:standard-package :swing-frame create-frame set-icons)

(defun get-image (file)
  (java-utils:jcheck-type file "java.io.File")
  (jcall "getImage"
    (jnew "javax.swing.ImageIcon" (file-utils:file-to-string file))))

(defun create-frame (title)
  (check-type title string)
  (jnew "javax.swing.JFrame" title))

(defun set-icons (frame &rest icon-files)
  (jstatic "JFrameSetIcons" "dm_java.ApiHelpers"
    frame
    (jnew-array-from-list "java.awt.Image"
      (loop for file in icon-files
        collect (get-image file)))))
