
(globals:standard-package :swing-frame create-frame set-icons set-menu-bar set-close-operation set-visible request-focus)

(defun get-image (file)
  (file-utils:check-type-is-file file)
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

(defun set-menu-bar (frame menu)
  (java-utils:jcheck-type frame "javax.swing.JFrame")
  (java-utils:jcheck-type menu "javax.swing.JMenuBar")

  (jcall "setJMenuBar" frame menu))

(defun get-close-operation (op)
  (jfield "javax.swing.JFrame"
    (case op
      (:dispose "DISPOSE_ON_CLOSE")
      (:nothing "DO_NOTHING_ON_CLOSE")
      (:exit "EXIT_ON_CLOSE")
      (:hide "HIDE_ON_CLOSE")
      (otherwise
        (error "Value: ~S, is not a valid frame close operation. Accepted values: :dispose, :nothing, :exit, :hide." op)))))

(defun set-close-operation (frame op)
  (java-utils:jcheck-type frame "javax.swing.JFrame")
  (jcall "setDefaultCloseOperation" frame (get-close-operation op)))

(defun set-visible (frame visible)
  (java-utils:jcheck-type frame "javax.swing.JFrame")
  (jcall "setVisible" frame (if visible +TRUE+ +FALSE)))

(defun request-focus (frame)
  (java-utils:jcheck-type frame "javax.swing.JFrame")
  (jcall "requestFocus" frame))

(defun add (frame c)
  (java-utils:jcheck-type frame "javax.swing.JFrame")
  (java-utils:jcheck-type c "java.awt.Component")
  (jcall "add" frame c))
