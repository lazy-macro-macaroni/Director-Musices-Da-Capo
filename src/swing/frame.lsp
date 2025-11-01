
(globals:standard-package :swing-frame
  create-frame
  create-child-frame
  set-title
  set-icons
  set-menu-bar
  set-close-operation
  set-visible
  request-focus
  add)

(defmacro check-type-is-frame (obj)
  `(java-utils:jcheck-type ,obj "javax.swing.JFrame" "javax.swing.JDialog"))

(defun get-image (file)
  (file-utils:check-type-is-file file)
  (jcall "getImage"
    (jnew "javax.swing.ImageIcon" (file-utils:file-to-string file))))

(defun create-frame (title)
  (check-type title string)
  (jnew "javax.swing.JFrame" title))

(defun create-child-frame (parent title)
  (check-type-is-frame parent)
  (check-type title string)
  (jnew "javax.swing.JDialog" parent title))

(defun set-title (frame title)
  (check-type-is-frame frame)
  (check-type title string)
  (jcall "setTitle" frame title))

(defun set-icons (frame &rest icon-files)
  (jstatic "JFrameSetIcons" "dm_java.ApiHelpers"
    frame
    (jnew-array-from-list "java.awt.Image"
      (loop for file in icon-files
        collect (get-image file)))))

(defun set-menu-bar (frame menu)
  (check-type-is-frame frame)
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
  (check-type-is-frame frame)
  (jcall "setDefaultCloseOperation" frame (get-close-operation op)))

(defun set-visible (frame visible)
  (check-type-is-frame frame)
  (jcall "setVisible" frame (if visible +TRUE+ +FALSE)))

(defun request-focus (frame)
  (check-type-is-frame frame)
  (jcall "requestFocus" frame))

(defun add (frame c)
  (check-type-is-frame frame)
  (java-utils:jcheck-type c "java.awt.Component")
  (jcall "add" frame c))
