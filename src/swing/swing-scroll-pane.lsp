(globals:standard-package :swing-scroll-pane
  create-scroll-pane)

(defun create-scroll-pane (component)
  (java-utils:jcheck-type component "java.awt.Component")

  (jnew "javax.swing.JScrollPane" component))
