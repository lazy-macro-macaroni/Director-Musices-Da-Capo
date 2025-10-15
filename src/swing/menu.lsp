
(globals:standard-package :swing-menu menu separator item)

(defun menu (title)
  (check-type title string)

  (jnew "javax.swing.JMenu" title))

(defun separator (menu)
  (java-utils:jcheck-type menu "javax.swing.JMenu")

  (jcall "addSeparator" menu))

(defun item (menu title action)
  (java-utils:jcheck-type menu "javax.swing.JMenu")
  (check-type title string)
  (check-type action function)

  (let ((mitem (jnew "javax.swing.JMenuItem" title)))
    (jcall "addActionListener" mitem
      (jinterface-implementation
        "java.awt.event.ActionListener" "actionPerformed"
        (globals:safe-lambda "Menu Action Listener" (e)
          (funcall action))))
    (jcall "add" menu mitem)))
