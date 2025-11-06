
(globals:standard-package :swing-tabbed-pane
  create-tabbed-pane
  add-tab
  insert-tab
  select-tab
  get-tab-count)

(defun create-tabbed-pane ()
  (jnew "javax.swing.JTabbedPane"))

(defun add-tab (tp title component)
  (java-utils:jcheck-type tp "javax.swing.JTabbedPane")
  (check-type title string)
  (java-utils:jcheck-type component "java.awt.Component")

  (jcall "addTab" tp title component))

(defun insert-tab (tp index title component &key tooltip)
  (java-utils:jcheck-type tp "javax.swing.JTabbedPane")
  (check-type index integer)
  (check-type title string)
  (java-utils:jcheck-type component "java.awt.Component")

  (jcall "insertTab" tp title +NULL+ component (if tooltip tooltip +NULL+) index))

(defun select-tab (tp index)
  (java-utils:jcheck-type tp "javax.swing.JTabbedPane")
  (check-type index integer)

  (jcall "setSelectedIndex" tp index))

(defun get-tab-count (tp)
  (java-utils:jcheck-type tp "javax.swing.JTabbedPane")
  (jcall "getTabCount" tp))
