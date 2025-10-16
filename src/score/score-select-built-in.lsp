
(globals:standard-package :score-select-built-in open-dialog)

(defun create-list ()
  (let ((list (jnew "javax.swing.JList")))
    (jcall "setSelectionMode" list (jfield "javax.swing.ListSelectionModel" "SINGLE_SELECTION"))

    (jcall "addListSelectionListener" editor-pane
      (jinterface-implementation "javax.swing.event.ListSelectionListener" "valueChanged"
        (globals:safe-lambda "List Selection Listener" (e)

        )))

    (jnew "javax.swing.JScrollPane" list)))

(defun open-dialog ()
  (let* ((dialog (jnew "javax.swing.JDialog" (globals:get-main-window) "Select Built-In Score"))
         (left-list (jnew "javax.swing.JList"))
         (right-list (jnew "javax.swing.JList"))
         (split-pane (jnew "javax.swing.JSplitPane"
                       (jfield "javax.swing.JSplitPane" "HORIZONTAL_SPLIT")
                         (jnew "javax.swing.JScrollPane" left-list) right-list)))
    (jcall "add" dialog split-pane)
    (jcall "setSize" dialog 600 400)
    (jcall "setLocationRelativeTo" dialog (globals:get-main-window))

    (swing-threads:invoke-later
      (jcall "setDividerLocation" split-pane 0.3))

    (jcall "show" dialog)))
