
(globals:standard-package :main :main)

(defun main2 ()
  (jstatic "sleep" "java.lang.Thread" 200)
  (jstatic "hide" "dm_java.ProgressManager")
  (let ((frame (jnew "javax.swing.JFrame" "Director-Musices"))
        (label (jnew "javax.swing.JLabel" "")))

    (globals:set-main-window frame)

    (jcall "setJMenuBar" frame (ui-menu::create-menu))
    (jcall "add" frame label)

    (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))

    (window-calculate-window-size:set-window-size frame 0.5 0.7)

    (jcall "setVisible" frame +TRUE+)
    (jcall "requestFocus" frame))
  )

(defun main ()
  (if (eq (globals:handle-errors (main2) :failure :failed) :failed)
    (java-utils:exit 1)))
