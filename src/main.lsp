
(globals:standard-package :main :main)

(defun main2 ()
  (let ((frame (swing-frame:create-frame "Director-Musices"))
        (label (jnew "javax.swing.JLabel" "")))

    (globals:set-main-window frame)

    (swing-frame:set-icons frame
      (file-utils:jfile "." "resources" "icon" "icon64.png")
      (file-utils:jfile "." "resources" "icon" "icon128.png")
      (file-utils:jfile "." "resources" "icon" "icon256.png")
      (file-utils:jfile "." "resources" "icon" "icon512.png"))

    (jcall "setJMenuBar" frame (ui-menu::create-menu))
    (jcall "add" frame label)

    (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))

    (window-calculate-window-size:set-window-size frame 0.5 0.7)

    (jstatic "hide" "dm_java.ProgressManager")
    (jcall "setVisible" frame +TRUE+)
    (jcall "requestFocus" frame))
  )

(defun main ()
  (when (eq (globals:handle-errors (main2) :failure :failed) :failed)
    (java-utils:exit 1)))
