
(defpackage :init
  (:use :cl :java)
  (:export :init))

(in-package :init)

(defun init ()
  (globals:handle-errors (load "src/startup/load-files.lsp"))

  (jstatic "hide" "dm_java.ProgressManager")
  (let ((frame (jnew "javax.swing.JFrame" "Test Frame"))
        (label (jnew "javax.swing.JLabel" "Start Success.")))

    (jcall "add" frame label)
    (jcall "setSize" frame 300 200)
    (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))
    (jcall "setLocationRelativeTo" frame +NULL+)
    (jcall "requestFocus" frame)
    (globals:println "Before")
    (jcall "setVisible" frame +TRUE+)
    (globals:println "After")
    (jcall "requestFocus" frame))
        ; JFrame frame = new JFrame("Test Frame");
        ; JLabel label = new JLabel("SUCCESS");
        ; frame.add(label);
        ; frame.setSize(300, 200);
        ; frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        ; frame.setLocationRelativeTo(null);
        ; frame.setVisible(true);
  ; (globals:invoke-and-wait
  ;   (jstatic "showMessageDialog" "javax.swing.JOptionPane" +NULL+ "Eggs are not supposed to be green."))
  ; (java-utils:exit)
  )
