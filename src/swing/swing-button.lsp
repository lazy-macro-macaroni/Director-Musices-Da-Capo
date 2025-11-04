
(globals:standard-package :swing-button
  create-button)

(defun create-button (label callback)
  (check-type label string)
  (check-type callback function)

  (let ((b (jnew "javax.swing.JButton" label)))

    (jcall "addActionListener" b
      (jinterface-implementation "java.awt.event.ActionListener" "actionPerformed"
        (globals:safe-lambda (globals:format-string "Button Action. Label: ~S" label) (e)
          (funcall callback b e))))

    b))
