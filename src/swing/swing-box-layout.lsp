
(globals:standard-package :swing-box-layout
  create-box-layout
  add
  add-glue
  add-spacer)

(defun get-axis (direction)
  (jfield "javax.swing.BoxLayout"
    (case direction
      (:horizontal "X_AXIS")
      (:vertical "Y_AXIS")
      (otherwise
        (error "Value: ~S, is not a valid box layout direction. Accepted values: :horizontal, :vertical." op)))))

(defun create-box-layout (direction)
  (let* ((panel (jnew "javax.swing.JPanel"))
         (layout (jnew "javax.swing.BoxLayout" panel (get-axis direction))))
    (jcall "setLayout" panel layout)
    panel))

(defun add (panel component)
  (java-utils:jcheck-type panel "javax.swing.JPanel")
  (java-utils:jcheck-type component "java.awt.Component")
  (jcall "add" panel component))

(defun get-direction (panel)
  (java-utils:jcheck-type panel "javax.swing.JPanel")

  (let ((layout (jcall "getLayout" panel)))
    (java-utils:jcheck-type layout "javax.swing.BoxLayout")
    (let ((axis (jcall "getAxis" layout)))
      (cond
        ((= axis (jfield "javax.swing.BoxLayout" "X_AXIS")) :horizontal)
        ((= axis (jfield "javax.swing.BoxLayout" "Y_AXIS")) :vertical)
        (otherwise
          (error "Got unexpected box layout axis, value: ~S." op))))))

(defun add-glue (panel direction)
  (java-utils:jcheck-type panel "javax.swing.JPanel")

  (add panel
    (case direction
      (:horizontal (jstatic "createHorizontalGlue" "javax.swing.Box"))
      (:vertical (jstatic "createVerticalGlue" "javax.swing.Box"))
      (otherwise
        (error "Value: ~S, is not a valid box layout direction. Accepted values: :horizontal, :vertical." op)))))

(defun add-spacer (panel size)
  (check-type size integer)

  (add panel
    (case (get-direction panel)
      (:horizontal (jstatic "createHorizontalStrut" "javax.swing.Box" size))
      (:vertical (jstatic "createVerticalStrut" "javax.swing.Box" size)))))
