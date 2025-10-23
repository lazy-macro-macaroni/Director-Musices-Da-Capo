
(globals:standard-package :swing-split-pane create-split-pane set-divider-location)

(defun get-direction (direction)
  (jfield "javax.swing.JSplitPane"
    (case direction
      (:horizontal "HORIZONTAL_SPLIT")
      (:vertical "VERTICAL_SPLIT")
      (otherwise
        (error "Value: ~S, is not a valid split pane direction. Accepted values: :horizontal, :vertical." op)))))

(defun create-split-pane (direction component1 component2)
  (java-utils:jcheck-type component1 "java.awt.Component")
  (java-utils:jcheck-type component2 "java.awt.Component")

  (let ((split-pane (jnew "javax.swing.JSplitPane" (get-direction direction) +TRUE+ component1 component2)))
    (jcall "setOneTouchExpandable" split-pane nil)
    split-pane))

(defun set-divider-location (split-pane location)
  (java-utils:jcheck-type split-pane "javax.swing.JSplitPane")

  ; Setting the location right after creating a split pane doesn't work,
  ; so we need invoke-later here.
  (swing-threads:invoke-later
    (jcall "setDividerLocation" split-pane location)))
