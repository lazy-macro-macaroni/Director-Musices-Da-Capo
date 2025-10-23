
(globals:standard-package :swing-editor-pane
  create-editor-pane
  set-content-type
  set-editable
  set-text
  add-hyperlink-listener)

(defun create-editor-pane ()
  (jnew "javax.swing.JEditorPane"))

(defun set-content-type (pane content-type)
  (java-utils:jcheck-type pane "javax.swing.JEditorPane")

  (jcall "setContentType" pane
    (case content-type
      (:html "text/html")
      (:plain "text/plain")
      (:rtf "text/rtf")
      (otherwise
        (error "Value: ~S, is not a valid editor pane content type. Accepted values: :html, :plain, :rtf" content-type)))))

(defun set-editable (pane editable)
  (java-utils:jcheck-type pane "javax.swing.JEditorPane")

  (jcall "setEditable" pane (if editable +TRUE+ +FALSE+)))

(defun set-text (pane text)
  (java-utils:jcheck-type pane "javax.swing.JEditorPane")
  (check-type text string)

  (jcall "setText" pane text))

(defmacro add-hyperlink-listener (pane url-symbol &rest listener-forms)
  `(progn
    (java-utils:jcheck-type ,pane "javax.swing.JEditorPane")
    (jcall "addHyperlinkListener" ,pane
      (jinterface-implementation "javax.swing.event.HyperlinkListener" "hyperlinkUpdate"
        (globals:safe-lambda "Hyperlink Listener" (e)
          (when (jcall "equals" (jcall "getEventType" e) (jfield "javax.swing.event.HyperlinkEvent$EventType" "ACTIVATED"))
            (let ((,url-symbol (jcall "getDescription" e)))
              (progn ,@listener-forms))))))))
