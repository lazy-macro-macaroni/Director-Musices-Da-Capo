
(globals:standard-package :score-select-built-in create-menu)

(defun open-score (file)
  (java-utils:jcheck-type file "java.io.File")
  (globals:println "Not Implemented: score-select:open-score"))

(defparameter *file-tree* nil)

(defun scan-files (file current-list)
  (java-utils:jcheck-type file "java.io.File")
  (check-type current-list list)

  (assert (jcall "exists" file) (file) "Input path doesn't exist: ~A" (file-utils:file-to-string file))

  (cond
    ((jcall "isFile" file) (setf current-list (nconc current-list (list file))))
    ((jcall "isDirectory" file)
      (let ((contents (jcall "listFiles" file))
            (dir-list (list (file-utils:file-name file))))
        (setf current-list (nconc current-list (list dir-list)))
        (loop
          for item across contents
          when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
          do
          (setf dir-list (scan-files item dir-list)))))
    (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string file))))

  current-list)

(defun ensure-file-tree ()
  (when (eq *file-tree* nil)
    (setf *file-tree* (scan-files (file-utils:jfile "." "dm" "scores") '()))))

(defun create-menu-items (menu tree is-top)
  (cond
    ((eq tree nil) nil)
    ((java-utils:jinstance-of tree "java.io.File")
      (swing-menu:item menu (file-utils:file-name-no-extension tree) (lambda () (open-score tree))))
    ((listp tree)
      (let ((sub-menu (if is-top menu (swing-menu:create-sub-menu menu (car tree)))))
        (loop for item in (cdr tree)
          do
          (create-menu-items sub-menu item nil))))))

(defun create-menu (menu)
  (ensure-file-tree)
  (create-menu-items (swing-menu:create-sub-menu menu "Open Built-In Score") (car *file-tree*) t))

; (defun create-list ()
;   (let ((list (jnew "javax.swing.JList")))
;     (jcall "setSelectionMode" list (jfield "javax.swing.ListSelectionModel" "SINGLE_SELECTION"))

;     (jcall "addListSelectionListener" editor-pane
;       (jinterface-implementation "javax.swing.event.ListSelectionListener" "valueChanged"
;         (globals:safe-lambda "List Selection Listener" (e)

;         )))

;     (jnew "javax.swing.JScrollPane" list)))

; (defun open-dialog ()
;   (let* ((dialog (jnew "javax.swing.JDialog" (globals:get-main-window) "Select Built-In Score"))
;          (left-list (jnew "javax.swing.JList"))
;          (right-list (jnew "javax.swing.JList"))
;          (split-pane (jnew "javax.swing.JSplitPane"
;                        (jfield "javax.swing.JSplitPane" "HORIZONTAL_SPLIT")
;                          (jnew "javax.swing.JScrollPane" left-list) right-list)))
;     (jcall "add" dialog split-pane)
;     (jcall "setSize" dialog 600 400)
;     (jcall "setLocationRelativeTo" dialog (globals:get-main-window))

;     (swing-threads:invoke-later
;       (jcall "setDividerLocation" split-pane 0.3))

;     (jcall "show" dialog)))
