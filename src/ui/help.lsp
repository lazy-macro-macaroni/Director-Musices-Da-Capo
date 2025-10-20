(globals:standard-package :ui-help open-help)

(defun scan-nodes (node file)
  (file-utils:check-type-is-file file)

  (assert (jcall "exists" file) (file) "Input path doesn't exist: ~A" (file-utils:file-to-string file))

  (cond
    ((jcall "isFile" file)
      (swing-tree:add-child node (swing-tree:node (file-utils:file-name-no-extension file) (file-utils:file-to-string file))))
    ((jcall "isDirectory" file)
      (let ((folder-node (swing-tree:node (file-utils:file-name-no-extension file)))
            (contents (jcall "listFiles" file)))
        (if (= (length contents) 0) (return-from scan-nodes))
        (swing-tree:add-child node folder-node)
        (loop
          for item across contents
          when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
          do
          (scan-nodes folder-node item))))
    (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string file)))))

(defun create-tree-nodes ()
  (let* ((root-node (swing-tree:node "Root")))
    (scan-nodes root-node (file-utils:jfile "." "resources" "help"))

    (let ((new-root (jcall "getFirstChild" root-node)))
      (swing-tree:rename-node new-root "Director-Musices")
       ; Very important to remove the parent reference or EVERYTHING will get messed up! :D
      (jcall "removeFromParent" new-root)
      new-root)))

(defun open-help-file (tree editor-pane url)
  (let ((file (if (string-utils:starts-with-p url "/")
                (apply #'file-utils:jfile "." "resources" "help" (string-utils:split-string-on-all-char url #\/))
                (file-utils:jfile url))))

    (let ((node (swing-tree:find-node-by-id tree (file-utils:file-to-string file)))
            (selected (jcall "getLastSelectedPathComponent" tree)))
      (swing-tree:select-node tree node))))

(defun create-tree ()
  (let* ((tree (swing-tree:tree (create-tree-nodes)))
         (editor-pane (jnew "javax.swing.JEditorPane")))

    (swing-tree:add-selection-listener tree
      (lambda (id)
        (when (string-utils:ends-with-p id ".html")
          (jcall "setText" editor-pane (file-utils:read-from-file (file-utils:jfile id))))))

    (jcall "addHyperlinkListener" editor-pane
      (jinterface-implementation "javax.swing.event.HyperlinkListener" "hyperlinkUpdate"
        (globals:safe-lambda "Hyperlink Listener" (e)
          (when (jcall "equals" (jcall "getEventType" e) (jfield "javax.swing.event.HyperlinkEvent$EventType" "ACTIVATED"))
            (open-help-file tree editor-pane (jcall "getDescription" e))))))

    (jcall "setEditable" editor-pane nil)
    (jcall "setContentType" editor-pane "text/html")
    (jcall "setText" editor-pane "<h1>HELLORLD</h1> <p>Hell world!</p>")

    (open-help-file tree editor-pane "/Welcome.html")

    (cons (jnew "javax.swing.JScrollPane" tree) (jnew "javax.swing.JScrollPane" editor-pane))))

(defun open-help ()
  (let* ((frame (jnew "javax.swing.JDialog" (globals:get-main-window) "DM Help"))
         (tree (create-tree))
         (split-pane (jnew "javax.swing.JSplitPane" (jfield "javax.swing.JSplitPane" "HORIZONTAL_SPLIT") (car tree) (cdr tree)))
         (label (jnew "javax.swing.JLabel" "")))

    ; (swing-frame:set-icons frame
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help64.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help128.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help256.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help512.png"))

    (jcall "setOneTouchExpandable" split-pane nil)

    (jcall "add" frame split-pane)

    (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "DISPOSE_ON_CLOSE"))

    (window-calculate-window-size:set-window-size frame 0.3 0.4)

    (swing-threads:invoke-later
      (jcall "setDividerLocation" split-pane 0.3))

    (jcall "setVisible" frame +TRUE+)
    (jcall "requestFocus" frame)))
