
(globals:standard-package :ui-help open-help)

(defun scan-nodes (node file)
  (file-utils:check-type-is-file file)

  (assert (file-utils:file-exists-on-disk file) (file) "Input path doesn't exist: ~A" (file-utils:file-to-string file))

  (cond
    ((file-utils:file-is-file-on-disk file)
      (swing-tree:add-child node (swing-tree:node (file-utils:file-name-no-extension file) (file-utils:file-to-string file))))
    ((file-utils:file-is-dir-on-disk file)
      (let ((folder-node (swing-tree:node (file-utils:file-name-no-extension file)))
            (contents (file-utils:list-files file)))
        (if (= (length contents) 0) (return-from scan-nodes))
        (swing-tree:add-child node folder-node)
        (loop
          for item in contents
          when (not (file-utils:file-is-symlink-on-disk item))
          do
          (scan-nodes folder-node item))))
    (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string file)))))

(defun create-tree-nodes ()
  (let* ((root-node (swing-tree:node "Root")))
    (scan-nodes root-node (file-utils:jfile "." "resources" "help"))

    (let ((new-root (swing-tree:get-node-first-child root-node)))
      (swing-tree:rename-node new-root "Director-Musices")
       ; Very important to remove the parent reference or EVERYTHING will get messed up! :D
      (swing-tree:remove-node-parent new-root)
      new-root)))

(defun open-help-file (tree editor-pane url)
  (let* ((file (if (string-utils:starts-with-p url "/")
                (apply #'file-utils:jfile "." "resources" "help" (string-utils:split-string-on-all-char url #\/))
                (file-utils:jfile url)))
         (node (swing-tree:find-node-by-id tree (file-utils:file-to-string file))))
    (swing-tree:select-node tree node)))

(defun create-tree ()
  (let* ((tree (swing-tree:tree (create-tree-nodes)))
         (editor-pane (swing-editor-pane:create-editor-pane)))

    (swing-tree:add-selection-listener tree
      (lambda (id)
        (when (string-utils:ends-with-p id ".html")
          (swing-editor-pane:set-text editor-pane (file-utils:read-from-file (file-utils:jfile id))))))

    (swing-editor-pane:add-hyperlink-listener editor-pane url
      (open-help-file tree editor-pane url))

    (swing-editor-pane:set-editable editor-pane nil)
    (swing-editor-pane:set-content-type editor-pane :html)
    (swing-editor-pane:set-text editor-pane "NO TEXT")

    (open-help-file tree editor-pane "/Welcome.html")

    (cons (swing-scroll-pane:create-scroll-pane tree) (swing-scroll-pane:create-scroll-pane editor-pane))))

(defun open-help ()
  (let* ((frame (swing-frame:create-child-frame (globals:get-main-window) "DM Help"))
         (tree (create-tree))
         (split-pane (swing-split-pane:create-split-pane :horizontal (car tree) (cdr tree))))

    ; (swing-frame:set-icons frame
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help64.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help128.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help256.png")
    ;   (file-utils:jfile "." "resources" "icon_help" "icon_help512.png"))

    (swing-frame:add frame split-pane)
    (swing-frame:set-close-operation frame :dispose)
    (window-calculate-window-size:set-window-size frame 0.3 0.4)
    (swing-split-pane:set-divider-location split-pane 0.3)
    (swing-frame:set-visible frame t)))
