
(globals:standard-package :swing-tree
  tree
  get-root-node
  node get-node-title get-node-id
  add-child add-selection-listener rename-node
  find-node-by-id select-node)

(defun tree (root-node)
  (java-utils:jcheck-type root-node "javax.swing.tree.DefaultMutableTreeNode")

  (let ((tree (jnew "javax.swing.JTree" root-node)))
    (jcall "setExpandsSelectedPaths" tree +TRUE+)
    (jcall "setSelectionMode" (jcall "getSelectionModel" tree) (jfield "javax.swing.tree.TreeSelectionModel" "SINGLE_TREE_SELECTION"))
    tree))

(defun get-root-node (tree)
  (java-utils:jcheck-type tree "javax.swing.JTree")
  (jcall "getRoot" (jcall "getModel" tree)))

(defun node (title &optional (id title)) ; Optional argument id which defaults to equal title
  (check-type title string)
  (check-type id string)

  (jnew "javax.swing.tree.DefaultMutableTreeNode" (jnew "dm_java.TreeNodeInfo" title id)))

(defun get-node-title (node)
  (java-utils:jcheck-type node "javax.swing.tree.DefaultMutableTreeNode")
  (jcall "getTitle" (jcall "getUserObject" node)))

(defun get-node-id (node)
  (java-utils:jcheck-type node "javax.swing.tree.DefaultMutableTreeNode")
  (jcall "getID" (jcall "getUserObject" node)))

(defun add-child (parent child)
  (java-utils:jcheck-type parent "javax.swing.tree.DefaultMutableTreeNode")
  (java-utils:jcheck-type child "javax.swing.tree.DefaultMutableTreeNode")

  (jcall "add" parent child))

(defun add-selection-listener (tree listener)
  (java-utils:jcheck-type tree "javax.swing.JTree")
  (check-type listener function)

  (let ((listener (jinterface-implementation "javax.swing.event.TreeSelectionListener" "valueChanged"
          (globals:safe-lambda "Tree Selection Listener" (e)
            (let ((node (jcall "getLastSelectedPathComponent" tree)))
              (when (not (eq node nil))
                (funcall listener (get-node-id node))))))))
    (jcall "addTreeSelectionListener" tree listener)))

(defun rename-node (node title &optional (id title)) ; Optional argument id which defaults to equal title
  (java-utils:jcheck-type node "javax.swing.tree.DefaultMutableTreeNode")
  (check-type title string)
  (check-type id string)

  (jcall "setUserObject" node (jnew "dm_java.TreeNodeInfo" title id)))

(defun find-node-by-id2 (root-node id)
  (java-utils:jcheck-type root-node "javax.swing.tree.DefaultMutableTreeNode")
  (check-type id string)

  (if (string= (get-node-id root-node) id)
    root-node

    (loop for i from 0 below (jcall "getChildCount" root-node)
      do
      (let ((node (find-node-by-id2 (jcall-raw "getChildAt" root-node i) id)))
        (when (not (eq node nil))
          (return node))))))

(defun find-node-by-id (tree id)
  (java-utils:jcheck-type tree "javax.swing.JTree")
  (check-type id string)

  (let ((e (jcall "depthFirstEnumeration" (get-root-node tree))))
    (loop while (jcall "hasMoreElements" e)
      do
      (let ((node (jcall "nextElement" e)))
        (when (string= (get-node-id node) id)
          (return node))))))

(defun select-node (tree node)
  (java-utils:jcheck-type tree "javax.swing.JTree")
  (java-utils:jcheck-type node "javax.swing.tree.DefaultMutableTreeNode")

  (let ((paths (jcall "getPath" node))
        (path (jnew "javax.swing.tree.TreePath" (jcall-raw "getPath" node))))
    (jcall "expandPath" tree path)
    (jcall "setSelectionPath" tree path)
    (jcall "scrollPathToVisible" tree path)))
