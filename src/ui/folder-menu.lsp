
(globals:standard-package :folder-menu create-tree create-menu)

(defclass folder-tree ()
  ((tree :initarg :tree :accessor get-tree)
   (callback :accessor get-callback)))

(defun scan-files (file current-list)
  (file-utils:check-type-is-file file)
  (check-type current-list list)

  (assert (jcall "exists" file) (file) "Input path doesn't exist: ~A" (file-utils:file-to-string file))

  (cond
    ((jcall "isFile" file) (setf current-list (nconc current-list (list file))))
    ((jcall "isDirectory" file)
      (let* ((contents1 (jcall "listFiles" file))
             (folders (loop for item across contents1 when (file-utils:file-is-dir-on-disk item) collect item))
             (files (loop for item across contents1 when (file-utils:file-is-file-on-disk item) collect item))
             (contents (concatenate 'list folders files))
             (dir-list (list (file-utils:file-name file))))
        (setf current-list (nconc current-list (list dir-list)))
        (loop
          for item in contents
          when (not (jstatic "isSymbolicLink" "java.nio.file.Files" (jcall "toPath" item)))
          do
          (setf dir-list (scan-files item dir-list)))))
    (t (globals:println "WARNING: Somehow path \"~A\" is not a directory or file." (file-utils:file-to-string file))))

  current-list)

(defun create-tree (file)
  (file-utils:check-type-is-file file)
  (make-instance 'folder-tree :tree (scan-files file '())))

(defmethod create-menu-items ((ftree folder-tree) menu current-tree is-top)
  (cond
    ((eq current-tree nil) nil)
    ((file-utils:filep current-tree)
      (swing-menu:item menu (file-utils:file-name-no-extension current-tree) (globals:safe-lambda "Folder Menu Item Callback" () (funcall (get-callback ftree) current-tree))))
    ((listp current-tree)
      (let ((sub-menu (if is-top menu (swing-menu:create-sub-menu menu (car current-tree)))))
        (loop for item in (cdr current-tree)
          do
          (create-menu-items ftree sub-menu item nil))))))

(defmethod create-menu ((ftree folder-tree) parent-menu title callback)
  (check-type title string)
  (check-type callback function)

  (setf (get-callback ftree) callback)

  (create-menu-items ftree (swing-menu:create-sub-menu parent-menu title) (car (get-tree ftree)) t))
