
(globals:standard-package :tab-manager
  create-tab-manager
  get-component
  add-extra-tab)

(defclass tab-manager ()
  ((component :accessor component-a :initform (swing-tabbed-pane:create-tabbed-pane))
   (list :initarg :list :accessor list-a)
   (comp-list :initarg :comp-list :accessor comp-list-a)
   (comp-ref :initform (make-hash-table :test #'eq) :accessor comp-ref-a)
   (get-title :accessor get-title-a :initarg :get-title)
   (create-view :accessor create-view-a :initarg :create-view)
   (extra-tabs :accessor extra-tabs-a :initform '())))

(defun update-tabs (tm)
  (check-type tm tab-manager)
  (let ((c (component-a tm)))
    (swing-tabbed-pane:remove-all-tabs c)
    (loop for item in (append (data-value-list:get-list (comp-list-a tm)) (extra-tabs-a tm))
      do (swing-tabbed-pane:add-tab c (nth 0 item) (nth 1 item)))))

(defun get-tab-component (tm value)
  (check-type tm tab-manager)

  (let* ((h (comp-ref-a tm))
         (comp (gethash h value)))

    (if (eq comp nil)
      (setf (gethash h value) (funcall (create-view-a tm) value)))

    (gethash h value)))

(defun make-comp-list (data-list get-title-function create-view-function)
  (data-value-list:mapped-data-value-list (globals:format-string "Tab Manager Components For List: ~A" (data-value-list:get-name data-list))
    'list
    data-list
    (globals:safe-lambda "Comp List" (value)
      (list (funcall get-title-function value) (funcall create-view-function value)))))

(defun create-tab-manager (data-list get-title-function create-view-function)
  (check-type data-list data-value-list:data-value-list)
  (check-type get-title-function function)
  (check-type create-view-function function)

  (let* ((comp-list (make-comp-list data-list get-title-function create-view-function))
         (tm (make-instance 'tab-manager :list data-list :get-title get-title-function :create-view create-view-function :comp-list comp-list))
         (c (component-a tm)))

    (data-value-list:add-listener comp-list
      (globals:safe-lambda "Tab Manager Tabs Updater"
        (update-type value)
        (update-tabs tm)))

    tm))

(defun get-component (tm)
  (check-type tm tab-manager)
  (component-a tm))

(defun add-extra-tab (tm title comp)
  (check-type tm tab-manager)
  (check-type title string)
  (java-utils:jcheck-type comp "java.awt.Component")

  (setf (extra-tabs-a tm) (cons (list title comp) (extra-tabs-a tm)))
  (update-tabs tm))
