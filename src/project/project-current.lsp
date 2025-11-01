
(globals:standard-package :project-current
  get-project
  new
  open
  save
  save-as
  ensure-project-saved)

(defparameter *current-project* nil)

(defun reset-project ()
  (setf *current-project* nil)
  (ensure-project))

(defun ensure-project ()
  (if (eq *current-project* nil)
    (setf *current-project* (project-data:create-project))))

(defun get-project ()
  (ensure-project)
  *current-project*)

(defun load-current (file)
  (project-data:load-from-file (get-project) file))

(defun save-current (file)
  (project-data:save-to-file (get-project) file))

(defparameter dialog-file-type '("DM Project File" ("dmproj")))

(defun load-dialog ()
  (let ((f (apply #'swing-dialogs:choose-file dialog-file-type)))
    (if (not (eq f nil))
      (load-current f))))

(defun save-dialog ()
  (let ((f (apply #'swing-dialogs:choose-file (append dialog-file-type '(:is-save t)))))
    (if (eq f nil)
      (return-from save-dialog))

    (setf f (file-utils:ensure-file-ending f ".dmproj"))

    (if (file-utils:file-exists-on-disk f)
      (if (not (swing-dialogs:ok-cancel-dialog "File Exists!" "File already exists. Overwrite?"))
        (return-from save-dialog)))

    (save-current f)))

(defun new ()
  (project-data:reset-project (get-project)))

(defun open ()
  (load-dialog))

(defun save ()
  (let ((f (data-value:get-value (project-data:get-current-file (get-project)))))
    (if (eq f nil)
      (save-dialog)
      (save-current f))))

(defun save-as ()
  (save-dialog))

(defun ensure-project-saved ()
  (globals:println "Not implemented: project-current:ensure-project-saved")
  t)
