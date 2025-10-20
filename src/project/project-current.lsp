
(globals:standard-package :project-current open save save-as ensure-project-saved)

(defparameter *current-project* nil)

(defun reset-project ()
  (setf *current-project* nil)
  (ensure-project))

(defun ensure-project ()
  (if (eq *current-project* nil)
    (setf *current-project* (project-data:create-project))))

(defun load-current (file)
  (ensure-project)
  (project-data:load-from-file *current-project* file))

(defun save-current (file)
  (ensure-project)
  (project-data:save-to-file *current-project* file))

(defparameter dialog-file-type '("DM Project File" ("dmproj")))

(defun load-dialog ()
  (let ((f (apply #'swing-dialogs:choose-file dialog-file-type)))
    (if (not (eq f nil))
      (load-current f))))

(defun save-dialog ()
  (let ((f (apply #'swing-dialogs:choose-file (append dialog-file-type '(:is-save t)))))
    (if (not (eq f nil))
      (save-current f))))

(defun open ()
  (load-dialog))

(defun save ()
  (ensure-project)
  (let ((f (data-utils:get-value (project-data:get-current-file *current-project*))))
    (if (eq f nil)
      (save-dialog)
      (project-data:save-to-file f))))

(defun save-as ()
  (save-dialog))

(defun ensure-project-saved ()
  (globals:println "Not implemented: project-current:ensure-project-saved")
  t)
