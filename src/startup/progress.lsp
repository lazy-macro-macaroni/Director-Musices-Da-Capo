
(globals:standard-package :startup-progress :show-progress :hide-progress :set-indeterminate :set-progress)

(defclass progress ()
  ((dialog :accessor dialog)))

(defun make-progress ()
  (let* ((obj (make-instance 'progress)))
    obj))

(defun define-global ()
  (defvar cl::*dm-progress-instance*))

(defun get-progress-instance ()
  (define-global)
  (if (not cl::*dm-progress-instance*)
    (setf cl::*dm-progress-instance* (make-progress)))
  cl::*dm-progress-instance*)

(defun show-progress () )

(defun hide-progress () )

(defun set-indeterminate () )

(defun set-progress (progress) )
