

(defpackage :ui-glue-java
  (:use :cl :java)
  (:export :reload-lisp :get-main-frame :loading-failed :show-logs))

(in-package :ui-glue-java)

(defun reload-lisp () (jstatic "reloadLisp" "dm_java.CLManager"))

(defun get-main-frame () (jstatic "getMainFrame" "dm_java.CLManager"))

(defun show-logs () (jstatic "showLogs" "dm_java.CLManager"))
