
(globals:standard-package :rulepalette-manage open-rulepalette-dialog export-rulepalette-dialog)

(defun open-rulepalette-dialog ()
  (let ((f (swing-dialogs:choose-file "Rulepalette Files" '("pal"))))
    (when (not (eq f nil))
      (globals:println "Not Implemented: rulepalette-manage:open-rulepalette-dialog"))))

(defun export-rulepalette-dialog ()
  (let ((f (swing-dialogs:choose-file "Rulepalette Files" '("pal") :is-save t)))
    (when (not (eq f nil))
      (globals:println "Not Implemented: rulepalette-manage:export-rulepalette-dialog"))))
