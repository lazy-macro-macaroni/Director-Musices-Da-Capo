
(globals:standard-package :rulepalette-manage
  new-from-file
  open-rulepalette-dialog
  export-rulepalette-dialog)

(defun new-from-file (file)
  (file-utils:check-type-is-file file)
  (let ((rp (rulepalette-data:create-rulepalette)))
    (rulepalette-data:load-from-file rp file)
    (project-data:add-rulepalette (project-current:get-project) rp)
    rp))

(defun open-rulepalette-dialog ()
  (let ((f (swing-dialogs:choose-file "Rulepalette Files" '("pal"))))
    (when (not (eq f nil))
      (new-from-file f))))

(defun export-rulepalette-dialog ()
  (let ((f (swing-dialogs:choose-file "Rulepalette Files" '("pal") :is-save t)))
    (when (not (eq f nil))
      (globals:println "Not Implemented: rulepalette-manage:export-rulepalette-dialog"))))
