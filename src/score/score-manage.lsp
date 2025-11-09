
(globals:standard-package :score-manage
  new-from-file
  open-score-dialog
  import-midi-dialog
  export-score-dialog)

(defun new-from-file (file)
  (file-utils:check-type-is-file file)

  (let ((score (score-data:create-score))
        (first-line (string-utils:trim (file-utils:read-first-line-from-file file))))

    (if (string= "INIFILE" first-line)
      (score-data:load-from-file score file)
      (score-data:parse-legacy-score score file))

    (project-data:add-score (project-current:get-project) score)
    score))

(defun open-score-dialog ()
  (let ((f (swing-dialogs:choose-file "Score Files" '("mus"))))
    (when (not (eq f nil))
      (new-from-file f))))

(defun import-midi-dialog ()
  (let ((f (swing-dialogs:choose-file "MIDI Files" '("mid" "midi"))))
    (when (not (eq f nil))
      (globals:println "Not Implemented: score-manage:import-midi-dialog"))))

(defun export-score-dialog ()
  (let ((f (swing-dialogs:choose-file "Score Files" '("mus") :is-save t)))
    (when (not (eq f nil))
      (globals:println "Not Implemented: score-manage:export-score-dialog"))))
