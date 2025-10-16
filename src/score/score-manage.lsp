
(globals:standard-package :score-manage open-score-dialog import-midi-dialog export-score-dialog)

(defun open-score-dialog ()
  (let ((f (swing-dialogs:choose-file "Score Files" '("mus"))))
    (when (not (eq f nil))
      (globals:println "Not Implemented: score-manage:open-score-dialog"))))

(defun import-midi-dialog ()
  (let ((f (swing-dialogs:choose-file "MIDI Files" '("mid" "midi"))))
    (when (not (eq f nil))
      (globals:println "Not Implemented: score-manage:import-midi-dialog"))))

(defun export-score-dialog ()
  (let ((f (swing-dialogs:choose-file "Score Files" '("mus") :is-save t)))
    (when (not (eq f nil))
      (globals:println "Not Implemented: score-manage:export-score-dialog"))))
