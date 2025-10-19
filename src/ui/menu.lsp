
(globals:standard-package :ui-menu create-menu)

;; MENU FUNCTIONS ;;

(defun not-implemented ()
  (swing-dialogs:error-dialog "Not implemented!"))

; TODO: Grayed out menu item if score is not loaded
(defun item-if-score (menu title action)
  (let ((mitem (swing-menu:item menu title action)))
    ; (jcall "setEnabled" mitem NIL)
    mitem))

;; FILE MENU ;;

(defun file-menu (menu-bar)
  (let ((menu (swing-menu:create-menu menu-bar "File" :keyboard-shortcut #\F)))
    (swing-menu:item menu "Open Project..." #'project-current:load-dialog :keyboard-shortcut #\O)
    (swing-menu:item menu "Save Project" #'project-current:save-current-file :keyboard-shortcut #\S)
    (swing-menu:item menu "Save Project As..." #'project-current:save-dialog :keyboard-shortcut #\S :need-shift t)

    (swing-menu:separator menu)

    (swing-menu:item menu "Open Score..." #'score-manage:open-score-dialog)
    (score-select-built-in:create-menu menu)
    (swing-menu:item menu "Import Score from midi file..." #'score-manage:import-midi-dialog)
    (item-if-score menu "Export Score..." #'score-manage:export-score-dialog)

    (swing-menu:separator menu)
    (swing-menu:item menu "Open Rulepalette..." #'rulepalette-manage:open-rulepalette-dialog)
    (rulepalette-select-built-in:create-menu menu)
    (item-if-score menu "Export Rulepalette..." #'rulepalette-manage:export-rulepalette-dialog)

    (swing-menu:separator menu)
    (swing-menu:item menu "Quit" (lambda () (if (project-current:ensure-project-saved) (java-utils:exit 0))) :keyboard-shortcut #\Q)))

;; EDIT MENU ;;

(defun edit-menu (menu-bar)
  (let ((menu (swing-menu:create-menu menu-bar "Edit" :keyboard-shortcut #\E)))
    (swing-menu:item menu "Tempo" #'not-implemented)))

;; HELP MENU ;;

(defun help-menu (menu-bar)
  (let ((menu (swing-menu:create-menu menu-bar "Help" :keyboard-shortcut #\H)))
    (swing-menu:item menu "Help Text..." #'ui-help:open-help :keyboard-shortcut #\H)))

;; MENU BAR ;;

(defun create-menu ()
  (let ((menu-bar (swing-menu:create-menu-bar)))
    (file-menu menu-bar)
    ; (edit-menu menu-bar)
    (help-menu menu-bar)
    menu-bar))
