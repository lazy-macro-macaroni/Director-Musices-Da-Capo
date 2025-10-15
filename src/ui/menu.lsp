
(globals:standard-package :ui-menu create-menu)

;; MENU FUNCTIONS ;;

(defun not-implemented ()
  (swing-dialogs:error-dialog "Not implemented!"))

;; EDIT MENU ;;

; TODO: Grayed out menu item if score is not loaded
(defun item-if-score (menu title action)
  (let ((mitem (swing-menu:item menu title action)))
    (jcall "setEnabled" mitem NIL)
    mitem))

(defun edit-menu ()
  (let ((menu (swing-menu:menu "Edit")))
    (swing-menu:item menu "Tempo" #'not-implemented)
    menu))

;; FILE MENU ;;

(defun file-menu ()
  (let ((menu (swing-menu:menu "File")))
    (swing-menu:item menu "Open Project..." #'not-implemented)
    (swing-menu:item menu "Save Project" #'not-implemented)
    (swing-menu:item menu "Save Project As..." #'not-implemented)

    (swing-menu:separator menu)

    (swing-menu:item menu "Open Score..." #'ui-score:choose-and-open-score)
    (item-if-score menu "Export Score..." #'not-implemented)
    (swing-menu:item menu "Import Score from midi file..." #'not-implemented)

    (swing-menu:separator menu)
    (item-if-score menu "Open Rule Palette..." #'not-implemented)
    (item-if-score menu "Export Rule Palette..." #'not-implemented)

    (swing-menu:separator menu)
    (swing-menu:item menu "Quit" (lambda () (java-utils:exit 0)))
    menu))

;; HELP MENU ;;

(defun help-menu ()
  (let ((menu (swing-menu:menu "Help")))
    (swing-menu:item menu "Help Text..." #'ui-help:open-help)
    menu))

;; MENU BAR ;;

(defun create-menu ()
  (let ((menubar (jnew "javax.swing.JMenuBar")))
    (jcall "add" menubar (file-menu))
    (jcall "add" menubar (edit-menu))
    (jcall "add" menubar (help-menu))
    menubar))
