
(globals:standard-package :swing-menu create-menu-bar create-menu create-sub-menu separator item)

(defun char-to-keyevent (ch)
  (check-type ch character)
  (jstatic "getExtendedKeyCodeForChar" "java.awt.event.KeyEvent" ch))

(defun create-menu-bar ()
  (jnew "javax.swing.JMenuBar"))

(defun create-menu (menu-bar title &key keyboard-shortcut)
  (check-type title string)

  (let ((m (jnew "javax.swing.JMenu" title)))
    (when (not (eq keyboard-shortcut nil))
      (check-type keyboard-shortcut character)
      (jcall "setMnemonic" m (char-to-keyevent keyboard-shortcut)))
    (jcall "add" menu-bar m)
    m))

(defun create-sub-menu (menu title)
  (java-utils:jcheck-type menu "javax.swing.JMenu")
  (check-type title string)
  (let ((sub-menu (jnew "javax.swing.JMenu" title)))
    (jcall "add" menu sub-menu)
    sub-menu))

(defun separator (menu)
  (java-utils:jcheck-type menu "javax.swing.JMenu")

  (jcall "addSeparator" menu))

(defun item (menu title action &key keyboard-shortcut &key need-shift)
  (java-utils:jcheck-type menu "javax.swing.JMenu")
  (check-type title string)
  (check-type action function)

  (let ((mitem (jnew "javax.swing.JMenuItem" title)))
    (jcall "addActionListener" mitem
      (jinterface-implementation
        "java.awt.event.ActionListener" "actionPerformed"
        (globals:safe-lambda "Menu Action Listener" (e)
          (funcall action))))

    (when (not (eq keyboard-shortcut nil))
      (check-type keyboard-shortcut character)

      (let ((shortcut-mask (jcall "getMenuShortcutKeyMaskEx" (jstatic "getDefaultToolkit" "java.awt.Toolkit"))))
        (if need-shift
          (setf shortcut-mask (logior shortcut-mask (jfield "java.awt.event.InputEvent" "SHIFT_DOWN_MASK"))))
        (jcall "setAccelerator" mitem
          (jstatic "getKeyStroke" "javax.swing.KeyStroke"
            (char-to-keyevent keyboard-shortcut)
            shortcut-mask))))

    (jcall "add" menu mitem)
    mitem))
