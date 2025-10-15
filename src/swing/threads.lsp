
(globals:standard-package :swing-threads invoke-later invoke-and-wait)

(defmacro invoke-later (&rest forms)
  "Runs FORMS on the ui thread. Necessary when accessing the ui. Does not run instantly."
  `(jstatic "invokeLater" "javax.swing.SwingUtilities" (java-utils:runnable "Swing Invoke Later" ,@forms)))

(defmacro invoke-and-wait (&rest forms)
  "Runs FORMS on the ui thread. Necessary when accessing the ui. Runs instantly and the current thread is blocked until completion."
  `(jstatic "invokeAndWait" "javax.swing.SwingUtilities" (java-utils:runnable "Swing Invoke And Wait" ,@forms)))
