
(globals:standard-package :window-main-window :make-window)

; (defpackage :window-main-window
;   (:use :cl :java)
;   (:export :make-window))

; (in-package :window-main-window)

(defclass main-window ()
  (frame))
  ; ((frame (jnew "javax.swing.JFrame"))
  ;  (loading-panel (
  ; ((test-slot :initform (format t "HELLO INIT~%"))))
;     JFrame frame;
;     JPanel loadingPanel;
;     LogsFrame logsFrame;

;     JProgressBar progress;
;     JLabel progressText;
;     JButton progressRetry;

;     CLManager clManager;
; )

 ; JAVA:+TRUE+)))

        ; frame = new JFrame();
        ; logsFrame = new LogsFrame(frame);

        ; frame.addWindowListener(new OnCloseListener());

        ; frame.setTitle("Director Musices 2024");
        ; frame.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);

        ; CalculateWindowSize.setSpecs(frame, 0f, 0f, 0f, 0.303f);

        ; createLoadingPanel();
        ; showLoadingPanel();
        ; frame.setVisible(true);

        ; clManager = new CLManager(this);
        ; clManager.load();

(defun make-window ()
  (/ 1 0)
  (let* ((obj (make-instance 'main-window))
         (frame (jnew "javax.swing.JFrame")))
    (setf (slot-value obj 'frame) frame)
    ; (window-logs-window:make-logs-window frame)
    (jcall "setTitle" frame "Director Musices 2024")
    (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))
    (window-calculate-window-size:set-window-size frame 0 0 0 0.303)
    (jcall "setVisible" frame t)))
