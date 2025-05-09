
(defpackage :window-logs-window
  (:use :cl :java)
  (:export :make-logs-window))

(in-package :window-logs-window)
(require :abcl-contrib) ; This is required to fix a bug with jnew-runtime-class. See: https://github.com/armedbear/abcl/issues/685

;; Logs-Stream

(defclass logs-stream ()
  ((window :initarg :window :accessor window)
   (color :initarg :color :accessor color)
   (default-stream :initarg :default-stream :accessor default-stream)
   (string-builder :accessor string-builder :initform (jnew "java.lang.StringBuilder"))
   (output-stream :accessor output-stream)))

(defmethod write-to-stream ((obj logs-stream) b)
  (jcall "write" (default-stream obj) b)
  (if (not (eq b 13)) ; \r character
    (if (eq b 10) ; \n character
      (let ((text (concatenate 'string (jcall "toString" (string-builder obj)) "\n")))
        (append-text (window obj) (color obj) text)
        (jcall "setLength" (string-builder obj)))
      (jcall "append" (string-builder obj) b))))

(defmethod make-output-stream ((obj logs-stream))
  (setf (output-stream obj)
    ; (jnew "dm_java.DMOutputStream" (lambda (b) (write-to-stream obj b)))))
    (jnew (jnew-runtime-class
      "dm_cl.OutputStream"
      :superclass "java.io.OutputStream"
      :methods
      '(("write" :void (:int) (lambda (b) (write-to-stream obj b))))))))

(defun make-logs-stream (window color default-stream)
  (let ((obj (make-instance 'logs-stream :window window :color color :default-stream default-stream)))
    (make-output-stream obj)
    obj))

;; Logs-Window

(defclass logs-window ()
  ((dialog :accessor dialog)
   (text-pane :accessor text-pane :initform (jnew "javax.swing.JTextPane"))))

(defmethod append-text ((obj logs-window) color text)
  (let* ((text-pane (slot-value obj 'text-pane))
         (sc (jstatic "getDefaultStyleContext" "javax.swing.text.StyleContext"))
         (aset (jcall "addAttribute" sc (jfield "javax.swing.text.SimpleAttributeSet" "EMPTY") (jfield "javax.swing.text.StyleConstants" "Foreground") color))
         (len (jcall "getLength" (jcall "getDocument" text-pane))))
    (setf aset (jcall "addAttribute" sc aset (jfield "javax.swing.text.StyleConstants" "FontFamily") "Lucida Console"))
    (setf aset (jcall "addAttribute" sc aset (jfield "javax.swing.text.StyleConstants" "Alignment") (jfield "javax.swing.text.StyleConstants" "ALIGN_JUSTIFIED")))
    (jcall "setEditable" text-pane +TRUE+)
    (jcall "setCaretPosition" text-pane len)
    (jcall "setCharacterAttributes" text-pane aset +FALSE+)
    (jcall "replaceSelection" text-pane text)
    (jcall "setEditable" text-pane +FALSE+)))

(defun make-logs-window (parent-window)
  (let* ((obj (make-instance 'logs-window))
         (dialog (jnew "javax.swing.JDialog" parent-window))
         (text-pane (slot-value obj 'text-pane))
         (logs-stream-out (make-logs-stream obj (jfield "java.awt.Color" "BLACK") (jfield "java.lang.System" "out")))
         (logs-stream-err (make-logs-stream obj (jfield "java.awt.Color" "RED") (jfield "java.lang.System" "err"))))
    (setf (slot-value obj 'dialog) dialog)

    (jcall "setEditable" text-pane +FALSE+)

    (jstatic "setOut" "java.lang.System" (jnew "java.io.PrintStream" (output-stream logs-stream-out)))
    (jstatic "setErr" "java.lang.System" (jnew "java.io.PrintStream" (output-stream logs-stream-err)))

    (jcall "setTitle" dialog "Logs")
    (jcall "setDefaultCloseOperation" dialog (jfield "javax.swing.JFrame" "HIDE_ON_CLOSE"))

    (window-calculate-window-size:set-window-size dialog 0 0.7 0 0)
    (jcall "setLayout" dialog +NULL+)

    (let ((scroll-pane (jnew "javax.swing.JScrollPane" text-pane)))
      (jcall "setContentPane" dialog scroll-pane))

    (jcall "setVisible" dialog +TRUE+)

    (append-text obj (jfield "java.awt.Color" "GREEN") "hello text")

    obj))

; (defmethod initialize-instance :after ((obj logs-window) &key)
; (defmethod

;     final JTextPane textPane;

;     public LogsFrame(JFrame parent) {
;         super(parent);

;         textPane = new JTextPane();
;         textPane.setEditable(false);
;         // JPanel noWrapPanel = new JPanel(new BorderLayout());
;         // noWrapPanel.add(textPane);

;         System.setOut(new PrintStream(new LogsStream(Color.BLACK, System.out)));
;         System.setErr(new PrintStream(new LogsStream(Color.RED, System.err)));

;         // try {
;         //     File font_file = new File("resources/freefont/FreeMono.ttf");
;         //     Font font = Font.createFont(Font.TRUETYPE_FONT, font_file);
;         //     font = font.deriveFont(14f);
;         //     textPane.setFont(font);
;         // } catch(Exception e) {
;         //     e.printStackTrace();
;         // }

;         setTitle("Logs");
;         setDefaultCloseOperation(javax.swing.JFrame.HIDE_ON_CLOSE);
;         CalculateWindowSize.setSpecs(this, 0f, 0.7f, 0f, 0f);
;         setLayout(null);
;         JScrollPane sp = new JScrollPane(textPane); //noWrapPanel);
;         setContentPane(sp);
;         setVisible(true);
;     }

;     private void _appendText(String text, Color color) {
;         textPane.setEditable(true);
;         StyleContext sc = StyleContext.getDefaultStyleContext();
;         AttributeSet aset = sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground, color);
;         aset = sc.addAttribute(aset, StyleConstants.FontFamily, "Lucida Console");
;         aset = sc.addAttribute(aset, StyleConstants.Alignment, StyleConstants.ALIGN_JUSTIFIED);

;         int len = textPane.getDocument().getLength();
;         textPane.setCaretPosition(len);
;         textPane.setCharacterAttributes(aset, false);
;         textPane.replaceSelection(text);
;         textPane.setEditable(false);
;     }

;     public void appendText(String text, Color color) {
;         SwingUtilities.invokeLater(() -> {
;             _appendText(text, color);
;         });
;     }

;     public void showLogs() {
;         setVisible(true);
;     }