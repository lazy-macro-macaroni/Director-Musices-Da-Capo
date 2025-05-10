
; (defun load-with-progress (paths)
;   (let* ((count (list-length paths)))
;     (loop
;       for path in paths
;       and index from 1 do
;       (handler-case
;         (progn
;           (format t "Loading: ~A~%" path)
;           (load path)
;           (jstatic "loadingProgress" "dm_java.CLManager" index count))
;         (condition (c)
;           (let* ((message (format nil "Failed loading file: \"~A\", Error: ~A~%" path c)))
;             (jstatic "loadingFailed" "dm_java.CLManager")
;             (format t "[CL-ERR] ~A~%" message)))))))

; (defun show-ui ()
;   (let ((frame (jnew "javax.swing.JFrame" "HelloWorldSwing"))
;         (label (jnew "javax.swing.JLabel" "Hello world label")))
;     (jcall "setDefaultCloseOperation" frame (jfield "javax.swing.JFrame" "EXIT_ON_CLOSE"))
;     (jcall "add" (jcall "getContentPane" frame) label)
;     (jcall "setJMenuBar" frame (ui-menu::create-menu))
;     (jcall "pack" frame)
;     (jcall "setSize" frame 600 400)
;     (jcall "setLocationRelativeTo" frame JAVA:+NULL+)
;     (jcall "setVisible" frame T)))

(defun load-files ()
  ; (load-with-progress
  (jstatic "loadFiles" "dm_java.CLManager"
    (java:jnew-array-from-list "java.lang.String"
      (list
        "dm/package-dm.lsp"

        "dm/lib-core/scoreobjects.lsp"
        "dm/lib-core/basicmacros.lsp"
        "dm/lib-core/infixmath.lsp"
        "dm/lib-core/musicio.lsp"
        "dm/lib-core/rulemacros.lsp"
        "dm/lib-core/parallelrulemacros.lsp"
        "dm/lib-core/dm-objects.lsp"
        "dm/lib-core/initconvert.lsp"
        "dm/lib-core/save-pdm-score.lsp"
        "dm/lib-core/rule-groups.lsp"
        "dm/lib-core/syntobjects.lsp"
        "dm/lib-core/shapeobjects.lsp"
        "dm/lib-core/midifileoutput.lsp"
        "dm/lib-core/midifileinput.lsp"
        "dm/lib-core/playlist.lsp"
        "dm/lib-core/utilityrules.lsp"
        "dm/lib-core/midibasic-lw.lsp"

        "dm/init.lsp"

        "dm/rules/articulation.lsp"
        "dm/rules/frules1.lsp"
        "dm/rules/frules2.lsp"
        "dm/rules/FinalRitard.lsp"
        "dm/rules/Intonation.lsp"
        "dm/rules/noise.lsp"
        "dm/rules/punctuation.lsp"
        "dm/rules/phrasearch.lsp"
        "dm/rules/swing.lsp"
        "dm/rules/SyncOnMel.lsp"
        "dm/rules/violinvibrato.lsp"
        "dm/rules/accent-analysis.lsp"
        "dm/rules/accent-rule-ebrp.lsp"

        "src/ui/swing.lsp"
        "src/ui/glue.lsp"
        "src/ui/score.lsp"
        "src/ui/player.lsp"
        "src/ui/menu.lsp"

        "src/window/calculate-window-size.lsp"
        "src/window/main-window.lsp"

        "src/ui/main.lsp"
      ))))

(load-files)
