
(globals:standard-package :startup-load-files)

(defun load-with-progress (&rest paths)
  (let* ((count (list-length paths)))
    (loop
      for path in paths
      and index from 1 do
      (handler-case
        (let ((loading-message (format nil "Loading: ~A~%" path)))
          (format t loading-message)
          (jstatic "setPercentage" "dm_java.ProgressManager" loading-message (float (/ index count)))
          (load path))
        (condition (c)
          (let* ((message (format nil "Failed loading file: \"~A\", Error: ~A~%" path c)))
            (format t message)))))))

(defun load-files ()
  (load-with-progress
    "src/startup/package-dm.lsp"

    "dm/dm-source/lib-core/scoreobjects.lsp"
    "dm/dm-source/lib-core/basicmacros.lsp"
    "dm/dm-source/lib-core/infixmath.lsp"
    "dm/dm-source/lib-core/musicio.lsp"
    "dm/dm-source/lib-core/rulemacros.lsp"
    "dm/dm-source/lib-core/parallelrulemacros.lsp"
    "dm/dm-source/lib-core/dm-objects.lsp"
    "dm/dm-source/lib-core/initconvert.lsp"
    "dm/dm-source/lib-core/save-pdm-score.lsp"
    "dm/dm-source/lib-core/rule-groups.lsp"
    "dm/dm-source/lib-core/syntobjects.lsp"
    "dm/dm-source/lib-core/shapeobjects.lsp"
    "dm/dm-source/lib-core/midifileoutput.lsp"
    "dm/dm-source/lib-core/midifileinput.lsp"
    "dm/dm-source/lib-core/playlist.lsp"
    "dm/dm-source/lib-core/utilityrules.lsp"
    "dm/dm-source/lib-core/midibasic-lw.lsp"

    "dm/dm-source/init.lsp"

    "dm/dm-source/rules/articulation.lsp"
    "dm/dm-source/rules/frules1.lsp"
    "dm/dm-source/rules/frules2.lsp"
    "dm/dm-source/rules/FinalRitard.lsp"
    "dm/dm-source/rules/Intonation.lsp"
    "dm/dm-source/rules/noise.lsp"
    "dm/dm-source/rules/punctuation.lsp"
    "dm/dm-source/rules/phrasearch.lsp"
    "dm/dm-source/rules/swing.lsp"
    "dm/dm-source/rules/SyncOnMel.lsp"
    "dm/dm-source/rules/violinvibrato.lsp"
    "dm/dm-source/rules/accent-analysis.lsp"
    "dm/dm-source/rules/accent-rule-ebrp.lsp"

    "src/ui/swing.lsp"
    "src/ui/glue.lsp"
    "src/ui/score.lsp"
    "src/ui/player.lsp"
    "src/ui/menu.lsp"

    "src/window/calculate-window-size.lsp"
    "src/window/main-window.lsp"

    "src/ui/main.lsp"))

(load-files)
