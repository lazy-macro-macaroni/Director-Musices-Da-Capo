
(globals:standard-package :load-files
  :load-files :dm-files :src-files :build-files :test-files)

; Copied here since list-utils is not available when this file runs.
(defun flatten (lst)
  (cond
    ((null lst) nil)
    ((listp (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
    (t (cons (car lst) (flatten (cdr lst))))))

(defun load-with-progress (with-ui &rest paths1)
  (assert (or (eq with-ui t) (eq with-ui nil)) () "WITH-UI is not a boolean.")

  (format t "Loading files...")
  (finish-output) ; flush output

  (let* ((paths (flatten paths1))
         (count (list-length paths)))
    (loop
      for path in paths
      and index from 1 do
      (handler-case
        (let ((loading-message (format nil "Loading: ~A~%" path)))
          ; (format t loading-message)
          (if with-ui
            (jstatic "setPercentage" "dm_java.ProgressManager" loading-message (float (/ index count))))
          (load path))
        (condition (c)
          (let* ((message (format nil "FAIL~%Failed loading file: \"~A\", Error: ~A~%" path c))
                 (short-message (format nil "Failed loading file: \"~A\"." path))
                 (long-message (format nil "~A" c)))
            (format t message)
            (jstatic "Error" "dm_java.LoadingError" (if with-ui java::+TRUE+ java::+FALSE+) short-message long-message)
            (return nil))))
      finally
      (progn
        (format t "OK~%")
        (return t)))))

(defun dm-files ()
  `("src/startup/package-dm.lsp"

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
    "dm/dm-source/rules/accent-rule-ebrp.lsp"))

(defun src-files ()
  '("src/version.lsp"
    "src/utils/java-utils.lsp"
    "src/utils/list-utils.lsp"
    "src/utils/string-utils.lsp"
    "src/utils/file-utils.lsp"
    "src/utils/misc-utils.lsp"
    "src/utils/swing-utils.lsp"

    "src/ui/glue.lsp"
    "src/ui/score.lsp"
    "src/ui/player.lsp"
    "src/ui/menu.lsp"

    "src/window/calculate-window-size.lsp"
    "src/window/main-window.lsp"

    "src/ui/main.lsp"
    "src/main.lsp"))

(defun build-files ()
  '("src-build/paths.lsp"
    "src-build/utils.lsp"
    "src-build/download.lsp"
    "src-build/bundle.lsp"
    "src-build/build.lsp"
    "src-build/build-main.lsp"))

(defun test-files ()
  '("src-test/test-lib.lsp"))

(defun load-files (with-ui &rest files)
  (let ((result (load-with-progress with-ui files)))
    (jstatic "sleep" "java.lang.Thread" 200)
    result))
