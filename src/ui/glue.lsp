
(defpackage :ui-glue
  (:use :cl :java)
  (:export :load-active-score-from-file :save-midi-to-path))

(in-package :ui-glue)

(defmacro in-dm (&rest forms)
  `(let ((*package* (find-package 'dm))) (progn ,@forms)))

; (ns director-musices.glue
;   (:use clojure.java.io
;         (director-musices interpreter
;           [utils :only [with-indeterminate-progress]]))
;   (:require [seesaw.core :as ssw]))

; (def dm-init? (atom nil))

; (defn load-package-dm []
;   (load-abcl "dm:package-dm.lsp"))

; (defn load-core []
;   (load-multiple-abcl
;     "dm:lib-core:"
;     ["scoreobjects.lsp" "basicmacros.lsp" "infixmath.lsp" "musicio.lsp" "rulemacros.lsp" "parallelrulemacros.lsp"
;      "utilityrules.lsp"
;      "dm-objects.lsp" "initconvert.lsp" "save-pdm-score.lsp" "rule-groups.lsp" "syntobjects.lsp" "shapeobjects.lsp"
;      "midifileoutput.lsp" "midifileinput.lsp" "playlist.lsp" "midibasic-lw.lsp"])
;   (load-abcl "dm:init.lsp"))

; (defn load-rules []
;   (load-multiple-abcl
;     "dm:rules:"
;     ["frules1.lsp" "frules2.lsp" "Intonation.lsp"
;      "FinalRitard.lsp" "Punctuation.lsp"
;      "phrasearch.lsp" "SyncOnMel.lsp" "accent-rule-ebrp.lsp" "articulation.lsp"]))

; (defn init-dm []
;   (when-not @dm-init?
;     (with-indeterminate-progress "Loading lisp environment"
;       (load-package-dm)
;       (load-core)
;       (load-rules)
;       (swap! dm-init? (constantly true)))))

; (defn str->abcl [s]
;   (eval-abcl (str "\"" s "\"")))

; ;(use 'clojure.pprint)

; (defn load-active-score [string]
;   (init-dm)
;   (eval-abcl
;     (str "(in-package :dm)
;           (read-active-score-from-string \"" string
;          "\")
;           (init-music-score)")))
; ;  (eval-abcl "(in-package :dm)")
; ;  (.execute (abcl-f "DM" "read-score-from-string") (str->abcl string))
; ;  (eval-abcl "(init-music-score)"))

(defun load-active-score-from-file (path)
  ; (in-package :dm)
  (let ((*package* (find-package 'dm)))
    (setq path (jcall "replace" path "\\" "/"))
    (format t "Loading mus file: ~A~%" path)
    (read-active-score-from-file path)
    (format t "~%Score read~%")
    (init-music-score)
    (format t "Score initiated~%")))

(defun load-active-score-from-string (content)
  (format t "Loading mus file.")
  ; (let ((content (jstatic "readString" "java.nio.file.Files" path)))
  ; (format t "Got content: ~A~%" content)
  (dm::read-active-score-from-string content)
  ; (dm
  ; (dm::load-score-fpath path)
  ; (format t "Score: ~A~%" (dm::print-music-round))
  (format t "~%Score read~%")
  (dm::init-music-score)
  (format t "Score initiated~%"))

; (defn load-active-score-from-midi-file [path]
;   (init-dm)
;   (eval-abcl "(in-package :dm)")
;   (.execute (abcl-f "DM" "load-midifile-fpath") (str->abcl path))
;   (eval-abcl "(init-music-score)"))

; (defn get-active-score []
;   (.execute (abcl-f "DM" "get-active-score")))

; (defn apply-rules [rulelist-string sync-rule & [rule-interaction-c]]
;   (init-dm)
;   (if rule-interaction-c
;     (eval-abcl (str "(in-package :dm)
;                     (reset-music)
;                     (rule-interaction-apply-rules-sync '(" rulelist-string ") '" rule-interaction-c ")"))
;     (eval-abcl (str "(in-package :dm)
;                     (reset-music)
;                     (rule-apply-list-sync '("
;                     rulelist-string ") '" sync-rule ")"))))

(defun save-midi-to-path (path)
  (dm::save-performance-midifile1-fpath path))
