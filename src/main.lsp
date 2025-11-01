
(globals:standard-package :main :main)

(defun main2 ()
  (let ((frame (swing-frame:create-frame "Director-Musices")))

    (globals:set-main-window frame)

    (swing-frame:set-icons frame
      (file-utils:jfile "." "resources" "icon" "icon64.png")
      (file-utils:jfile "." "resources" "icon" "icon128.png")
      (file-utils:jfile "." "resources" "icon" "icon256.png")
      (file-utils:jfile "." "resources" "icon" "icon512.png"))

    (swing-frame:set-menu-bar frame (ui-menu::create-menu))
    (swing-frame:set-close-operation frame :exit)

    (window-calculate-window-size:set-window-size frame 0.5 0.7)

    (data-value:add-listener (project-data:get-current-file (project-current:get-project))
      (lambda (f)
        (swing-frame:set-title frame
          (globals:format-string "Director-Musices - ~A"
            (if (eq f nil)
              "Unsaved Project"
              (file-utils:file-name f))))))

    (java-dm:hide-splash)

    (swing-frame:set-visible frame t)
    (swing-frame:request-focus frame)))

(defun main ()
  (when (eq (globals:handle-errors (main2) :failure :failed) :failed)
    (java-utils:exit 1)))
