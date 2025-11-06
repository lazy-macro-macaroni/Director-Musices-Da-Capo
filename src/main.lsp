
(globals:standard-package :main :main)

(defun score-pane ()
  (let ((score-popup-menu (score-select-built-in:create-popup-menu))
        (pane (swing-tabbed-pane:create-tabbed-pane))
        (add-song-pane (swing-box-layout:create-box-layout :horizontal)))

    (swing-tabbed-pane:add-tab pane "+" add-song-pane)

    (swing-box-layout:add-glue add-song-pane :horizontal)
    (swing-box-layout:add
      add-song-pane
      (swing-button:create-button "Open Built-In Score"
        (lambda (button event)
          (swing-menu:show-popup-menu score-popup-menu button 0 (jcall "getHeight" button)))))
    (swing-box-layout:add-spacer add-song-pane 15)
    (swing-box-layout:add
      add-song-pane
      (swing-button:create-button "Open Score"
        (lambda (button event) (score-manage:open-score-dialog))))
    (swing-box-layout:add-glue add-song-pane :horizontal)

    pane))

(defun rulepalette-pane ()
  (let ((rulepalette-popup-menu (rulepalette-select-built-in:create-popup-menu))
        (pane (swing-tabbed-pane:create-tabbed-pane))
        (add-rp-pane (swing-box-layout:create-box-layout :horizontal)))

    (swing-tabbed-pane:add-tab pane "+" add-rp-pane)

    (swing-box-layout:add-glue add-rp-pane :horizontal)
    (swing-box-layout:add
      add-rp-pane
      (swing-button:create-button "Open Built-In Rulepalette"
        (lambda (button event)
          (swing-menu:show-popup-menu rulepalette-popup-menu button 0 (jcall "getHeight" button)))))
    (swing-box-layout:add-spacer add-rp-pane 15)
    (swing-box-layout:add
      add-rp-pane
      (swing-button:create-button "Open Rulepalette"
        (lambda (button event) (rulepalette-manage:open-rulepalette-dialog))))
    (swing-box-layout:add-glue add-rp-pane :horizontal)

    (project-data:add-rulepalettes-listener (project-current:get-project)
      (globals:safe-lambda "Main UI Rulepalettes Listener"
        (update-type value)
        (when (eq update-type :add-value)
          (globals:println "Rulepalettes listener triggered. Update type: ~S, Value: ~S, Name: ~S" update-type value (rulepalette-data:get-name value))
          (swing-tabbed-pane:insert-tab pane 0 (rulepalette-data:get-name value) (swing-box-layout:create-box-layout :horizontal))
          (swing-tabbed-pane:select-tab pane 0))))

    pane))

(defun main-split-pane ()
  (let* ((top (score-pane))
         (bottom (rulepalette-pane))
         (p (swing-split-pane:create-split-pane :vertical top bottom)))
    p))

(defun main3 (frame)
  (let ((split-pane (main-split-pane)))

    (swing-frame:set-icons frame
      (file-utils:jfile "." "resources" "icon" "icon64.png")
      (file-utils:jfile "." "resources" "icon" "icon128.png")
      (file-utils:jfile "." "resources" "icon" "icon256.png")
      (file-utils:jfile "." "resources" "icon" "icon512.png"))

    (swing-frame:set-menu-bar frame (ui-menu::create-menu))
    (swing-frame:set-close-operation frame :exit)

    (swing-frame:add frame split-pane)

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
    (swing-frame:request-focus frame)

    (swing-split-pane:set-divider-location split-pane 0.5)))

(defun main2 ()
  (let ((frame (swing-frame:create-frame "Director-Musices")))
    ;; This needs to be called before doing anything else, or else we won't get error message windows.
    (globals:set-main-window frame)
    (main3 frame)))

(defun main ()
  (when (eq (globals:handle-errors (main2) :failure :failed) :failed)
    (java-utils:exit 1)))
