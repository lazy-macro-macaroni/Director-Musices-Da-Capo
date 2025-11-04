
(globals:standard-package :main :main)

(defun main-split-pane ()
  (let* ((score-popup-menu (score-select-built-in:create-popup-menu))
         (rulepalette-popup-menu (rulepalette-select-built-in:create-popup-menu))
         (top (swing-box-layout:create-box-layout :horizontal))
         (bottom (swing-box-layout:create-box-layout :horizontal))
         (p (swing-split-pane:create-split-pane :vertical top bottom)))

    (swing-box-layout:add-glue top :horizontal)
    (swing-box-layout:add
      top
      (swing-button:create-button "Open Built-In Score"
        (lambda (button event)
          (swing-menu:show-popup-menu score-popup-menu button 0 (jcall "getHeight" button)))))
    (swing-box-layout:add-spacer top 15)
    (swing-box-layout:add
      top
      (swing-button:create-button "Open Score"
        (lambda (button event) (score-manage:open-score-dialog))))
    (swing-box-layout:add-glue top :horizontal)

    (swing-box-layout:add-glue bottom :horizontal)
    (swing-box-layout:add
      bottom
      (swing-button:create-button "Open Built-In Rulepalette"
        (lambda (button event)
          (swing-menu:show-popup-menu rulepalette-popup-menu button 0 (jcall "getHeight" button)))))
    (swing-box-layout:add-spacer bottom 15)
    (swing-box-layout:add
      bottom
      (swing-button:create-button "Open Rulepalette"
        (lambda (button event) (rulepalette-manage:open-rulepalette-dialog))))
    (swing-box-layout:add-glue bottom :horizontal)

    p))

(defun main2 ()
  (let ((frame (swing-frame:create-frame "Director-Musices"))
        (split-pane (main-split-pane)))

    (globals:set-main-window frame)

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

(defun main ()
  (when (eq (globals:handle-errors (main2) :failure :failed) :failed)
    (java-utils:exit 1)))
