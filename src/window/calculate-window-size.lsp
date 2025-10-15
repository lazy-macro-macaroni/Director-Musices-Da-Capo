
(globals:standard-package :window-calculate-window-size :set-window-size-with-margins :set-window-size)

(defun get-screen-size ()
  (let* ((device (jcall "getDefaultScreenDevice" (jstatic "getLocalGraphicsEnvironment" "java.awt.GraphicsEnvironment")))
         (display-mode (jcall "getDisplayMode" device)))
    (list (jcall "getWidth" display-mode) (jcall "getHeight" display-mode))))

(defun set-window-size-with-margins (window width-percent height-percent margin-top margin-left margin-bottom margin-right)
  (let* ((screen-size (get-screen-size))
         (screen-size-width (first screen-size))
         (screen-size-height (second screen-size))

         (width1 (* screen-size-width width-percent))
         (height1 (* screen-size-height height-percent))

         (margin-top-px (* height1 margin-top))
         (margin-left-px (* width1 margin-left))
         (margin-bottom-px (* height1 margin-bottom))
         (margin-right-px (* width1 margin-right))

         (width (- width1 margin-left-px margin-right-px))
         (height (- height1 margin-top-px margin-bottom-px))

         (x (+ (/ (- screen-size-width width1) 2) margin-left-px))
         (y (+ (/ (- screen-size-height height1) 2) margin-top-px)))
    (jcall "setLocation" window (jnew "java.awt.Point" (round x) (round y)))
    (jcall "setSize" window (jnew "java.awt.Dimension" (round width) (round height)))))

(defun set-window-size (window width-percent height-percent)
  (set-window-size-with-margins window width-percent height-percent 0 0 0 0))
