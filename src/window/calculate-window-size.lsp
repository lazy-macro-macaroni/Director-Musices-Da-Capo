
(defpackage :window-calculate-window-size
  (:use :cl :java)
  (:export :set-window-size))

(in-package :window-calculate-window-size)

(defvar *screen-width-used* 0.8)
(defvar *screen-height-used* 0.8)

(defun get-screen-size ()
  (let* ((device (jcall "getDefaultScreenDevice" (jstatic "getLocalGraphicsEnvironment" "java.awt.GraphicsEnvironment")))
         (display-mode (jcall "getDisplayMode" device)))
    (list (jcall "getWidth" display-mode) (jcall "getHeight" display-mode))))

(defun set-window-size (window margin-top margin-left margin-bottom margin-right)
  (let* ((screen-size (get-screen-size))
         (screen-size-width (first screen-size))
         (screen-size-height (second screen-size))

         (width1 (* screen-size-width *screen-width-used*))
         (height1 (* screen-size-height *screen-height-used*))

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
