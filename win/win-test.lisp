(in-package :xcb)

(defun subpanels-create (container)
  (in-container (container container)
    (let* ((h1 (- height 16)))
      (container-insert container (make-panel  0 h1 width height) nil)
      (container-insert container (make-panel  0 0 width h1) nil)

;;      (container-insert container (make-panel  0  0 w1 height) nil)
;;      (container-insert container (make-panel  w1 0 width height) nil)
      )))

(defmethod panel-draw ((panel panel) win idx)


  (in-panel (panel panel)
    (format t "~%panel draw ~A" panel)
    (w-foreign-values (vals :uint16 x1. :uint16 y1.  :uint16 x2. :uint16 y2. ;;\
			    :uint16 x1. :uint16 y2. ;;bot
			    :uint16 x2. :uint16 y1. ;;/
			    :uint16 x1. :uint16 y1. ;;top
			    :uint16 x1. :uint16 y2. ;;left
			    )
      (check (poly-line *conn* COORD-MODE-ORIGIN (win-id win) (win-gc win) 6 vals)))
    (comp-string (win-pic win) (+ 3 x1.) (+ 12 y1.) (pen-pic *pen-white*)
		 (format nil "pane ~A ¤" idx))
    
        ))
;;----------------------------------------------------------------------------
(defun win-test-redraw (win)
  ;; erase
    ;;(clear-window *conn* (win-id win))
  (in-geo (panel win)
    (clear-area *conn* 0 (win-id win) 0 0 width height) )
  ;;  (flush c)
 
  (loop for i from 0
     for panel across (win-payload win) do
       (panel-draw panel win i))
  (flush *conn*))
;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defstruct (win-test (:include win-direct) (:constructor make-win-test%)
		     (:conc-name win-))
  )

(defun make-win-test (w h &optional maker)
  (let ((win (make-win-test% :x1 0 :y1 0 :x2 w :y2 h )))

    
    (setf *w* win)
    (subpanels-create win)
    (init-win win :maker maker )
    (xwin-fix-always-on-top  (win-id *w*) 1)
    win))

(defun win-test-container (win w h)
  (let ((payload (win-payload win)))
    (in-panel (panel (aref payload  0))
      (setf y1. (- h 16)
	    y2. h
	    x2. w))
    (in-panel (panel (aref payload  1))
      (setf y2. (- h 16)
	    x2. w)))
  (win-test-redraw win))

(defmethod win-on-resize ((win win-test) w h)

  (win-test-container win w h)
)


(defmethod win-on-expose ((win win-test) x y w h count event)
  (format t "~%EXPO")
   (win-test-redraw win)  )

;; initialize 2 sub-panels


(defun test2 ()
  (make-win-test 640 480;; :maker #'win-make-window1
		 )

  (sleep 0.5)
  (events-process)(flush *conn*)

 )
