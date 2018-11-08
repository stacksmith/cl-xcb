(in-package :xcb)

(defun subpanels-create (layout)
  (in-layout (layout layout)
    (let* ((w1 100))
      (layout-insert layout (make-panel  0  0 w1 height) nil)
      (layout-insert layout (make-panel  w1 0 width height) nil)
      )))

(defmethod panel-draw ((panel panel) win)
  (describe c)

  (in-panel (panel panel)
    (format t "~%panel draw ~A" panel)
    (w-foreign-values (vals :uint16 x1. :uint16 y1.  :uint16 x2. :uint16 y2.)
      (check (poly-line c COORD-MODE-ORIGIN (win-id win) (win-gc win) 2 vals)))))

(defun win-test-redraw (win)
  (loop for panel across (win-payload win) do
       (panel-draw panel win)))
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

    win))

(defun win-test-layout (win w h)
  (let ((payload (win-payload win)))
    (in-panel (panel (aref payload  0))
      (setf y2. h))
    (in-panel (panel (aref payload  1))
      (setf y2. h
	    x2. w)))
  (win-test-redraw win))

(defmethod win-on-resize ((win win-test) w h)

  (win-test-layout win w h)
)


(defmethod win-on-expose ((win win-test) x y w h count event)

   (win-test-redraw win)  )

;; initialize 2 sub-panels


(defun test2 ()
  (make-win-test 640 480;; :maker #'win-make-window1
		 )
  (sleep 0.1)
  (events-process)(flush c)

 )
