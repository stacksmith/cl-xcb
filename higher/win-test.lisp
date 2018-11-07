(in-package :xcb)

(defmethod panel-draw ((panel panel) win)
  (in-panel (panel panel)
    (w-foreign-values (vals :uint16 x. :uint16 y.  :uint16 w. :uint16 h.))
    (check (poly-line c COORD-MODE-ORIGIN (win-id win) (win-gc win) 2 vals)))
  )

;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defstruct (win-test (:include win-direct) (:constructor make-win-test%)
		     (:conc-name win-))
  )

(defun make-win-test (w h &optional maker)
  (let ((win (make-win-test% :w w :h h )))
    (setf *w* win)
    (init-win *w* :maker maker )
    win))


;; initialize 2 sub-panels
(defun subpanels-create (layout)
  (in-layout (layout layout)
    (let* ((w1 100) (w2 (- work-width w1)))
      (layout-insert layout (make-panel :x 0 :y 0 :w w1 :h work-height) nil)
      (layout-insert layout (make-panel :x w1 :y 0 :w w2 :h work-height) nil)
      )))

(defun test2 ()
  (make-win-test 640 480;; :maker #'win-make-window1
		 )
  (sleep 0.1)
  (events-process)(flush c)

 )
