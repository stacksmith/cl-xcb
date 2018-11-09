(in-package :xcb)


;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defstruct (win-direct (:include win-base) (:constructor make-win-direct%)
		       (:conc-name win-))
  (pic 0 :type U32))

(defun make-win-direct (w h &optional maker)
  (let ((win (make-win-direct% :x2 w :y2 h )))
    (setf *w* win)
    (init-win *w* :maker maker )
    win))


;; since we want all instance to make the window above...
(defmethod win-make-xcb-window ((win win-direct))
  (format t "~%win-make-xcb-window direct...")
   (in-layout (layout win)
     (with-foreign-slots ((root root-visual black-pixel) *setup* (:struct screen-t))
       (w-foreign-values (vals
			 :uint32 black-pixel
			 :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			 :uint32 (+ EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS )) 
	(check (create-window *conn* COPY-FROM-PARENT (win-id win)
			      root
			      x1. y1. width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-BACK-PIXEL
				 CW-BIT-GRAVITY  CW-EVENT-MASK   ) vals))
	))))

(defmethod init-win :after ((win win-direct) &key &allow-other-keys)
  (with-slots (pic id) win
    (setf pic (generate-id *conn*))
    (check (create-picture *conn* pic id +RGB24+ 0 (null-pointer)))))

(defmethod on-destroy :before ((win win-direct))
  (check (free-picture *conn* (win-pic win)))
)

;;==============================================================================
;; handle resizing
;;
;; On-configure-notify tracks window size/position.  While not officially doc'd
;; 2 consecutive invocations with identical x y w h imply that we are done
;; resizing.
;;
(defmethod win-on-resize ((win win-direct) w h)
  (format t "RESIZED")
  t)

(defmethod win-on-expose ((win win-direct) x y w h *conn* event)  )
;;------------------------------------------------------------------------------


