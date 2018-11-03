(in-package :xcb)


;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass win-direct (win-base)
  ((pic :accessor pic :initform nil)))

(defun win-make-window1 (win)
 )
;; since we want all instance to make the window above...
(defmethod win-make-xcb-window ((win win-direct))
   (with-slots (width height id) win
    (with-foreign-slots ((root root-visual black-pixel) s (:struct screen-t))
      (w-foreign-values (vals
			 :uint32 black-pixel
			 :uint32 GRAVITY-NORTH-WEST ;; Leave contents on resize.
			 :uint32 (+ EVENT-MASK-EXPOSURE
				    EVENT-MASK-STRUCTURE-NOTIFY
				    ;;EVENT-MASK-RESIZE-REDIRECT
				    ;; EVENT-MASK-BUTTON-PRESS
				    EVENT-MASK-KEY-PRESS )) 
	(check (create-window c COPY-FROM-PARENT id
			      root
			      0 0 width height 10
			      WINDOW-CLASS-INPUT-OUTPUT
			      root-visual
			      (+ CW-BACK-PIXEL
				 CW-BIT-GRAVITY  CW-EVENT-MASK   ) vals))
	))))

(defmethod initialize-instance :after ((win win-direct) &key &allow-other-keys)
  (with-slots (pic id) win
    (setf pic (generate-id c))
    (check (create-picture c pic id +RGB24+ 0 (null-pointer)))))

(defmethod on-destroy :before ((win win-direct))
  (check (free-picture c (pic win)))
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
;;------------------------------------------------------------------------------
;;(defmethod win-on-expose ((win win-base) x y width height count event))

;;==================================
;; Initialize with (init)  -- see xcb-system.lisp
;;
(defun test1 ()
  (make-instance 'win-direct :w 640 :h 480;; :maker #'win-make-window1
		 )
  (sleep 0.1)
  (events-process)(flush c)

  (test-out "Hello World" *w* 20 20 ))
