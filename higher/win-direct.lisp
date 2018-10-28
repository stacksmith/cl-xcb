(in-package :xcb)


;;=============================================================================
;; Bufwin is a generic window with an off-screen buffer.
(defclass win-direct (win-base)
  ((pic :accessor pic :initform nil)))

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
(defun wintest ()
  (make-instance 'win-direct :w 640 :h 480)
  (sleep 0.1)
  (events-process)(flush c)

  (test-out "Hello World" *w* 20 20 ))
