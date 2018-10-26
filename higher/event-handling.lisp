(in-package :xcb)
;;==========================================================================
;;--------------------------------------------------------------------------
;; On a per-connection basis, the windows and events are handled here.
;;
;; Windows are associated to lisp objects via a hashtable, and may be
;; looked up by x ids.
;;
;; Incoming event handlers may use the table to find the lisp object
;;

;;==========================================================================
;; a sensible way to handle event dispatch is jump via a 36-element dispatch
;; table.  Initially filled with defaults. 
;;
;; The event table is good for the entire connection.
;; resolve window ids to Lisp windows
(defparameter *event-dispatch-table* nil)

(defun event-dispatch-reset ()
  (setf *event-dispatch-table*
	(make-array 36 :initial-element  #'default-event-handler)))

;;==========================================================================
;; event-type  pull type out of event
;;


;;==========================================================================
;; default  - any unhandled events go here.
;;
(defun default-event-handler (event-pointer)
  (when *show-unhandled-events*
    (format *q* "~&[Unhandled event: ~A~&" (aref events (event-type event-pointer))))
  t )
;;------------------------------------------------------------------------------
;;
(defmacro event-type (event)
  "return type of event"
  `(ldb (byte 7 0) (mem-ref ,event :uint8))) ;contained in low 7 bits

(defmacro event-get-handler (i)
  `(aref *event-dispatch-table* ,i))

(defmacro event-set-handler (i fun)
  `(setf  (aref *event-dispatch-table* ,i) ,fun))

(defun event-dispatch (event)
  (let ((i (event-type event)))
   ;; (format *q* "~%(~A)~A: " ord (aref events i))
    (if (< i EVENT-LAST-EVENT)
	(funcall (event-get-handler i) event)
	(progn
	  (format *q* "UNEXPECTED EVENT ~A~&" i)))
    (foreign-free event)))
;;======================================================
;;

(defun steps (&optional (num 1))
  (loop for i from 0 below (* num 10) do
     ;;  (format t "...e...") (force-output)
       (event-step)
  ;;     (nv:vin)
       (sleep 0.1)))
(defun event-loop ()
  (loop
     until (zerop (hash-table-count windows))do
       (event-step t)))

;;----------------------------------------------------------------
#||(defun default-expose (e)
  (with-foreign-slots ((window x y width height count)
			       e (:struct ES-EXPOSE))
    (format *q* "exposing window ~A; (~A ~A) ~A  ~A; ~A more."
	    window x y width height count)
    t))
||#
;; dedicated blocking thread.
#||
;; TODO: fix me 
(defun event-thread-proc ()
  (loop while t do (event-step t)))

(defun event-step (&optional (block nil))
  (let ((e (if block
	       (wait-for-event c)
	       (poll-for-event c))))
    (if (null-pointer-p e)
	nil
	(progn (event-dispatch e) t))))
||#
;;================================================================
;; A simple unthreaded event processor
;; process all available events.  Return count of events processed.
(defun events-process ()
  (loop for e = (poll-for-event c)
     for i from 0
     until (null-pointer-p e) do
       (event-dispatch e)
     finally (return i)))
