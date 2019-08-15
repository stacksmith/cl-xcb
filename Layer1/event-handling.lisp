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
;; A single event table is good for the entire connection, as long as we
;; delay resolving window ids until later...
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
(defun default-event-handler (event)
  (when *show-unhandled-events*
    (format *q* "~&[Unhandled event: ~A~&" (aref events (event-type event))))
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


;;============================================================================
;; Dispatch
;;
(defun event-dispatch (event)
  (declare (type simple-vector *event-dispatch-table* ))
  (let ((i (event-type event)))
   ;; (format *q* "~%(~A)~A: " ord (aref events i))
    (if (< i EVENT-LAST-EVENT)
	(funcall (event-get-handler (logand #x7F i)) event)
	(progn
	  (format *q* "UNEXPECTED EVENT ~A~&" i)))
    (foreign-free event)))

(defun event-step (&optional (block nil))
  (let ((e (if block
	       (wait-for-event *conn*)
	       (poll-for-event *conn*))))
    (unless (null-pointer-p e)
      (event-dispatch e))))

(defun steps (&optional (num 1))
  (loop for i below num do
       (event-step)
  ;;     (nv:vin)
       (sleep 0.1)))

;;TODO: fix assumptions abount windows, or move up to a higher level
(defun event-loop ()
  (loop until (zerop (hash-table-count windows)) do
       (event-step t)))

(defun event-thread-proc ()
  (loop while t do (event-step t)))

;;----------------------------------------------------------------
#||(defun default-expose (e)
  (with-foreign-slots ((window x y width height count)
			       e (:struct ES-EXPOSE))
    (format *q* "exposing window ~A; (~A ~A) ~A  ~A; ~A more."
	    window x y width height count)
    t))
||#

;;================================================================
;; A simple unthreaded event processor
;; process all available events.  Return count of events processed.
(defun events-process ()
  (loop for e = (poll-for-event *conn*)
     for i from 0
     until (null-pointer-p e) do
       (event-dispatch e)
     finally (return i)))

