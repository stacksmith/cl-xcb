(in-package :xcb)
;; Now we build a foreign gap buffer containing a number of chunks.  Gap
;; motion is on chunk-level exclusively; within each chunk we edit the hard way.
;;
(defstruct (buffer (:constructor make-buffer%) (:conc-name buf-))
  (buf nil :type foreign-pointer)
  (size 0 :type U32))


(defstruct (gap-buffer (:include buffer)
		       (:constructor make-gap-buffer%)
		       (:conc-name gb-))
  (gap 0  :type U32)
  (point 0 :type U32))

(defun gap-buffer-allocate (&optional (size #x10000))
  (make-gap-buffer%
   :buf (foreign-alloc :uint8 :count size)
   :size size
   :gap 0
   :point size))

(defun gap-buffer-free (gb)
  (foreign-free (gb-buf gb))
  )
;; TODO: look for a more portable solution
(defun gb-copy (dest src src-size gap point;;cnt-low cnt-high
		dif)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 src-size gap point dif))
  (sb-kernel::system-area-ub8-copy src 0 dest 0 gap) ;; copy below gap
  (sb-kernel::system-area-ub8-copy
   src point
   dest (+ point dif)
   (- src-size point)
   )
  
  ;;  (loop for i fixnum from 0 by 8    while (< i gap) do      (setf (mem-ref dest :UINT64 i) (mem-ref src :UINT64 i)))
)
;;(foreign-funcall "memcpy" dest src gap)      
  
(defmacro in-gb ((name gb) &body body)
  `(let ((,name ,gb))
     (with-accessors ((buf. gb-buf) (size. gb-size)
		      (gap. gb-gap) (point. gb-point)) ,name
       (symbol-macrolet ((gapsize (- point. gap.)))
	 ,@body))))

(defun gb-resize (gb required)
  (in-gb (gb gb)
    (prog ((proposed size.)
	   (available gapsize))
     again
     (incf proposed proposed); double proposed size
     (when (< (incf available proposed) required)
       (go again))
     ;; now, make it so.
     (let ((new (foreign-alloc :uint8 :count proposed))
	   (dif (- proposed size.)))
       (gb-copy new buf. size. gap. point. dif)
       (foreign-free buf.)
       (setf buf. new
	     size. proposed
	     point. (+ point. dif)))))
  
  )
;;==============================================================================
(defun gb-assure (gb bytes)
;;  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum bytes))
  (in-gb (gb gb)
    (when (< bytes gapsize)
      (gb-resize gb bytes))
    gb)
  )
