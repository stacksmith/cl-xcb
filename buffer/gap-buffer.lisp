(in-package :xcb)
;; utf8x
;;
;; UTF8 extends naturally by definining an otherwise illegal header F8

;;1 	7 	U+0000 	 U+007F   0xxxxxxx 			
;;2 	11 	U+0080 	 U+07FF   110xxxxx  10xxxxxx 		
;;3 	16 	U+0800 	 U+FFFF   1110xxxx  10xxxxxx  10xxxxxx 	
;;4 	21 	U+10000  U+10FFFF 11110xxx  10xxxxxx  10xxxxxx 	10xxxxxx
;;5     8-bit mark                111110xx  10xxxxxx
;;6     tag                       111111xx  10xxxxx?
;;
;; Format 5 is used to store tags.
;;
(defmacro gb-memcpy (dst doff src soff cnt)
  `(sb-kernel::system-area-ub8-copy ,src ,soff ,dst ,doff ,cnt)) ;; copy below gap
;; A ubuf is a foreign gap buffer for utf8x characters
(defstruct (gap-buffer ;;(:include buffer)
		       (:constructor make-gap-buffer%)
		       (:conc-name gb-))
  (buf nil :type foreign-pointer)  ;; buffer foreign pointer
  (size 0 :type fixnum)               ;; buffer size in bytes
  (gap 0  :type fixnum)               ;; gap byte offset
  (gapsize 0 :type fixnum)              ;; width of gap
  ;;
  (tagbytes 1 :type fixnum)
  (tags nil :type simple-vector)   ;; lot storing tags
  ;;
  (tagstack nil :type list) ;; current tag
  )
(declaim (sb-ext:freeze-type gap-buffer ))
;;==============================================================================
(defmacro in-gb ((name gb) &body body)
  `(let ((,name ,gb))
     (declare (type gap-buffer ,name))
     (with-accessors ((buf. gb-buf) (size. gb-size)
		      (gap. gb-gap) (gapsize. gb-gapsize)
		      (tagbytes. gb-tagbytes) (tags. gb-tags)) ,name
       (declare (type foreign-pointer buf.)
		(type fixnum size. gap. gapsize. tagbytes.)
		(type simple-vector tags.))
       (symbol-macrolet ((point% (+ gap. gapsize.))
			 (bytes-above% (- size. point%)))
	 (declare (type fixnum point% bytes-above%))
	 (macrolet ((gap++ (bytes)
		      `(let ((bytes ,bytes))
			 (decf gapsize. bytes)
			 (incf gap. bytes))))

	   ,@body)))))
;;==============================================================================
(defun gap-buffer-allocate (&optional (size #x10000))
  (let* ((gb
	  (make-gap-buffer%
	  :buf (foreign-alloc :uint8 :count size)
	  :size size
	  :gap 0
	  :gapsize size
	  ;;
	  :tagbytes 2
	  :tags (make-lot :alloc-size 128))))
    (in-gb (gb gb)
      (setf (lot-callback tags.)
	    (lambda (tags newsize)
	      (setf (gb-tags gb) tags)
	      (setf (gb-tagbytes gb)
		    (if (< newsize #x80) ;;7 bits or fewer
			2
			3)))))
    gb))

;;==============================================================================
;; TODO: deallocate tags
(defun gap-buffer-free (gb)
  (foreign-free (gb-buf gb)))

;;==============================================================================
;; calculate size that would accomodate next request of REQUIRED bytes.
;;
(defun gb-determine-size (gb required)
  (in-gb (gb gb)
    (prog ((proposed size.)
	   (available gapsize.))
     again
     (incf proposed proposed); double proposed size
     (when (< (incf available proposed) required)
       (go again))
     (return-from gb-determine-size proposed))))
;;==============================================================================
;; resize
;;
;; Expand buffer, copying data below and above the gap.  Expand gap.
(defun gb-resize (gb required)
  (in-gb (gb gb) 
    (let* ((new-size (gb-determine-size gb required))
	   (new (foreign-alloc :uint8 :count new-size))
	   (difference (- new-size size.))
	   (new-point (+ point% difference)))
      (gb-memcpy new 0  buf. 0  gap.) ;; copy below gap
      (gb-memcpy new new-point  buf. point% bytes-above% )
      (foreign-free buf.)
      (setf buf. new
	    size. new-size)
      (incf gapsize. difference))))
;;==============================================================================
;; assure
;;
;; make sure we have at least BYTES bytes in buffer, expanding if necessary.
;;
(defun gb-assure (gb bytes)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum bytes))
  (in-gb (gb gb)
    (when (< bytes gapsize.)
      (gb-resize gb bytes))
    gb))
;;==============================================================================
;; resize-up
;;
;; Resize buffer to next size up; currently double.
;;
(defun gb-resize-up (gb)
  (gb-resize gb (* 2 (gb-size gb))))

;;==============================================================================
;;
;; Output a byte
;;
;; Resize if needed.  Not optimal - multi-byte output can be faster.
(defun gb-out-byte (gb byte)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type fixnum byte))
  (in-gb (gb gb)
    (when (zerop gapsize.) (gb-resize-up gb))
    (setf (mem-ref buf. :UINT8 gap.) byte)
    (gap++ 1)))

#||
(defun ttt (gb byte)
  (declare (optimize (speed 3) (safety 0)))
  (in-gb (gb gb)
    (let ((ptr (inc-pointer buf. gap.)))
;;      (setf (mem-ref ptr :UINT8) byte )
      (setf (mem-ref buf. :UINT8 gap.) byte )
        ))

  )
||#

(defun gb-ingest-string (gb string)
  (loop for char across string do
       (gb-write-utf8 gb (char-code char))))

(defun gb-dump-range (gb start end)
  (in-gb (gb gb)
    (prog ((index start))
     again
     (when (< index end)
       (mvbind (obj next) (gb-read gb index)
	 (format t "~A" obj)
	 (setf index next))
       (go again))
     (return-from gb-dump-range index))))

(defun gb-dump (gb)
  (in-gb (gb gb)
    (gb-dump-range gb 0 gap.)
    (format t "<GAP~A:~A>"gap. gapsize.)
    (gb-dump-range gb  point% size.)
    gb
))

;;===================================================================================
;; tagging stuff
(defun gb-register (gb object)
  (in-gb (gb gb)
    (let ((tag-id (lot-store tags. object)))
      (tag-id-assigned object tag-id))))


(defmethod tag-id-assigned ((obj string) tag-id))

(defmethod tag-id-query ((obj string) )
  2)


(defparameter *gb* nil)
;;==============================================================================
(defun make-buffer ()
   
   (setf *gb* (gap-buffer-allocate 128))
   (gb-register *gb* "ok" ) ;;string
   *gb*)


(defclass gb-in-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((gb :accessor gb :initarg :gb)
   (pos :accessor pos :initform 0))
  
  )

(defmethod trivial-gray-streams:stream-read-char ((stream gb-in-stream))
  (with-slots (gb) stream
    (%gtb-get-iter-at-mark buffer iter mark); current position
 ;;   (format t "TAGS AT ~A ~A~&" (gti-offset iter) (gti-get-tags iter))
    (if (gti-has-tag iter tag)
	(prog1
	    (gti-get-char iter)
	  (gti-forward-char iter)
	  (gtb-move-mark buffer mark iter))
	:eof)))

(defclass gb-out-stream
    (buffer-stream trivial-gray-streams:fundamental-character-output-stream)
  ((gb :accessor gb :initarg :gb)))

(defmethod trivial-gray-streams:stream-write-char ((stream gb-out-stream) char)
  (with-slots (gb) stream
    (gb-write-utf8 gb (char-code char))
