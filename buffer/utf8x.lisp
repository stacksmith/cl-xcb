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
;; Format 5 is used to store 8-bit marks in 2 bytes.
;; Format 6 is for tags.  Low bit is OPENER when 1. 
;;   Tags start with 7-bit tagid in 2 bytes, and may
;;   be expanded to 13 bits in 3 bytes or 19 bits in 4 bytes by rewriting the
;;   buffer and updating TAGBYTES slot which starts at 2.
;;


;;little-endian
;;==============================================================================


(defmacro utf8-encode-1 (val)
  `(values ,val 1))

(defmacro utf8-encode-2 (val)
  `(let ((val ,val))
     (values (logior #x80C0
		     (ash val -6) ;; upper 
		     (ash (logand val #x3F) 8))
	     2)))
(defmacro utf8-encode-3 (val)
  `(let ((val ,val))
     (values (logior #x8080E0
		     (ash val -12)		;; upper
		     (ash (logand val #xFC0) 2) ;; from 6 to 8
		     (ash (logand val #x3F) 16))
	     3)))
(defmacro utf8-encode-4 (val)
  `(let ((val ,val))
     (values (logior #x808080F0
		     (ash val -18)		   ;; from 18 to 0 (3 bits)
		     (ash (logand val #x3F000) -4) ;; from 12 to 8
		     (ash (logand val #xFC0) 10)   ;; from 6 to 16
		     (ash (logand val #x3F) 24)) ;; from 0 to 24
	     4)))
(defmacro utf8-encode (code)
  `(let ((code ,code))
     (cond
       ((< code #x80) (utf8-encode-1 code))
       ((< code #x800)(utf8-encode-2 code))
       ((< code #x10000) (utf8-encode-3 code))
       (t (utf8-encode-4 code)))))


;;==============================================================================
;; tags
;; 
;; 2-byte tag (7 bits + opener)
(defmacro tag-encode-2 (val)
  `(let ((val ,val))
     (logior #x80FC
	     (ash val -6)              ;; fro 6 to 0 (2 bits
	     (ash (logand val #x3F) 8)); from 0 to 8 (6 bits
))
;; 15-bit tag
(defmacro tag-encode-3 (val)
  `(let ((val ,val))
     (values (logior #x8080FC
		     (ash val -12)		;; upper 2
		     (ash (logand val #xFC0) 2) ;; from 6 to 8
		     (ash (logand val #x3F) 16)) ;; from 0 to 16
	     3)))
;; 21-bit tag
(defmacro tag-encode-4 (val)
  `(let ((val ,val))
     (logior #x808080FC
	     (ash val -18)		   ;; from 18 to 0 (2 bits)
	     (ash (logand val #x3F000) -4) ;; from 12 to 8
	     (ash (logand val #xFC0) 10)   ;; from 6 to 16
	     (ash (logand val #x3F) 24))   ;; from 0 to 24
))
(defmacro tag-encode (code tagsize)
  `(let ((code ,code))
     (case ,tagsize
       (2 (-->tag-2 code))
       (3 (-->tag-3 code))
       (4 (-->tag-4 code)))))
;;==============================================================================
;; marks, 8-bit id
(defmacro mark-encode (val)
  `(let ((val ,val))
     (logior #x80F8
	     (ash val -6)                ;;upper 2
	     (ash (logand val #x3F) 8)))) ;;from 0 to 8 (6 bits)

;;==============================================================================
(defun gb-write-utf8 (gb code)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 code))
  (in-gb (gb gb)
    (when (< gapsize. 4) (gb-resize-up gb))
    (mvbind (val bytes) (utf8-encode code)
      (setf (mem-ref buf. :uint32 gap.) val)
      (gap++ bytes))))

;;==============================================================================
;; 
(defun gb-write-tag (gb tagid opener)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 tagid opener))
  (in-gb (gb gb)
    (when (< gapsize. 4) (gb-resize-up gb))
    (let ((bytes tagbytes.))
      (let ((val (tag-encode (logior opener (ash tagid 1)) bytes)))
	(setf (mem-ref buf. :uint32 gap.) val)
	(gap++ bytes)))))

;;==============================================================================
(defun gb-write-mark (gb markid )
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 markid))
  (in-gb (gb gb)
    (when (< gapsize. 2) (gb-resize-up gb))
    (setf (mem-ref buf. :uint16 gap.) (mark-encode  markid))
    (gap++ 2)))

;;==============================================================================
;; Decode first UTF8 byte.  Return the high bits and the number of remaining
;; bytes.  Check for ascii before using this for a fastpath.
#||(defmacro utf8-byte1 (byte)
  (cond ((< byte #xD0) (logand #x1F byte) 2
	 (< byte #xF0) (logand #xF byte) 3
	 (< byte #xF8) (logand #x7 byte) 4
	 t (logand #x7 byte) 0)))
||#
(defun gb-byte-status (byte)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 byte))
  (cond
    ((< byte #x80) (values :character 1))
    ((< byte #xC0) (values :middle    1))
    ((< byte #xD0) (values :character 2)) 
    ((< byte #xF0) (values :character 3))
    ((< byte #xF8) (values :character 4)) ;;
    ((< byte #xFC) (values :mark 2))
    (t (values :tag 0))))



;;==============================================================================
;; Read a variable-size UTF8x item at offset.  Return updated offset and data.
;;
(defun gb-read (gb off)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 off))
  (in-gb (gb gb)
    (let* ((byte (mem-ref buf. :UINT8 off)))
      (if (< byte #x80)
	  (values (code-char byte) (1+ off))
	  (progn
	    (when (< byte #xC0)
	      (error "gb-read: UTF8x leader ~X at ~A is bad." byte off))
	    (gb-read-prim
	     gb off byte
	     (cond
	       ((< byte #xD0) 1) ;; 1011
	       ((< byte #xF0) 2)	;;
	       ((< byte #xF8) 3)	;;
	       ((< byte #xFC)
		(return-from gb-read (gb-read-mark-prim gb off byte)))
	       (t
		(return-from gb-read (gb-read-tag-prim gb off byte))))))))))
;;
;; Primitives called after reading the first UTF8x byte and figuring out
;; what kind we have.  All return updated offset as first value.
;;
;;------------------------------------------------------------------------------
;; CHARACTER.  
(defun gb-read-prim (gb off val cnt)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 off val)
	   (type (or nil U8) cnt))
  (in-gb (gb gb)
    (prog ((ptr (inc-pointer buf. off))
	   (i 1))
       (setf val (logand val (ash  #x3F (- cnt))))
       again
       (let ((byte (mem-ref ptr :uint8 i)))
	 (unless (= #x80 (logand byte #xC0)) (error "gb-read-prim: invalid UTF8 byte ~A at ~A + ~A" byte off i))
	 (setf val (logior (ash val 6) (logand byte #x3F)))
	 (unless (> (incf i) cnt)
	   (go again)))
       (values (+ off i)
	       (code-char val) ))))
;;------------------------------------------------------------------------------
;; TAG.  Return updated offset, tag object and opener bit.
(defun gb-read-tag-prim (gb off val)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 off val))
  (in-gb (gb gb)
    (prog ((ptr (inc-pointer buf. off))
	   (i 1)
	   (cnt tagbytes.))
       (declare (type U32 i))
       (setf val (logand val 3)) ;; start with 2 bits
       again
       (let ((byte (mem-ref ptr :uint8 i)))
	 (unless (= #x80 (logand byte #xC0))
	   (error "gb-read-tag-prim: invalid UTF8 byte ~A at ~A + ~A" byte off i))
	 (setf val (logior (ash val 6) (logand byte #x3F)))
	 (unless (> (incf i) cnt)
	   (go again)))
       (values (+ off i)
	       (svref tags. (ash val -1))
	       (logand val 1)))))
;;------------------------------------------------------------------------------
;;
(defun gb-read-mark-prim (gb off val)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type U32 off val))
  (in-gb (gb gb)
    (let* ((ptr (inc-pointer buf. off))
	   (byte (mem-ref ptr :uint8 1)))
      (unless (= #x80 (logand byte #xC0))
	(error "gb-read-mark-prim: invalid UTF8 byte ~A at ~A + ~A" byte off 1))
      (values (logior (ash (logand val 3) 6)
		      (logand byte #x3F))
	      (+ off 2)))))
;;------------------------------------------------------------------------------
;;

