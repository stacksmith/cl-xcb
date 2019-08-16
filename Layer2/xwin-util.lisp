(in-package :xcb)
;;================================================================================
#|| XWIN-UTIL

X-WINDOWS-specific functions...

||#

;;----------------------------------------------------------------------------
;; TO make a window display a GO-AWAY box and be deletable:
(defun xwin-fix-deletable (id)
  (w-foreign-values (patom :uint32 +WM-DELETE-WINDOW+)
    (check (change-property *conn* PROP-MODE-REPLACE id +WM-PROTOCOLS+				4 32 1 patom))))

(defun xwin-fix-always-on-top (id on/off)
  ;;
  (let ((NET-WM-STATE       (easy-atom *conn* "_NET_WM_STATE"))
	(NET-WM-STATE-ABOVE (easy-atom *conn* "_NET_WM_STATE_ABOVE")))
    (w-foreign-values (event
		       :uint8  EVENT-Client-Message
		       :UINT8  32 ;ICCM
		       :UINT16 0  ;sequence
		       window-t id
		       atom-t   NET-WM-STATE ;type
		       :UINT32 on/off ;NET-WM-STATE-ADD
		       :UINT32 NET-WM-STATE-ABOVE
		       :UINT32 0
		       :UINT32 0
		       :UINT32 0)
      (check (send-event *conn* 0 root-window
			 (logior EVENT-MASK-SUBSTRUCTURE-REDIRECT
				 EVENT-MASK-STRUCTURE-NOTIFY)
			 event)))))

;;==============================================================================
;; comp-string
;;
(let ((xbuf (foreign-alloc :UINT8 :count (+ 1024 8) ))) ;; enough for 256 characters
  (defun comp-string (pic x y penpic string
		      &optional (start 0)(end (length string)))
    (let ((cnt (- end start)))
      ;;      (format t "~%~A ~A ~A" start end cnt) (force-output t)
      ;; XCB/XRENDER composite data starts with a header:
      (setf (mem-ref xbuf :UINT32 0) cnt
	    (mem-ref xbuf :UINT16 4) x 
	    (mem-ref xbuf :UINT16 6) y )
      ;; immediately followed by 32-bit glyph indices (UNICODE in our case).
      (loop for i from 8 by 4
	 for sindex from start below end 
	 for code = (char-code (char string sindex))
	 do (setf (mem-ref xbuf :UINT32 i) code)
	   (glyph-assure *font-normal* code ))
      ;;      (dump xbuf)
      ;; Finally, send XCB request.
      (check (composite-glyphs-32
	      *conn* OP-OVER
	      penpic pic
	      +ARGB32+ (font-glyphset *font-normal*)
	      0 0 (+ 8 (* 4  cnt)) xbuf)))))
