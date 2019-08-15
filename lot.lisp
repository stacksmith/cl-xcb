(in-package :xcb)

;;==============================================================================
;; A lot is a dynamic indexed storage based on a vector.
;;
;; When an element is added to a lot, an index is returned.  The index may be
;; used to reference the data in the lot using the familiar (aref lot index)
;;
;; An element may be removed from a lot, which frees its slot and index for
;; future use.
;;
(defstruct (lot (:constructor make-lot%))
  (data (make-array 16))
  (free 0 :type fixnum))


;; called only when free is nil and lot is extended
(defun lot-clear (lot from to)
  (with-slots (data free) lot
    (let ((top (1- to)))
      (loop for i :from from :below top
	 do (setf (svref data i) (1+ i)))
      (setf (svref data top) -1
	    free from)))
  lot)

(defun lot-upsize (lot)
  (with-slots (data) lot
    (let* ((old-length (length data))
	   (new-length  (ash old-length 1)))
      (setf data (adjust-array data new-length))
      (lot-clear lot old-length new-length ))))

(defun make-lot ()
  (lot-clear (make-lot%) 0 16))

(defun lot-insert (item lot)
  (when (minusp (lot-free lot))
    (lot-upsize lot))
  (with-slots (data free) lot
    (shiftf free (svref data free) item)))

(defun lot-delete (index lot)
  (with-slots (data free) lot
    (when (integerp (svref data index))
      (error "lot-delete tried to delete an already deleted item"))
    (shiftf (svref data index) free index)))

(defun lot-deref (index lot)
  (svref (lot-data lot) index))
