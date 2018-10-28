(in-package :xcb)
;;=============================================================================
;; atom
;;
(defun easy-atom (c name &optional only-if-exists)
  (let ((atom-cookie
	 (xcb::intern-atom c (if only-if-exists 1 0)
			   (length name)
			   name)))
    (when atom-cookie
      (let ((reply (xcb::intern-atom-reply c atom-cookie
					   (null-pointer))))
	(prog1
	    (mem-ref reply :uint 8)
	  (foreign-free reply))))))

;;=============================================================================
;; DYNARRAY
;;
;; A dynamic array.  Behaves like a normal array of T, except items may be
;; marked as deleted.  A free list (index chain from item 0) allows reuse of
;; slots.
;;
;; Rationale: for tracking slow objects with a lifespan, like window handlers,
;; this is a better solution than a hashtable.
;;
;; TODO: later.
