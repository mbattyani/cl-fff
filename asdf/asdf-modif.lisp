(defun safe-file-write-date (file)
  (let ((result 0))
    (setf result (ignore-errors (file-write-date file)))
    (if result
	result
	(progn (warn "Source file not found: ~s" file)
	       0))))

(defmethod operation-done-p ((o operation) (c component))
  (let ((out-files (output-files o c))
	(in-files (input-files o c)))
    (cond ((and (not in-files) (not out-files))
	   ;; arbitrary decision: an operation that uses nothing to
	   ;; produce nothing probably isn't doing much 
	   t)
	  ((not out-files) 
	   (let ((op-done
		  (gethash (type-of o)
			   (component-operation-times c))))
	     (and op-done
		  (>= op-done
		      (or (apply #'max
				 (mapcar 'file-write-date in-files)) 0)))))
	  ((not in-files) nil)
	  (t
	   (and
	    (every #'probe-file out-files)
	    (> (apply #'min (mapcar 'file-write-date out-files))
	       (apply #'max (mapcar 'safe-file-write-date in-files)) ))))))
