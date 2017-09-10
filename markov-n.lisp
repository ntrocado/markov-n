(in-package :markov-n)

(defparameter *table* (make-hash-table))
(defparameter *rand* 0)

(defun combine-bytes (bytes)
  (loop :for n :downfrom (1- (length bytes))
     :for b :in bytes
     :summing (* b (expt 65536 n))))
  
(defun read-file (file n)
  (with-open-file (data
		   file
		   :direction :input
		   :element-type '(signed-byte 16))
    (let* ((data-seq (make-sequence 'list (file-length data)))
	   (data-length (length data-seq)))
      (read-sequence data-seq data)
      (format t "File read. Size is ~a samples.~%" data-length)
      (format t "~a~%" (make-string 100 :initial-element #\_))
      (nconc data-seq (subseq data-seq 0 n))
      (loop :with counter-step := (ceiling (/ data-length 100))
      	 :for next :in (subseq data-seq n)
      	 :for i :from n
      	 :for subseq-start := (- i n)
      	 :do (let* ((data-subseq (subseq data-seq subseq-start i))
      	 	    (key (combine-bytes data-subseq)))
      	       (if (not (hash-table-p (gethash key *table*)))
      		   (progn
      		     (setf (gethash key *table*) (make-hash-table))
      		     (setf (gethash next (gethash key *table*)) 1))
      	 	   (if (not (gethash next (gethash key *table*)))
		       (setf (gethash next (gethash key *table*)) 1)
		       (incf (gethash next (gethash key *table*)))))
      	       (when (zerop (mod i counter-step))
		 (progn (format t ".") (finish-output))))))))

(defun get-next (current)
  (let* ((hash-size (hash-table-count (gethash current *table*)))
	 (all-keys (alexandria:hash-table-keys (gethash current *table*)))
	 (all-values (alexandria:hash-table-values (gethash current *table*)))
	 (value-total (reduce #'+ all-values))
	 (prob-array (make-array hash-size
				 :initial-contents (mapcar
						    #'(lambda (x) (/ x value-total))
						    all-values)))
	 (values-array (make-array hash-size
				   :initial-contents all-keys))
	 (rp (make-discrete-random-var prob-array values-array)))
    (funcall rp)))

(defun write-file (file n size &optional (initial-list (make-list (1+ n) :initial-element 0)))
  (with-open-file (out
		   file
		   :direction :output
		   :element-type '(signed-byte 16)
		   :if-exists :supersede)
    (let ((buffer (subseq initial-list 0 n)))
      (format t "~a~%" (make-string 100 :initial-element #\_))
      (dotimes (i size t)
	(let ((new-byte (if (> (the fixnum i) (the fixnum n))
			    (get-next (combine-bytes buffer))
			    (elt initial-list i))))
	  (write-byte new-byte out)
	  (setf buffer (rest (nconc buffer (list new-byte))))
	  (when (zerop (ceiling (mod i (/ size 100))))
	    (progn (format t ".") (finish-output))))))))

(defun main (order input output size)
  "Analyses <input> file and creates a new <output> file with <size> bytes."
  (read-file input order)
  (format t "~%Creating new file...~%")
  (let ((first-bytes (make-sequence 'list (1+ order))))
    (with-open-file (file input :direction :input :element-type '(signed-byte 16))
      (read-sequence first-bytes file))    
    (write-file output order size first-bytes))
  (values))


