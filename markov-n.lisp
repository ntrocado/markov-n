(in-package :markov-n)

(defparameter *table* (make-hash-table))
(defparameter *rand* 0)

(defun combine-bytes (bytes)
  (loop :for n :downfrom (1- (length bytes))
	:for b :in bytes
	:summing (* b (expt 65536 n))))

(defun get-chunk (file)
  (loop :for n :in (wav:read-wav-file file))
  :when (equalp (getf n :chunk-id) "data")
    :return (getf n :chunk-data))

(defun wave-frames (file)
  (let ((frames nil))
    (cl-wave:with-open-wave (wav file)
      (format t "channels: ~a.~%" (cl-wave:get-num-channels wav))
      (format t "sample rate: ~a.~%" (cl-wave:get-sample-rate wav))
      (format t "bitrate: ~a.~%" (cl-wave:get-sample-width wav :bits t))
      (setf frames (cl-wave:get-frames wav)))
    frames))
					;    (mapcar (lambda (x) (+ x 32768)) frames)))

(defun read-file (file n)
  (let* ((data (coerce (wave-frames file) 'vector))
	 (first-bytes (subseq data 0 n))
	 (data-seq (concatenate 'vector first-bytes data))
	 (data-length (length data-seq)))
    (format t "File read. Size is ~a samples.~%" data-length)
    (format t "~a~%" (make-string 100 :initial-element #\_))
    (loop :with counter-step := (ceiling (/ data-length 100))
	  :for next :across (subseq data-seq n)
	  :for i :from n
	  :for subseq-start := (- i n)
	  :do (let* ((data-subseq (subseq data-seq subseq-start i))
		     (key (combine-bytes (coerce data-subseq 'list))))
		(if (not (hash-table-p (gethash key *table*)))
		    (progn
		      (setf (gethash key *table*) (make-hash-table))
		      (setf (gethash next (gethash key *table*)) 1))
		    (if (not (gethash next (gethash key *table*)))
			(setf (gethash next (gethash key *table*)) 1)
			(incf (gethash next (gethash key *table*)))))
		(when (zerop (mod i counter-step))
		  (progn (format t ".") (finish-output)))))))

(defun get-next (current)
  (let* ((hash-size (hash-table-count (gethash current *table*)))
	 (all-keys (alexandria:hash-table-keys (gethash current *table*)))
	 (all-values (alexandria:hash-table-values (gethash current *table*)))
	 (value-total (reduce #'+ all-values))
	 (prob-array (make-array hash-size
				 :initial-contents (mapcar (lambda (x) (/ x value-total))
							   all-values)))
	 (values-array (make-array hash-size
				   :initial-contents all-keys))
	 (rp (make-discrete-random-var prob-array values-array)))
    (funcall rp)))

(defun make-wave-file (file &key frames
			      (channels 1)
			      (sample-rate 44100)
			      (bitrate 16))
  (cl-wave:with-open-wave 
      (wav file :direction :output)
    (cl-wave:set-num-channels wav channels)
    (cl-wave:set-sample-rate wav sample-rate)
    (cl-wave:set-sample-width wav (/ bitrate 8))
    (cl-wave:set-frames wav frames))) 

(defun write-file (file n size &optional (initial-list (make-list (1+ n) :initial-element 0)))
  (let ((buffer (subseq initial-list 0 n))
	(frames nil))
    (format t "~a~%" (make-string 100 :initial-element #\_))
    (dotimes (i size t)
      (let ((new-byte (if (> (the fixnum i) (the fixnum n))
			  (get-next (combine-bytes buffer))
			  (elt initial-list i))))
	(push new-byte frames)
	(setf buffer (rest (nconc buffer (list new-byte))))
	(when (zerop (ceiling (mod i (/ size 100))))
	  (progn (format t ".") (finish-output)))))
    (make-wave-file file :frames (reverse frames))))

(defun main (order input output size)
  "Analyses <input> file and creates a new <output> file with <size> bytes."
  (defparameter *table* (make-hash-table))
  (read-file input order)
  (format t "~%Creating new file...~%")
  (let ((first-bytes (subseq (wave-frames input) 0 (1+ order))))
    (write-file output order size first-bytes)))

(defun write-wave (input output)
  "Just for testing the wave library."
  (cl-wave:with-open-wave
      (wav output :direction :output)
    (cl-wave:set-num-channels wav 1)
    (cl-wave:set-sample-rate wav 44100)
    (cl-wave:set-sample-width wav 2)
    (cl-wave:set-frames wav (wave-frames input))))
