;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.


;;; Mathematical helper procedures.
(module bt-maths
    *
  (import scheme (chicken base) (chicken string)
	  srfi-1)

  ;;; Scale the list of FIELD-VALUES to fit into the integer range AMIN,AMAX.
  (define (scale-values field-values amin amax)
    (let ((rmin (min amin amax))
	  (rmax (max amin amax)))
      (if (every null? field-values)
	  field-values
	  (let ((minval (apply min (remove null? field-values)))
		(maxval (apply max (remove null? field-values))))
	    (map (lambda (x)
		   (if (null? x)
		       '()
		       (if (= minval maxval)
			   (cond
			    ((< x rmin) minval)
			    ((> x rmax) maxval)
			    (else x))
			   (inexact->exact (round (+ (/ (* (- rmax rmin)
							   (- x minval))
							(- maxval minval))
						     rmin))))))
		 field-values)))))

  ;;; Interpolate the list of integer values VALS. The input list may contain
  ;;; `null` values. By default, linear interpolation is used. This may be
  ;;; overridden by specifying TYPE. Currently, the only other available
  ;;; interpolation type is `cosine`.
  (define (interpolate vals #!optional (type 'linear))
    (cond
     ((< (length (remove null? vals)) 2) vals)
     ((null? (car vals))
      (append (take-while null? vals)
	      (interpolate (drop-while null? vals) type)))
     (else
      (case type
	((linear)
	 (let ((start-val (car vals))
	       (end-val (car (drop-while null? (cdr vals))))
	       (len (+ 1 (length (take-while null? (cdr vals))))))
	   (if (< len 2)
	       (cons start-val (interpolate (cdr vals) type))
	       (append
		(cons start-val
		      (map (o inexact->exact round)
			   (iota (sub1 len)
				 (+ start-val (/ (- end-val start-val) len))
				 (/ (- end-val start-val) len))))
		(interpolate (drop vals len) type)))))
	((cosine)
	 (let* ((start-val (car vals))
		(end-val (car (drop-while null? (cdr vals))))
		(len (+ 1 (length (take-while null? (cdr vals)))))
		(cosine-interpolate
		 (lambda (mu)
		   (let ((mu2 (/ (- 1 (cos (* mu 3.1415926535)))
				 2)))
		     (inexact->exact (round (+ (* start-val (- 1 mu2))
					       (* end-val mu2))))))))
	   (if (< len 2)
	       (cons start-val (interpolate (cdr vals) type))
	       (append
		(cons start-val
		      (map cosine-interpolate (iota (sub1 len) 0 (/ 1 len))))
		(interpolate (drop vals len) type)))))
	;; spline, trigonometric
	((polynominal) vals)
	(else (error 'interpolate
		     (string-append "Unknown interpolation type "
				    (->string type))))))))

  ) ;; end module bt-maths
