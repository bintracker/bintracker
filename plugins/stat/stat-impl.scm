;; Copyright (c) 2020 Michael Neidel

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Various pseudo-random number generators.
(module stat
    (stat::gauss)

  (import scheme (chicken base) (chicken random)
	  srfi-1 srfi-13)

  (define-constant pi 3.14159265358979323846)
  (define-constant e 2.71828182845904523536)

  (define (rnd-well amount max)
    (letrec ((make-values (lambda (n)
			    (if (zero? n)
				'()
				(cons (pseudo-random-integer (+ 1 max))
				      (make-values (sub1 n)))))))
      (make-values amount)))


  ;;; Generate normally distributed pseudo-random integers with a mean of
  ;;; (2^(bits-1) / 2) and a standard variance of (2^(bits - 1)/4), bounded at
  ;;; (0, 2^bits - 1), using UNIFORM-GENERATOR to generate (presumably)
  ;;; uniformly distributed input. UNIFORM-GENERATOR may be a procedure taking
  ;;; two arguments and outputting a list of integers. The first argument to the
  ;;; procedure is the amount of numbers to generate, the second argument is the
  ;;; integer size of the generated integers.
  (define (stat::gauss amount maxint
		       #!optional (uniform-generator rnd-well))
    (letrec* ((uniform-nums (list->vector
			     (map (lambda (n)
				    (exact->inexact (/ n #x100000000)))
				  (uniform-generator (* 2 amount)
						     #xffffffff))))
	      (make-gauss
	       (lambda (n)
		 (if (zero? n)
		     '()
		     (cons
		      (* (sqrt (* -2 (log (vector-ref uniform-nums (sub1 n)))))
			 (cos (* 2 pi (vector-ref uniform-nums
						  (+ n #x7ffffffff)))))
		      (make-gauss (sub1 n)))))))
      (map (lambda (r)
	     (cond
	      ((<= r -4) 0)
	      ((>= r 4) maxint)
	      (else (inexact->exact (round (+ (/ maxint 2)
					      (* 0.25 r (/ maxint 2))))))))
	   (make-gauss amount))))

  ) ;; end module stat
