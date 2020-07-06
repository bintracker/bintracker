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

(module algocomp
    (algocomp::bjorklund-rhythm-generator)

  (import scheme (chicken base) srfi-1)

  ;;; Euclidean/Bjorklund rhythm generator
  ;;; G. T. Toussaint, The Euclidean algorithm generates traditional musical
  ;;; rhythms, Proceedings of BRIDGES: Mathematical Connections in Art, Music,
  ;;; and Science, Banff, Alberta, Canada, July 31 to August 3, 2005, pp. 47–56.
  ;;; http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf
  (define (algocomp::bjorklund-rhythm-generator len weight
						#!optional (falsy '()))
    (if (>= weight len)
	(error "len must be equal to or greater than weight")
	(letrec
	    ((run-bjorklund
	      (lambda (pulses remainder)
		(if (<= (length remainder) 1)
		    (flatten (append pulses remainder))
		    (if (>= (length remainder) (length pulses))
			(run-bjorklund (map (lambda (x)
					      (append x (car remainder)))
					    pulses)
				       (drop remainder (length pulses)))
			(run-bjorklund (map (lambda (x)
					      (append x (car remainder)))
					    (take pulses (length remainder)))
				       (drop pulses (length remainder))))))))
	  (map (lambda (x) (or x falsy))
	       (run-bjorklund (make-list weight '(#t))
			      (make-list (- len weight) '(#f)))))))

  ) ;; end module algocomp
