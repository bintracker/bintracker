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
(module prng
    (prng::middle-square
     prng::middle-square-weyl-seq
     prng::blum-blum-shub
     prng::pcg
     prng::xorshift64
     prng::randu
     prng::el-cheapo-zx
     prng::dmg-noise
     prng::sid-noise
     prng::tia-noise
     prng::pi-digits
     prng::info)

  (import scheme (chicken base) (chicken bitwise) (chicken random)
	  srfi-1 srfi-13)

  ;;; > Anyone who considers arithmetical methods of producing random digits is,
  ;;; > of course, in a state of sin.
  ;;;
  ;;; *J. v. Neumann*

  (define-constant pi 3.14159265358979323846)
  (define-constant e 2.71828182845904523536)

  ;;; Alias for `arithmetic-shift`
  (define << arithmetic-shift)

  (define (rand bits)
    (pseudo-random-integer (sub1 (expt 2 bits))))

  ;;; Addition of arbitrary-sized integers.
  (define (uint-arithmetic operator integer-bits operands)
    (bitwise-and (apply operator operands)
		 (sub1 (expt 2 integer-bits))))

  (define (add-uint integer-bits operands)
    (uint-arithmetic + integer-bits operands))

  (define (mul-uint integer-bits operands)
    (uint-arithmetic * integer-bits operands))

  ;;; 8-bit unsigned addition
  (define (add/8 . operands)
    (add-uint 8 operands))

  ;;; 16-bit unsigned addition
  (define (add/16 . operands)
    (add-uint 16 operands))

  ;;; 32-bit unsigned addition
  (define (add/32 . operands)
    (add-uint 32 operands))

  ;;; 64-bit unsigned addition
  (define (add/64 . operands)
    (add-uint 64 operands))

  ;;; 8-bit unsigned multiply
  (define (mul/8 . operands)
    (mul-uint 8 operands))

  ;;; 16-bit unsigned multiply
  (define (mul/16 . operands)
    (mul-uint 16 operands))

  ;;; 32-bit unsigned multiply
  (define (mul/32 . operands)
    (mul-uint 32 operands))

  ;;; 64-bit unsigned multiply
  (define (mul/64 . operands)
    (mul-uint 64 operands))

  ;;; 8-bit rotation
  (define (rol/8 i amount)
    (let ((j (bitwise-and i #xff)))
      (if (zero? amount)
	  j
	  (rol/8 (bitwise-ior (<< (bitwise-and j #x80)
				  -7)
			      (<< j 1))
		 (sub1 amount)))))

  ;;; Collect bits represented the list of zeroes and ones BSTREAM into
  ;;; integers of size BITS.
  (define (bitstream->int bstream bits)
    (letrec* ((collect-bits (lambda (bs val count)
			      (if (zero? count)
				  val
				  (collect-bits (cdr bs)
						(bitwise-ior (car bs)
							     (* 2 val))
						(sub1 count)))))
	      (collect-vals (lambda (bs)
			      (if (null? bs)
				  '()
				  (cons (collect-bits bs 0 bits)
					(collect-vals (drop bs bits))))))
	      (pad (- bits (modulo (length bstream) bits))))
      (collect-vals (if (zero? (modulo (length bstream) bits))
			bstream
			(append bstream
				(make-list (- bits (modulo (length bstream)
							   bits))
					   0))))))

  ;;; Adjust the size of the integer N by performing a logical bitshift. WANT
  ;;; is the desired size (number of bits) of the resulting integer, and HAVE is
  ;;; the actual size of integer N.
  (define (adjust-integer-size n want have)
    (bitwise-and (arithmetic-shift n (- want have))
    		 (sub1 (expt 2 want))))

  ;; ;;; Calculate the number of 1-bits in the integer x.
  ;; (define (population-count x)
  ;;   (arithmetic-shift (mul/64 #x1111111111111111
  ;; 			      (bitwise-and #x1111111111111111
  ;; 					   (mul/64 x #x2000400080010)))
  ;; 		      -60))

  ;;; A generator based on the Middle-Square Method, as devised by John von
  ;;; Neumann in 1946. It is statistically very poor and may break with
  ;;; unsuitable seeds. This implementation deviates from von Neumann's design
  ;;; by considering only the lower half of the 8 digits extracted from the
  ;;; 16-digit square. This is done to mitigate the effects of the convergence
  ;;; towards lower number sequences that is a common trait of the Middle Square
  ;;; method.
  (define (prng::middle-square amount maxint #!optional (seed 12345678))
    (letrec ((gen-next
	      (lambda (n s)
		(if (zero? n)
		    '()
		    (let ((squared-str (string-pad (number->string (expt s 2))
						   16 #\0)))
		      (cons (string->number
			     (string-drop (string-take squared-str 12) 8))
			    (gen-next
			     (sub1 n)
			     (string->number
			      (string-append (string-take squared-str 4)
					     (string-take-right squared-str
								4))))))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x 10000)))))
	   (gen-next amount seed))))

  ;;; A variation of von Neumann's Middle Square Method that applies a Weyl
  ;;; sequence to the Middle Square generator, developed by Bernard Widynski.
  ;;; Very good statistical quality. See https://arxiv.org/abs/1704.00358v4
  (define (prng::middle-square-weyl-seq amount maxint
					#!optional (seed (rand 32))
					(magic #xb5ad4eceda1ce2a9))
    (letrec ((gen-next
	      (lambda (n x w)
		(if (zero? n)
		    '()
		    (let* ((next-w (add/64 w magic))
			   (x-squared+w (add/64 next-w (expt x 2)))
			   (next-x (bitwise-ior
				    (<< x-squared+w -32)
				    (bitwise-and #xffffffffffffffff
						 (<< x-squared+w 32)))))
		      (cons (bitwise-and next-x #xffffffff)
			    (gen-next (sub1 n) next-x next-w)))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x #x100000000)))))
	   (gen-next amount seed 0))))

  ;;; Blum Blum Shub Generator. SEED must be greater than 1, and neither p nor q
  ;;; may be a factor of SEED. The generator will replace invalid seeds with a
  ;;; random seed if SEED fails to satisfy these conditions.
  ;;; https://en.wikipedia.org/wiki/Blum_Blum_Shub
  (define (prng::blum-blum-shub amount maxint
  				#!optional (seed (rand 32)) (p 5651) (q 5623))
    (letrec* ((mod (* p q))
  	      (make-bits (lambda (n seed)
  			   (if (zero? n)
  			       '()
  			       (let* ((next-seed (modulo (expt seed 2) mod)))
  				 (cons (bitwise-and next-seed 1)
  				       (make-bits (sub1 n) next-seed)))))))
      (if (and (> seed 1)
	       (not (zero? (modulo seed p)))
	       (not (zero? (modulo seed q))))
	  (map (lambda (x)
		 (inexact->exact (round (* maxint (/ x #x10000000000000000)))))
	       (bitstream->int (map (cute bitwise-and 1 <>)
				    (make-bits (* amount 64) seed))
			       64))
	  (prng::blum-blum-shub amount maxint (rand 32) p q))))

  ;; TODO tends to break after ~60 values.
  ;;; Permuted Congruential Generator, https://www.pcg-random.org/
  ;;; `seed` and `increment` must be a 64-bit integer seed. `bits` must be
  ;;; between 1 and 32.
  (define (prng::pcg amount maxint
		     #!optional (seed (rand 64)) (increment (rand 64)))
    (letrec*
	((inc (bitwise-ior increment 1))
	 (gen-next
	  (lambda (n state)
	    (if (zero? n)
		'()
		(let ((xor-shifted (<< (bitwise-xor state (<< state -18))
				       -27))
		      (rot (<< state -59)))
		  (cons (bitwise-and
			 #xffffffff
			 (bitwise-ior (<< xor-shifted (- rot))
				      (<< xor-shifted
					  (bitwise-ior 31 (- rot)))))
			(gen-next (sub1 n)
				  (add/64 (mul/64 state 6364136223846793005)
					  state))))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x #x100000000)))))
	   (cdr (gen-next (+ 1 amount) seed)))))

  ;;; Classic non-scrambled 64-bit Xorshift generator, as developed by George
  ;;; Marsaglia. See https://en.wikipedia.org/wiki/Xorshift
  (define (prng::xorshift64 amount maxint #!optional (state (rand 64)))
    (letrec ((make-values
	       (lambda (amount state)
		 (if (zero? amount)
		     '()
		     (let* ((x1 (bitwise-xor
				 state
				 (bitwise-and #xffffffffffffffff
					      (arithmetic-shift state 13))))
			    (x2 (bitwise-xor x1 (arithmetic-shift x1 -17)))
			    (next-state (bitwise-xor
					 x2
					 (bitwise-and
					  #xffffffffffffffff
					  (arithmetic-shift x2 5)))))
		       (cons next-state
			     (make-values (sub1 amount) next-state)))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x #x10000000000000000)))))
	   (make-values amount state))))

  ;;; A notoriously flawed LCG-type PRNG developed by IBM in the 1960s.
  (define (prng::randu amount maxint #!optional (seed (rand 32)))
    (letrec ((make-values
    	      (lambda (v0 n)
    		(if (zero? n)
    		    '()
    		    (let ((v1 (bitwise-and #xffffffff
    					   (* 65539 (modulo v0 (expt 2 31))))))
    		      (cons v1 (make-values v1 (sub1 n))))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x #x100000000)))))
    	   (make-values (bitwise-ior seed 1) amount))))

  ;;; Very fast but extremely poor 8-bit PRNG used to generate noise in various
  ;;; ZX Spectrum beeper engines.
  (define (prng::el-cheapo-zx amount maxint
			      #!optional (seed (rand 16)) (magic #x2175))
    (letrec ((make-values
	      (lambda (amount state)
		(if (zero? amount)
		    '()
		    (let* ((next-accu (add/16 state magic))
			   (next-state (add/16 (<< (rol/8 (<< next-accu -8) 1)
						   8)
					       (bitwise-and next-accu #xff))))
		      (cons (quotient next-state #x100)
			    (make-values (sub1 amount) next-state)))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x #x100)))))
	   (make-values amount seed))))

  ;;; Returns a Linear feedback shift register implementation. SIZE shall be
  ;;; the number of bits of the LFSR, and FEEDBACK-FN shall be a procedure that
  ;;; takes an integer state as argument and returns the next state.
  (define (make-lfsr size feedback-fn)
    (lambda (amount maxint #!optional (seed (rand (sub1 size))))
      (letrec ((make-values
		(lambda (amount state)
		  (if (zero? amount)
		      '()
		      (let ((next-state (feedback-fn state)))
			(cons next-state
			      (make-values (sub1 amount) next-state)))))))
	(if (= 1 maxint)
	    (map (cute bitwise-and 1 <>)
		 (cons seed
		       (make-values (sub1 amount)
				    (bitwise-and (sub1 (expt 2 size))
						 (bitwise-ior seed 1)))))
	    (let ((bits (inexact->exact
			 (+ 1 (floor (/ (log maxint) (log 2)))))))
	      (map (lambda (x)
		     (inexact->exact (round (* maxint (/ x (expt 2 bits))))))
		   (bitstream->int
		    (map (cute bitwise-and 1 <>)
			 (cons seed
			       (make-values
				(* (sub1 amount) bits)
				(bitwise-and (sub1 (expt 2 size))
					     (bitwise-ior seed 1)))))
		    bits)))))))

  ;;; A PRNG based on the noise waveform generator of the Gameboy APU, which is
  ;;; a 15-bit LFSR with a tap at bit 1.
  (define prng::dmg-noise
    (make-lfsr 15
	       (lambda (in)
		 (bitwise-ior (bitwise-and #x3fff (quotient in 2))
  			      (arithmetic-shift
  			       (bitwise-xor (modulo in 2)
  					    (modulo (quotient in 2) 2))
  			       14)))))

  ;;; A PRNG based on the noise waveform on the MOS 6581/8580 Sound Interface
  ;;; Device, which is a Fibonacci LFSR using the feedback polynomial
  ;;; x^22 + x^17 + 1. See http://www.sidmusic.org/sid/sidtech5.html. For added
  ;;; authenticity, initialize SEED to #x7ffff8.
  (define prng::sid-noise
    (make-lfsr 23
	       (lambda (in)
		 (bitwise-and #x7fffff
			      (bitwise-ior (arithmetic-shift in -1)
			       (arithmetic-shift
				(bitwise-xor (arithmetic-shift in -1)
					     (arithmetic-shift in -6))
				22))))))

  ;;; A PRNG based on the noise waveform (AUDCx = 8) on the Atari VCS/2600,
  ;;; which is a 9-bit LFSR with a tap at bit 4, resulting in a period of 511.
  ;;; For added authenticity, initialize SEED to #x1ff.
  (define prng::tia-noise
    (make-lfsr 9
	       (lambda (in)
		 (bitwise-and #x1ff
			      (bitwise-ior (arithmetic-shift in -1)
			       (arithmetic-shift
				(bitwise-and 1 (bitwise-xor
						in
						(arithmetic-shift in -4)))
				8))))))

  ;;; Not an actual PRNG. Creates random-looking integer sequences from pi.
  ;;; Uses the Bailey–Borwein–Plouffe formula to calculate hexadecimal digits
  ;;; of pi. SEED is the initial fractional digit index, ie. SEED = 0 starts
  ;;; extracting digits from the first fractional digit.
  (define (prng::pi-digits amount maxint #!optional (seed (rand 10)))
    (letrec* ((powermod (lambda (base exp mod)
			  (letrec
			      ((pm (lambda (b e acc)
				     (if (zero? e)
					 acc
					 (pm (modulo (* b b) mod)
					     (arithmetic-shift e -1)
					     (if (even? e)
						 acc
						 (modulo (* b acc) mod)))))))
			    (pm base exp 1))))
	      (fractional-part (lambda (r) (- r (floor r))))
	      (finite-sum (lambda (s k n denom)
			    (if (> k n)
				(cons s denom)
				(finite-sum
				 (fractional-part
				  (exact->inexact
				   (+ s (/ (powermod 16 (- n k) denom)
					   denom))))
				 (+ k 1)
				 n
				 (+ denom 8)))))
	      (infinite-sum (lambda (s num denom lastfrac)
			      (let ((newfrac (exact->inexact (/ num denom))))
				(if (= lastfrac newfrac)
				    s
				    (infinite-sum (+ s newfrac)
						  (/ num 16)
						  (+ denom 8)
						  newfrac)))))
	      (sigma (lambda (n j)
		       (let ((finite (finite-sum 0.0 0 n j)))
			 (fractional-part
			  (infinite-sum (car finite)
					(/ 1 16)
					(cdr finite)
					0)))))
	      (nth-digit (lambda (n)
			   (inexact->exact
			    (floor (* 16 (fractional-part
					  (- (* 4 (sigma (sub1 n) 1))
					     (* 2 (sigma (sub1 n) 4))
					     (sigma (sub1 n) 5)
					     (sigma (sub1 n) 6))))))))
	      (digit->bin-lst (lambda (d)
				(list (quotient (bitwise-and d 8) 8)
				      (quotient (bitwise-and d 4) 4)
				      (quotient (bitwise-and d 2) 2)
				      (bitwise-and d 1))))
	      (bits (inexact->exact (+ 1 (floor (/ (log maxint) (log 2)))))))
      (map (lambda (x)
	     (inexact->exact (round (* maxint (/ x (expt 2 bits))))))
	   (bitstream->int
	    (flatten (map (o digit->bin-lst nth-digit)
			  (iota (if (zero? (modulo bits 4))
				    (* amount (quotient bits 4))
				    (+ 1 (* amount (quotient bits 4))))
				(+ 1 seed)
				1)))
	    bits))))

  ;;; Retrieve information on the pseudo-random number generators available in
  ;;; this package. Call with no arguments to retrieve the complete list. Call
  ;;; with a symbol naming a procedure in this package to retrieve the
  ;;; documentation for that procedure.
  (define (prng::info . args)
    (let ((prngs
	   `((prng::middle-square
	      .
	      ,(string-append
		"A generator based on the Middle-Square Method, as devised by"
		" John von Neumann in 1946. It is statistically very poor and"
		" may break with unsuitable seeds. This implementation deviates"
		" from von Neumann's design by considering only the lower half"
		" of the 8 digits extracted from the 16-digit square. This is"
		" done to mitigate the effects of the convergence towards lower"
		" number sequences that is a common trait of the Middle Square"
		" method."))
	     (prng::middle-square-weyl-seq
	      .
	      ,(string-append
		"A variation of von Neumann's Middle Square Method that applies"
		" a Weyl sequence to the Middle Square generator, developed by"
		" Bernard Widynski. Very good statistical quality."
		" See https://arxiv.org/abs/1704.00358v4"))
	     (prng::blum-blum-shub
	      .
	      ,(string-append
		"The original 1986 design by Lenore Blum, Manuel Blum and"
		" Michael Shub. SEED must be greater than 1, and neither p nor"
		" q may be a factor of SEED. The generator will replace invalid"
		" seeds with a random seed if SEED fails to satisfy these"
		" conditions."
		" See https://en.wikipedia.org/wiki/Blum_blum_shub"))
	     (prng::pcg
	      .
	      ,(string-append "Permuted Congruential Generator."
			      " See https://www.pcg-random.org/"))
	     (prng::xorshift64
	      .
	      ,(string-append
		"Classic non-scrambled 64-bit Xorshift generator, as developed"
		" by George Marsaglia. See"
		" https://en.wikipedia.org/wiki/Xorshift"))
	     (prng::randu
	      .
	      ,(string-append "A notoriously flawed LCG-type PRNG developed by"
			      " IBM in the 1960s."))
	     (prng::el-cheapo-zx
	      .
	      ,(string-append
		"Very fast but extremely poor 8-bit PRNG used to generate noise"
		" in various ZX Spectrum beeper engines."))
	     (prng::dmg-noise
	      .
	      ,(string-append
		"A PRNG based on the noise waveform generator of the Gameboy"
		" APU, which is a 15-bit LFSR with a tap at bit 1."))
	     (prng::sid-noise
	      .
	      ,(string-append
		"A PRNG based on the noise waveform on the MOS"
		" 6581/8580 Sound Interface Device, which is a Fibonacci LFSR"
		" using the feedback polynomial x^22 + x^17 + 1. See"
		" http://www.sidmusic.org/sid/sidtech5.html. For added"
		" authenticity, initialize SEED to #x7ffff8."))
	     (prng::tia-noise
	      .
	      ,(string-append
		"A PRNG based on the noise waveform (AUDCx = 8) on"
		" the Atari VCS/2600, which is a 9-bit LFSR with a tap at bit"
		" 4, resulting in a period of 511. For added authenticity,"
		" initialize SEED to #x1ff."))
	     (prng::pi-digits
	      .
	      ,(string-append
		"Not an actual PRNG. Creates random-looking integer sequences"
		" from pi. Uses the Bailey–Borwein–Plouffe formula to calculate"
		" hexadecimal digits of pi. SEED is the initial fractional"
		" digit index, ie. SEED = 0 starts extracting digits from the"
		" first fractional digit.")))))
      (if (null? args) prngs (alist-ref (car args) prngs))))

  ) ;; end module prng
