; This file is part of the libmdal library.
; Copyright (c) utz/irrlicht project 2018
; See LICENSE for license details.

;;; # Module MD-NOTE-TABLE
;;; Generate note to frequency divider/lookup value mappings

(module md-note-table
    (md:make-counters
     md:make-dividers-range
     md:make-inverse-dividers-range
     md:make-dividers
     md:make-inverse-dividers
     md:highest-note
     md:lowest-note
     md:note-table-range)

  (import scheme (chicken base) srfi-69)

  (define md:note-names
    (vector "c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"))

  ;; fn = f0 * (2^(1/12))^n
  ;; using c-9 as base note (f0)
  (define (md:offset->freq offset)
    (* 8372.018 (expt (expt 2 (/ 1 12))
                      (- (- 108 offset)))))

  ;; TODO: temporarily hardcode **cpu-speed** until we can pass it down through
  ;; fn call

  (define **cpu-speed** 3500000)

  (define (md:freq->divider freq cycles bits)
    (inexact->exact (round (* (/ (* freq cycles) **cpu-speed**)
                              (expt 2 bits)))))

  (define (md:freq->inverse-divider freq cycles)
    (inexact->exact (round (/ (/ **cpu-speed** cycles)
			      freq))))

  (define (md:offset->divider offset cycles bits)
    (md:freq->divider (md:offset->freq offset) cycles bits))

  (define (md:offset->inverse-divider offset cycles)
    (md:freq->inverse-divider (md:offset->freq offset) cycles))

  (define (md:offset->octave offset) (quotient offset 12))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (md:get-lower-bound cycles bits)
    (do ((offs 0 (+ offs 1)))
	((and (> (md:offset->divider offs cycles bits) 0)
              (not (= (md:offset->divider offs cycles bits)
                      (md:offset->divider (+ offs 1) cycles bits)))
	      (not (= (md:offset->divider (+ offs 1) cycles bits)
		      (md:offset->divider (+ offs 2) cycles bits))))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (md:get-upper-bound-inverse cycles bits)
    (do ((offs (md:get-lower-bound-inverse cycles bits) (+ offs 1)))
	((or (<= (md:offset->inverse-divider offs cycles) 0)
             (= (md:offset->inverse-divider offs cycles)
                (md:offset->inverse-divider (+ offs 1) cycles))
	     (= (md:offset->inverse-divider (+ offs 1) cycles)
		(md:offset->inverse-divider (+ offs 2) cycles)))
	 offs)))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (md:get-lower-bound-inverse cycles bits)
    (do ((offs 0 (+ offs 1)))
	((< (md:offset->inverse-divider offs cycles)
            (expt 2 bits))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (md:get-upper-bound cycles bits)
    (do ((offs 0 (+ offs 1)))
	((>= (md:offset->divider offs cycles bits)
             (expt 2 bits))
	 offs)))

  (define (md:offset->note-name offset)
    (string-append (vector-ref md:note-names (modulo offset 12))
                   (number->string (md:offset->octave offset))))

  ;;;
  (define (md:make-dividers-range cycles beg end rest bits)
    (if (> beg end)
        (list (list "rest" rest))
        (cons (list (md:offset->note-name beg)
                    (md:offset->divider beg cycles bits))
              (md:make-dividers-range cycles (+ 1 beg) end rest bits))))

  ;;;
  (define (md:make-inverse-dividers-range cycles beg end rest)
    (if (> beg end)
	(list (list "rest" rest))
	(cons (list (md:offset->note-name beg)
                    (md:offset->inverse-divider beg cycles))
              (md:make-inverse-dividers-range cycles (+ 1 beg) end rest))))

  ;;; generate a note table with divider->note-name mappings
  ;;; wrapper func for make-dividers-range that will auto-deduce optimal range
  ;;; parameters: cycles - number of cycles in sound generation loop
  ;;;             bits - size of the dividers, as number of bits
  ;;;             rest - the value that represents a rest/note-off
  ;;;             [shift] - number of octaves to shift the table
  (define (md:make-dividers cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (md:make-dividers-range prescaled-cycles
                               (md:get-lower-bound prescaled-cycles bits)
                               (md:get-upper-bound prescaled-cycles bits)
                               rest bits))))

  ;;; generate a note table with inverse divider->node-name mappings
  ;;; ie. dividers are countdown values
  ;;; see `md:make-dividers` for further documentation
  (define (md:make-inverse-dividers cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (md:make-inverse-dividers-range
	prescaled-cycles
        (md:get-lower-bound-inverse prescaled-cycles bits)
        (md:get-upper-bound-inverse prescaled-cycles bits)
        rest))))

  ;;; generate a note table with simple note-name->index mappings
  ;;; beg   lowest note, as offset from c-0
  ;;; end   highest note, as offset from c-0
  ;;; first-index   index of the lowest note
  ;;; rest-index    index of the rest/note-off
  (define (md:make-counters beg end first-index rest-index)
    (letrec ((mkcounters
              (lambda (beg end first rest)
		(if (> beg end)
                    (list (list "rest" rest))
                    (cons (list (md:offset->note-name beg) first)
                          (mkcounters (+ 1 beg) end (+ 1 first) rest))))))
      (alist->hash-table (mkcounters beg end first-index rest-index))))

  ;;; returns the lowest note in the given note table
  (define (md:lowest-note table)
    (letrec
	((try-lower
	  (lambda (offset tbl)
            (if (hash-table-ref/default tbl (md:offset->note-name offset) #f)
		(md:offset->note-name offset)
		(try-lower (+ offset 1) tbl)))))
      (try-lower 0 table)))

  ;;; returns the highest note in the given note table
  (define (md:highest-note table)
    (letrec
	((try-upper
	  (lambda (offset tbl)
            (if (hash-table-ref/default tbl (md:offset->note-name offset) #f)
		(md:offset->note-name offset)
		(try-upper (- offset 1) tbl)))))
      (try-upper 131 table)))

  ;;; returns (lowest highest) notes in the given note table
  (define (md:note-table-range table)
    (list (md:lowest-note table) (md:highest-note table)))

  )  ;; end module md-note-tables
