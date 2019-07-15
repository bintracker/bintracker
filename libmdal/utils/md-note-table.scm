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

  (define (md:freq->divider freq cycles bits cpu-speed)
    (inexact->exact (round (* (/ (* freq cycles) cpu-speed)
                              (expt 2 bits)))))

  (define (md:freq->inverse-divider freq cycles cpu-speed)
    (inexact->exact (round (/ (/ cpu-speed cycles)
			      freq))))

  (define (md:offset->divider offset cycles bits cpu-speed)
    (md:freq->divider (md:offset->freq offset) cycles bits cpu-speed))

  (define (md:offset->inverse-divider offset cycles cpu-speed)
    (md:freq->inverse-divider (md:offset->freq offset) cycles cpu-speed))

  (define (md:offset->octave offset) (quotient offset 12))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (md:get-lower-bound cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((and (> (md:offset->divider offs cycles bits cpu-speed) 0)
              (not (= (md:offset->divider offs cycles bits cpu-speed)
                      (md:offset->divider (+ offs 1) cycles bits cpu-speed)))
	      (not (= (md:offset->divider (+ offs 1) cycles bits cpu-speed)
		      (md:offset->divider (+ offs 2) cycles bits cpu-speed))))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (md:get-upper-bound-inverse cycles bits cpu-speed)
    (do ((offs (md:get-lower-bound-inverse cycles bits cpu-speed) (+ offs 1)))
	((or (<= (md:offset->inverse-divider offs cycles cpu-speed) 0)
             (= (md:offset->inverse-divider offs cycles cpu-speed)
                (md:offset->inverse-divider (+ offs 1) cycles cpu-speed))
	     (= (md:offset->inverse-divider (+ offs 1) cycles cpu-speed)
		(md:offset->inverse-divider (+ offs 2) cycles cpu-speed)))
	 offs)))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (md:get-lower-bound-inverse cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((< (md:offset->inverse-divider offs cycles cpu-speed)
            (expt 2 bits))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (md:get-upper-bound cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((>= (md:offset->divider offs cycles bits cpu-speed)
             (expt 2 bits))
	 offs)))

  (define (md:offset->note-name offset)
    (string-append (vector-ref md:note-names (modulo offset 12))
                   (number->string (md:offset->octave offset))))

  ;;;
  (define (md:make-dividers-range cycles beg end rest bits cpu-speed)
    (if (> beg end)
        (list (list "rest" rest))
        (cons (list (md:offset->note-name beg)
                    (md:offset->divider beg cycles bits cpu-speed))
              (md:make-dividers-range cycles (+ 1 beg) end rest bits
				      cpu-speed))))

  ;;;
  (define (md:make-inverse-dividers-range cycles beg end rest cpu-speed)
    (if (> beg end)
	(list (list "rest" rest))
	(cons (list (md:offset->note-name beg)
                    (md:offset->inverse-divider beg cycles cpu-speed))
              (md:make-inverse-dividers-range cycles (+ 1 beg) end rest
					      cpu-speed))))

  ;;; generate a note table with divider->note-name mappings
  ;;; wrapper func for make-dividers-range that will auto-deduce optimal range
  ;;; parameters: cycles - number of cycles in sound generation loop
  ;;;             bits - size of the dividers, as number of bits
  ;;;             rest - the value that represents a rest/note-off
  ;;;             [shift] - number of octaves to shift the table
  (define (md:make-dividers cpu-speed cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (md:make-dividers-range
	prescaled-cycles
        (md:get-lower-bound prescaled-cycles bits cpu-speed)
        (md:get-upper-bound prescaled-cycles bits cpu-speed)
        rest bits cpu-speed))))

  ;;; generate a note table with inverse divider->node-name mappings
  ;;; ie. dividers are countdown values
  ;;; see `md:make-dividers` for further documentation
  (define (md:make-inverse-dividers cpu-speed cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (md:make-inverse-dividers-range
	prescaled-cycles
        (md:get-lower-bound-inverse prescaled-cycles bits cpu-speed)
        (md:get-upper-bound-inverse prescaled-cycles bits cpu-speed)
        rest cpu-speed))))

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
