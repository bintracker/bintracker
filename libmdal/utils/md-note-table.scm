;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;;; Generate note to frequency divider/lookup value mappings
(module md-note-table
    (make-counters
     make-dividers-range
     make-inverse-dividers-range
     make-dividers
     make-inverse-dividers
     highest-note
     lowest-note
     note-table-range)

  (import scheme (chicken base) srfi-69)

  (define note-names
    (vector "c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b"))

  ;; fn = f0 * (2^(1/12))^n
  ;; using c-9 as base note (f0)
  (define (offset->freq offset)
    (* 8372.018 (expt (expt 2 (/ 1 12))
                      (- (- 108 offset)))))

  (define (freq->divider freq cycles bits cpu-speed)
    (inexact->exact (round (* (/ (* freq cycles) cpu-speed)
                              (expt 2 bits)))))

  (define (freq->inverse-divider freq cycles cpu-speed)
    (inexact->exact (round (/ (/ cpu-speed cycles)
			      freq))))

  (define (offset->divider offset cycles bits cpu-speed)
    (freq->divider (offset->freq offset) cycles bits cpu-speed))

  (define (offset->inverse-divider offset cycles cpu-speed)
    (freq->inverse-divider (offset->freq offset) cycles cpu-speed))

  (define (offset->octave offset) (quotient offset 12))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (get-lower-bound cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((and (> (offset->divider offs cycles bits cpu-speed) 0)
              (not (= (offset->divider offs cycles bits cpu-speed)
                      (offset->divider (+ offs 1) cycles bits cpu-speed)))
	      (not (= (offset->divider (+ offs 1) cycles bits cpu-speed)
		      (offset->divider (+ offs 2) cycles bits cpu-speed))))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is 1) > 0, and 2) distinct from the divider
  ;; value produced by (+ offset 1)
  (define (get-upper-bound-inverse cycles bits cpu-speed)
    (do ((offs (get-lower-bound-inverse cycles bits cpu-speed) (+ offs 1)))
	((or (<= (offset->inverse-divider offs cycles cpu-speed) 0)
             (= (offset->inverse-divider offs cycles cpu-speed)
                (offset->inverse-divider (+ offs 1) cycles cpu-speed))
	     (= (offset->inverse-divider (+ offs 1) cycles cpu-speed)
		(offset->inverse-divider (+ offs 2) cycles cpu-speed)))
	 offs)))

  ;; lower bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (get-lower-bound-inverse cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((< (offset->inverse-divider offs cycles cpu-speed)
            (expt 2 bits))
	 offs)))

  ;; upper bound defined as: the offset in half-tones from C-0 that will
  ;; produce a divider value that is larger than the max integer value
  ;; representable in <bits>
  (define (get-upper-bound cycles bits cpu-speed)
    (do ((offs 0 (+ offs 1)))
	((>= (offset->divider offs cycles bits cpu-speed)
             (expt 2 bits))
	 offs)))

  (define (offset->note-name offset)
    (string->symbol (string-append (vector-ref note-names (modulo offset 12))
				   (number->string (offset->octave offset)))))

  ;;;
  (define (make-dividers-range cycles beg end rest bits cpu-speed)
    (if (> beg end)
        (list (cons 'rest rest))
        (cons (cons (offset->note-name beg)
                    (offset->divider beg cycles bits cpu-speed))
              (make-dividers-range cycles (+ 1 beg) end rest bits
				   cpu-speed))))

  ;;;
  (define (make-inverse-dividers-range cycles beg end rest cpu-speed)
    (if (> beg end)
	(list (cons 'rest rest))
	(cons (cons (offset->note-name beg)
                    (offset->inverse-divider beg cycles cpu-speed))
              (make-inverse-dividers-range cycles (+ 1 beg) end rest
					   cpu-speed))))

  ;;; generate a note table with divider->note-name mappings
  ;;; wrapper func for make-dividers-range that will auto-deduce optimal range
  ;;; parameters: cycles - number of cycles in sound generation loop
  ;;;             bits - size of the dividers, as number of bits
  ;;;             rest - the value that represents a rest/note-off
  ;;;             [shift] - number of octaves to shift the table
  (define (make-dividers cpu-speed cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (make-dividers-range
	prescaled-cycles
        (get-lower-bound prescaled-cycles bits cpu-speed)
        (get-upper-bound prescaled-cycles bits cpu-speed)
        rest bits cpu-speed))))

  ;;; generate a note table with inverse divider->node-name mappings
  ;;; ie. dividers are countdown values
  ;;; see `make-dividers` for further documentation
  (define (make-inverse-dividers cpu-speed cycles bits rest . shift)
    (let* ((prescaler (if (null? shift)
			  1
			  (expt 2 (- (car shift)))))
	   (prescaled-cycles (* cycles prescaler)))
      (alist->hash-table
       (make-inverse-dividers-range
	prescaled-cycles
        (get-lower-bound-inverse prescaled-cycles bits cpu-speed)
        (get-upper-bound-inverse prescaled-cycles bits cpu-speed)
        rest cpu-speed))))

  ;;; Generate a note table with simple note-name->index mappings.
  ;;;
  ;;; BEG - lowest note, as offset from c-0
  ;;;
  ;;; END - highest note, as offset from c-0
  ;;;
  ;;; FIRST-INDEX - index of the lowest note
  ;;;
  ;;; REST-INDEX - index of the rest/note-off
  (define (make-counters beg end first-index rest-index)
    (letrec ((mkcounters
              (lambda (beg end first rest)
		(if (> beg end)
                    (list (cons 'rest rest))
                    (cons (cons (offset->note-name beg) first)
                          (mkcounters (+ 1 beg) end (+ 1 first) rest))))))
      (alist->hash-table (mkcounters beg end first-index rest-index))))

  ;;; Returns the lowest note in the given note table.
  (define (lowest-note table)
    (letrec
	((try-lower
	  (lambda (offset tbl)
            (if (hash-table-ref/default tbl (offset->note-name offset) #f)
		(offset->note-name offset)
		(try-lower (+ offset 1) tbl)))))
      (try-lower 0 table)))

  ;;; Returns the highest note in the given note table.
  (define (highest-note table)
    (letrec
	((try-upper
	  (lambda (offset tbl)
            (if (hash-table-ref/default tbl (offset->note-name offset) #f)
		(offset->note-name offset)
		(try-upper (- offset 1) tbl)))))
      (try-upper 131 table)))

  ;;; Returns (lowest highest) notes in the given note table.
  (define (note-table-range table)
    (list (lowest-note table) (highest-note table)))

  )  ;; end module md-note-tables
