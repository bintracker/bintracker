(define note-names
  (vector "c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b"))

; fn = f0 * (2^(1/12))^n
; using c-9 as base note (f0)
(define (offset->freq offset)
  (* 8372.018 (expt (expt 2 (/ 1 12))
                    (- (- 108 offset)))))

(define (freq->divider freq cycles bits)
  (exact (round (* (/ (* freq cycles) **cpu-speed**)
                   (expt 2 bits)))))

(define (offset->divider offset cycles bits)
  (freq->divider (offset->freq offset)
                 cycles bits))

(define (offset->octave offset) (quotient offset 12))

; lower bound defined as: the offset in half-tones from C-0 that will
; produce a divider value that is 1) > 0, and 2) distinct from the divider
; value produced by (+ offset 1)
(define (get-lower-bound cycles bits)
  (do ((offs 0 (+ offs 1)))
    ((and (> (offset->divider offs cycles bits) 0)
          (not (= (offset->divider offs cycles bits)
                  (offset->divider (+ offs 1) cycles bits))))
     offs)))

; upper bound defined as: the offset in half-tones from C-0 that will
; produce a divider value that is larger than the max integer value
; representable in <bits>
(define (get-upper-bound cycles bits)
  (do ((offs 0 (+ offs 1)))
    ((>= (offset->divider offs cycles bits)
         (expt 2 bits))
    offs)))

(define (offset->note-name offset)
  (string-append (vector-ref note-names (modulo offset 12))
                 (number->string (offset->octave offset))))

(define (make-dividers-range cycles beg end rest bits)
    (if (= beg end)
        (list (list "rest" rest))
        (cons (list (offset->note-name beg)
                    (offset->divider beg cycles bits))
              (make-dividers-range cycles (+ 1 beg) end rest bits))))

; generate a note table with divider->note-name mappings
; wrapper func for make-dividers-range that will auto-deduce optimal range
; parameters: cycles - number of cycles in sound generation loop
;             bits - size of the dividers, as number of bits
;             rest - the value that represents a rest/note-off
(define (make-dividers cycles bits rest)
  (make-dividers-range cycles
                       (get-lower-bound cycles bits)
                       (get-upper-bound cycles bits)
                       rest bits))

; generate a note table with simple note-name->offset mappings
(define (make-counters beg end rest)
    (if (= beg end)
        (list (list "rest" rest))
        (cons (list (offset->note-name beg) beg)
                    (make-counters (+ 1 beg) end rest))))

