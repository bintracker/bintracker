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


;;; Tools for analyzing key and scales.
(module key
    (key::guess)

  (import scheme (chicken base)
	  srfi-1 srfi-13)

  ;; Key detection based on Krumhansl-Schmuckler-Temperley key finding
  ;; algorithm. See Temperley, David: What's Key for Key? The Krumhansl-
  ;; Schmuckler Key-Finding Algorithm Reconsidered

  (define note-names '(c c# d d# e f f# g g# a a# b))

  (define kk-minor-profile
    '(6.33 2.68 3.52 5.38 2.60 3.53 2.54 4.75 3.98 2.69 3.34 3.17))

  ;; Temperley key profiles
  (define major-profile
    '(5.0 2.0 3.5 2.0 4.5 4.0 2.0 4.5 2.0 3.5 1.5 4.0))

  (define natural-minor-profile
    '(5.0 2.0 3.5 4.5 2.0 4.0 2.0 4.5 3.5 2.0 4.0 1.5))

  (define harmonic-minor-profile
    '(5.0 2.0 3.5 4.5 2.0 4.0 2.0 4.5 3.5 2.0 1.5 4.0))

  ;; Calculate key scores by forming the dot products of each key profile
  ;; and PTABLE, which must be an alist as emitted by
  ;; `cumulative-pitch-durations`.
  (define (key-scores ptable)
    (let ((run-profile
	   (lambda (mode profile)
	     (map (lambda (offs)
		    (list (list-ref note-names offs)
			  mode
			  (apply + (map *
					(append (take-right profile offs)
						(drop-right profile offs))
					(map cdr ptable)))))
		  (iota 12)))))
      (append (run-profile 'major major-profile)
	      (run-profile 'natural-minor natural-minor-profile)
	      (run-profile 'harmonic-minor harmonic-minor-profile))))

  ;; Find the most likely key from a given PTABLE, as emitted by
  ;; `cumulative-pitch-durations`.
  (define (guess-key ptable)
    (letrec* ((find-max-score
	       (lambda (scores current-max)
		 (if (null? scores)
		     current-max
		     (find-max-score (cdr scores)
				     (if (< (caddr current-max)
					    (caddr (car scores)))
					 (car scores)
					 current-max)))))
	      (res (find-max-score (key-scores ptable) '(0 0 0))))
      (cons (car res) (cadr res))))

  ;; Calculate total durations for each pitch (regardless of octave) from a list
  ;; of notes.
  ;; TODO consider ticks
  (define (cumulative-pitch-durations notes)
    (letrec* ((normalize-note-name
	       (lambda (note)
		 (let ((nstring (symbol->string note)))
		   (string->symbol
		    (string-take nstring
				 (if (string-contains nstring "#") 2 1))))))
	      (add-to-ptable
	       (lambda (note ptable)
		 (let ((nn (normalize-note-name note)))
		   (alist-update nn (+ 1 (alist-ref nn ptable)) ptable))))
	      (accumulate
	       (lambda (nts previous ptable)
		 (if (null? nts)
		     ptable
		     (apply accumulate
			    (cons (cdr nts)
				  (cond
				   ((null? (car nts))
				    (if previous
					(list previous (add-to-ptable previous
								      ptable))
					(list previous ptable)))
				   ((eqv? 'rest (car nts))
				    (list #f ptable))
				   (else (list (car nts)
					       (add-to-ptable (car nts)
							      ptable))))))))))
      (accumulate notes #f (map (cute cons <> 0) note-names))))

  ;; (define dictionary
  ;;   '((major (0 2 4 5 7 9 11))
  ;;     (natural-minor (0 2 3 5 7 8 10))
  ;;     (harmonic-minor (0 2 3 5 7 8 11))
  ;;     (melodic-minor (0 2 3 5 7 9 11) (0 2 3 5 7 8 10))
  ;;     ;; there's also aeolian #4 aka "gypsy minor scale" which doesn't have
  ;;     ;; 7th degree raised
  ;;     (hungarian-minor (0 2 3 6 7 8 11))
  ;;     (persian (0 1 4 5 6 8 11))))

  ;;; Guess the musical key from a given list of notes. Returns a pair with the
  ;;; root note in car, and the scale pattern in cdr. Due to the nature of the
  ;;; task at hand, there is of course no guarantee that a correct guess is
  ;;; made. For note material of average complexity, success rate will be
  ;;; around 80%.
  (define (key::guess notes)
    (let ((filtered (remove (lambda (x) (or (null? x) (eqv? 'rest x)))
			    notes)))
      (cond
       ((null? filtered) #f)
       ((= 1 (length filtered))
	(let ((nstring (symbol->string (car filtered))))
	  (cons (string->symbol
		 (string-take nstring (if (string-contains nstring "#") 2 1)))
		'major)))
       (else (guess-key (cumulative-pitch-durations notes))))))

  ) ;; end module stat
