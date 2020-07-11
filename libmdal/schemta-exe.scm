;; Schemta binary executable wrapper

;; (c) 2019-2020 Michael Neidel
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import scheme
	(only (chicken base) print)
	(only (chicken process-context) argv command-line-arguments)
	(only (chicken io) read-string write-string)
	(chicken port)
	schemta
	args)

(eval '(import schemta srfi-1 srfi-13 srfi-14 srfi-69 comparse
	       (chicken string) (chicken bitwise)))

;; todo must also eval-import

(define cmdline-opts
  (list (args:make-option (i infile)
			  #:required "input file name")
	(args:make-option (o outfile)
			  #:required "output file name")
	(args:make-option (org)
			  #:required "origin address")
	(args:make-option (cpu)
			  #:required "target CPU identifier")
	(args:make-option (equ)
			  #:required "associative list of additional symbols")
	(args:make-option (max-passes)
			  #:required "maximum number of passes")
	(args:make-option (h help)
			  #:none "display this text" (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " -i infile [options...]")
      (newline)
      (print (args:usage cmdline-opts))))
  (exit 1))

(receive (options operands)
    (args:parse (command-line-arguments)
		cmdline-opts
		#:unrecognized-proc args:ignore-unrecognized-options)
  (let* ((infile (alist-ref 'i options))
	 (outfile (or (alist-ref 'o options)
		      (and infile (string-append infile ".bin"))))
	 (org (string->number (or (alist-ref 'org options) "0")))
	 (cpu (string->symbol (or (alist-ref 'cpu options) "z80")))
	 (equ (alist-ref 'equ options))
	 (extra-symbols (if equ (with-input-from-string equ read) '()))
	 (max-passes (string->number (or (alist-ref 'max-passes options) "3")))
	 (source (read-string #f (or (and infile (open-input-file infile text:))
				     (current-input-port))))
	 (res (or (assemble cpu
			    source
			    org: org
			    extra-symbols: extra-symbols
			    max-passes: max-passes)
		  (error "Failed to assemble source."))))
    (if outfile
	(call-with-output-file outfile
	  (lambda (port) (write-string (list->string res) #f port)))
	(write-string (list->string res)))))
