;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-PARSER
;;; .mdal module parser

(module md-parser *

  (import scheme chicken srfi-1 srfi-13 srfi-14 extras data-structures)
  (use comparse)

  (define (md:string-parser char-set convert-fn)
    (bind (as-string (one-or-more (in char-set)))
	  (lambda (s)
	    (result (convert-fn s)))))

  (define md:identifier-chars (char-set-adjoin char-set:letter+digit #\_))

  (define md:identifier
    ;;(md:string-parser md:identifier-chars (o string->symbol string-upcase))
    (md:string-parser md:identifier-chars string-upcase))

  (define md:decimal (md:string-parser char-set:digit string->number))

  (define md:hexadecimal
    (sequence* ((_ (is #\$))
		(valstr (as-string (one-or-more (in char-set:hex-digit)))))
	       (result (string->number valstr 16))))

  (define md:numeric-arg (any-of md:decimal md:hexadecimal))

  (define md:string-chars
    (char-set-union char-set:letter+digit char-set:blank))

  (define md:quoted-string
    (enclosed-by (is #\")
		 (as-string
		  (zero-or-more
		   (in (char-set-union
			char-set:letter+digit char-set:blank
			(char-set-delete char-set:punctuation #\")))))
		 (is #\")))

  (define md:linebreak (sequence (one-or-more (is #\newline))))

  (define md:instance-id
    (enclosed-by (is #\()
		 md:numeric-arg (is #\))))

  (define md:dotted-line
    (sequence* ((val (enclosed-by (is #\.)
				  (maybe md:numeric-arg 1)
				  md:linebreak)))
	       (result (list 'dotted val))))

  (define md:line-comment
    (sequence* ((_ (char-seq "//"))
		(text (as-string (zero-or-more (in md:string-chars)))))
	       (result (list 'comment text))))

  (define md:block-comment-begin (char-seq "/*"))

  (define md:block-comment-end (char-seq "*/"))

  (define md:block-comment
    (sequence* ((text (enclosed-by
		       md:block-comment-begin
		       (as-string (zero-or-more (none-of* md:block-comment-end
							  item)))
		       md:block-comment-end)))
	       (result text)))

  (define (md:value-set . added)
    (apply any-of
	   (append added
		   (list md:numeric-arg md:quoted-string
			 (as-string (one-or-more (in md:string-chars)))))))

  (define (md:normalize-order-ids assignments parent-id)
    (map (lambda (a)
	   (if (and (eq? 'assign (car a))
		    (string=? "ORDER" (cadr a)))
	       (cons 'assign
		     (cons (string-append parent-id "_ORDER")
			   (drop a 2)))
	       a))
	 assignments))

  (define (md:assignment-parser val-parser)
    (sequence* ((identifier md:identifier)
		(instance-id (maybe md:instance-id))
		(instance-name (maybe md:quoted-string))
		(_ (is #\=))
		(val val-parser)
		(_ (maybe md:line-comment)))
	       (result (list 'assign identifier
			     (if instance-id instance-id 0)
			     (if instance-name instance-name "")
			     (if (pair? val)
				 (md:normalize-order-ids val identifier)
				 val)))))

  (define md:row-assignment (md:assignment-parser (md:value-set)))

  (define (md:csv-parser val-parser prefix-sym)
    (bind (one-or-more (sequence* ((val val-parser)
				   (_ (any-of (is #\,)
					      md:line-comment
					      md:linebreak)))
				  (result val)))
	  (lambda (a)
	    (result (list prefix-sym a)))))

  (define md:csv-shorthand (md:csv-parser md:numeric-arg 'csv-shorthand))

  (define md:csv (md:csv-parser md:row-assignment 'csv))

  (define md:block
    (recursive-parser
     (enclosed-by (is #\{)
		  (one-or-more
		   (enclosed-by (maybe md:linebreak)
					    (any-of md:dotted-line
						    md:csv-shorthand
						    md:csv
						    (md:assignment-parser
						     (md:value-set md:block)))
					    (maybe md:linebreak)))
		  (is #\}))))

  (define md:assignments
    (zero-or-more
     (enclosed-by (maybe md:linebreak)
		  (sequence* ((assignment (md:assignment-parser
					   (md:value-set md:block)))
			      (_ (maybe md:block-comment)))
			     (result assignment))
		  (any-of md:linebreak end-of-input))))

  (define md:file
    (followed-by (any-of md:assignments md:csv-shorthand md:csv)
		 end-of-input))

  ;;; strip whitespace from MDMOD text, except where enclosed in double quotes
  (define (md:purge-ws lines)
    (letrec ((purge-ws-from-every-other
	      (lambda (ls odd)
		(if (null? ls)
		    '()
		    (cons (if odd
			      (string-delete char-set:whitespace (car ls))
			      (string-append "\"" (car ls) "\""))
			  (purge-ws-from-every-other (cdr ls) (not odd)))))))
      (map (lambda (line)
	     (string-concatenate
	      (purge-ws-from-every-other (string-split line "\"" #t) #t)))
	   lines)))

  ;;; convert .mdal module file to internal s-expression representation, to be
  ;;; processed further into a md:module record
  (define (md:file->sexp filepath)
    (parse md:file
	   (string-concatenate
	    (flatten (zip (md:purge-ws (read-lines filepath))
			  (circular-list "\n"))))))

  ;;; extract assignments for the given {{identifier}} from the given
  ;;; expressions
  (define (md:get-assignments exprs identifier)
    (filter (lambda (e)
	      (and (eq? 'assign (car e))
		   (string=? identifier (cadr e))))
	    exprs))

  ) ;; end module md-parser
