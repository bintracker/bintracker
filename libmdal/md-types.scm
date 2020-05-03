;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018-2020
;; See LICENSE for license details.

;;; md-module record types and additional accessors
(module md-types *

  (import scheme (chicken base) (chicken string)
	  srfi-1 srfi-13 md-helpers)

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: Input Nodes
  ;; ---------------------------------------------------------------------------

  ;; TODO should this always succeed as well? Then we can construct whole node
  ;; trees for eg. groups that don't exist.
  ;;; Returns the subnode with the given SUBNODE-ID.
  (define (subnode-ref subnode-id inode-instance)
    (assv subnode-id (cddr inode-instance)))

  ;;; Returns the inode instance with the given INSTANCE-ID, or a newly created,
  ;;; empty inode if the referenced inode instance does not exist.
  (define (inode-instance-ref instance-id inode)
    (or (assq instance-id (cdr inode)) '(,instance-id #f)))

  ;;; Returns the value of the field at FIELD-INDEX in ROW of
  ;;; `block-instance`. Returns null if the requested ROW does not exist.
  (define (block-field-ref block-instance row field-index)
    (let ((rows (cddr block-instance)))
      (if (>= row (length rows))
	  '()
	  (list-ref (list-ref rows row)
		    field-index))))


  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: Module
  ;; ---------------------------------------------------------------------------

  ;;; The internal representation of an MDAL module is a pair containing
  ;;; an MDAL `config` as the first element, and the global module node as the
  ;;; second element, so `(config . global-node)`. libmdal provides the
  ;;; accessors `mdmod-config` and `mdmod-global-node` for dealing with the
  ;;; elements of a module.
  ;;;
  ;;; The structure of `global-node` mirrors that of the MDAL module
  ;;; s-expression, with the following changes:
  ;;;
  ;;; - The GLOBAL node is explicit.
  ;;; - Each node is represented as an element in an alist, where the key is
  ;;;   the node ID, and the value (remainder) is an alist of node instances.
  ;;; - Each node instance is represented as an element in an alist, where the
  ;;;   key is the instance ID, the first value of the remainder is the node
  ;;;   instance name (or `#f` if none was given), and the rest of the remainder
  ;;;   is the actual node instance value, depending on the node type.
  ;;; - In block node instances, all rows are expanded to a list containing the
  ;;;   values for each block field subnode. Unset (empty) fields are
  ;;;   represented by `null` (the empty list).

  (define (mdmod-config m) (car m))
  (define (mdmod-global-node m) (cdr m))

  ;;----------------------------------------------------------------------------
  ;;; ### mod accessor functions
  ;;----------------------------------------------------------------------------

  ;;; Generate a function that takes an inode as parameter, and returns the
  ;;; subnode or node instance matching the given path P, which must be a
  ;;; string in the form "instance-id/node-id...". For example,
  ;;;
  ;;; ```Scheme
  ;;; (node-path "0/PATTERNS/0/CH1")
  ;;; ```
  ;;;
  ;;; will return a procedure that, when called with the global node as
  ;;; argument, will return the node CH1 in instance 0 of the PATTERN node,
  ;;; assuming that the node exists and is defined in the module's
  ;;; configuration.
  (define (node-path p)
    (let* ((path (string-split p "/"))
	   (path-suffix (last path))
	   (last-path-elem (or (string->number path-suffix)
			       (string->symbol path-suffix)))
	   (accessor-proc
	    (apply
	     compose
	     (cons (lambda (contents)
		     (assv last-path-elem contents))
		   (cdr (reverse
			 (map (lambda (path-elem subnode-access)
				(if subnode-access
				    (lambda (contents)
				      (alist-ref (string->symbol path-elem)
						 contents))
				    (lambda (contents)
				      (cdr (alist-ref (string->number path-elem)
						      contents)))))
			      path (circular-list #f #t))))))))
      (lambda (node)
	(accessor-proc (cdr node)))))

  ;;; Given the contents of a block instance, return the contents such that
  ;;; empty fields are replaced with the last set value.
  ;;; Helper for `mod-get-order-values` and `derive-single-row-mdmod`.
  (define (repeat-block-row-values rows)
    (letrec ((repeat-values
	      (lambda (rows previous-row)
		(if (null-list? rows)
  		    '()
  		    (cons (map (lambda (pos previous-pos)
  				 (if (null? pos)
  				     previous-pos pos))
  			       (car rows) previous-row)
  			  (repeat-values (cdr rows) (car rows)))))))
      (repeat-values rows (make-list (length (car rows))
				     '()))))

  ) ;; end module md-types
