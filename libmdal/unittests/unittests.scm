;; -*- geiser-scheme-implementation: 'chicken -*-

(import scheme (chicken base) (chicken io) (chicken bitwise)
	srfi-1 simple-exceptions
	mdal test simple-md5 srfi-13 srfi-69)

(define my-config-path "unittests/config/")
(define my-target (eval (car (read-list
			      (open-input-file "targets/spectrum48.scm")))))
(define my-cfg (file->config "unittests/config/Huby/Huby.mdconf"))
(define my-mod (file->mdmod "unittests/modules/huby-test.mdal"
			    my-config-path))
(define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
			"}" "CH2(0)={" "NOTE2=a-3" "}"))
(define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
(define (hash-table-equal? ht1 ht2)
  (null? (lset-difference equal? (hash-table->alist ht1)
			  (hash-table->alist ht2))))


(test-group
 "MD-Helpers"

 (test-assert "in-range?"
   (and (in-range? 1 (make-range min: 0 max: 2))
	(not (in-range? 3 (make-range min: 0 max: 2)))))

 (test "bits->range"
       '((-128 127)
	 (0 255))
       (let ((r1 (bits->range 8 #t))
	     (r2 (bits->range 8 #f)))
	 `((,(range-min r1) ,(range-max r1))
	   (,(range-min r2) ,(range-max r2)))))

 (test "int->bytes"
       (list 0 0 0 8)
       (int->bytes 8 4 'big-endian))

 (test "make-pairs" '((a 1) (b 2)) (make-pairs '(a 1 b 2)))

 (test "remove-keyword-args"
       '(0 1 baz 4)
       (remove-keyword-args '(0 1 foo: 2 bar: 3 baz 4)
			    '(foo: bar: baz:)))

 (test "add-hash-table-entry" '()
       (lset-difference
	equal?
	(hash-table->alist
	 (add-hash-table-entry (alist->hash-table '((a 1) (b 2)))
			       'c 3))
	'((a 1) (b 2) (c 3))))

 (test "add-to-list"
       '(1 (2 (3 4)) (5 6) 7)
       (add-to-list '(0 (1 (2 3)) (4 5) 6) 1))
 )


(test-group
 "utils/MD-Note-Table"

 (test "note-table-range" '("a1" "c2")
       (note-table-range (alist->hash-table '(("a1" 1) ("b1" 2) ("c2" 3)))))

 (test-assert "make-counters"
   (let ((my-note-table (make-counters 12 47 1 0)))
     (and (= 37 (hash-table-size my-note-table))
	  (string= "c1" (lowest-note my-note-table))
	  (string= "b3" (highest-note my-note-table))
	  (= 13 (car (hash-table-ref my-note-table "c2"))))))

 (test-assert "make-dividers"
   (let ((my-note-table (make-dividers 3500000 118 8 0 -4)))
     (and (= 56 (hash-table-size my-note-table))
	  (string= "e2" (lowest-note my-note-table))
	  (string= "a#6" (highest-note my-note-table))))))


(test-group
 "MD-Config/Master Config"

 (test "creating system target" "spectrum48"
       (target-platform-id (config-target my-cfg)))

 (test "parsing description" "f224aa0c3de07810142a50825ad6a523"
       (string->md5sum (config-description my-cfg)))

 (test-assert "default commands created"
   (and (hash-table-exists? (config-commands my-cfg) 'AUTHOR)
	(hash-table-exists? (config-commands my-cfg) 'TITLE)
	(hash-table-exists? (config-commands my-cfg) 'LICENSE)))

 (test-assert "order commands created"
   (and (hash-table-exists? (config-commands my-cfg) 'PATTERNS_LENGTH)
	(hash-table-exists? (config-commands my-cfg) 'R_DRUMS)
	(hash-table-exists? (config-commands my-cfg) 'R_CH1)
	(hash-table-exists? (config-commands my-cfg) 'R_CH2)))

 (test "all commands created" 10
       (hash-table-size (config-commands my-cfg))))


(test-group
 "MD-Config/Auxilliary Accessors"

 (define my-itree (config-itree my-cfg))

 (test "config-command-ref"
       (list (car (hash-table-ref (config-commands my-cfg) 'AUTHOR))
	     #f)
       (list (config-command-ref 'AUTHOR my-cfg)
	     (config-command-ref 'INVALID my-cfg)))

 (test "config-inode-ref"
       (list (car (hash-table-ref (config-inodes my-cfg) 'AUTHOR))
	     #f)
       (list (config-inode-ref 'AUTHOR my-cfg)
	     (config-inode-ref 'INVALID my-cfg)))

 (test "config-get-parent-node-id" 'CH2
       (config-get-parent-node-id 'NOTE2 my-itree))

 (test "config-get-node-ancestors-ids" '(CH1 PATTERNS GLOBAL)
       (config-get-node-ancestors-ids 'NOTE1 my-itree))

 (test "config-get-subnode-ids" '(DRUMS CH1 CH2 PATTERNS_ORDER)
       (config-get-subnode-ids 'PATTERNS my-itree))

 (test "md-config-get-subnode-type-ids" '(AUTHOR TITLE LICENSE BPM)
       (config-get-subnode-type-ids 'GLOBAL my-cfg 'field))

 (test "config-get-inode-source-command"
       (car (hash-table-ref (config-commands my-cfg) 'DRUM))
       (config-get-inode-source-command 'DRUM my-cfg))

 (test "config-get-node-default" #f
       (config-get-node-default 'DRUM my-cfg)))

(test-group
 "MD-Module/Parser"

 (define my-mod-expr (read (open-input-file "unittests/modules/huby-test.mdal"
					    text:)))

 (define my-global-node-contents (remove-keyword-args (cdr my-mod-expr)
						      '(version: config:)))

 (test-group
  "MDMOD integrity checks"

  (test "not an MDAL module"
	"Not an MDAL module"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (apply check-mdmod-version
				   '(mdal-modul version: 4)))))

  (test "valid version"
	2 (apply check-mdmod-version my-mod-expr))

  (test "invalid version"
	"Unsupported MDAL version: 4"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (apply check-mdmod-version
				   '(mdal-module version: 4)))))

  (test "mod-get-config-name"
	"Huby"
	(apply mod-get-config-name my-mod-expr)))


 ;; (define my-drum-inode
 ;;   (make-inode config-id: 'DRUM
 ;; 	       instances: (zip (iota 16)
 ;; 			       (circular-list (make-inode-instance val: "on")
 ;; 					      (make-inode-instance)
 ;; 					      (make-inode-instance)
 ;; 					      (make-inode-instance)))))

 ;; (define my-note1-inode
 ;;   (make-inode config-id: 'NOTE1
 ;; 	       instances: `((0 ,(make-inode-instance val: "a3"))
 ;; 			    (1 ,(make-inode-instance))
 ;; 			    (2 ,(make-inode-instance val: "rest"))
 ;; 			    (3 ,(make-inode-instance))
 ;; 			    (4 ,(make-inode-instance val: "c4"))
 ;; 			    (5 ,(make-inode-instance))
 ;; 			    (6 ,(make-inode-instance val: "rest"))
 ;; 			    (7 ,(make-inode-instance))
 ;; 			    (8 ,(make-inode-instance val: "e4"))
 ;; 			    (9 ,(make-inode-instance))
 ;; 			    (10 ,(make-inode-instance val: "rest"))
 ;; 			    (11 ,(make-inode-instance))
 ;; 			    (12 ,(make-inode-instance val: "g4"))
 ;; 			    (13 ,(make-inode-instance))
 ;; 			    (14 ,(make-inode-instance val: "rest"))
 ;; 			    (15 ,(make-inode-instance)))))

 ;; (define my-note2-inode0
 ;;   (make-inode config-id: 'NOTE2
 ;; 	       instances: (zip (iota 16)
 ;; 			       (cons (make-inode-instance val: "a2")
 ;; 				     (make-list 15 (make-inode-instance))))))

 ;; (define my-note2-inode1
 ;;   (make-inode config-id: 'NOTE2
 ;; 	       instances: (zip (iota 16)
 ;; 			       (cons (make-inode-instance val: "e2")
 ;; 				     (make-list 15 (make-inode-instance))))))

 ;; (define my-patterns-order-inode
 ;;   (make-inode
 ;;    config-id: 'PATTERNS_ORDER
 ;;    instances:
 ;;    `((0 ,(make-inode-instance
 ;; 	   val: (list (make-inode
 ;; 		       config-id: 'PATTERNS_LENGTH
 ;; 		       instances: `((0 ,(make-inode-instance val: 16))
 ;; 				    (1 ,(make-inode-instance))))
 ;; 		      (make-inode
 ;; 		       config-id: 'R_DRUMS
 ;; 		       instances: `((0 ,(make-inode-instance val: 0))
 ;; 				    (1 ,(make-inode-instance))))
 ;; 		      (make-inode
 ;; 		       config-id: 'R_CH1
 ;; 		       instances: `((0 ,(make-inode-instance val: 0))
 ;; 				    (1 ,(make-inode-instance))))
 ;; 		      (make-inode
 ;; 		       config-id: 'R_CH2
 ;; 		       instances: `((0 ,(make-inode-instance val: 0))
 ;; 				    (1 ,(make-inode-instance val: 1))))))))))

 ;; (define my-patterns-subnodes
 ;;   (list (make-inode config-id: 'DRUMS
 ;; 		     instances: `((0 ,(make-inode-instance
 ;; 				       val: (list my-drum-inode)
 ;; 				       name: "beat0"))))
 ;; 	 (make-inode config-id: 'CH1
 ;; 		     instances: `((0 ,(make-inode-instance
 ;; 				       val: (list my-note1-inode)))))
 ;; 	 (make-inode config-id: 'CH2
 ;; 		     instances: `((0 ,(make-inode-instance
 ;; 				       val: (list my-note2-inode0)))
 ;; 				  (1 ,(make-inode-instance
 ;; 				       val: (list my-note2-inode1)))))
 ;; 	 my-patterns-order-inode))


 (test "parsing group fields"
       '((AUTHOR (0 #f . "utz"))
	 (BPM (0 #f . 120)))
       `(,(mod-parse-group-field 'AUTHOR my-cfg my-global-node-contents)
	 ,(mod-parse-group-field 'BPM my-cfg my-global-node-contents)))

 (test-group
  "parsing blocks"

  (test "replacing empty block rows"
	'((0 1 2 3)
	  (() () () ())
	  (foo)
	  (() () () ())
	  (() () () ()))
	(mod-replace-empty-block-rows 4 '((0 1 2 3) 1 (foo) 2)))

  (test "detect mixed shorthand/full syntax"
	"In block node XYZ, ID 0: row \"(1 2 (BAZ 3))\" does not match specification"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (mod-parse-block-row '(1 2 (BAZ 3))
						 '(FOO BAR BAZ)
						 'XYZ 0))))

  (test "detect unrecognized node"
	"Unknown node ZAP"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (mod-parse-block-row '((ZAP 1))
						 '(FOO BAR BAZ)
						 'XYZ 0))))

  (test "detect invalid row length"
	"In block node XYZ, ID 0: row \"(1 2)\" does not match specification"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (mod-parse-block-row '(1 2)
						 '(FOO BAR BAZ)
						 'XYZ 0))))

  (test "parsing valid block rows"
	'((1 2 3 )
	  (1 () 3))
	`(,(mod-parse-block-row '(1 2 3)
				'(FOO BAR BAZ)
				'X 0)
	  ,(mod-parse-block-row '((FOO 1) (BAZ 3))
				'(FOO BAR BAZ)
				'X 0)))

  (test "parsing block instance"
	'(0 #f
	    (#x10 #x00 #x00 #x00)
	    (() () () #x01))
	(apply mod-parse-block-instance `(,my-cfg PATTERNS_ORDER
						  (#x10 #x00 #x00 #x00)
						  ((R_CH2 #x01))))))

 (test "parsing groups"
       `(0 #f
	   (AUTHOR (0 #f . "utz"))
	   (TITLE (0 #f . "Huby Test"))
	   (LICENSE (0 #f . "Creative Commons CC0"))
	   (BPM (0 #f . 120))
	   (PATTERNS (0 #f
			(DRUMS ,(append (list 0 "beat0")
					(concatenate
					 (make-list 4 `((#t) (()) (()) (()))))))
			(CH1 (0 #f
				(a3) (()) (rest) (()) (c4) (()) (rest) (())
				(e4) (()) (rest) (()) (g4) (()) (rest) (())))
			(CH2 ,(append (list 0 #f '(a2))
				      (make-list 15 '(())))
			     ,(append (list 1 #f '(e2))
				      (make-list 15 '(()))))
			(PATTERNS_ORDER (0 #f
					   (#x10 #x00 #x00 #x00)
					   (() () () #x01))))))
       (apply mod-parse-group-instance
	      (append `(,my-cfg GLOBAL)
		      (remove-keyword-args (cdr my-mod-expr)
					   '(version: config:)))))

 )


(test-group
 "MD-MODULE/Inodes"

 (define my-global-inode-instance (cadr (mdmod-global-node my-mod)))

 (test "get-subnode"
       (find (lambda (node)
 	       (eqv? (car node) 'PATTERNS))
 	     (cddr my-global-inode-instance))
       (get-subnode my-global-inode-instance 'PATTERNS))

 (define my-ch2-inode
   (get-subnode
    (inode-instance-ref 0 (get-subnode my-global-inode-instance 'PATTERNS))
    'CH2))

 (test "inode-count-instances" 2 (inode-count-instances my-ch2-inode))

 ;; (define my-note1-inode
 ;;   (get-subnode
 ;;    (car (alist-ref
 ;; 	  0 (inode-instances
 ;; 	     (get-subnode
 ;; 	      (car (alist-ref
 ;; 		    0 (inode-instances
 ;; 		       (get-subnode my-global-inode-instance 'PATTERNS))))
 ;; 	      'CH1))))
 ;;    'NOTE1))

 ;; (test "get-node-command-cfg"
 ;;       (car (hash-table-ref (config-commands my-cfg) 'NOTE))
 ;;       (get-node-command-cfg my-note1-inode my-cfg))

 ;; (test "eval-field-last-set" "c4"
 ;;       (eval-field-last-set
 ;; 	5 my-note1-inode (get-node-command-cfg my-note1-inode my-cfg)))

 ;; (test "eval-field" 0
 ;;       (eval-field 3 my-note1-inode
 ;; 		   (get-node-command-cfg my-note1-inode my-cfg)))

 ;; (define (make-test-inode instances)
 ;;   (make-inode config-id: 'FOO
 ;; 	       instances: (map (lambda (i)
 ;; 				 (list (car i)
 ;; 				       (make-inode-instance val: (cadr i))))
 ;; 			       instances)))

 ;; (define my-mutable-inode
 ;;   (make-test-inode '((0 "n0") (1 "n1") (2 "n2"))))

 ;; (test "node-set!"
 ;;       (make-test-inode '((0 "n0") (1 "foo") (2 "bar")))
 ;;       (begin (node-set! my-mutable-inode '((1 "foo") (2 "bar")))
 ;; 	      my-mutable-inode))

 ;; (test "node-remove!"
 ;;       (make-test-inode '((0 "n0") (1 "bar")))
 ;;       (begin (node-remove! my-mutable-inode '(1) #t)
 ;; 	      my-mutable-inode))

 ;; (test "node-insert!"
 ;;       (make-test-inode '((0 "n0") (1 "baz") (2 "bar")))
 ;;       (begin (node-insert! my-mutable-inode '((1 "baz")) #t)
 ;; 	      my-mutable-inode))
 )

(test-group
 "MD-Module/Accessors"

 (test "node-path to inode instance"
       (append '(1 #f (e2))
 	       (make-list 15 '(())))
       ((node-path "0/PATTERNS/0/CH2/1") (mdmod-global-node my-mod)))

 (test "node-path to subnode"
       (list 'CH2
	     (append '(0 #f (a2))
 		     (make-list 15 '(())))
	     (append '(1 #f (e2))
 		     (make-list 15 '(()))))
       ((node-path "0/PATTERNS/0/CH2") (mdmod-global-node my-mod)))

 ;; TODO move into subgroup "high level accessors"
 (test "mod-get-group-instance-blocks"
       (list ((node-path "0/PATTERNS/0/DRUMS") (mdmod-global-node my-mod))
 	     ((node-path "0/PATTERNS/0/CH1") (mdmod-global-node my-mod))
 	     ((node-path "0/PATTERNS/0/CH2") (mdmod-global-node my-mod)))
       (mod-get-group-instance-blocks
 	((node-path "0/PATTERNS/0") (mdmod-global-node my-mod))
 	'PATTERNS my-cfg))

 (test "mod-get-group-instance-order"
       ((node-path "0/PATTERNS/0/PATTERNS_ORDER/0")
 	(mdmod-global-node my-mod))
       (mod-get-group-instance-order
 	((node-path "0/PATTERNS/0") (mdmod-global-node my-mod))
 	'PATTERNS))

 (test "mod-get-order-values"
       '((16 0 0 0)
	 (16 0 0 1))
       (mod-get-order-values 'PATTERNS ((node-path "0/PATTERNS/0")
					(mdmod-global-node my-mod))))

 (test "get-ordered-group-length"
       32
       (get-ordered-group-length 'PATTERNS
 				 ((node-path "0/PATTERNS/0")
 				  (mdmod-global-node my-mod))))

 ;;  (test "mod-split-node-list-at"
 ;;        (list (list ((node-path "0/PATTERNS/0/DRUMS")
 ;; 		    (mdmod-global-node my-mod)))
 ;; 	     (list ((node-path "0/PATTERNS/0/CH1")
 ;; 		    (mdmod-global-node my-mod))
 ;; 		   ((node-path "0/PATTERNS/0/CH2")
 ;; 		    (mdmod-global-node my-mod))
 ;; 		   ((node-path "0/PATTERNS/0/PATTERNS_ORDER")
 ;; 		    (mdmod-global-node my-mod))))
 ;;        (mod-split-node-list-at
 ;; 	'CH1 (inode-instance-val ((node-instance-path "0/PATTERNS/0")
 ;; 				  (mdmod-global-node my-mod)))))

 ;;  (test "mod-split-instances-at"
 ;;        (list (zip (iota 5)
 ;; 		  (cons (make-inode-instance val: "a2")
 ;; 			(make-list 4 (make-inode-instance))))
 ;; 	     (zip (iota 11 5)
 ;; 		  (make-list 11 (make-inode-instance))))
 ;;        (mod-split-instances-at
 ;; 	5 (inode-instances ((node-path "0/PATTERNS/0/CH2/0/NOTE2")
 ;; 			    (mdmod-global-node my-mod)))))

 ;;  (test "mod-replace-subnode"
 ;;        (make-inode-instance
 ;; 	val: (list (make-inode config-id: 'NOTE1
 ;; 			       instances:
 ;; 			       (zip (iota 4)
 ;; 				    (make-list 4 (make-inode-instance))))))
 ;;        (mod-replace-subnode
 ;; 	((node-instance-path "0/PATTERNS/0/CH1/0")
 ;; 	 (mdmod-global-node my-mod))
 ;; 	(make-inode config-id: 'NOTE1
 ;; 		    instances: (zip (iota 4)
 ;; 				    (make-list 4 (make-inode-instance))))))

 ;;  (test "mod-replace-inode-instance"
 ;;        (make-inode config-id: 'NOTE2
 ;; 		   instances: (zip (iota 16)
 ;; 				   (make-list 16 (make-inode-instance))))
 ;;        (mod-replace-inode-instance
 ;; 	((node-path "0/PATTERNS/0/CH2/0/NOTE2")
 ;; 	 (mdmod-global-node my-mod))
 ;; 	0 (make-inode-instance)))

 ;;  (test "mod-node-setter"
 ;;        (make-inode
 ;; 	config-id: 'CH2
 ;; 	instances:
 ;; 	`((0 ,(make-inode-instance
 ;; 	       val: (list (make-inode
 ;; 			   config-id: 'NOTE2
 ;; 			   instances:
 ;; 			   (zip (iota 16)
 ;; 				(make-list 16 (make-inode-instance)))))))
 ;; 	  (1 ,(make-inode-instance
 ;; 	       val: (list
 ;; 		     (make-inode
 ;; 		      config-id: 'NOTE2
 ;; 		      instances:
 ;; 		      (zip (iota 16)
 ;; 			   (cons (make-inode-instance val: "e2")
 ;; 				 (make-list 15 (make-inode-instance))))))))))
 ;;        ((mod-node-setter "0")
 ;; 	(make-inode config-id: 'NOTE2
 ;; 		    instances: (zip (iota 16)
 ;; 				    (make-list 16 (make-inode-instance))))
 ;; 	((node-path "0/PATTERNS/0/CH2")
 ;; 	 (mdmod-global-node my-mod))))

  (test "mod-get-row-values"
        '(#t "c4" #f)
        (mod-get-row-values ((node-path "0/PATTERNS/0")
 			     (mdmod-global-node my-mod))
 			    '(0 0 0)
 			    4))

  (test "mod-get-block-values"
        '((#t "a3" "a2")
  	 (#f #f #f)
  	 (#f "rest" #f)
  	 (#f #f #f)
  	 (#t "c4" #f)
  	 (#f #f #f)
  	 (#f "rest" #f)
  	 (#f #f #f)
  	 (#t "e4" #f)
  	 (#f #f #f)
  	 (#f "rest" #f)
  	 (#f #f #f)
  	 (#t "g4" #f)
  	 (#f #f #f)
  	 (#f "rest" #f)
  	 (#f #f #f))
        (mod-get-block-values ((node-path "0/PATTERNS/0")
  			       (mdmod-global-node my-mod))
  			      '(0 0 0)))
 )


;; (test-group
;;  "New Config Compiler Generator"

;;  (define my-parent-node ((node-instance-path "0")
;; 			 (mdmod-global-node my-mod)))

;;  ;; TODO new Huby.mdconf uses 140 bpm as default
;;  (test "transform-compose-expr"
;;        (quotient 1779661 120)
;;        ((transform-compose-expr '(quotient 1779661 ?BPM))
;; 	0 my-parent-node '() my-cfg))

;;  (test "make-order-transformer"
;;        '(1 5 2 6 3 7 4 8)
;;        ((make-order-transformer 'shared-numeric-matrix 1)
;; 	'((0 0) (1 1) (2 2) (3 3))))

;;  (test "make-ofield"
;;        (list (int->bytes (quotient 1779661 120) 2 'little-endian)
;; 	     (list 0 0))
;;        (let ((my-onode1 (make-ofield my-cfg "" bytes: 2
;; 				     compose: '(quotient 1779661 ?BPM)))
;; 	     (my-onode2 (make-ofield my-cfg "" bytes: 2
;; 				     compose: '(- $my-sym 8))))
;; 	 (list (onode-val (car ((onode-fn my-onode1) my-onode1
;; 				my-parent-node my-cfg 0 '())))
;; 	       (onode-val (car ((onode-fn my-onode2) my-onode2
;; 				my-parent-node my-cfg 0 '((my-sym 8))))))))

;;  (test "make-osymbol"
;;        8
;;        (let ((my-onode (make-osymbol my-cfg "" id: 'my-sym)))
;; 	 (car (alist-ref 'my-sym
;; 			 (third ((onode-fn my-onode) my-onode my-parent-node
;; 				 my-cfg 8 '())))))))

(test-exit)
