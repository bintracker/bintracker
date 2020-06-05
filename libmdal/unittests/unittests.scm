;; -*- geiser-scheme-implementation: 'chicken -*-

(import scheme (chicken base) (chicken io) (chicken bitwise)
	srfi-1 simple-exceptions
	mdal test simple-md5 srfi-13 srfi-69)

(define my-config-path "mdef/")
(define my-cfg (file->config my-config-path "Huby"))
(define my-mod (file->mdmod "tunes/demotunes/huby-test.mdal"
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
       '(#\null #\null #\null #\backspace)
       (int->bytes 8 4 'big-endian))

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

 (test "note-table-range" '(a1 c2)
       (note-table-range (alist->hash-table '((a1 . 1) (b1 . 2) (c2 . 3)))))

 (test-assert "make-counters"
   (let ((my-note-table (make-counters 12 47 1 0)))
     (and (= 37 (hash-table-size my-note-table))
	  (eqv? 'c1 (lowest-note my-note-table))
	  (eqv? 'b3 (highest-note my-note-table))
	  (= 13 (hash-table-ref my-note-table 'c2)))))

 (test-assert "make-dividers"
   (let ((my-note-table (make-dividers 3500000 118 8 0 -4)))
     (and (= 56 (hash-table-size my-note-table))
	  (eqv? 'e2 (lowest-note my-note-table))
	  (eqv? 'a#6 (highest-note my-note-table))))))


(test-group
 "MD-Config/Master Config"

 (test "parsing plugin version"
       '(1 0)
       (list (plugin-version-major (config-plugin-version my-cfg))
	     (plugin-version-minor (config-plugin-version my-cfg))))

 (test "plugin version compatibility check"
       '(#t #t #f #f)
       (let ((available-version (make-plugin-version major: 1 minor: 4)))
	 (list (plugin-versions-compatible?
		available-version (make-plugin-version major: 1 minor: 4))
	       (plugin-versions-compatible?
		available-version (make-plugin-version major: 1 minor: 2))
	       (plugin-versions-compatible?
		available-version (make-plugin-version major: 1 minor: 6))
	       (plugin-versions-compatible?
		available-version (make-plugin-version major: 2 minor: 4)))))

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
       (list (hash-table-ref (config-commands my-cfg) 'AUTHOR)
	     #f)
       (list (config-command-ref 'AUTHOR my-cfg)
	     (config-command-ref 'INVALID my-cfg)))

 (test "config-inode-ref"
       (list (hash-table-ref (config-inodes my-cfg) 'AUTHOR)
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
       (hash-table-ref (config-commands my-cfg) 'DRUM)
       (config-get-inode-source-command 'DRUM my-cfg))

 (test "config-get-node-default" #f
       (config-get-node-default 'DRUM my-cfg)))

(test-group
 "MD-Module/Parser"

 (define my-mod-expr (read (open-input-file "tunes/demotunes/huby-test.mdal"
					    text:)))

 (define my-global-node-contents (remove-keyword-args
				  (cdr my-mod-expr)
				  '(version: config: config-version:)))

 (test-group
  "MDMOD integrity checks"

  (test "not an MDAL module"
	"Not an MDAL module"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (apply check-mdmod-version
				   '(mdal-modul version: 4)))))

  (test "valid mdmod version"
	2 (apply check-mdmod-version my-mod-expr))

  (test "invalid mdmod version"
	"Unsupported MDAL version: 4"
	(with-exn-handler (lambda (e) (message e))
			  (lambda ()
			    (apply check-mdmod-version
				   '(mdal-module version: 4)))))

  (test "mod-get-config-name"
	"Huby"
	(apply mod-get-config-name my-mod-expr)))


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
					 (make-list 3 `((#t) (()) (()) (()))))
					'((#t))))
			(CH1 (0 #f
				(a3) (()) (rest) (()) (c4) (()) (rest) (())
				(e4) (()) (rest) (()) (g4) (()) (rest)))
			(CH2 (0 #f (a2))
			     (1 #f (e2)))
			(PATTERNS_ORDER (0 #f
					   (#x10 #x00 #x00 #x00)
					   (() () () #x01))))))
       (apply mod-parse-group-instance
	      (append `(,my-cfg GLOBAL)
		      (remove-keyword-args
		       (cdr my-mod-expr)
		       '(version: config: config-version:)))))

 )


(test-group
 "MD-MODULE/Inodes"

 (define my-global-inode-instance (cadr (mdmod-global-node my-mod)))

 (test "subnode-ref"
       (find (lambda (node)
 	       (eqv? (car node) 'PATTERNS))
 	     (cddr my-global-inode-instance))
       (subnode-ref 'PATTERNS my-global-inode-instance))

 (define my-ch2-inode
   (subnode-ref
    'CH2
    (inode-instance-ref 0 (subnode-ref 'PATTERNS my-global-inode-instance))))
 )

(test-group
 "MD-Module/Accessors"

 (test "node-path to inode instance"
       '(1 #f (e2))
       ((node-path "0/PATTERNS/0/CH2/1") (mdmod-global-node my-mod)))

 (test "node-path to subnode"
       '(CH2 (0 #f (a2))
	     (1 #f (e2)))
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

  (test "mod-get-row-values"
        '(#t c4 #f)
        (mod-get-row-values ((node-path "0/PATTERNS/0")
 			     (mdmod-global-node my-mod))
 			    '(0 0 0)
 			    4
			    my-cfg))

  (test "mod-get-block-values"
        '((#t a3 a2)
  	 (#f #f #f)
  	 (#f rest #f)
  	 (#f #f #f)
  	 (#t c4 #f)
  	 (#f #f #f)
  	 (#f rest #f)
  	 (#f #f #f)
  	 (#t e4 #f)
  	 (#f #f #f)
  	 (#f rest #f)
  	 (#f #f #f)
  	 (#t g4 #f)
  	 (#f #f #f)
  	 (#f rest #f)
  	 (#f #f #f))
        (mod-get-block-values ((node-path "0/PATTERNS/0")
  			       (mdmod-global-node my-mod))
  			      '(16 0 0 0)
			      my-cfg))
  )

(test-group
 "Module Generator"

 (test "Generating an empty module"
       (cons my-cfg `(GLOBAL
		      (0 #f
			 (AUTHOR (0 #f . "unknown"))
			 (TITLE (0 #f . "untitled"))
			 (LICENSE (0 #f . "All Rights Reserved"))
			 (BPM (0 #f . 140))
			 (PATTERNS
			  (0 #f
			     (DRUMS ,(append '(0 #f) (make-list 16 '(()))))
			     (CH1 ,(append '(0 #f) (make-list 16 '(()))))
			     (CH2 ,(append '(0 #f) (make-list 16 '(()))))
			     (PATTERNS_ORDER (0 #f (16 0 0 0))))))))
       (generate-new-mdmod my-cfg 16))

 (test "Generate a one-row module"
       '(GLOBAL (0 #f
		   (AUTHOR (0 #f . "utz"))
		   (TITLE (0 #f . "Huby Test"))
		   (LICENSE (0 #f . "Creative Commons CC0"))
		   (BPM (0 #f . 120))
		   (PATTERNS (0 #f
				(DRUMS (0 #f (#t)))
				(CH1 (0 #f (a3)))
				(CH2 (0 #f (a2)))
				(PATTERNS_ORDER (0 #f (1 0 0 0)))))))
       (mdmod-global-node (derive-single-row-mdmod my-mod 'PATTERNS 0 0)))
 )


(test-group
 "Compiler Generator"

 (define my-parent-node ((node-path "0")
			 (mdmod-global-node my-mod)))

 (test  "eval-group-field"
	120
	(eval-group-field
	 (subnode-ref 'BPM (inode-instance-ref 0 (mdmod-global-node my-mod)))
	 0 (config-get-inode-source-command 'BPM my-cfg)))

 (test "eval-block-field"
       (hash-table-ref (command-keys (config-get-inode-source-command
				      'NOTE2 my-cfg))
		       'a2)
       (eval-block-field ((node-path "0/PATTERNS/0/CH2/0")
			  (mdmod-global-node my-mod))
			 0 10 (config-command-ref 'NOTE my-cfg)))

 (test "get-required-symbols"
       '((foo)
	 (foo bar baz)
	 ())
       (list (get-required-symbols '$foo)
	     (get-required-symbols '($foo ($bar (0 ($baz boo)))))
	     (get-required-symbols '(foo bar (baz ())))))

 (test "transform-compose-expr"
       (quotient 1779661 120)
       ((transform-compose-expr '(quotient 1779661 ?BPM) my-cfg)
 	0 my-parent-node '() my-cfg))

 (test "make-order-transformer"
       '(1 5 2 6 3 7 4 8)
       ((make-order-transformer 'shared-numeric-matrix 1)
 	'((0 0) (1 1) (2 2) (3 3))))

 (test "block-repeat-last-set"
       '((() 2 3)
	 (() () ()))
       (block-repeat-last-set
	'((() () ())
	  (() () ()))
	'((() 2 ())
	  (() () 3))
	'(#f #t #t)))

 (test "split-block-instance-contents"
       '((0 #f (a2) (rest) (()) (())))
       (split-block-instance-contents
	4 'CH2 my-cfg
	(cddr ((node-path "0/PATTERNS/0/CH2/0") (mdmod-global-node my-mod)))))

 (test "resize-block-instances"
       '((CH2 (0 #f (a2) (()) (()) (()) (()) (()) (()) (()))
	      (1 #f (a2) (()) (()) (()) (()) (()) (()) (()))
	      (2 #f (e2) (()) (()) (()) (()) (()) (()) (()))
	      (3 #f (e2) (()) (()) (()) (()) (()) (()) (())))
	 (CH2 (0 #f (a2) (rest) (()) (()))))
       `(,(resize-block-instances
	   ((node-path "0/PATTERNS/0/CH2") (mdmod-global-node my-mod))
	   8
	   ((node-path "0/PATTERNS/0/PATTERNS_ORDER") (mdmod-global-node my-mod))
	   my-cfg)
	 ,(resize-block-instances
	   '(CH2 (0 #f (a2)))
	   4
	   '(PATTERNS_ORDER (0 #f (1 0 0 0)))
	   my-cfg)))

 (test "resize-blocks"
       '(0 #f
	   (DRUMS (0 #f (#t) (()) (()) (()) (#t) (()) (()) (()))
		     (1 #f (#t) (()) (()) (()) (#t) (()) (()) (()))
		     (2 #f (#t) (()) (()) (()) (#t) (()) (()) (()))
		     (3 #f (#t) (()) (()) (()) (#t) (()) (()) (())))
	   (CH1 (0 #f (a3) (()) (rest) (()) (c4) (()) (rest) (()))
		(1 #f (e4) (()) (rest) (()) (g4) (()) (rest) (()))
		(2 #f (a3) (()) (rest) (()) (c4) (()) (rest) (()))
		(3 #f (e4) (()) (rest) (()) (g4) (()) (rest) (())))
	   (CH2 (0 #f (a2) (()) (()) (()) (()) (()) (()) (()))
		(1 #f (a2) (()) (()) (()) (()) (()) (()) (()))
		(2 #f (e2) (()) (()) (()) (()) (()) (()) (()))
		(3 #f (e2) (()) (()) (()) (()) (()) (()) (())))
	   (PATTERNS_ORDER (0 #f
			      (8 0 0 0)
			      (8 1 1 1)
			      (8 2 2 2)
			      (8 3 3 3))))
       (resize-blocks
	((node-path "0/PATTERNS/0") (mdmod-global-node my-mod))
	'PATTERNS 8 my-cfg))

 (test "make-order-alist"
       '((0 (0 0)) (1 (1 1)) (2 (2 2)) (3 (3 3)))
       (make-order-alist
	(subnode-ref 'PATTERNS_ORDER
		     (resize-blocks
		      ((node-path "0/PATTERNS/0") (mdmod-global-node my-mod))
		      'PATTERNS 8 my-cfg))
	'(CH1 CH2)
	my-cfg))

 (test "make-ofield"
       `(,(int->bytes (quotient 1779661 120) 2 'little-endian)
	 (#\null #\null))
       (let ((my-onode1 (make-ofield my-cfg "" "" bytes: 2
 				     compose: '(quotient 1779661 ?BPM)))
 	     (my-onode2 (make-ofield my-cfg "" "" bytes: 2
 				     compose: '(- $my-sym 8))))
 	 (list (onode-val (car ((onode-fn my-onode1) my-onode1
 				my-parent-node my-cfg 0 '())))
 	       (onode-val (car ((onode-fn my-onode2) my-onode2
 	       			my-parent-node my-cfg 0 '((my-sym 8))))))))

 (test "make-osymbol"
       8
       (let ((my-onode (make-osymbol my-cfg "" "" id: 'my-sym)))
 	 (car (alist-ref 'my-sym
 			 (third ((onode-fn my-onode) my-onode my-parent-node
 				 my-cfg 8 '()))))))

 (test "order-oblock-sources"
       '(DRUMS CH1)
       (order-oblock-sources '(CH1 DRUMS) 'PATTERNS my-cfg))
 )

(test-group
 "Export & Compilation"

 (test "mdmod->file"
       "013ed7fbb6d1602bc267afe2d16788b2"
       (begin
	 (mdmod->file my-mod "test.mdal")
	 (file-md5sum "test.mdal")))

 (test "mod->bin"
       (map integer->char
	    '(33 103 128 78 35 70 35 94 35 86 35 126 35 183 40 240 229 213 197
		 110 6 2 38 0 41 41 41 25 229 111 16 246 217 225 209 6 8 26 19
		 217 103 87 217 126 35 217 111 95 254 44 40 1 175 50 65 128 193
		 197 243 175 29 32 3 93 149 0 21 32 2 84 148 159 230 16 211 254
		 219 254 47 230 31 32 5 11 120 177 32 227 33 88 39 217 251 32 2
		 16 196 193 209 225 40 164 201 238 57 108 128 1 5 2 6 3 7 4 8 0
		 44 67 0 0 44 57 0 0 44 45 0 0 44 38 0 0 44 67 0 0 44 57 0 0 44
		 45 0 0 44 38 0 0 135 135 135 135 135 135 135 135 135 135 135
		 135 135 135 135 135 180 180 180 180 180 180 180 180 180 180 180
		 180 180 180 180 180))
       (mod->bin my-mod #x8000))
 )

(test-exit)
