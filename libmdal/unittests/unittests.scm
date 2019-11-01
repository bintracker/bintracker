;; -*- geiser-scheme-implementation: 'chicken -*-

(import scheme (chicken base) (chicken io) (chicken bitwise)
	srfi-1 simple-exceptions
	mdal test simple-md5 srfi-13 srfi-69)

(define my-config-path "unittests/config/")
(define my-target (eval (car (read-list
			      (open-input-file "targets/spectrum48.scm")))))
(define my-cfg (md:file->config "unittests/config/Huby/Huby.mdconf"))
(define my-mod (md:file->module "unittests/modules/huby-test.mdal"
				my-config-path))
(define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
			"}" "CH2(0)={" "NOTE2=a-3" "}"))
(define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
(define (hash-table-equal? ht1 ht2)
  (null? (lset-difference equal? (hash-table->alist ht1)
			  (hash-table->alist ht2))))


(test-group
 "MD-Helpers"

 (test-assert "md:in-range?"
   (and (md:in-range? 1 (md:make-range 0 2))
	(not (md:in-range? 3 (md:make-range 0 2)))))

 (test "md:make-pairs" '((a 1) (b 2)) (md:make-pairs '(a 1 b 2)))

 (test "md:add-hash-table-entry" '()
       (lset-difference
	equal?
	(hash-table->alist
	 (md:add-hash-table-entry (alist->hash-table '((a 1) (b 2)))
				  'c 3))
	'((a 1) (b 2) (c 3))))

 (test "md:add-to-list"
       '(1 (2 (3 4)) (5 6) 7)
       (md:add-to-list '(0 (1 (2 3)) (4 5) 6) 1))
 )


(test-group
 "utils/MD-Note-Table"

 (test "md:note-table-range" '("a1" "c2")
       (md:note-table-range (alist->hash-table '(("a1" 1) ("b1" 2) ("c2" 3)))))

 (test-assert "md:make-counters"
   (let ((my-note-table (md:make-counters 12 47 1 0)))
     (and (= 37 (hash-table-size my-note-table))
	  (string= "c1" (md:lowest-note my-note-table))
	  (string= "b3" (md:highest-note my-note-table))
	  (= 13 (car (hash-table-ref my-note-table "c2"))))))

 (test-assert "md:make-dividers"
   (let ((my-note-table (md:make-dividers 3500000 118 8 0 -4)))
     (and (= 56 (hash-table-size my-note-table))
	  (string= "e2" (md:lowest-note my-note-table))
	  (string= "a#6" (md:highest-note my-note-table))))))


(test-group
 "MD-Config/Master Config"

 (test "creating system target" "spectrum48"
       (md:target-id (md:config-target my-cfg)))

 (test "parsing description" "f224aa0c3de07810142a50825ad6a523"
       (string->md5sum (md:config-description my-cfg)))

 (test-assert "default commands created"
   (and (hash-table-exists? (md:config-commands my-cfg) 'AUTHOR)
	(hash-table-exists? (md:config-commands my-cfg) 'TITLE)
	(hash-table-exists? (md:config-commands my-cfg) 'LICENSE)))

 (test-assert "order commands created"
   (and (hash-table-exists? (md:config-commands my-cfg) 'R_DRUMS)
	(hash-table-exists? (md:config-commands my-cfg) 'R_CH1)
	(hash-table-exists? (md:config-commands my-cfg) 'R_CH2)))

 (test "all commands created" 9
       (hash-table-size (md:config-commands my-cfg))))


(test-group
 "MD-Config/Auxilliary Accessors"

 (define my-itree (md:config-itree my-cfg))

 (test "md:config-command-ref"
       (list (car (hash-table-ref (md:config-commands my-cfg) 'AUTHOR))
	     #f)
       (list (md:config-command-ref 'AUTHOR my-cfg)
	     (md:config-command-ref 'INVALID my-cfg)))

 (test "md:config-inode-ref"
       (list (car (hash-table-ref (md:config-inodes my-cfg) 'AUTHOR))
	     #f)
       (list (md:config-inode-ref 'AUTHOR my-cfg)
	     (md:config-inode-ref 'INVALID my-cfg)))

 (test "md:config-get-parent-node-id" 'CH2
       (md:config-get-parent-node-id 'NOTE2 my-itree))

 (test "md:config-get-node-ancestors-ids" '(CH1 PATTERNS GLOBAL)
       (md:config-get-node-ancestors-ids 'NOTE1 my-itree))

 (test "md:config-get-subnode-ids" '(DRUMS CH1 CH2 PATTERNS_ORDER)
       (md:config-get-subnode-ids 'PATTERNS my-itree))

 (test "md-config-get-subnode-type-ids" '(AUTHOR TITLE LICENSE BPM)
       (md:config-get-subnode-type-ids 'GLOBAL my-cfg 'field))

 (test "md:config-get-inode-source-command"
       (car (hash-table-ref (md:config-commands my-cfg) 'DRUM))
       (md:config-get-inode-source-command 'DRUM my-cfg))

 (test "md:config-get-node-default" #f
       (md:config-get-node-default 'DRUM my-cfg)))


(test-group
 "MD-Module/Inodes"

 (define my-global-inode-instance
   (car (alist-ref 0 (md:inode-instances (md:module-global-node my-mod)))))

 (test "md:get-subnode"
       (find (lambda (node)
	       (eq? (md:inode-cfg-id node) 'PATTERNS))
	     (md:inode-instance-val my-global-inode-instance))
       (md:get-subnode my-global-inode-instance 'PATTERNS))

 (define my-ch2-inode
   (md:get-subnode
    (car (alist-ref 0 (md:inode-instances
		       (md:get-subnode my-global-inode-instance 'PATTERNS))))
    'CH2))

 (test "md:inode-count-instances" 2 (md:inode-count-instances my-ch2-inode))

 (define my-note1-inode
   (md:get-subnode
    (car (alist-ref
	  0 (md:inode-instances
	     (md:get-subnode
	      (car (alist-ref
		    0 (md:inode-instances
		       (md:get-subnode my-global-inode-instance 'PATTERNS))))
	      'CH1))))
    'NOTE1))

 (test "md:get-node-command-cfg"
       (car (hash-table-ref (md:config-commands my-cfg) 'NOTE))
       (md:get-node-command-cfg my-note1-inode my-cfg))

 (test "md:eval-field-last-set" "c4"
       (md:eval-field-last-set
	5 my-note1-inode (md:get-node-command-cfg my-note1-inode my-cfg)))

 (test "md:eval-field" 0
       (md:eval-field 3 my-note1-inode
		      (md:get-node-command-cfg my-note1-inode my-cfg)))

 (define (make-test-inode instances)
   (md:make-inode 'FOO (map (lambda (i)
			      (list (car i)
				    (md:make-inode-instance (cadr i))))
			    instances)))

 (define my-mutable-inode
   (make-test-inode '((0 "n0") (1 "n1") (2 "n2"))))

 (test "md:node-set!"
       (make-test-inode '((0 "n0") (1 "foo") (2 "bar")))
       (begin (md:node-set! my-mutable-inode '((1 "foo") (2 "bar")))
	      my-mutable-inode))

 (test "md:node-remove!"
       (make-test-inode '((0 "n0") (1 "bar")))
       (begin (md:node-remove! my-mutable-inode '(1) #t)
	      my-mutable-inode))

 (test "md:node-insert!"
       (make-test-inode '((0 "n0") (1 "baz") (2 "bar")))
       (begin (md:node-insert! my-mutable-inode '((1 "baz")) #t)
	      my-mutable-inode)))


(test-group
 "MD-Module/Accessors"

 ;; GLOBAL/0/PATTERNS/0/CH1/0/NOTE1
 (define my-test-node
   (md:get-subnode
    (car (alist-ref
	  0
	  (md:inode-instances
	   (md:get-subnode
	    (car (alist-ref
		  0
		  (md:inode-instances
		   (md:get-subnode
		    (car (alist-ref
			  0
			  (md:inode-instances
			   (md:module-global-node my-mod))))
		    'PATTERNS))))
	    'CH1))))
    'NOTE1))

 (test "md:node-path" my-test-node
       ((md:node-path "0/PATTERNS/0/CH1/0/NOTE1")
	(md:module-global-node my-mod)))

 (test "md:node-instance-path"
       (car (alist-ref 4 (md:inode-instances my-test-node)))
       ((md:node-instance-path "0/PATTERNS/0/CH1/0/NOTE1/4")
	(md:module-global-node my-mod))))


(test-group
 "MD-Module/Parser"

 (define my-mod-expr (md:file->sexp "unittests/modules/huby-test.mdal"))

 (define my-drum-inode
   (md:make-inode 'DRUM
		  (zip (iota 16)
		       (circular-list (md:make-inode-instance "on")
				      (md:make-inode-instance '())
				      (md:make-inode-instance '())
				      (md:make-inode-instance '())))))

 (define my-note1-inode
   (md:make-inode 'NOTE1
		  (list (list 0 (md:make-inode-instance "a3"))
			(list 1 (md:make-inode-instance '()))
			(list 2 (md:make-inode-instance "rest"))
			(list 3 (md:make-inode-instance '()))
			(list 4 (md:make-inode-instance "c4"))
			(list 5 (md:make-inode-instance '()))
			(list 6 (md:make-inode-instance "rest"))
			(list 7 (md:make-inode-instance '()))
			(list 8 (md:make-inode-instance "e4"))
			(list 9 (md:make-inode-instance '()))
			(list 10 (md:make-inode-instance "rest"))
			(list 11 (md:make-inode-instance '()))
			(list 12 (md:make-inode-instance "g4"))
			(list 13 (md:make-inode-instance '()))
			(list 14 (md:make-inode-instance "rest"))
			(list 15 (md:make-inode-instance '())))))

 (define my-note2-inode0
   (md:make-inode 'NOTE2
		  (zip (iota 16)
		       (cons (md:make-inode-instance "a2")
			     (make-list 15 (md:make-inode-instance '()))))))

 (define my-note2-inode1
   (md:make-inode 'NOTE2
		  (zip (iota 16)
		       (cons (md:make-inode-instance "e2")
			     (make-list 15 (md:make-inode-instance '()))))))

 (define my-patterns-order-inode
   (md:make-inode
    'PATTERNS_ORDER
    (list (list 0 (md:make-inode-instance
		   (list (md:make-inode
			  'R_DRUMS
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance '()))))
			 (md:make-inode
			  'R_CH1
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance '()))))
			 (md:make-inode
			  'R_CH2
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance 1))))))))))

 (define my-patterns-subnodes
   (list (md:make-inode 'DRUMS (list (list 0 (md:make-inode-instance
					      (list my-drum-inode)
					      "beat0"))))
	 (md:make-inode 'CH1 (list (list 0 (md:make-inode-instance
					    (list my-note1-inode)))))
	 (md:make-inode 'CH2 (list (list 0 (md:make-inode-instance
					    (list my-note2-inode0)))
				   (list 1 (md:make-inode-instance
					    (list my-note2-inode1)))))
	 my-patterns-order-inode))


 (test "md:mod-parse-group-fields"
       (list
	(md:make-inode 'AUTHOR (list (list 0 (md:make-inode-instance
					       "utz"))))
	(md:make-inode 'TITLE (list (list 0 (md:make-inode-instance
					      "Huby Test"))))
	(md:make-inode 'LICENSE (list (list 0 (md:make-inode-instance
						"Creative Commons CC0"))))
	(md:make-inode 'BPM (list (list 0 (md:make-inode-instance 120)))))
       (md:mod-parse-group-fields my-mod-expr 'GLOBAL my-cfg))


 (test "md:mod-parse-block-fields"
       (list my-note1-inode)
       (md:mod-parse-block-fields
	(last (car (md:get-assignments
		    (last (car (md:get-assignments my-mod-expr 'PATTERNS)))
		    'CH1)))
 	'CH1 my-cfg))

 (test "md:mod-parse-group-blocks"
       my-patterns-subnodes
       (md:mod-parse-group-blocks
	(last (car (md:get-assignments my-mod-expr 'PATTERNS)))
 	'PATTERNS my-cfg))

 (define my-global-subnodes
   (append (md:mod-parse-group-fields my-mod-expr 'GLOBAL my-cfg)
	   (list (md:make-inode 'PATTERNS
				(list (list 0 (md:make-inode-instance
					       my-patterns-subnodes
					       "")))))))

 (test "md:mod-parse-group"
       my-global-subnodes
       (md:mod-parse-group my-mod-expr 'GLOBAL my-cfg))

 (test-assert "md:check-module-version: valid"
   (md:check-module-version my-mod-expr))
 (test "md:check-module-version: invalid"
       "Unsupported MDAL version: 4"
       (with-exn-handler (lambda (e) (message e))
			 (lambda ()
			   (md:check-module-version
			    '((assign MDAL_VERSION 0 "" 4))))))

 (test "md:mod-get-config-name"
       "Huby"
       (md:mod-get-config-name my-mod-expr))

 (test "md:mod-string->number"
       '(64 64 64)
       (list (md:mod-string->number "64")
	     (md:mod-string->number "$40")
	     (md:mod-string->number "#x40")))

 (test "md:file->module"
       (md:make-inode 'GLOBAL (list (list 0 (md:make-inode-instance
					      my-global-subnodes))))
       (md:module-global-node
	(md:file->module "unittests/modules/huby-test.mdal"
			 my-config-path)))

 (test "md:mod-get-group-instance-blocks"
       (list ((md:node-path "0/PATTERNS/0/DRUMS")
	      (md:module-global-node my-mod))
 	     ((md:node-path "0/PATTERNS/0/CH1") (md:module-global-node my-mod))
 	     ((md:node-path "0/PATTERNS/0/CH2") (md:module-global-node my-mod)))
       (md:mod-get-group-instance-blocks
 	((md:node-instance-path "0/PATTERNS/0") (md:module-global-node my-mod))
 	'PATTERNS my-cfg))

 (test "md:mod-get-group-instance-order"
       ((md:node-instance-path "0/PATTERNS/0/PATTERNS_ORDER/0")
 	(md:module-global-node my-mod))
       (md:mod-get-group-instance-order
 	((md:node-instance-path "0/PATTERNS/0") (md:module-global-node my-mod))
 	'PATTERNS)))


(test-group
 "MD-Module/Accessors II"

 (test "md:mod-split-node-list-at"
       (list (list ((md:node-path "0/PATTERNS/0/DRUMS")
		    (md:module-global-node my-mod)))
	     (list ((md:node-path "0/PATTERNS/0/CH1")
		    (md:module-global-node my-mod))
		   ((md:node-path "0/PATTERNS/0/CH2")
		    (md:module-global-node my-mod))
		   ((md:node-path "0/PATTERNS/0/PATTERNS_ORDER")
		    (md:module-global-node my-mod))))
       (md:mod-split-node-list-at
	'CH1 (md:inode-instance-val ((md:node-instance-path "0/PATTERNS/0")
				      (md:module-global-node my-mod)))))

 (test "md:mod-split-instances-at"
       (list (zip (iota 5)
		  (cons (md:make-inode-instance "a2")
			(make-list 4 (md:make-inode-instance '()))))
	     (zip (iota 11 5)
		  (make-list 11 (md:make-inode-instance '()))))
       (md:mod-split-instances-at
	5 (md:inode-instances ((md:node-path "0/PATTERNS/0/CH2/0/NOTE2")
			       (md:module-global-node my-mod)))))

 (test "md:mod-replace-subnode"
       (md:make-inode-instance
	(list (md:make-inode 'NOTE1
			     (zip (iota 4)
				  (make-list
				   4 (md:make-inode-instance '()))))))
       (md:mod-replace-subnode
	((md:node-instance-path "0/PATTERNS/0/CH1/0")
	 (md:module-global-node my-mod))
	(md:make-inode 'NOTE1
		       (zip (iota 4)
			    (make-list 4 (md:make-inode-instance '()))))))

 (test "md:mod-replace-inode-instance"
       (md:make-inode 'NOTE2
		      (zip (iota 16)
			   (make-list 16 (md:make-inode-instance '()))))
       (md:mod-replace-inode-instance
	((md:node-path "0/PATTERNS/0/CH2/0/NOTE2")
	 (md:module-global-node my-mod))
	0 (md:make-inode-instance '())))

 (test "md:mod-node-setter"
       (md:make-inode
	'CH2
	(list
	 (list 0 (md:make-inode-instance
		  (list (md:make-inode
			 'NOTE2
			 (zip (iota 16)
			      (make-list 16 (md:make-inode-instance '())))))))
	 (list 1 (md:make-inode-instance
		  (list (md:make-inode
			 'NOTE2
			 (zip (iota 16)
			      (cons (md:make-inode-instance "e2")
				    (make-list
				     15 (md:make-inode-instance '()))))))))))
       ((md:mod-node-setter "0")
	(md:make-inode 'NOTE2
		       (zip (iota 16)
			    (make-list 16 (md:make-inode-instance '()))))
	((md:node-path "0/PATTERNS/0/CH2")
	 (md:module-global-node my-mod))))

 (test "md:mod-get-row-values"
       '("on" "c4" #f)
       (md:mod-get-row-values ((md:node-instance-path "0/PATTERNS/0")
			       (md:module-global-node my-mod))
			      '(0 0 0)
			      4))

 (test "md:mod-get-block-values"
       '(("on" "a3" "a2")
	 (#f #f #f)
	 (#f "rest" #f)
	 (#f #f #f)
	 ("on" "c4" #f)
	 (#f #f #f)
	 (#f "rest" #f)
	 (#f #f #f)
	 ("on" "e4" #f)
	 (#f #f #f)
	 (#f "rest" #f)
	 (#f #f #f)
	 ("on" "g4" #f)
	 (#f #f #f)
	 (#f "rest" #f)
	 (#f #f #f))
       (md:mod-get-block-values ((md:node-instance-path "0/PATTERNS/0")
				 (md:module-global-node my-mod))
				'(0 0 0))))

(test-group
 "MD-Module/Compilation"

 (test "md:module->file"
       "00faea58672c888f872750c13c1daf1c"
       (begin
	 (md:module->file my-mod "test.mdal")
	 (file-md5sum "test.mdal")))

 (test "md:mod->bin"
       (list 33 108 128 205 9 128 195 0 128 78 35 70 35 94 35 86 35 126 35 183
	     200 229 213 197 110 6 2 38 0 41 41 41 25 229 111 16 246 217 225 209
	     6 8 26 19 217 103 87 217 126 35 217 111 95 254 44 40 1 175 50 70
	     128 193 197 243 175 29 32 3 93 149 0 21 32 2 84 148 159 230 16 211
	     254 219 254 47 230 31 32 5 11 120 177 32 227 33 88 39 217 251 32 2
	     16 196 193 209 225 40 165 201
	     238 57
	     113 128
	     1 5 2 6 3 7 4 8 0
	     44 67 0 0 44 57 0 0 44 45 0 0 44 38 0 0
	     44 67 0 0 44 57 0 0 44 45 0 0 44 38 0 0
	     135 135 135 135 135 135 135 135 135 135 135 135 135 135 135 135
	     180 180 180 180 180 180 180 180 180 180 180 180 180 180 180 180)
       (md:mod->bin my-mod #x8000))
 )

(test-group
 "New Config Compiler Generator"

 (define my-parent-node ((md:node-instance-path "0")
			 (md:module-global-node my-mod)))

 (test "md:int->bytes"
       (list 0 0 0 8)
       (md:int->bytes 8 4 'md:big-endian))

 ;; TODO new Huby.mdconf uses 140 bpm as default
 (test "md:transform-compose-expr"
       (quotient 1779661 120)
       ((md:transform-compose-expr '(quotient 1779661 ?BPM))
	0 my-parent-node '() my-cfg))

 (test "md:make-order-transformer"
       '(1 5 2 6 3 7 4 8)
       ((md:make-order-transformer 'shared-numeric-matrix 1)
	'((0 0) (1 1) (2 2) (3 3))))

 (test "md:make-ofield"
       (list (md:int->bytes (quotient 1779661 120) 2 'md:little-endian)
	     (list 0 0))
       (let ((my-onode1 (md:make-ofield my-cfg "" bytes: 2
					compose: '(quotient 1779661 ?BPM)))
	     (my-onode2 (md:make-ofield my-cfg "" bytes: 2
					compose: '(- $my-sym 8))))
	 (list (md:onode-val (car ((md:onode-fn my-onode1) my-onode1
				   my-parent-node my-cfg 0 '())))
	       (md:onode-val (car ((md:onode-fn my-onode2) my-onode2
				   my-parent-node my-cfg 0 '((my-sym 8))))))))

 (test "md:make-osymbol"
       8
       (let ((my-onode (md:make-osymbol my-cfg "" id: 'my-sym)))
	 (car (alist-ref 'my-sym
			 (third ((md:onode-fn my-onode) my-onode my-parent-node
				 my-cfg 8 '())))))))

(test-exit)
