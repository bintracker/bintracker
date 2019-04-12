;; -*- geiser-scheme-implementation: 'chicken -*-

(import scheme (chicken base) (chicken io) (chicken bitwise) srfi-1
	mdal test simple-md5 srfi-13 srfi-69 ssax sxpath sxpath-lolevel)

(define my-config-path "unittests/config/")
(define my-target (eval (car (read-list
			      (open-input-file "targets/spectrum48.scm")))))
(define my-cfg-data (call-with-input-file "unittests/config/Huby/Huby.mdconf"
		      (lambda (x) (ssax:xml->sxml x '()))))
(define my-cfg (md:mdconf->config "unittests/config/Huby/Huby.mdconf"))
(define my-mod (md:file->module "unittests/modules/huby-test.mdal"
				my-config-path))
(define my-global-node '("AUTHOR=\"foo\"" "TITLE=\"baz\""))
(define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
			"}" "CH2(0)={" "NOTE2=a-3" "}"))
(define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
(define (hash-table-equal? ht1 ht2)
  (null? (lset-difference equal? (hash-table->alist ht1)
			  (hash-table->alist ht2))))
(define (string->sxml-node str)
  (cadr (ssax:xml->sxml (open-input-string str) '())))


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
   (let ((my-note-table (md:make-dividers 118 8 0 -4)))
     (and (= 56 (hash-table-size my-note-table))
	  (string= "e2" (md:lowest-note my-note-table))
	  (string= "a#6" (md:highest-note my-note-table))))))


(test-group
 "MD-Command"

 (test "md:xml-command-node->command-flags"
       '(enable_modifiers use_last_set is_note)
       (md:xml-command-node->command-flags
	(cadr ((sxpath "mdalconfig/command") my-cfg-data))))

 (test-assert "md:xml-command-node->map"
   ;; TODO test with map file
   (let ((my-node (cadr ((sxpath "mdalconfig/command") my-cfg-data))))
     (hash-table-equal? (md:make-dividers 118 8 0 -4)
			(md:xml-command-node->map my-node my-config-path))))

 (test-assert "md:xml-node->command"
   (let ((my-cmd (md:xml-node->command
		  (cadr ((sxpath "mdalconfig/command") my-cfg-data))
		  my-target my-config-path)))
     (and (eq? 'ukey (md:command-type my-cmd))
	  (= 8 (md:command-bits my-cmd))
	  (string= "rest" (md:command-default my-cmd))
	  (not (md:command-reference-to my-cmd))
	  (equal? '(enable_modifiers use_last_set is_note)
		  (md:command-flags my-cmd))
	  (hash-table-equal? (md:make-dividers 118 8 0 -4)
			     (md:command-keys my-cmd))
	  (not (md:command-range my-cmd))
	  (string= "Set the note for the given channel."
		   (md:command-description my-cmd)))))

 (test-assert "md:xml-command-nodes->commands"
   (let ((my-commands (md:xml-command-nodes->commands
			((sxpath "mdalconfig/command") my-cfg-data)
			my-target my-config-path)))
     (and (hash-table-exists? my-commands "AUTHOR")
	  (hash-table-exists? my-commands "TITLE")
	  (hash-table-exists? my-commands "BPM")
	  (hash-table-exists? my-commands "DRUM")
	  (hash-table-exists? my-commands "NOTE")))))


(test-group
 "MD-Config/Inodes"

 (test-assert "md:xml-inode-get-range-arg"
   (let ((range-equal? (lambda (r1 r2)
			 (and (equal? (md:instance-range-min r1)
				      (md:instance-range-min r2))
			      (equal? (md:instance-range-max r1)
				      (md:instance-range-max r2))))))
     (and (range-equal? (md:make-instance-range 1 #f)
			(md:xml-inode-get-range-arg
			 (string->sxml-node "<id/>")))
	  (range-equal? (md:make-instance-range 2 2)
	  		(md:xml-inode-get-range-arg
	  		 (string->sxml-node "<id length=\"2\"/>")))
	  (range-equal? (md:make-instance-range 1 3)
	  		(md:xml-inode-get-range-arg
			 (string->sxml-node "<id max-length=\"3\"/>")))
	  (range-equal? (md:make-instance-range 2 4)
	  		(md:xml-inode-get-range-arg
			 (string->sxml-node
	  		  "<id min-length=\"2\" max-length=\"4\"/>"))))))

 (test-assert "md:parse-inode-config-id"
   (and (string= "TEST" (md:parse-inode-config-id
			 (string->sxml-node
			  "<node id=\"TEST\" from=\"other\"/>")))
	(string= "TEST2" (md:parse-inode-config-id
			  (string->sxml-node "<node from=\"TEST2\"/>")))))

 (test "md:generate-inode-order-tree"
       '("FOO_ORDER" (("R_BAR") ("R_BAZ")))
       (md:generate-inode-order-tree
	"FOO" '(("BAR") ("BAZ"))))

 (test "md:clone-inode-tree"
       '(("FOO1" (("BAR1") ("BAZ1"))) ("TOO1")
	 ("FOO2" (("BAR2") ("BAZ2"))) ("TOO2"))
       (md:clone-inode-tree '(("FOO" (("BAR") ("BAZ"))) ("TOO")) 2))

 (define my-itree
   '(("GLOBAL" (("AUTHOR") ("TITLE") ("BPM")
		("PATTERNS" (("DRUMS" (("DRUM"))) ("CH1" (("NOTE1")))
			     ("CH2" (("NOTE2")))
			     ("PATTERNS_ORDER" (("R_DRUMS") ("R_CH1")
						("R_CH2")))))))))

 (test "md:parse-inode-tree" my-itree (md:parse-inode-tree my-cfg-data))

 (test "md:create-order-commands" '()
       (lset-difference string=? '("R_DRUMS" "R_CH1" "R_CH2")
			(map car (hash-table->alist
				  (md:create-order-commands my-itree)))))

 (define my-iorder-configs
   (list (list "PATTERNS_ORDER"
		    (md:make-inode-config 'block (md:make-single-instance)
					  #f #f #f))
	      (list "R_DRUMS"
		    (md:make-inode-config 'field (md:make-instance-range 1 #f)
					  #f "R_DRUMS" #f))
	      (list "R_CH1"
		    (md:make-inode-config 'field (md:make-instance-range 1 #f)
					  #f "R_CH1" #f))
	      (list "R_CH2"
		    (md:make-inode-config 'field (md:make-instance-range 1 #f)
					  #f "R_CH2" #f))))

 (test "md:create-order-inodes" '()
       (lset-difference
	equal?
	my-iorder-configs
	(hash-table->alist (md:create-iorder-inodes my-itree))))

 (test "md:parse-ifield-config"
       (list "FOO" (md:make-inode-config 'field (md:make-single-instance)
					 #f "BAR" #f))
       (md:parse-ifield-config
	(string->sxml-node "<ifield id=\"FOO\" from=\"BAR\"/>")
	(md:make-single-instance)))

 (define my-drum-iblock-configs
   (list (list "DRUMS"
	       (md:make-inode-config 'block (md:make-instance-range 1 #f)
				     #f #f #f))
	 (list "DRUM"
	       (md:make-inode-config 'field (md:make-instance-range 1 #f)
				     #f "DRUM" #f))))

 (define my-cloned-iblock-configs
   (list (list "CH1"
	       (md:make-inode-config 'block (md:make-instance-range 1 #f)
				     #f #f #f))
	 (list "NOTE1"
	       (md:make-inode-config 'field (md:make-instance-range 1 #f)
				     #f "NOTE" #f))
	 (list "CH2"
	       (md:make-inode-config 'block (md:make-instance-range 1 #f)
				     #f #f #f))
	 (list "NOTE2"
	       (md:make-inode-config 'field (md:make-instance-range 1 #f)
				     #f "NOTE" #f))))

 (define my-igroup-configs
   (list (list "PATTERNS"
	       (md:make-inode-config
		'group (md:make-single-instance) #f #f "PATTERNS_ORDER"))))

 (define my-global-inode-configs
   (list (list "GLOBAL" (md:make-inode-config
			 'group (md:make-single-instance) #f #f #f))
	 (list "AUTHOR" (md:make-inode-config
			 'field (md:make-single-instance) #f "AUTHOR" #f))
	 (list "TITLE" (md:make-inode-config
			'field (md:make-single-instance) #f "TITLE" #f))
	 (list "BPM" (md:make-inode-config 'field (md:make-single-instance)
					   #f "BPM" #f))))

 (test "md:parse-iblock-config"
       ;; TODO Why does this not actually set subnodes in main node?
       my-drum-iblock-configs
       (md:parse-iblock-config
	(car ((sxpath "mdalconfig/igroup/iblock") my-cfg-data))
	(md:make-instance-range 1 #f)))

 (test "md:parse-clone-config"
       my-cloned-iblock-configs
       (md:parse-clone-config
	(car ((sxpath "mdalconfig/igroup/clone") my-cfg-data))
	(md:make-instance-range 1 #f)))

 (test "md:parse-igroup-config" '()
       (lset-difference
	equal?
	(append my-drum-iblock-configs my-cloned-iblock-configs
		my-igroup-configs)
	(md:parse-igroup-config
	 (car ((sxpath "mdalconfig/igroup") my-cfg-data))
	 (md:make-single-instance))))

 (test "md:make-global-group-inodes" '()
       ;; TODO: Misnomer, this creates CONFIG inodes
       (lset-difference
	equal?
	my-global-inode-configs
	(hash-table->alist (md:make-global-group-inodes my-cfg-data))))

 (test "md:mdconf->inodes" '()
       (lset-difference
	equal?
	(append my-igroup-configs my-drum-iblock-configs my-iorder-configs
		my-cloned-iblock-configs my-global-inode-configs)
	(hash-table->alist (md:mdconf->inodes my-cfg-data)))))

(test-group
 "MD-Config/Compiler Function Generation"

 (test-assert "md:config-transform-conditional-arg"
   (and (equal? 0 (md:config-transform-conditional-arg 0 #f))
	(equal? '((md:node-instance-path
		   (string-append "" parent-path "FOO" "/"
				  (->string instance-id)))
		  (md:mod-global-node mod))
		(md:config-transform-conditional-arg "?FOO" ""))))

 (test "md:config-transform-fn-arg"
       '((md:eval-field instance-id
			 ((md:node-path
			   (string-append "0/FOO/0" parent-path "BAR"))
			  (md:mod-global-node mod))
			 (md:config-get-inode-source-command
			  "BAR" (md:mod-cfg mod)))
	 (car (hash-table-ref symbols (read (open-input-string "FOO"))))
	 (car (hash-table-ref symbols mdal_order_FOO))
	 "FOO")
       (list (md:config-transform-fn-arg "?BAR" "0/FOO/0")
	     (md:config-transform-fn-arg "$FOO" "")
	     (md:config-transform-fn-arg "!FOO" "")
	     ;; TODO: does not yet recurse, and nesting ist wrong
	     ;; (md:config-transform-fn-arg "(if (md:is-set ?FOO) ?FOO ?BAR)" "0")
	     (md:config-transform-fn-arg "FOO" "")))

 ;; (test "md:config-resolve-fn-call")

 (test "md:config-fn-string->list"
       '(foo bar baz)
       (md:config-fn-string->list "(foo bar baz)"))

 ;; TODO fails for nested fns
 (test "md:config-direct-resolvable?"
       '(#t #f)
       (list (md:config-direct-resolvable? "(foo bar baz)")
	     (md:config-direct-resolvable? "(foo ?bar baz)")))

 ;; TODO only evaluates $, not !
 (test "md:config-get-required-symbols"
       '()
       (md:config-get-required-symbols
	(car ((sxpath "mdalconfig/output/group/block/field") my-cfg-data))))

 (test "md:config-make-resolve-check"
       '(#t #f)
       (let ((my-resolve-check
	      (md:config-make-resolve-check
	       (cadr ((sxpath "mdalconfig/output/field") my-cfg-data)))))
	 (list (my-resolve-check (alist->hash-table '((sequence_end 0))))
	       (my-resolve-check (make-hash-table)))))

 (test "md:config-make-converter-fn"
       '(#x04 #x80)
       ((md:config-make-converter-fn 2 #t) #x8004))

 ;; applying to end-of-order-marker field (direct-resolvable), bpm field
 ;; (regular node) and sequence-end field (symbol lookup)
 (test "md:config-make-ofield"
       '((field 1 (0))
	 (field 2 (#xef #x39))
	 (field 2 (#x05 #x80)))
       (let ((my-ofield0
	      (md:config-make-ofield
	       (list-ref ((sxpath "mdalconfig/output/field") my-cfg-data)
			 2)
	       "" my-cfg))
	     (my-ofield1
	      (car ((md:onode-fn
		     (md:config-make-ofield
		      (car ((sxpath "mdalconfig/output/field") my-cfg-data))
		      "0/" my-cfg))
		    my-mod "" 0 '() '())))
	     (my-ofield2
	      (car ((md:onode-fn
		     (md:config-make-ofield
		      (cadr ((sxpath "mdalconfig/output/field") my-cfg-data))
		      "0/" my-cfg))
		    my-mod "" 0 (alist->hash-table '((sequence_end #x800d)))
		    '()))))
	 (list (list (md:onode-type my-ofield0)
		     (md:onode-size my-ofield0)
		     (md:onode-val my-ofield0))
	       (list (md:onode-type my-ofield1)
		     (md:onode-size my-ofield1)
		     (md:onode-val my-ofield1))
	       (list (md:onode-type my-ofield2)
		     (md:onode-size my-ofield2)
		     (md:onode-val my-ofield2)))))

 (test "md:config-make-osymbol"
       '(symbol 0 #f #x800d)
       (let ((my-osym-eval-result
	      ((md:onode-fn
		(md:config-make-osymbol
		 (car ((sxpath "mdalconfig/output/symbol") my-cfg-data))
		 "0/" my-cfg))
	       my-mod "" 0 (alist->hash-table '((mdal_output_origin #x8000)))
	       (make-list 13 (md:make-onode 'field 1 0 #f)))))
	 (list (md:onode-type (car my-osym-eval-result))
	       (md:onode-size (car my-osym-eval-result))
	       (md:onode-val (car my-osym-eval-result))
	       (car (hash-table-ref (cadr my-osym-eval-result)
				    'sequence_end)))))


 (test "md:config-get-onode-source-ids"
       '("CH1" "DRUMS")
       (md:config-get-onode-source-ids
	(car ((sxpath "mdalconfig/output/group/block") my-cfg-data))))

 ;; (test "md:config-oblock-ofield-prototypes")
 ;; (test "md:mod-get-inode-instances")
 ;; (test "md:config-ifield-evaluator-prototypes")
 ;; (test "md:mod-get-iblock-lengths")
 ;; (test "md:config-make-block-compiler")
 ;; (test "md:config-make-oblock")

 (test "md:config-make-oorder"
       '(order 8 ((1 5) (2 6) (3 7) (4 8)))
       (let ((my-oorder
	      (car ((md:onode-fn
		     (md:config-make-oorder
		      (car ((sxpath "mdalconfig/output/order") my-cfg-data))
		      "0/PATTERNS/0" my-cfg))
		    my-mod "" 0
		    (alist->hash-table '((mdal_order_PATTERNS
					  ((0 4) (1 5) (2 6) (3 7)))))
		    '()))))
	 (list (md:onode-type my-oorder)
	       (md:onode-size my-oorder)
	       (md:onode-val my-oorder))))
 )


(test-group
 "MD-Config/Master Config"

 (test "creating system target" "spectrum48"
       (md:target-id (md:config-target my-cfg)))

 (test "parsing description" "12de8ca6e0afb0fc85e86287cc79d0f8"
       (string->md5sum (md:config-description my-cfg)))

 (test-assert "default commands created"
   (and (hash-table-exists? (md:config-commands my-cfg) "AUTHOR")
	(hash-table-exists? (md:config-commands my-cfg) "TITLE")))

 (test-assert "order commands created"
   (and (hash-table-exists? (md:config-commands my-cfg) "R_DRUMS")
	(hash-table-exists? (md:config-commands my-cfg) "R_CH1")
	(hash-table-exists? (md:config-commands my-cfg) "R_CH2")))

 (test "all commands created" 8
       (hash-table-size (md:config-commands my-cfg))))


(test-group
 "MD-Config/Auxilliary Accessors"

 (define my-itree (md:config-itree my-cfg))

 (test "md:config-get-parent-node-id" "CH2"
       (md:config-get-parent-node-id "NOTE2" my-itree))

 (test "md:config-get-node-ancestors-ids" '("CH1" "PATTERNS" "GLOBAL")
       (md:config-get-node-ancestors-ids "NOTE1" my-itree))

 (test "md:config-get-subnode-ids" '("DRUMS" "CH1" "CH2" "PATTERNS_ORDER")
       (md:config-get-subnode-ids "PATTERNS" my-itree))

 (test "md-config-get-subnode-type-ids" '("AUTHOR" "TITLE" "BPM")
       (md:config-get-subnode-type-ids "GLOBAL" my-cfg 'field))

 (test "md:config-get-inode-source-command"
       (car (hash-table-ref (md:config-commands my-cfg) "DRUM"))
       (md:config-get-inode-source-command "DRUM" my-cfg))

 (test "md:config-get-node-default" "false"
       (md:config-get-node-default "DRUM" my-cfg)))


(test-group
 "MD-Module/Inodes"

 (define my-global-inode-instance
   (car (alist-ref 0 (md:inode-instances (md:mod-global-node my-mod)))))

 (test "md:get-subnode"
       (find (lambda (node)
	       (string=? (md:inode-cfg-id node) "PATTERNS"))
	     (md:inode-instance-val my-global-inode-instance))
       (md:get-subnode my-global-inode-instance "PATTERNS"))

 (define my-ch2-inode
   (md:get-subnode
    (car (alist-ref 0 (md:inode-instances
		       (md:get-subnode my-global-inode-instance "PATTERNS"))))
    "CH2"))

 (test "md:inode-count-instances" 2 (md:inode-count-instances my-ch2-inode))

 (define my-note1-inode
   (md:get-subnode
    (car (alist-ref
	  0 (md:inode-instances
	     (md:get-subnode
	      (car (alist-ref
		    0 (md:inode-instances
		       (md:get-subnode my-global-inode-instance "PATTERNS"))))
	      "CH1"))))
    "NOTE1"))

 (test "md:get-node-comamnd-cfg"
       (car (hash-table-ref (md:config-commands my-cfg) "NOTE"))
       (md:get-node-command-cfg my-note1-inode my-cfg))

 (test "md:eval-field-last-set" "c4"
       (md:eval-field-last-set
	5 my-note1-inode (md:get-node-command-cfg my-note1-inode my-cfg)))

 (test "md:eval-field" 0
       (md:eval-field 3 my-note1-inode
		      (md:get-node-command-cfg my-note1-inode my-cfg))))


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
			   (md:mod-global-node my-mod))))
		    "PATTERNS"))))
	    "CH1"))))
    "NOTE1"))

 (test "md:node-path" my-test-node
       ((md:node-path "0/PATTERNS/0/CH1/0/NOTE1") (md:mod-global-node my-mod)))

 (test "md:node-instance-path"
       (car (alist-ref 4 (md:inode-instances my-test-node)))
       ((md:node-instance-path "0/PATTERNS/0/CH1/0/NOTE1/4")
	(md:mod-global-node my-mod))))


(test-group
 "MD-Module/Parser"

 (define my-mod-expr (md:file->sexp "unittests/modules/huby-test.mdal"))

 (define my-drum-inode
   (md:make-inode "DRUM"
		  (zip (iota 16)
		       (circular-list (md:make-inode-instance "on")
				      (md:make-inode-instance '())
				      (md:make-inode-instance '())
				      (md:make-inode-instance '())))))

 (define my-note1-inode
   (md:make-inode "NOTE1"
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
   (md:make-inode "NOTE2"
		  (zip (iota 16)
		       (cons (md:make-inode-instance "a2")
			     (make-list 15 (md:make-inode-instance '()))))))

 (define my-note2-inode1
   (md:make-inode "NOTE2"
		  (zip (iota 16)
		       (cons (md:make-inode-instance "e2")
			     (make-list 15 (md:make-inode-instance '()))))))

 (define my-patterns-order-inode
   (md:make-inode
    "PATTERNS_ORDER"
    (list (list 0 (md:make-inode-instance
		   (list (md:make-inode
			  "R_DRUMS"
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance '()))))
			 (md:make-inode
			  "R_CH1"
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance '()))))
			 (md:make-inode
			  "R_CH2"
			  (list (list 0 (md:make-inode-instance 0))
				(list 1 (md:make-inode-instance 1))))))))))

 (define my-patterns-subnodes
   (list (md:make-inode "DRUMS" (list (list 0 (md:make-inode-instance
					       (list my-drum-inode)
					       "beat0"))))
	 (md:make-inode "CH1" (list (list 0 (md:make-inode-instance
					     (list my-note1-inode)))))
	 (md:make-inode "CH2" (list (list 0 (md:make-inode-instance
					     (list my-note2-inode0)))
				    (list 1 (md:make-inode-instance
					     (list my-note2-inode1)))))
	 my-patterns-order-inode))


 (test "md:mod-parse-group-fields"
       (list
	(md:make-inode "AUTHOR" (list (list 0 (md:make-inode-instance
					       "utz"))))
	(md:make-inode "TITLE" (list (list 0 (md:make-inode-instance
					      "Huby Test"))))
	(md:make-inode "BPM" (list (list 0 (md:make-inode-instance 120)))))
       (md:mod-parse-group-fields my-mod-expr "GLOBAL" my-cfg))


 (test "md:mod-parse-block-fields"
       (list my-note1-inode)
       (md:mod-parse-block-fields
	(last (car (md:get-assignments
		    (last (car (md:get-assignments my-mod-expr "PATTERNS")))
		    "CH1")))
 	"CH1" my-cfg))

 (test "md:mod-parse-group-blocks"
       my-patterns-subnodes
       (md:mod-parse-group-blocks
	(last (car (md:get-assignments my-mod-expr "PATTERNS")))
 	"PATTERNS" my-cfg))

 (define my-global-subnodes
   (append (md:mod-parse-group-fields my-mod-expr "GLOBAL" my-cfg)
	   (list (md:make-inode "PATTERNS"
				(list (list 0 (md:make-inode-instance
					       my-patterns-subnodes
					       "")))))))

 (test "md:mod-parse-group"
       my-global-subnodes
       (md:mod-parse-group my-mod-expr "GLOBAL" my-cfg))

 (test-assert "md:check-module-version: valid"
   (md:check-module-version my-mod-expr))
 (test-error "md:check-module-version: invalid"
	     (md:check-module-version '((assign CONFIG 0 "" 4))))

 (test "md:mod-get-config-name"
       "Huby"
       (md:mod-get-config-name my-mod-expr))

 (test "md:mod-string->number"
       '(64 64 64)
       (list (md:mod-string->number "64")
	     (md:mod-string->number "$40")
	     (md:mod-string->number "#x40")))

 (test "md:file->module"
       (md:make-inode "GLOBAL" (list (list 0 (md:make-inode-instance
					      my-global-subnodes))))
       (md:mod-global-node
	(md:file->module "unittests/modules/huby-test.mdal"
			 my-config-path)))

 (test "md:mod-get-group-instance-blocks"
       (list ((md:node-path "0/PATTERNS/0/DRUMS") (md:mod-global-node my-mod))
	     ((md:node-path "0/PATTERNS/0/CH1") (md:mod-global-node my-mod))
	     ((md:node-path "0/PATTERNS/0/CH2") (md:mod-global-node my-mod)))
       (md:mod-get-group-instance-blocks
	((md:node-instance-path "0/PATTERNS/0") (md:mod-global-node my-mod))
	"PATTERNS" my-cfg))

 (test "md:mod-get-group-instance-order"
       ((md:node-instance-path "0/PATTERNS/0/PATTERNS_ORDER/0")
	(md:mod-global-node my-mod))
       (md:mod-get-group-instance-order
	((md:node-instance-path "0/PATTERNS/0") (md:mod-global-node my-mod))
	"PATTERNS"))

 (test "md:mod-make-default-order"
       (md:make-inode
	"PATTERNS_ORDER"
	(list
	 (list 0 (md:make-inode-instance
		  (list (md:make-inode
			 "R_DRUMS"
			 (list (list 0 (md:make-inode-instance 0 ""))))
			(md:make-inode
			 "R_CH1"
			 (list (list 0 (md:make-inode-instance 0 ""))))
			(md:make-inode
			 "R_CH2"
			 (list (list 0 (md:make-inode-instance 0 "")))))))))
       (md:mod-make-default-order 1 "PATTERNS" my-cfg)))


(test-group
 "MD-Module/Node Reordering"

 (test "md:mod-extract-field-values"
       (cons "a2" (make-list 15 '()))
       (md:mod-extract-field-values
	((md:node-instance-path "0/PATTERNS/0/CH2/0")
	 (md:mod-global-node my-mod))
	"NOTE2"))

 (test "md:mod-fill-empty-values"
       (append (make-list 3 "foo")
	       (make-list 3 "bar")
	       (make-list 3 "baz"))
       (md:mod-fill-empty-values '("foo" () () "bar" () () "baz" () ())))

 (test "md:mod-enumerate-instances"
       '((1 "foo") (2 "bar") (3 "baz"))
       (md:mod-enumerate-instances 1 '("foo" "bar" "baz")))

 (test "md:mod-merge-fields"
       (append (cons (md:make-inode-instance "a2")
		     (make-list 15 (md:make-inode-instance '())))
	       (cons (md:make-inode-instance "e2")
		     (make-list 15 (md:make-inode-instance '()))))
       (md:mod-merge-fields ((md:node-path "0/PATTERNS/0/CH2")
			 (md:mod-global-node my-mod))
			"NOTE2" '(0 1)))

 (define my-field-chunks
   (append
    (make-list 2 (zip (iota 8)
		      (cons (md:make-inode-instance "a2")
			    (make-list
			     7 (md:make-inode-instance '())))))
    (make-list 2 (zip (iota 8)
		      (cons (md:make-inode-instance "e2")
			    (make-list
			     7 (md:make-inode-instance '())))))))

 (test "md:mod-split-fields"
       my-field-chunks
       (md:mod-split-fields
	8
	(md:mod-merge-fields ((md:node-path "0/PATTERNS/0/CH2")
			      (md:mod-global-node my-mod))
			     "NOTE2" '(0 1))
	"NOTE2" my-cfg))

 (define my-resized-block-instances
   (zip (iota 4)
	(map (lambda (chunk)
	       (md:make-inode-instance (list (md:make-inode "NOTE2" chunk))))
	     my-field-chunks)))

 (test "md:mod-chunks->block-instances"
       my-resized-block-instances
       (md:mod-chunks->block-instances (list my-field-chunks) '("NOTE2")))

 (test "md:mod-split-block-instances"
       (md:make-inode "CH2" my-resized-block-instances)
       (md:mod-split-block-instances
	8 ((md:node-path "0/PATTERNS/0/CH2") (md:mod-global-node my-mod))
	((md:node-instance-path "0/PATTERNS/0/PATTERNS_ORDER/0")
	 (md:mod-global-node my-mod))
	my-cfg))

 (define (my-resized-group-blocks-generator)
   (let ((order-inode-instances
	  (zip (iota 4)
	       (list (md:make-inode-instance 0)
		     (md:make-inode-instance 1)
		     (md:make-inode-instance 2)
		     (md:make-inode-instance 3)))))
     (md:make-inode-instance
      (list
       (md:make-inode
	"DRUMS"
	(zip (iota 4)
	     (make-list
	      4 (md:make-inode-instance
		 (list (md:make-inode
			"DRUM"
			(zip (iota 8)
			     (list (md:make-inode-instance "on")
				   (md:make-inode-instance '())
				   (md:make-inode-instance '())
				   (md:make-inode-instance '())
				   (md:make-inode-instance "on")
				   (md:make-inode-instance '())
				   (md:make-inode-instance '())
				   (md:make-inode-instance '())))))))))
       (md:make-inode
	"CH1"
	(list
	 (list 0 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE1"
			 (zip (iota 8)
			      (list (md:make-inode-instance "a3")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "c4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())))))))
	 (list 1 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE1"
			 (zip (iota 8)
			      (list (md:make-inode-instance "e4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "g4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())))))))
	 (list 2 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE1"
			 (zip (iota 8)
			      (list (md:make-inode-instance "a3")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "c4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())))))))
	 (list 3 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE1"
			 (zip (iota 8)
			      (list (md:make-inode-instance "e4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "g4")
				    (md:make-inode-instance '())
				    (md:make-inode-instance "rest")
				    (md:make-inode-instance '())))))))))
       (md:make-inode "CH2" my-resized-block-instances)
       (md:make-inode
	"PATTERNS_ORDER"
	(list
	 (list 0 (md:make-inode-instance
		  (list (md:make-inode "R_DRUMS" order-inode-instances)
			(md:make-inode "R_CH1" order-inode-instances)
			(md:make-inode "R_CH2" order-inode-instances))
		  "")))))
      "")))

 (test "md:mod-split-group-instance-blocks"
       (my-resized-group-blocks-generator)
       (md:mod-split-group-instance-blocks
	8 ((md:node-instance-path "0/PATTERNS/0")
	   (md:mod-global-node my-mod))
	"PATTERNS" my-cfg))

 (test "md:mod-reorder-group"
       (md:make-inode "PATTERNS"
		      (list (list 0 (my-resized-group-blocks-generator))))
       (md:mod-reorder-group
	8 ((md:node-path "0/PATTERNS") (md:mod-global-node my-mod))
	my-cfg))
 )

(test-group
 "MD-Module/Accessors II"

 (test "md:mod-split-node-list-at"
       (list (list ((md:node-path "0/PATTERNS/0/DRUMS")
		    (md:mod-global-node my-mod)))
	     (list ((md:node-path "0/PATTERNS/0/CH1")
		    (md:mod-global-node my-mod))
		   ((md:node-path "0/PATTERNS/0/CH2")
		    (md:mod-global-node my-mod))
		   ((md:node-path "0/PATTERNS/0/PATTERNS_ORDER")
		    (md:mod-global-node my-mod))))
       (md:mod-split-node-list-at
	"CH1" (md:inode-instance-val ((md:node-instance-path "0/PATTERNS/0")
				      (md:mod-global-node my-mod)))))

 (test "md:mod-split-instances-at"
       (list (zip (iota 5)
		  (cons (md:make-inode-instance "a2")
			(make-list 4 (md:make-inode-instance '()))))
	     (zip (iota 11 5)
		  (make-list 11 (md:make-inode-instance '()))))
       (md:mod-split-instances-at
	5 (md:inode-instances ((md:node-path "0/PATTERNS/0/CH2/0/NOTE2")
			       (md:mod-global-node my-mod)))))

 (test "md:mod-replace-subnode"
       (md:make-inode-instance
	(list (md:make-inode "NOTE1"
			     (zip (iota 4)
				  (make-list
				   4 (md:make-inode-instance '()))))))
       (md:mod-replace-subnode
	((md:node-instance-path "0/PATTERNS/0/CH1/0")
	 (md:mod-global-node my-mod))
	(md:make-inode "NOTE1"
		       (zip (iota 4)
			    (make-list 4 (md:make-inode-instance '()))))))

 (test "md:mod-replace-inode-instance"
       (md:make-inode "NOTE2"
		      (zip (iota 16)
			   (make-list 16 (md:make-inode-instance '()))))
       (md:mod-replace-inode-instance
	((md:node-path "0/PATTERNS/0/CH2/0/NOTE2")
	 (md:mod-global-node my-mod))
	0 (md:make-inode-instance '())))

 (test "md:mod-node-setter"
       (md:make-inode
	"CH2"
	(list
	 (list 0 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE2"
			 (zip (iota 16)
			      (make-list 16 (md:make-inode-instance '())))))))
	 (list 1 (md:make-inode-instance
		  (list (md:make-inode
			 "NOTE2"
			 (zip (iota 16)
			      (cons (md:make-inode-instance "e2")
				    (make-list
				     15 (md:make-inode-instance '()))))))))))
       ((md:mod-node-setter "0")
	(md:make-inode "NOTE2"
		       (zip (iota 16)
			    (make-list 16 (md:make-inode-instance '()))))
	((md:node-path "0/PATTERNS/0/CH2")
	 (md:mod-global-node my-mod)))))

(test-group
 "MD-Module/Compilation"

 (test "md:mod->bin"
       (list #xef #x39
	     #x05 #x80
	     #x01 #x05
	     #x02 #x06
	     #x03 #x07
	     #x04 #x08
             #x00
	     #x1e #x1e #x00 #x00 #x24 #x24 #x00 #x00
	     #x2e #x2e #x00 #x00 #x36 #x36 #x00 #x00
	     #x1e #x1e #x00 #x00 #x24 #x24 #x00 #x00
	     #x2e #x2e #x00 #x00 #x36 #x36 #x00 #x00
	     #x0f #x0f #x0f #x0f #x0f #x0f #x0f #x0f
	     #x0f #x0f #x0f #x0f #x0f #x0f #x0f #x0f
	     #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b
	     #x0b #x0b #x0b #x0b #x0b #x0b #x0b #x0b)
       (md:mod->bin my-mod #x8000)))

(test-exit)
