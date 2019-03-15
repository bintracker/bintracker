;; -*- geiser-scheme-implementation: 'chicken -*-

(use mdal test simple-md5 srfi-13 srfi-69 ports ssax sxpath sxpath-lolevel)

(define my-config-path "unittests/config/")
(define my-target (eval (car (read-file "targets/spectrum48.scm"))))
(define my-cfg-data (call-with-input-file "unittests/config/Huby/Huby.mdconf"
		      (lambda (x) (ssax:xml->sxml x '()))))
(define my-cfg (md:mdconf->config "unittests/config/Huby/Huby.mdconf"))
(define my-mod (md:parse-module-file "unittests/modules/huby-test.mdal"
				     my-config-path))
(define my-global-node '("AUTHOR=\"foo\"" "TITLE=\"baz\""))
(define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
			"}" "CH2(0)={" "NOTE2=a-3" "}"))
(define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
(define (hash-table-equal? ht1 ht2)
  (null? (lset-difference equal? (hash-table->alist ht1)
			  (hash-table->alist ht2))))
(define (string->sxml-node str)
  (cadr (call-with-input-string str (lambda (x) (ssax:xml->sxml x '())))))


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
	'((a 1) (b 2) (c 3)))))


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


(test-exit)


;; ;; (define modlines (remove string-null?
;; ;; 			   (md:purge-comments
;; ;; 			    (md:purge-whitespace
;; ;; 			     (read-lines "../modules/huby-test.mdal")))))

;; (define my-global-node '("AUTHOR=\"foo\"" "TITLE=\"baz\""))
;; (define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
;; 			"}" "CH2(0)={" "NOTE2=a-3" "}"))
;; (define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
;; (define my-test-field (md:eval-field
;; 		       4 ((md:node-path "0/PATTERNS/0/CH1/0/NOTE1")
;; 			  (md:mod-global-node my-mod))
;; 		       (md:get-node-command-cfg
;; 			((md:node-path "0/PATTERNS/0/CH1/0/NOTE1")
;; 			 (md:mod-global-node my-mod)) my-cfg)))
;; ;; (define my-test-onode ((md:onode-fn (second (md:config-otree my-cfg)))
;; ;; 		       my-mod "" 0 '() '()))
;; (define my-reordered-node
;;   (md:mod-reorder-group 8 ((md:node-path "0/PATTERNS")
;; 			   (md:mod-global-node my-mod)) my-cfg))
;; ;; (define my-reordered-global-node
;; ;;   ((md:mod-node-setter "0") my-reordered-node (md:mod-global-node my-mod)))
;; (define my-resize-fn (md:config-make-resize-fn cfg))
;; (define my-reordered-global-node
;;   (my-resize-fn (md:mod-global-node my-mod) my-cfg))
;; (define my-compile-fn (md:config-make-compiler cfg))
