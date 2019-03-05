;; -*- geiser-scheme-implementation: 'chicken -*-

(use mdal test simple-md5 srfi-13 srfi-69 ssax sxpath sxpath-lolevel)

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

(test-group
 "MD-Helpers"
 (test-assert "md:in-range?"
   (and (md:in-range? 1 (md:make-range 0 2))
	(not (md:in-range? 3 (md:make-range 0 2)))))
 (test "md:make-pairs" '((a 1) (b 2)) (md:make-pairs '(a 1 b 2)))
 (test-assert "md:add-hash-table-entry"
   (hash-table-equal?
    (md:add-hash-table-entry (alist->hash-table '((a 1) (b 2)))
			     'c 3)
    (alist->hash-table '((a 1) (b 2) (c 3))))))

(test-group
 "utils/MD-Note-Table"
 (define my-test-table (alist->hash-table '(("a1" 1) ("b1" 2) ("c2" 3))))
 (test "md:lowest-note" "a1" (md:lowest-note my-test-table))
 (test "md:highest-note" "c2" (md:highest-note my-test-table))
 ;; TODO test make-counters
 (test-assert "md:make-dividers"
   (let ((my-note-table (md:make-dividers 118 8 0)))
     (and (= 91 (hash-table-size my-note-table))
	  (string= "e3" (md:lowest-note my-note-table))
	  (string= "b9" (md:highest-note my-note-table))))))

;; (md:make-dividers 118 8 0)

(test-group
 "MD-Command"
 (test "md:xml-command-node->command-flags"
       '(enable_modifiers use_last_set is_note)
       (md:xml-command-node->command-flags
	(cadr ((sxpath "mdalconfig/command") my-cfg-data))))
 (test-assert "md:xml-command-node->map"
   ;; TODO test with map file
   (let ((my-node (cadr ((sxpath "mdalconfig/command") my-cfg-data))))
     (hash-table-equal? (md:make-dividers 118 8 0)
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
	  (hash-table-equal? (md:make-dividers 118 8 0)
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
 "MD-Config"
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

(test-exit)


;; ;; (define my-ifield (car ((sxpath "mdalconfig/ifield") cfg)))
;; ;; (define my-node (md:parse-inode-config my-ifield (md:make-single-instance)))
;; ;; (define blknode (car ((sxpath "mdalconfig/igroup/clone/iblock") cfg)))
;; ;; (define blk (md:parse-iblock-config blknode (md:make-instance-range 1 #f)))
;; ;; (define grpnode (car ((sxpath "mdalconfig/igroup") cfg)))
;; ;; (define grp (md:parse-igroup-config grpnode (md:make-single-instance)))
;; ;; (define cln (md:parse-clone-config
;; ;; 	     (car ((sxpath "mdalconfig/igroup/clone") cfg))
;; ;; 	     (md:make-single-instance)))
;; ;; (define modlines (remove string-null?
;; ;; 			   (md:purge-comments
;; ;; 			    (md:purge-whitespace
;; ;; 			     (read-lines "../modules/huby-test.mdal")))))
;; (define my-mod (md:parse-module-file "unittests/modules/huby-test.mdal"
;; 				     "unittests/config/"))
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
