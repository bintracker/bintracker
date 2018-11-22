(load "mdal.scm")
;; (define cfg (ssax:xml->sxml (open-input-file "config/Huby/Huby.mdconf") '()))
;; (define cfg (call-with-input-file "config/Huby/Huby.mdconf"
;; 	      (lambda (x) (ssax:xml->sxml x '()))))
;; (define cmd-node (car ((sxpath "mdalconfig/command") cfg)))
;; (define my-cmd (md:xml-node->command cmd-node))
(define my-cfg (md:mdconf->config "unittests/config/Huby/Huby.mdconf"))
;; (define my-ifield (car ((sxpath "mdalconfig/ifield") cfg)))
;; (define my-node (md:parse-inode-config my-ifield (md:make-single-instance)))
;; (define globals (md:make-global-group-config cfg))
;; (define blknode (car ((sxpath "mdalconfig/igroup/clone/iblock") cfg)))
;; (define blk (md:parse-iblock-config blknode (md:make-instance-range 1 #f)))
;; (define grpnode (car ((sxpath "mdalconfig/igroup") cfg)))
;; (define grp (md:parse-igroup-config grpnode (md:make-single-instance)))
;; (define cln (md:parse-clone-config
;; 	     (car ((sxpath "mdalconfig/igroup/clone") cfg))
;; 	     (md:make-single-instance)))
;; (define modlines (remove string-null?
;; 			   (md:purge-comments
;; 			    (md:purge-whitespace
;; 			     (read-lines "../modules/huby-test.mdal")))))
(define my-mod (md:parse-module-file "unittests/modules/huby-test.mdal"
				     "unittests/config/"))
(define my-global-node '("AUTHOR=\"foo\"" "TITLE=\"baz\""))
(define my-group-node '("CH1(0)={" "NOTE1=a-1" "." "}" "CH1(1)={" "NOTE1=a-2"
			"}" "CH2(0)={" "NOTE2=a-3" "}"))
(define my-block-node '("NOTE1=a-3" "." "NOTE1=a-4"))
(define my-test-field (md:eval-field
		       4 ((md:node-path "0/PATTERNS/0/CH1/0/NOTE1")
			  (md:mod-global-node my-mod))
		       (md:get-node-command-cfg
			((md:node-path "0/PATTERNS/0/CH1/0/NOTE1")
			 (md:mod-global-node my-mod)) my-cfg)))
(define my-test-onode ((md:onode-fn (second (md:config-otree my-cfg)))
		       my-mod "" 0 '()))
