(load "mdal.scm")
;; (define cfg (ssax:xml->sxml (open-input-file "config/Huby/Huby.mdconf") '()))
(define cfg (call-with-input-file "config/Huby/Huby.mdconf"
	      (lambda (x) (ssax:xml->sxml x '()))))
;; (define cmd-node (car ((sxpath "mdalconfig/command") cfg)))
;; (define my-cmd (md:xml-node->command cmd-node))
(define my-cfg (md:mdconf->config "config/Huby/Huby.mdconf"))
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
(define modlines (remove string-null?
			   (md:purge-comments
			    (md:purge-whitespace
			     (read-lines "../modules/huby-test.mdal")))))
(define my-mod (md:parse-module-file "../modules/huby-test.mdal" "config/"))
