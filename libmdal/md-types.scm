;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-TYPES
;;; md-module record types and additional accessors

(module md-types *

  (import scheme chicken srfi-1 srfi-13 data-structures)
  (use srfi-69 md-helpers)

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: INPUT NODES
  ;; ---------------------------------------------------------------------------

  ;;; val can be one of
  ;;;   () -> inactive node
  ;;;   a string of the actual value
  ;;;   a list of subnodes
  (define-record-type md:inode-instance
    (md:make-inode-instance-base val name)
    md:node-instance?
    (val md:inode-instance-val md:set-inode-instance-val!)
    (name md:inode-instance-name md:set-inode-instance-name!))

  (define (md:make-inode-instance val #!optional (name ""))
    (md:make-inode-instance-base val name))

  (define-record-printer (md:inode-instance i out)
    (begin
      (fprintf out "#<md:inode-instance>: ~A\n" (md:inode-instance-name i))
      (fprintf out "~S\n" (md:inode-instance-val i))))

  ;;; return the subnode of the given id
  (define (md:get-subnode inode-instance subnode-id)
    (find (lambda (node)
	    (string=? (md:inode-cfg-id node) subnode-id))
	  (md:inode-instance-val inode-instance)))

  ;;; it might be desirable to have 'instances' be a hash map, and only turn it
  ;;; into an alist which is then sorted on request
  ;;; (eg md:inode-get-sorted-inst)
  (define-record-type md:inode
    (md:make-inode cfg-id instances)
    md:inode?
    (cfg-id md:inode-cfg-id md:set-inode-cfg-id!)
    (instances md:inode-instances md:set-inode-instances!))

  (define-record-printer (md:inode node out)
    (begin
      (fprintf out "#<md:inode: ~A>\n" (md:inode-cfg-id node))
      (for-each (lambda (x) (fprintf out "instance ~S: ~S\n" (car x) (cdr x)))
		(md:inode-instances node))))

  ;;; return the number of instances in the given inode
  (define (md:inode-count-instances node)
    (if (not (md:inode-instances node))
	0
	(length (md:inode-instances node))))

  ;; ---------------------------------------------------------------------------
  ;;; ## MDMOD: MODULE
  ;; ---------------------------------------------------------------------------

  (define-record-type md:module
    (md:make-module cfg-id cfg global-node)
    md:module?
    (cfg-id md:mod-cfg-id md:set-mod-cfg-id!)
    (cfg md:mod-cfg md:set-mod-cfg!)
    (global-node md:mod-global-node md:set-mod-global-node!))

  (define-record-printer (md:module mod out)
    (begin
      (fprintf out "#<md:module>\n\nCONFIG ID: ~A\n\n" (md:mod-cfg-id mod))
      (fprintf out "CONFIG:\n~S\n" (md:mod-cfg mod))))

  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given numeric instance id
  ;;; TODO should check if node instance actually exists
  (define (md:mod-get-node-instance id)
    (lambda (node)
      (car (alist-ref id (md:inode-instances node)))))

  ;;; lo-level api, generate a function that takes an inode as param, and
  ;;; returns the node matching the given path
  (define (md:make-npath-fn pathlist)
    (if (= 2 (length pathlist))
	(lambda (node)
	  (find (lambda (subnode-id)
		  (string=? (md:inode-cfg-id subnode-id)
			    (cadr pathlist)))
		(md:inode-instance-val
		 ((md:mod-get-node-instance (string->number (car pathlist)))
		  node))))
	(lambda (node)
	  ((md:make-npath-fn (cddr pathlist))
	   ((md:make-npath-fn (take pathlist 2)) node)))))

  ;;; generate a function that takes an inode as parameter, and returns the node
  ;;; instance matching the given path
  (define (md:node-instance-path path)
    (letrec ((make-instance-path-fn
	      (lambda (pathlist)
		(if (= 1 (length pathlist))
		    (md:mod-get-node-instance (string->number (car pathlist)))
		    (lambda (node)
		      ((make-instance-path-fn (cddr pathlist))
		       ((md:make-npath-fn (take pathlist 2)) node)))))))
      (make-instance-path-fn (string-split path "/"))))

  ;;; generate a function that takes an inode as parameter, and returns the
  ;;; subnode matching the given path
  (define (md:node-path path)
    (md:make-npath-fn (string-split path "/")))

  ) ;; end module md-types
