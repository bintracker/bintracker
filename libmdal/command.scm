;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.


;; -----------------------------------------------------------------------------
;; MDCONF: COMMANDS
;; -----------------------------------------------------------------------------

(define md:cmd-type-int 0)
(define md:cmd-type-uint 1)
(define md:cmd-type-key 2)
(define md:cmd-type-ukey 3)
(define md:cmd-type-reference 5)
(define md:cmd-type-string 6)
(define md:cmd-type-trigger 7)
(define md:cmd-type-label 8)
(define md:cmd-type-unknown 9)

;; command config record
;; fields:
;; type - one of md:cmd-type-***
;; bits - uint number of bits in command
;; default - default value string
;; reference-to - #f or an identifier string
;; keys - #f or a hash-map
;; flags - list of command flags
;; tags - #f or a list of strings
;; range - #f or an md:range object
;; description - #f or a string
(define-record-type md:command
  (md:make-command type bits default reference-to keys flags tags range
                   description)
  md:command?
  (type md:command-type md:command-set-type!)
  (bits md:command-bits md:command-set-bits!)
  (default md:command-default md:command-set-default!)
  (reference-to md:command-reference-to md:command-set-reference-to!)
  (keys md:command-keys md:command-set-keys!)
  (flags md:command-flags md:command-set-flags!)
  (tags md:command-tags md:command-set-tags!)
  (range md:command-range md:command-set-range!)
  (description md:command-description md:command-set-description!))

;; check if the given command has the given tag
(define (md:command-has-tag? cmd tag)
  (if (md:command-tags cmd)
      (any (lambda (x) (string-ci= x tag)) (md:command-tags cmd))
      #f))

;; check if the given command has the given flag
(define (md:command-has-flag? cmd flag)
  (if (find (lambda (x)
	      (eq? x flag))
	    (md:command-flags cmd))
      #t #f))

(define (md:command-has-flags? cmd)
  (if (null? (md:command-flags cmd))
      #f #t))

(define (md:int-command? cmd) (= (md:command-type cmd) md:cmd-type-int))
(define (md:uint-command? cmd) (= (md:command-type cmd) md:cmd-type-uint))
(define (md:key-command? cmd) (= (md:command-type cmd) md:cmd-type-key))
(define (md:ukey-command? cmd) (= (md:command-type cmd) md:cmd-type-ukey))
(define (md:reference-command? cmd)
  (= (md:command-type cmd) md:cmd-type-reference))
(define (md:string-command? cmd) (= (md:command-type cmd) md:cmd-type-string))
(define (md:trigger-command? cmd) (= (md:command-type cmd) md:cmd-type-trigger))
(define (md:label-command? cmd) (= (md:command-type cmd) md:cmd-type-label))

(define (md:string->command-type str)
  (cond ((string-ci= str "int") md:cmd-type-int)
        ((string-ci= str "uint") md:cmd-type-uint)
        ((string-ci= str "key") md:cmd-type-key)
        ((string-ci= str "ukey") md:cmd-type-ukey)
        ((string-ci= str "reference") md:cmd-type-reference)
        ((string-ci= str "string") md:cmd-type-string)
        ((string-ci= str "trigger") md:cmd-type-trigger)
	((string-ci= str "label") md:cmd-type-label)
        (else #f)))

(define (md:command-type->string type)
  (cond ((= type md:cmd-type-int) "Int")
        ((= type md:cmd-type-uint) "UInt")
        ((= type md:cmd-type-key) "Key")
        ((= type md:cmd-type-ukey) "UKey")
        ((= type md:cmd-type-reference) "Reference")
        ((= type md:cmd-type-string) "String")
        ((= type md:cmd-type-trigger) "Trigger")
	((= type md:cmd-type-label) "Label")
        (else "invalid type")))

(define-record-printer (md:command cmd out)
  (begin
    (fprintf out "#<md:command>\ntype:    ~A\nbits:    ~S\ndefault: ~A~!"
             (md:command-type->string (md:command-type cmd))
             (md:command-bits cmd)
             (md:command-default cmd))
    (when (md:command-reference-to cmd)
      (fprintf out "\nref:     ~S" (md:command-reference-to cmd)))
    (when (md:command-keys cmd)
      (fprintf out "\nkeys:    ~S" (md:command-keys cmd)))
    (when (md:command-has-flags? cmd)
      (fprintf out "\nflags:   ~S" (md:command-flags cmd)))
    (when (md:command-tags cmd)
      (fprintf out "\ntags:    ~S" (md:command-tags cmd)))
    (when (md:command-range cmd)
      (fprintf out "\nrange:   ~S - ~S"
               (md:range-min (md:command-range cmd))
               (md:range-max (md:command-range cmd))))
    (when (md:command-description cmd)
      (fprintf out "\ndescription:\n~A~!" (md:command-description cmd)))))

;; extract the command flags from a command mdconf node
(define (md:xml-command-node->command-flags node)
  (let ((attr (sxml:attr node 'flags)))
    (if attr
	(map (lambda (s)
	       (read (open-input-string s)))
	     (string-split attr ","))
	'())))

;; utility function to extract the range argument from a 'command' mdconf node.
;; returns #f if no range is set, supplies missing min/max args from numeric
;; range of the command.
(define (md:xml-command-node->range node)
  ;; immediately abort if no range subnode found
  (if (null? ((sxpath "range") node))
      #f
      (let ((range-node (car ((sxpath "range") node)))
            (lower-limit (lambda (cmd-type bits)
                           (if (equal? cmd-type md:cmd-type-int)
                               (- (expt 2 (- bits 1)))
                               0)))
            (upper-limit (lambda (cmd-type bits)
                           (- (if (equal? cmd-type md:cmd-type-int)
                                  (expt 2 (- bits 1))
                                  (expt 2 bits))
                              1))))
        (if (equal? '() range-node)
            #f
            (md:make-range
             (if (sxml:num-attr range-node 'min)
                 (sxml:num-attr range-node 'min)
                 (lower-limit (sxml:attr node 'type)
                              (sxml:num-attr node 'bits)))
             (if (sxml:num-attr range-node 'max)
                 (sxml:num-attr range-node 'max)
                 (upper-limit (sxml:attr node 'type)
                              (sxml:num-attr node 'bits))))))))

(define (md:xml-attr->tags attr)
  (if (not attr)
      #f
      (string-split (string-delete char-set:whitespace attr) ",")))

;; construct a hash table from a file containing key/value definitions
(define (md:mapfile->map filepath)
  (alist->hash-table
   (map (lambda (str)
          (let ((entry (string-split (string-delete char-set:whitespace str)
                                     "=")))
            (list (car entry) (string->number (cadr entry)))))
        (filter (lambda (x) (string-contains x "="))
                (call-with-input-file filepath read-lines)))))

;; construct a keymap from a 'command' mdconf node
;; returns a hash table or #f
(define (md:xml-command-node->map node configpath)
  (if (sxml:attr node 'map)
      (let ((attr (sxml:attr node 'map)))
        (cond ((string-ci= (substring/shared attr 0 5) "file(")
               (md:mapfile->map
                (string-append configpath
			       (substring/shared attr 5
						 (- (string-length attr) 1)))))
              ((string-ci= (substring/shared attr 0 5) "func(")
               (eval (read (open-input-string (substring/shared attr 4)))))
              (else #f)))
      #f))

;; construct an md:command object from a 'command' mdconf node and a md:target
(define (md:xml-node->command node target configpath)
  (let ((cmd-type (md:string->command-type (sxml:attr node 'type))))
    (md:make-command
     cmd-type
     (if (or (equal? cmd-type md:cmd-type-trigger)
             (equal? cmd-type md:cmd-type-label))
         0
         (sxml:num-attr node 'bits))
     (sxml:attr node 'default)
     (sxml:attr node 'to)
     (md:xml-command-node->map node configpath)
     (md:xml-command-node->command-flags node)
     (md:xml-attr->tags (sxml:attr node 'tags))
     (md:xml-command-node->range node)
     (if (equal? '() ((sxpath "description/text()") node))
         #f
         (car ((sxpath "description/text()") node))))))


;; construct an alist containing the default commands AUTHOR and TITLE
(define (md:make-default-commands)
  (list
   (list "AUTHOR" (md:make-command md:cmd-type-string 0 "unknown" #f #f
                                   '() #f #f #f))
   (list "TITLE" (md:make-command md:cmd-type-string 0 "untitled" #f #f
                                  '() #f #f #f))))


;; generate a hash-table of md:commands from a given list of mdconf 'command'
;; nodes and a given target. Also generates AUTHOR/TITLE commands if not
;; specified in node list
(define (md:xml-command-nodes->commands node-list target configpath)
  (alist->hash-table
   (append
    (letrec ((make-commands
              (lambda (lst trgt)
                (if (null? lst)
                    '()
                    (cons (list (sxml:attr (car lst) 'id)
                                (md:xml-node->command
                                 (car lst) trgt configpath))
                          (make-commands (cdr lst) trgt))))))
      (make-commands node-list target))
    (md:make-default-commands))))
