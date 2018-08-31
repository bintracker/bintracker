; This file is part of the libmdal library.
; Copyright (c) utz/irrlicht project 2018
; See LICENSE for license details.

; (require-extension r7rs)
(use simple-exceptions ssax sxpath sxpath-lolevel hahn)


; -----------------------------------------------------------------------------
; MDAL: UTILITIES
; -----------------------------------------------------------------------------

(define-record-type md:range
  (md:make-range minimum maximum)
  md:range?
  (minimum md:range-min)
  (maximum md:range-max))

(define (md:in-range? val range)
  (and (>= val (md:range-min range))
       (<= val (md:range-max range))))

(define-record-type md:asm-syntax
  (make-md:asm-syntax hex-prefix byte-op word-op dword-op)
  md:asm-syntax?
  (hex-prefix md:asm-syntax-hex-prefix md:asm-syntax-set-hex-prefix!)
  (byte-op md:asm-syntax-byte-op md:asm-syntax-set-byte-op!)
  (word-op md:asm-syntax-word-op md:asm-syntax-set-word-op!)
  (dword-op md:asm-syntax-dword-op md:asm-syntax-set-dword-op!))

(define (md:default-asm-syntax)
  (make-md:asm-syntax "$" "db" "dw" "dl"))


; -----------------------------------------------------------------------------
; MDAL: GLOBAL VARS
; -----------------------------------------------------------------------------

(define *supported-versions* (md:make-range 2 2))
(define *library-path* "")
(define *config-path* "config/")
(define *config*)
(define *module*)
(define *selection*)
(define *asm-syntax* (md:default-asm-syntax))

(define **cpu-speed** 30000)
(load-relative "utils/note-tables.scm")


; -----------------------------------------------------------------------------
; MDCONF: TARGETS
; -----------------------------------------------------------------------------

(define md:little-endian 0)
(define md:big-endian 1)

(define-record-type md:cpu
  (md:make-cpu id endianness)
  md:cpu?
  (id md:cpu-id)
  (endianness md:cpu-endianness))

(define-record-type md:export-format
  (md:make-export-format id conversion-func)
  md:export-format?
  (id md:export-format-id)
  (conversion-func md:export-format-conversion-func))

(define-record-type md:target
  (md:make-target id cpu clock-speed export-formats)
  md:target?
  (id md:target-id)
  (cpu md:target-cpu)
  (clock-speed md:target-clock-speed)
  (export-format md:target-export-format))

; -----------------------------------------------------------------------------
; MDCONF: COMMANDS
; -----------------------------------------------------------------------------

(include "command.scm")

; -----------------------------------------------------------------------------
; MDCONF: INPUT NODE CONFIGURATION
; -----------------------------------------------------------------------------
; sub-nodes should be virtual (store id only)
; every node must have a unique id

; aux record type for tracking instantiation requirements of md:inode-config
(define-record-type md:instance-range
  (md:make-instance-range min-instances max-instances)
  md:instance-range?
  (min-instances md:instance-range-min md:set-instance-range-min)
  (max-instances md:instance-range-max md:set-instance-range-max))

(define-record-type md:inode-config
  (md:make-inode-config instance-range subnodes cmd-id order-id)
  md:inode-config?
  (instance-range md:inode-config-instance-range
                  md:set-inode-config-instance-range!)
  (subnodes md:inode-config-subnodes md:set-inode-config-subnodes!)
  (cmd-id md:inode-config-cmd-id md:set-inode-config-cmd-id!)
  (order-id md:inode-config-order-id md:set-inode-config-order-id!))

(define (md:inode-config-endpoint? inode-cfg)
  (if (md:inode-config-subnodes) #f #t))

; would need to pass block-member? in order to determine instance-range
; perhaps always need to pass range
(define (md:xml-ifield->inode-config node instance-range)
  (md:make-inode-config instance-range #f (sxml:attr node 'from) #f))

(define (md:xml-iblock->inode-config node instance-range)
  (md:make-inode-config instance-range #f #f #f))

(define (md:xml-igroup->inode-config node instance-range)
  (md:make-inode-config instance-range #f #f #f))

(define (md:xml-clone-node->inode-config node instance-range)
  (md:make-inode-config instance-range #f #f #f))

; dispatch function
(define (md:xml-node->inode-config node instance-range)
  (cond ((equal? (sxml:name node) "ifield")
         (md:xml-ifield->inode-config node instance-range))
        ((equal? (sxml:name node) "iblock")
         (md:xml-iblock->inode-config node instance-range))
        ((equal? (sxml:name node) "clone")
         (md:xml-clone-node->inode-config node instance-range))
        (else (md:xml-igroup->inode-config node instance-range))))

; TODO: generate orders, default inodes, clone blocks

; construct the input tree (forest) config from a given mdconf root node
(define (md:mdconf->inode-configs cfg-node)
  (list "GLOBAL" (md:make-inode-config (md:make-instance-range 1 1) #f #f #f)))

; -----------------------------------------------------------------------------
; MDCONF: OUTPUT NODE CONFIGURATION
; -----------------------------------------------------------------------------
; additional fields: fixed-length, max-length, min-instances, max-instances,
; sort-ascending, use-little-endian (aka override-endianness)
; order-layout reference-type
; order? list?
; some of these can probably be combined into a 'flags' field

; should perhaps prefer 'null' over #f where appropriate -> no, because it
; doesn't exist in chibi
; but should perhaps use empty list '()

(define-record-type md:onode-config
  (md:make-onode-config sources bytes sub-nodes composition-rule
                        requirement-condition)
  md:onode-config?
  )

; -----------------------------------------------------------------------------
; MDCONF: MASTER CONFIGURATION
; -----------------------------------------------------------------------------

; TODO: where to handle max-binsize?

(define-record-type md:config
  (md:make-config target description commands inodes onodes)
  md:config?
  (target md:config-target md:config-set-target!)
  (description md:config-description md:config-set-description!)
  (commands md:config-commands md:config-set-commands!)
  (inodes md:config-inodes md:config-set-inodes!)
  (onodes md:config-onodes md:config-set-onodes!))

(define-record-printer (md:config cfg out)
  (begin
    (fprintf out "#<md:config>\n\n")
    (when (md:config-description cfg)
      (fprintf out "DESCRIPTION:\n~A\n\n" (md:config-description cfg)))
    (fprintf out "COMMANDS:\n\n")
    (for-each (lambda (x)
                (fprintf out "~A: ~S\n\n" (car x) (cadr x)))
              (hash-table->alist (md:config-commands cfg)))
    (fprintf out "\nINPUT NODES:\n\n~S\n\n" (md:config-inodes cfg))))

; create an md:target from an mdconf root node
(define (md:config-node->target node)
  (eval (car (read-file (string-concatenate
                          (list "targets/"
                                (sxml:attr (car (sxml:content node)) 'target)
                                ".scm"))))))

; generate an md:config from a given .mdconf file
(define (md:mdconf->config filepath)
  (let ((cfg (ssax:xml->sxml (open-input-file filepath) '())))
    (let ((target (md:config-node->target cfg)))
      (md:make-config
        target
        (if (null-list? ((sxpath "mdalconfig/description") cfg))
            #f
            (car ((sxpath "mdalconfig/description/text()") cfg)))
        (md:xml-command-nodes->commands
          ((sxpath "mdalconfig/command") cfg) target)
        (md:mdconf->inode-configs cfg)
        #f    ;onodes
        ))))

; -----------------------------------------------------------------------------
; MDMOD: INPUT NODES
; -----------------------------------------------------------------------------
; instances?

#|
(define-record-type md:inode
  (make-md:inode cfg-id sub-nodes val is-active)
  md:inode?
  ; ...
  )
|#
; -----------------------------------------------------------------------------
; MDMOD: OUTPUT NODES
; -----------------------------------------------------------------------------
#|
(define-record-type md:onode
  (make-md:onode cfg-id sub-nodes instances val)
  ; ...
  )
|#
; -----------------------------------------------------------------------------
; MDMOD: MODULE
; -----------------------------------------------------------------------------
#|
(define-record-type md:module
  (make-md:module cfg input-nodes output-nodes)
  md:module?
  ; ...
  )
|#

; to parse: (read-lines "file.mdal")


#|
; -----------------------------------------------------------------------------
; MDMOD: INPUT FIELDS
; -----------------------------------------------------------------------------

(define-record-type md:field
  (make-md:field val is-active cfg-id)
  md:field?
  (val md:field-val md:field-set-val!)
  (is-active md:field-active md:field-set-active!)
  (cfg-id md:field-cfg md:field-set-cfg!))

(define (md:field-activate! field)
  (md:field-set-active! field #t))

(define (md:field-set! field val)
  (md:field-set-val! field val)
  (md:field-activate! field))

(define (md:field-clear! field)
  (md:field-set-val! field "")
  (md:field-set-active! field #f))

|#

