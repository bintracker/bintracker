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

(define-record-type md:inode-config
  (md:make-inode-config instance-range subnodes cmd-id order-id)
  md:inode-config?
  (instance-range md:inode-config-instance-range
                  md:set-inode-config-instance-range!)
  (subnodes md:inode-config-subnodes md:set-inode-config-subnodes!)
  (cmd-id md:inode-config-cmd-id md:set-inode-config-cmd-id!)
  (order-id md:inode-config-order-id md:set-inode-config-order-id!))


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

#|
(define-record-type md:onode-config
  (md:make-onode-config sources bytes sub-nodes composition-rule
                        requirement-condition)
  md:onode-config?
  )
|#

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

; TODO: record-printer

; create an md:target from an mdconf root node
(define (md:config-node->target node)
  (eval (car (read-file (string-concatenate
                          (list "targets/"
                                (sxml:attr (car (sxml:content node)) 'target)
                                ".scm"))))))

; construct an alist containing the default commands AUTHOR and TITLE
(define (md:make-default-commands)
  (list
    (list "AUTHOR" (md:make-command md:cmd-type-string 0 "unknown" #f #f
                                    (md:make-empty-command-flags) #f #f))
    (list "TITLE" (md:make-command md:cmd-type-string 0 "untitled" #f #f
                                   (md:make-empty-command-flags) #f #f))))

; generate a hash-table of md:commands from a given list of mdconf 'command'
; nodes and a given target. Also generates AUTHOR/TITLE commands if not
; specified in node list
(define (md:xml-command-nodes->commands node-list target)
  (alist->hash-table
    (append
      (letrec ((make-commands
                 (lambda (lst trgt)
                   (if (null-list? lst)
                       '()
                       (cons (list (sxml:attr (car lst) 'id)
                                   (md:xml-node->command (car lst) trgt))
                             (make-commands (cdr lst) trgt))))))
        (make-commands node-list target))
      (md:make-default-commands))))

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
        #f    ;inodes
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

