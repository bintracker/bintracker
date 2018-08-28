; (import (chibi))
; hash tables, bitwise arithmetic
; (import (srfi 125) (srfi 151))

(require-extension r7rs)
(require-extension ssax)
(require-extension sxpath)

(define *min-supported-version 2)
(define *max-supported-version 2)
(define *config*)
(define *module*)
(define *selection*)

(define **cpu-speed** 30000)
(load-relative "utils/note-tables.scm")

; -----------------------------------------------------------------------------
; MDCONF: ASSEMBLY SYNTAX RULES
; -----------------------------------------------------------------------------

(define-constant md:little-endian 0)
(define-constant md:big-endian 1)

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
; MDCONF: COMMANDS
; -----------------------------------------------------------------------------

(define-constant md:cmd-type-int 0)
(define-constant md:cmd-type-uint 1)
(define-constant md:cmd-type-key 2)
(define-constant md:cmd-type-ukey 3)
(define-constant md:cmd-type-note 4)
(define-constant md:cmd-type-reference 5)
(define-constant md:cmd-type-string 6)
(define-constant md:cmd-type-trigger 7)
(define-constant md:cmd-type-unknown 8)

(define-record-type md:command
  (md:make-command type bits default reference-to keys enable-modifiers
                   disable-labels range)
  md:command?
  (type md:command-type md:command-set-type!)
  (bits md:command-bits md:command-set-bits!)
  (default md:command-default md:command-set-default!)
  (reference-to md:command-reference-to md:command-set-reference-to!)
  (keys md:command-keys md:command-set-keys!)
  (enable-modifiers md:command-enable-modifiers
                    md:command-set-enable-modifiers!)
  (disable-labels md:command-disable-labels md:command-set-disable-labels!)
  (range md:command-range md:command-set-range!))

(define (md:make-int-command bits default enable-modifiers range)
  (md:make-command md:cmd-type-int bits default #f #f enable-modifiers
                   #t range))

(define (md:make-uint-command bits default enable-modifiers range)
  (md:make-command md:cmd-type-uint bits default #f #f enable-modifiers
                   #t range))

(define (md:make-key-command bits default keys enable-modifiers)
  (md:make-command md:cmd-type-key bits default #f keys enable-modifiers
                   #t #f))

(define (md:make-ukey-command bits default keys enable-modifiers)
  (md:make-command md:cmd-type-ukey bits default #f keys enable-modifiers
                   #t #f))

(define (md:make-note-command bits keys enable-modifiers)
  (md:make-command md:cmd-type-note bits "rest" #f keys
                   enable-modifiers #t #f))

(define (md:make-reference-command bits default reference-to disable-labels)
  (md:make-command md:cmd-type-reference bits default reference-to #f #f
                   disable-labels #f))

(define (md:make-string-command default)
  (md:make-command md:cmd-type-string 0 default #f #f #f #t #f))

(define (md:make-trigger-command)
  (md:make-command md:cmd-type-trigger 1 #f #f #f #t #f))

(define (md:int-command? cmd) (= (md:command-type cmd) md:cmd-type-int))
(define (md:uint-command? cmd) (= (md:command-type cmd) md:cmd-type-uint))
(define (md:key-command? cmd) (= (md:command-type cmd) md:cmd-type-key))
(define (md:ukey-command? cmd) (= (md:command-type cmd) md:cmd-type-ukey))
(define (md:note-command? cmd) (= (md:command-type cmd) md:cmd-type-note))
(define (md:reference-command? cmd)
  (= (md:command-type cmd) md:cmd-type-reference))
(define (md:string-command? cmd) (= (md:command-type cmd) md:cmd-type-string))
(define (md:trigger-command? cmd) (= (md:command-type cmd) md:cmd-type-trigger))

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
  ;...
  )
|#

; -----------------------------------------------------------------------------
; MDCONF: MASTER CONFIGURATION
; -----------------------------------------------------------------------------
#|
(define-record-type md:config
  (md:make-config asm-syntax target max-binsize inodes onodes)
  md:config?
  (asm-syntax md:config-asm-syntax md:config-set-asm-syntax!)
  (target md:config-target md:config-set-target!)
  (max-binsize md:config-max-binsize md:config-set-max-binsize!)
  ; ...
  )
|#

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
