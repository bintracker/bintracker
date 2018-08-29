; This file is part of the libmdal library.
; Copyright (c) utz/irrlicht project 2018
; See LICENSE for license details.

; (require-extension r7rs)
(use simple-exceptions ssax sxpath sxpath-lolevel hahn)

@(heading "libmdal")

(define *min-supported-version 2)
(define *max-supported-version 2)
(define *config*)
(define *module*)
(define *selection*)

(define **cpu-speed** 30000)
(load-relative "utils/note-tables.scm")


; -----------------------------------------------------------------------------
; MDCONF: UTILITIES
; -----------------------------------------------------------------------------

(define-record-type md:range
  (md:make-range minimum maximum)
  md:range?
  (minimum md:range-min)
  (maximum md:range-max))

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

