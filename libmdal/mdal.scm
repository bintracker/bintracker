;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

(module mdal *

  (import scheme (chicken base) (chicken module) srfi-4
	  md-config md-helpers md-types md-parser)
  (reexport md-config md-helpers md-types md-parser)

  ;;----------------------------------------------------------------------------
  ;;; ### output compilation
  ;;----------------------------------------------------------------------------

  ;;; compile an md:module to an onode tree
  ;;; TODO and a list of symbols for mod->asm?
  (define (md:mod-compile mod origin)
    ((md:config-compiler (md:mod-cfg mod)) mod origin))

  ;;; compile an md:module into a bytevec
  (define (md:mod->bin mod origin)
    (md:otree->bin (md:mod-compile mod origin)))

  ;;; compile an md:module into an assembly source
  (define (md:mod->asm mod origin)
    (let ((otree (md:mod-compile mod origin)))
      '()))

  ;;; compile the given md:module to a binary file
  (define (md:mod-export-bin filename mod origin)
    (call-with-output-file filename
      (lambda (port)
	(write-u8vector (list->u8vector (md:mod->bin mod origin))
			port))))

  ) ;; end module mdal
