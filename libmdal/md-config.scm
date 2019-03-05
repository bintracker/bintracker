;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-CONFIG

(module md-config *

  (import scheme chicken srfi-1 srfi-4 srfi-13 srfi-14 extras data-structures)
  (use srfi-69 simple-exceptions matchable
       ssax sxpath sxpath-lolevel
       md-helpers md-command md-note-table)
  (reexport md-command)



  ) ;; end module md-config
