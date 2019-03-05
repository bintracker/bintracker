;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of the libmdal library.
;; Copyright (c) utz/irrlicht project 2018
;; See LICENSE for license details.

;;; # Module MD-GLOBALS

(module md-globals *

  (import scheme chicken)
  (use srfi-69 md-helpers)

  ;; ---------------------------------------------------------------------------
  ;; MDAL: GLOBAL VARS
  ;; ---------------------------------------------------------------------------

  (define *supported-config-versions* (md:make-range 2 2))
  (define *supported-module-versions* (md:make-range 2 2))
  ;; (define *library-path* "")
  ;; (define *config-path* "config/")
  ;; (define *config*)
  ;; (define *module*)
  ;; (define *selection*)
  ;; (define *asm-syntax* (md:default-asm-syntax))

  ;; (define **cpu-speed** 30000)

  ) ;; end module md-globals
