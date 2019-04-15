#!/home/heinz/chickens/use-this/bin/csi -script
;; -*- geiser-scheme-implementation: 'chicken -*-

;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

(import scheme (chicken platform))
(repository-path (cons "libmdal" (repository-path)))
(import bintracker-core)
